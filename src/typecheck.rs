use crate::annotate::Annotated;
use crate::data::*;
use crate::error::*;
use im::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

pub type TypeResult = Result<Type, TypeError>;

pub fn check_program(defs: Vec<Box<Annotated<Defn>>>) -> Result<TEnv, TypeError> {
    use Type::*;
    let mut env: TEnv = vec![
        ("delay".into(), Type::CloT(Box::new(NumT), Box::new(UnitT))),
        ("print".into(), Type::CloT(Box::new(AnyT), Box::new(UnitT))),
        ("random".into(), Type::CloT(Box::new(UnitT), Box::new(NumT))),
        ("round".into(), Type::CloT(Box::new(NumT), Box::new(NumT))),
    ]
    .into_iter()
    .collect();
    // let env = defs.iter().fold(env, init_type_in_scope)
    let env = defs.iter().fold(env, check_data_defs);
    // let _ = defs.iter().map(env, );
    for def in defs {
        check_var_defs(&env, &def)?
    };

    Ok(env)
}

fn simplify(t: Type, env: &TEnv) -> Type {
    match t {
        Type::TypeVar(name) => env.get(&name).cloned().unwrap_or(Type::UnknownT),
        // Type::MutT(t) | Type::RefT(t) => Type::MutT(Box::new(simplify(env, t))),
        // Type::CloT(t_arg, t_body) => {
        //     let t_arg = Box::new(simplify(env, *t_arg));
        //     let t_body = Box::new(simplify(env, *t_body));
        //     Type::CloT(t_arg, t_body)
        // }
        // Type::ObjectT =>
        // Type::StructT =>
        // Type::EnumT =>
        _ => t
    }
}

fn equiv(env: &TEnv, t1: &Type, t2: &Type) -> bool {
    match (t1, t2) {
        (Type::AnyT, _) => true,
        (_, Type::AnyT) => true,
        (Type::TypeVar(lhs), Type::TypeVar(rhs)) if lhs == rhs => true,
        _ => simplify(t1.clone(), env) == simplify(t2.clone(), env)
    }
}

fn check_data_defs(env: TEnv, def: &Box<Annotated<Defn>>) -> TEnv {
    match def.clone().unwrap() {
        Defn::StructD(name, fields) => env.update(name.clone(), Type::StructT(name, fields)),
        Defn::EnumD(enum_name, variants) => {
            let env = env.update(enum_name.clone(), Type::EnumT(enum_name.clone(), variants.clone()));
            let enum_t = Type::TypeVar(enum_name);
            env.union(
                variants.iter()
                    .map(|(name, ts)| {
                        (name.clone(), ts.iter().rev().fold(
                            enum_t.clone(),
                            |acc, t| Type::CloT(Box::new(t.clone()), Box::new(acc))
                        ))
                    })
                    .collect()
            )
        },
        Defn::VarD(name, _, rt, _) => env.update(name.clone(), rt.clone()),
        // Defn::PrimD(name, mut xs) => env.update(name, v)
        _ => env
    }
}

fn check_var_defs(env: &TEnv, def: &Box<Annotated<Defn>>) -> Result<(), TypeError> {
    use Defn::*;
    match def.clone().unwrap() {
        Defn::VarD(name, args, rt, body) => {
            let arg_ts = rt.clone().into_iter().take(args.len());
            let env_body = args.iter()
                .zip(arg_ts)
                .fold(env.clone(), |env, (x, t)| env.update(x.clone(), t));

            let t_expected = rt.into_iter().skip(args.len()).collect();
            let t_body = check_expr(body, &env_body)?;
            if !equiv(&env, &t_body, &t_expected) {
                return Err(TypeError {
                    kind: TypeErrorKind::UnexpectedTypeError(t_expected, t_body),
                    idx: def.idx,
                    len: def.len,
                    loc: None,
                })
            }
        },
        _ => ()
    };
    Ok(())
}

pub fn check_expr(e: Box<Annotated<Expr>>, env: &TEnv) -> TypeResult {
    use Expr::*;
    use TypeErrorKind::*;
    use Type::*;
    let check = check_expr;

    let err_idx = e.idx;
    let err_len = e.len;
    let e = e.unwrap();

    macro_rules! err {
        ($variant:expr) => {
            TypeError {
                kind: $variant,
                idx: err_idx,
                len: err_len,
                loc: None,
            }
        };
    }

    // println!("{:?}", e);
    match e {
        Expr::NumE(i) => Ok(NumT),
        Expr::PlusE(e1, e2) => match (check(e1, env)?, check(e2, env)?) {
            (NumT, NumT) => Ok(NumT),
            (StringT, StringT) => Ok(StringT),
            (StringT, NumT) => Ok(StringT),
            (StringT, BoolT) => Ok(StringT),
            (StringT, ObjectT(_)) => Ok(StringT),
            (StringT, StructT(_, _)) => Ok(StringT),
            (StringT, MutT(_)) => Ok(StringT),
            _ => Err(err!(TempFiller(1))),
        },
        Expr::MinusE(e1, e2) |
        Expr::MultE(e1, e2) |
        Expr::DivE(e1, e2) => match (check(e1, env)?, check(e2, env)?) {
            (NumT, NumT) => Ok(NumT),
            _ => Err(err!(TempFiller(2))),
        },
        Expr::LtE(e1, e2) |
        Expr::GtE(e1, e2) => match (check(e1, env)?, check(e2, env)?) {
            (NumT, NumT) => Ok(BoolT),
            _ => Err(err!(TempFiller(3))),
        },
        Expr::NegE(e) => match check(e, env)? {
            NumT => Ok(NumT),
            _ => Err(err!(TempFiller(4))),
        },
        Expr::BoolE(b) => Ok(BoolT),
        Expr::AndE(e1, e2) | Expr::OrE(e1, e2) =>
            match (check(e1, env)?, check(e2, env)?) {
                (BoolT, BoolT) => Ok(BoolT),
                _ => Err(err!(TempFiller(5))),
            },
        Expr::EqE(e1, e2) => match (check(e1, env)?, check(e2, env)?) {
            (BoolT, BoolT) | (NumT, NumT) => Ok(BoolT),
            _ => Err(err!(TempFiller(6))),
        },
        Expr::IfE(pred, thn, els) => match check(pred, env)? {
            BoolT => {
                let t_thn = check(thn, env)?;
                let t_els = check(els, env)?;
                if equiv(env, &t_thn, &t_els) {
                    Ok(t_thn)
                } else {
                    Err(err!(TempFiller(7)))
                }
            },
            _ => Err(err!(TempFiller(8))),
        },
        Expr::StringE(s) => Ok(StringT),
        Expr::UnitE => Ok(UnitT),
        Expr::VarE(x) => env.get(&x).cloned().ok_or_else(|| err!(UndefinedError(x))),
        Expr::LetE(x, e, body) => {
            let t = check(e, env)?;
            let next_env = env.update(x, t);
            check(body, &next_env)
        }
        Expr::MutableE(x, e, body) => {
            let t = check(e, env)?;
            let t = MutT(Box::new(t));
            let next_env = env.update(x, t);
            check(body, &next_env)
        }
        Expr::MutateE(xe, e) => {
            let t_next = check(e, env)?;
            if let MutT(t_old) = check(xe, env)? {
                if equiv(env, &t_next, &*t_old) {
                    Ok(UnitT)
                } else {
                    Err(err!(TypeErrorKind::TempFiller(9)))
                }
            } else {
                Err(err!(TypeErrorKind::TempFiller(10)))
            }
        }
        Expr::DerefE(e) => {
            match check(e, env)? {
                MutT(inner) | RefT(inner) => Ok(*inner.clone()),
                t => Err(err!(DerefError(t))),
            }
        }
        Expr::SeqE(e1, e2) => {
            check(e1, env)?;
            check(e2, env)
        }
        Expr::FnE(name, x, t, body) => Ok(CloT(Box::new(t), Box::new(check(body, env)?))),
        Expr::AppE(f, param) => match (check(f, env)?, check(param, env)?) {
            (CloT(i, o), arg_t) => {
                if equiv(env, &*i, &arg_t) {
                    Ok(*o.clone())
                } else {
                    Err(err!(ApplicationError(*i.clone(), arg_t.clone())))
                }},
            // (PrimT(name), arg_v) => check_prim(name, vec![arg_v]),
            _ => Err(err!(TempFiller(12))),
        },
        Expr::WhileE(pred, body) => {
            match check(pred.clone(), env)? {
                Type::BoolT => (),
                _ => return Err(err!(TempFiller(13)))
            };
            check(body.clone(), env)?;
            Ok(UnitT)
        }
        Expr::InitObjectE(fields) => {
            let mut field_ts = HashMap::new();
            for (x, e) in fields.into_iter() {
                field_ts.insert(x, check(e, env)?);
            }

            Ok(ObjectT(field_ts))
        }
        Expr::InitStructE(name, fields) => {
            let allowed_fields = match env.get(&name) {
                Some(StructT(name, fields)) => fields,
                _ => return Err(err!(UndefinedError(name))),
            };
            let mut field_ts = HashMap::new();
            for (x, e) in fields.into_iter() {
                if !allowed_fields.contains_key(&x) {
                    return Err(err!(ExtraFieldError(x)));
                }
                field_ts.insert(x, check(e, env)?);
            }
            let keys: Vec<&String> = field_ts.keys().collect();
            for x in allowed_fields.iter() {
                if !keys.contains(&x.0) {
                    return Err(err!(MissingFieldError(x.0.clone())));
                }
            }
            Ok(Type::TypeVar(name))
        }
        Expr::AccessE(e, x) => {
            let mut t = check(e, env)?;
            if let TypeVar(name) = t {
                t = env.get(&name).cloned().ok_or_else(|| err!(UndefinedError(name)))?;
            }
            match t {
                ObjectT(fields) | StructT(_, fields) =>
                    fields.get(&x).cloned().ok_or_else(|| err!(UndefinedError(x))),
                _ => return Err(err!(TempFiller(14))),
            }
        }
        Expr::MatchE(subject, patterns) => {
            use MatchPattern::*;
            let subject = check(subject, env)?;
            let mut arm_ts: Vec<Type> = Vec::with_capacity(patterns.len());
            for (pattern, body) in patterns.iter() {
                if let Some(extension) = match_subject(&subject, pattern, env) {
                    let extended_env: TEnv = extension.union(env.clone());
                    let arm_t = check(body.clone(), &extended_env)?;
                    arm_ts.push(arm_t);
                } else {
                    return Err(err!(TempFiller(16)))
                }
            }
            let first = arm_ts.pop().unwrap();
            while arm_ts.len() > 0 {
                if !equiv(env, &first, &arm_ts.pop().unwrap()) {
                    return Err(err!(TempFiller(17)))
                }
            }
            Ok(first)
        }
        _ => Err(err!(UnimplementedBehavior)),
    }
}

// TODO: return Result instead of Option
fn match_subject(subject: &Type, pattern: &MatchPattern, env: &TEnv) -> Option<TEnv> {
    let subject = simplify(subject.clone(), env);
    match (&subject, pattern) {
        (Type::EnumT(enum_name, variants), MatchPattern::VariantPat(variant_name, patterns)) => {
            let ts = variants.get(variant_name)?;
            if ts.len() != patterns.len() {
                return None;
            }
            ts.iter()
                .zip(patterns)
                .map(|(s, p)| match_subject(s, p, env))
                .collect::<Option<Vec<TEnv>>>()
                .map(|envs| {
                    envs.into_iter().fold(HashMap::new(), HashMap::union)
                })
        }
        (Type::StringT, MatchPattern::StringPat(_)) => {
            Some(HashMap::new())
        }
        (Type::NumT, MatchPattern::NumPat(_)) => Some(HashMap::new()),
        (value, MatchPattern::AnyPat(name)) => {
            Some(HashMap::unit(name.clone(), value.clone()))
        }
        _ => None,
    }
}

// fn interp_prim(name: String, values: Vec<Value>) -> TypeResult {
//     use InterpErrorKind::*;
//     use Value::*;
//     match name.as_str() {
//         "print" => {
//             if let Some(v) = values.first() {
//                 match v {
//                     StringV(s) => println!("{}", s),
//                     _ => println!("{}", v),
//                 }
//             } else {
//                 println!("None");
//             }
//             Ok(PrimV("print".into()))
//         }
//         "delay" => {
//             use std::{thread, time};
//             if let Some(NumV(n)) = values.first() {
//                 let ten_millis = time::Duration::from_millis(*n as u64);
//                 thread::sleep(ten_millis);
//                 Ok(UnitV)
//             } else {
//                 Ok(PrimV("delay".into()))
//             }
//         }
//         "random" => Ok(NumV(rand::random())),
//         "round" => {
//             use std::{thread, time};
//             if let Some(NumV(n)) = values.first() {
//                 Ok(NumV(n.round()))
//             } else {
//                 Ok(PrimV("round".into()))
//             }
//         }
//         _ => panic!("no primitive named: {:?}", name),
//     }
// }
