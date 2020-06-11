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
    let env = defs.iter().try_fold(env, check_fn_defs);

    // let env_cell = RefCell::new(env);
    // let env_rc = unsafe { Rc::from_raw(env_cell.as_ptr()) };
    //
    // let denv: DefFnEnv = defs.into_iter().fold(HashMap::new(), interp_fn_defs);
    // for (name, (arg, body)) in denv.into_iter() {
    //     env_cell.borrow_mut().insert(
    //         name.clone(),
    //         Value::CloV(Some(name), arg, body, env_rc.clone()),
    //     );
    // }
    env
}

// fn simplify(env: &TEnv, t: &Type) -> Type {
//     match t {
//         Type::TypeVar(name) => env.get(name).cloned().unwrap_or(Type::UnknownT),
//         Type::MutT(t) | Type::RefT(t) => Type::MutT(Box::new(simplify(env, t))),
//         Type::CloT(t_arg, t_body) => {
//             let t_arg = Box::new(simplify(env, *t_arg));
//             let t_body = Box::new(simplify(env, *t_body));
//             Type::CloT(t_arg, t_body)
//         }
//         Type::ObjectT =>
//         Type::StructT =>
//         Type::EnumT =>
//         _ => t
//     }
// }

fn equiv(env: &TEnv, t1: &Type, t2: &Type) -> bool {
    // simplify(env, t1) == simplify(env, t2)
    match (t1, t2) {
        (Type::AnyT, _) => true,
        (_, Type::AnyT) => true,
        // (Type::TypeVar(lhs), Type::TypeVar(rhs)) if lhs == rhs => true,
        _ => t1 == t2
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
        // Defn::PrimD(name, mut xs) => env.update(name, v)
        _ => env
    }
}

fn check_fn_defs(env: TEnv, def: &Box<Annotated<Defn>>) -> Result<TEnv, TypeError> {
    use Defn::*;
    match def.clone().unwrap() {
        FnD(name, x, t, body) => {
            let t_body = check_expr(body, &env.update(x, t.clone()))?;
            Ok(env.update(name, Type::CloT(Box::new(t), Box::new(t_body))))
        },
        _ => Ok(env),
    }
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
            Err(TypeError {
                kind: $variant,
                idx: err_idx,
                len: err_len,
                loc: None,
            })
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
            _ => err!(TempFiller(1)),
        },
        Expr::MinusE(e1, e2) |
        Expr::MultE(e1, e2) |
        Expr::DivE(e1, e2) => match (check(e1, env)?, check(e2, env)?) {
            (NumT, NumT) => Ok(NumT),
            _ => err!(TempFiller(2)),
        },
        Expr::LtE(e1, e2) |
        Expr::GtE(e1, e2) => match (check(e1, env)?, check(e2, env)?) {
            (NumT, NumT) => Ok(BoolT),
            _ => err!(TempFiller(3)),
        },
        Expr::NegE(e) => match check(e, env)? {
            NumT => Ok(NumT),
            _ => err!(TempFiller(4)),
        },
        Expr::BoolE(b) => Ok(BoolT),
        Expr::AndE(e1, e2) | Expr::OrE(e1, e2) =>
            match (check(e1, env)?, check(e2, env)?) {
                (BoolT, BoolT) => Ok(BoolT),
                _ => err!(TempFiller(5)),
            },
        Expr::EqE(e1, e2) => match (check(e1, env)?, check(e2, env)?) {
            (BoolT, BoolT) | (NumT, NumT) => Ok(BoolT),
            _ => err!(TempFiller(6)),
        },
        Expr::IfE(pred, thn, els) => match check(pred, env)? {
            BoolT => {
                let t_thn = check(thn, env)?;
                let t_els = check(els, env)?;
                if equiv(env, &t_thn, &t_els) {
                    Ok(t_thn)
                } else {
                    err!(TempFiller(7))
                }
            },
            _ => err!(TempFiller(8)),
        },
        Expr::StringE(s) => Ok(StringT),
        Expr::UnitE => Ok(UnitT),
        Expr::VarE(x) => env.get(&x).cloned().ok_or_else(|| TypeError {
            kind: UndefinedError(x),
            idx: err_idx,
            len: err_len,
            loc: None,
        }),
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
                    err!(TypeErrorKind::TempFiller(9))
                }
            } else {
                err!(TypeErrorKind::TempFiller(10))
            }
        }
        Expr::DerefE(e) => {
            match check(e, env)? {
                MutT(inner) | RefT(inner) => Ok(*inner.clone()),
                t => err!(DerefError(t)),
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
                    err!(ApplicationError(*i.clone(), arg_t.clone()))
                }},
            // (PrimT(name), arg_v) => check_prim(name, vec![arg_v]),
            _ => err!(TempFiller(12)),
        },
        Expr::WhileE(pred, body) => {
            match check(pred.clone(), env)? {
                Type::BoolT => (),
                _ => return err!(TempFiller(13))
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
                _ => return err!(UndefinedError(name)),
            };
            let mut field_ts = HashMap::new();
            for (x, e) in fields.into_iter() {
                if !allowed_fields.contains_key(&x) {
                    return err!(ExtraFieldError(x));
                }
                field_ts.insert(x, check(e, env)?);
            }
            let keys: Vec<&String> = field_ts.keys().collect();
            for x in allowed_fields.iter() {
                if !keys.contains(&x.0) {
                    return err!(MissingFieldError(x.0.clone()));
                }
            }
            Ok(Type::TypeVar(name))
        }
        Expr::AccessE(e, x) => {
            match check(e, env)? {
                ObjectT(fields) | StructT(_, fields) => fields.get(&x).cloned().ok_or_else(|| TypeError {
                    kind: UndefinedError(x),
                    idx: err_idx,
                    len: err_len,
                    loc: None,
                }),
                _ => return err!(TempFiller(14)),
            }
        }
        // Expr::MatchE(subject, patterns) => {
        //     use MatchPattern::*;
        //     let subject = interp(subject, env)?;
        //     for (pattern, body) in patterns.iter() {
        //         if let Some(extension) = match_subject(&subject, pattern) {
        //             let extended_env: Env = extension.union(env.clone());
        //             return interp_expr(body.clone(), &extended_env);
        //         }
        //     }
        //     err!(NoMatchError(subject))
        // }
        _ => err!(UnimplementedBehavior),
    }
}

// fn match_subject(subject: &Value, pattern: &MatchPattern) -> Option<Env> {
//     match (subject, pattern) {
//         (Value::EnumV(enum_name, variant, vs), MatchPattern::VariantPat(variant_exp, patterns))
//             if patterns.len() == vs.len() && variant == variant_exp =>
//         {
//             vs.iter()
//                 .zip(patterns)
//                 .map(|(s, p)| match_subject(s, p))
//                 .collect::<Option<Vec<Env>>>()
//                 .map(|envs| {
//                     envs.iter().fold(HashMap::new(), |acc, env| acc.union(env.clone()))
//                 })
//         }
//         (Value::StringV(str_v), MatchPattern::StringPat(str_p)) if str_v == str_p => {
//             Some(HashMap::new())
//         }
//         (Value::NumV(num_v), MatchPattern::NumPat(num_p)) if num_v == num_p => Some(HashMap::new()),
//         (value, MatchPattern::AnyPat(name)) => {
//             // TODO: HashMap::singleton doesn't exist?
//             Some(HashMap::new().update(name.clone(), value.clone()))
//         }
//         _ => None,
//     }
// }
//
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
