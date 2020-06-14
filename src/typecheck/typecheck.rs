use crate::typecheck::*;
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
    // println!("{:?}", );
    let env = defs.iter().fold(env, check_data_defs);
    println!("{}", print_tenv(&env));
    // dbg!(&env);
    for def in defs {
        check_var_defs(&env, &def)?
    };

    Ok(env)
}

fn check_data_defs(env: TEnv, def: &Box<Annotated<Defn>>) -> TEnv {
    match def.clone().unwrap() {
        Defn::StructD(name, t_params, fields) => env.update(
            name.clone(),
            wrap_generics_defn(Type::StructT(name, fields), t_params)
        ),
        Defn::EnumD(enum_name, t_params, variants) => {
            let mut env = env.update(
                enum_name.clone(),
                wrap_generics_defn(Type::EnumT(enum_name.clone(), variants.clone()), t_params.clone())
            );
            let enum_t = Type::TypeVar(enum_name);
            for (name, ts) in variants.iter() {
                let constructor = ts.iter().rev().fold(
                    wrap_concrete_defn(enum_t.clone(), t_params.clone()),
                    |acc, t| Type::CloT(box t.clone(), box acc)
                );
                env.insert(name.clone(), wrap_generics_defn(constructor, t_params.clone()));
            }
            env
        },
        Defn::VarD(name, t_args, rt, _) => env.update(name.clone(), rt.clone()),
        // Defn::PrimD(name, mut xs) => env.update(name, v)
        _ => env
    }
}

fn check_var_defs(env: &TEnv, def: &Box<Annotated<Defn>>) -> Result<(), TypeError> {
    use Defn::*;
    // dbg!(def);
    match def.clone().unwrap() {
        Defn::VarD(name, args, rt, body) => {
            // println!("{:?}", args);
            // println!("{:?}", rt);
            let (env, rt_inner) = peel_generics_defn(env, rt);
            let arg_ts = rt_inner.clone().into_iter().take(args.len());
            println!("HELLO {:?}", rt_inner.clone().into_iter().take(args.len()).collect::<Vec<_>>());
            let env_body = args.iter()
                .zip(arg_ts)
                .fold(env.clone(), |env, (x, t)| env.update(x.clone(), t));

            let t_expected = rt_inner.into_iter().skip(args.len()).collect();
            dbg!(&env_body);
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
    Ok(simplify(match e {
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
        Expr::FnE(name, x, t, body) => Ok(CloT(box t, box check(body, env)?)),
        Expr::AppE(f, param) => {
            let f = check(f.clone(), env)?;
            let param = check(param.clone(), env)?;
            match (f, param) {
            (CloT(i, o), arg_t) => {
                if equiv(env, &*i, &arg_t) {
                    Ok(*o)
                } else {
                    println!("tenv {:?}", print_tenv(env));
                    Err(err!(ApplicationError(CloT(i, o), arg_t)))
                }},
            _ => {
                Err(err!(TempFiller(12)))
            },
        }},

        Expr::TypeAnnotationE(e, t) => {
            Ok(ConcreteT(box check(e, env)?, box t))
        }
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
                let t_cur = arm_ts.pop().unwrap();
                if !equiv(env, &first, &t_cur) {
                    return Err(err!(TypeErrorKind::UnexpectedTypeError(first, t_cur)))
                }
            }
            Ok(first)
        }
        _ => Err(err!(UnimplementedBehavior)),
    }?, env))
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
        _ => None
    }
}
