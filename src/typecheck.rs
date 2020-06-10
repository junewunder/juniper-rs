use crate::annotate::Annotated;
use crate::data::*;
use crate::error::*;
use im::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

pub type TypeResult = std::result::Result<Type, TypeError>;

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
            // TODO: (StringT, s2 @ ObjectV(_, _)) => Ok(StringT),
            // TODO: (StringV(s1), MutV(s2)) => Ok(StringT),
            _ => err!(TempFiller),
        },
        Expr::MinusE(e1, e2) |
        Expr::MultE(e1, e2) |
        Expr::DivE(e1, e2) => match (check(e1, env)?, check(e2, env)?) {
            (NumT, NumT) => Ok(NumT),
            _ => err!(TempFiller),
        },
        Expr::LtE(e1, e2) |
        Expr::GtE(e1, e2) => match (check(e1, env)?, check(e2, env)?) {
            (NumT, NumT) => Ok(BoolT),
            _ => err!(TempFiller),
        },
        Expr::NegE(e) => match check(e, env)? {
            NumT => Ok(NumT),
            _ => err!(TempFiller),
        },
        Expr::BoolE(b) => Ok(BoolT),
        Expr::AndE(e1, e2) | Expr::OrE(e1, e2) =>
            match (check(e1, env)?, check(e2, env)?) {
                (BoolT, BoolT) => Ok(BoolT),
                _ => err!(TempFiller),
            },
        Expr::EqE(e1, e2) => match (check(e1, env)?, check(e2, env)?) {
            (BoolT, BoolT) | (NumT, NumT) => Ok(BoolT),
            _ => err!(TempFiller),
        },
        Expr::IfE(pred, thn, els) => match check(pred, env)? {
            BoolT => {
                let t_thn = check(thn, env)?;
                let t_els = check(els, env)?;
                if t_thn == t_els {
                    Ok(t_thn)
                } else {
                    err!(TempFiller)
                }
            },
            _ => err!(TempFiller),
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
            // TODO
            // let v = check(e, env)?;
            // if let MutT(t) = check(xe, env)? {
            //     x.replace(Box::new(v));
            // }
            Ok(UnitT)
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
        // TODO: functions
        // Expr::FnE(name, x, body) => Ok(CloT(name, UnitT/*TODO:get type from param*/, check(body))),
        // Expr::AppE(f, param) => match (interp(f, env)?, interp(param, env)?) {
        //     (CloV(_, arg, body, mut local_env), arg_v) => {
        //         interp(body, &local_env.update(arg, arg_v))
        //     }
        //     (PrimV(name), arg_v) => interp_prim(name, vec![arg_v]),
        //     _ => err!(TypeError),
        // },
        // Expr::AppPrimE(f, xs) => {
        //     interp_prim(f, xs.iter().map(|x| env.get(x).cloned().unwrap()).collect())
        // }
        Expr::WhileE(pred, body) => {
            check(pred.clone(), env)?;
            check(body.clone(), env)?;
            Ok(UnitT)
        }
        Expr::InitObjectE(fields) => {
            let mut field_vs = HashMap::new();
            for (x, e) in fields.into_iter() {
                field_vs.insert(x, Rc::new(interp(e, env)?));
            }

            Ok(ObjectV(None, field_vs))
        }
        Expr::InitStructE(name, fields) => {
            let allowed_fields = match env.get(&name) {
                Some(StructV(name, fields)) => fields,
                _ => return err!(UndefinedError(name)),
            };
            let mut field_vs = HashMap::new();
            for (x, e) in fields.into_iter() {
                if !allowed_fields.contains(&x) {
                    return err!(ExtraFieldError(x));
                }
                field_vs.insert(x, Rc::new(interp(e, env)?));
            }

            let keys: Vec<&String> = field_vs.keys().collect();
            for x in allowed_fields.iter() {
                if !keys.contains(&x) {
                    return err!(MissingFieldError(x.clone()));
                }
            }

            Ok(ObjectV(Some(name), field_vs))
        }
        Expr::InitEnumVariantE(enum_name, variant, args) => {
            let args = args
                .iter()
                .map(|arg| env.get(arg).unwrap().clone())
                .collect();
            Ok(EnumV(enum_name, variant, args))
        }
        Expr::AccessE(e, x) => {
            let v = match interp(e, env)? {
                ObjectV(_, fields) => fields.get(&x).cloned().ok_or_else(|| InterpError {
                    kind: UndefinedError(x),
                    idx: err_idx,
                    len: err_len,
                    loc: None,
                })?,
                _ => return err!(TypeError),
            };

            Ok(RefV(v))
        }
        Expr::MatchE(subject, patterns) => {
            use MatchPattern::*;
            // TODO: implement a recursive version of this as a separate function
            let subject = interp(subject, env)?;
            for (pattern, body) in patterns.iter() {
                if let Some(extension) = match_subject(&subject, pattern) {
                    let extended_env: Env = extension.union(env.clone());
                    return interp_expr(body.clone(), &extended_env);
                }
            }
            err!(NoMatchError(subject))
        }
        _ => err!(UnimplementedBehavior),
    }
}

fn match_subject(subject: &Value, pattern: &MatchPattern) -> Option<Env> {
    match (subject, pattern) {
        (Value::EnumV(enum_name, variant, vs), MatchPattern::VariantPat(variant_exp, patterns))
            if patterns.len() == vs.len() && variant == variant_exp =>
        {
            vs.iter()
                .zip(patterns)
                .map(|(s, p)| match_subject(s, p))
                .collect::<Option<Vec<Env>>>()
                .map(|envs| {
                    envs.iter().fold(HashMap::new(), |acc, env| acc.union(env.clone()))
                })
        }
        (Value::StringV(str_v), MatchPattern::StringPat(str_p)) if str_v == str_p => {
            Some(HashMap::new())
        }
        (Value::NumV(num_v), MatchPattern::NumPat(num_p)) if num_v == num_p => Some(HashMap::new()),
        (value, MatchPattern::AnyPat(name)) => {
            // TODO: HashMap::singleton doesn't exist?
            Some(HashMap::new().update(name.clone(), value.clone()))
        }
        _ => None,
    }
}

fn interp_prim(name: String, values: Vec<Value>) -> InterpResult {
    use InterpErrorKind::*;
    use Value::*;
    match name.as_str() {
        "print" => {
            if let Some(v) = values.first() {
                match v {
                    StringV(s) => println!("{}", s),
                    _ => println!("{}", v),
                }
            } else {
                println!("None");
            }
            Ok(PrimV("print".into()))
        }
        "delay" => {
            use std::{thread, time};
            if let Some(NumV(n)) = values.first() {
                let ten_millis = time::Duration::from_millis(*n as u64);
                thread::sleep(ten_millis);
                Ok(NullV)
            } else {
                Ok(PrimV("delay".into()))
            }
        }
        "random" => Ok(NumV(rand::random())),
        "round" => {
            use std::{thread, time};
            if let Some(NumV(n)) = values.first() {
                Ok(NumV(n.round()))
            } else {
                Ok(PrimV("round".into()))
            }
        }
        _ => panic!("no primitive named: {:?}", name),
    }
}
