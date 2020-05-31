use crate::annotate::Annotated;
use crate::data::*;
use crate::error::*;
use im::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

type DefFnEnv = HashMap<String, (String, Box<Annotated<Expr>>)>;
pub type InterpResult = std::result::Result<Value, InterpError>;

pub fn interp_program(defs: Vec<Box<Annotated<Defn>>>) -> InterpResult {
    let mut env: Env = vec![
        ("delay".into(), Value::PrimV("delay".into())),
        ("print".into(), Value::PrimV("print".into())),
        ("random".into(), Value::PrimV("random".into())),
        ("round".into(), Value::PrimV("round".into())),
    ]
    .into_iter()
    .collect();

    let env = defs.iter().fold(env, interp_scopeless_defs);

    let env_cell = RefCell::new(env);
    let env_rc = unsafe { Rc::from_raw(env_cell.as_ptr()) };

    let denv: DefFnEnv = defs.into_iter().fold(HashMap::new(), interp_fn_defs);
    for (name, (arg, body)) in denv.into_iter() {
        env_cell.borrow_mut().insert(
            name.clone(),
            Value::CloV(Some(name), arg, body, env_rc.clone()),
        );
    }

    // println!("{}", print_env_safe(&env_rc));
    if let Some(Value::CloV(_, arg, body, env)) = env_rc.get("main".into()) {
        return interp_expr(body.clone(), &env);
    }

    panic!("no <main> method")
}

fn interp_fn_defs(denv: DefFnEnv, def: Box<Annotated<Defn>>) -> DefFnEnv {
    use Defn::*;
    match def.unwrap() {
        FnD(name, x, expr) => denv.update(name, (x, expr)),
        _ => denv,
    }
}

fn interp_scopeless_defs(env: Env, def: &Box<Annotated<Defn>>) -> Env {
    use Defn::*;
    use Value::*;
    match def.cloned() {
        StructD(name, fields) => {
            env.update(name.clone(), Value::StructV(name.clone(), fields.clone()))
        }
        PrimD(name, mut xs) => {
            let app = box Expr::AppPrimE(name.clone(), xs.clone());
            let arg0 = xs.remove(0);
            let body = xs.iter().fold(box Annotated::zero(*app), |acc, x| {
                box Annotated::zero(Expr::FnE(None, x.into(), acc))
            });
            env.update(name, CloV(None, arg0, body, Rc::new(HashMap::new())))
        }
        EnumD(enum_name, variants) => variants.iter().fold(env, |env, (name, xs)| {
            if xs.len() == 0 {
                return env.update(
                    name.clone(),
                    Value::EnumV(enum_name.clone(), name.clone(), Vec::with_capacity(0)),
                );
            }
            let mut xs = xs.clone();
            let init_enum_var = Expr::InitEnumVariantE(enum_name.clone(), name.clone(), xs.clone());
            let arg0 = xs.remove(0);
            let body = xs
                .iter()
                .fold(box Annotated::zero(init_enum_var), |acc, x| {
                    box Annotated::zero(Expr::FnE(None, x.into(), acc))
                });
            env.update(
                name.clone(),
                CloV(None, arg0, body, Rc::new(HashMap::new())),
            )
        }),
        _ => env,
    }
}

pub fn interp_expr(e: Box<Annotated<Expr>>, env: &Env) -> InterpResult {
    use Expr::*;
    use InterpErrorKind::*;
    use Value::*;
    let interp = interp_expr;

    let err_idx = e.idx;
    let err_len = e.len;
    let e = e.unwrap();

    macro_rules! err {
        ($variant:expr) => {
            Err(InterpError {
                kind: $variant,
                idx: err_idx,
                len: err_len,
                loc: None,
            })
        };
    }

    // println!("{:?}", e);
    match e {
        Expr::NumE(i) => Ok(NumV(i)),
        Expr::PlusE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (NumV(i1), NumV(i2)) => Ok(NumV(i1 + i2)),
            (StringV(s1), StringV(s2)) => Ok(StringV(s1 + &s2)),
            (StringV(s1), NumV(s2)) => Ok(StringV(s1 + &s2.to_string())),
            (StringV(s1), BoolV(s2)) => Ok(StringV(s1 + &s2.to_string())),
            (StringV(s1), s2 @ ObjectV(_, _)) => Ok(StringV(s1 + &format!("{}", s2))),
            (StringV(s1), MutV(s2)) => Ok(StringV(s1 + &(*s2.borrow().clone()).to_string())),
            _ => err!(TypeError),
        },
        Expr::MinusE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (NumV(i1), NumV(i2)) => Ok(NumV(i1 - i2)),
            _ => err!(TypeError),
        },
        Expr::MultE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (NumV(i1), NumV(i2)) => Ok(NumV(i1 * i2)),
            _ => err!(TypeError),
        },
        Expr::DivE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (NumV(i1), NumV(i2)) => Ok(NumV(i1 / i2)),
            _ => err!(TypeError),
        },
        Expr::LtE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (NumV(i1), NumV(i2)) => Ok(BoolV(i1 < i2)),
            _ => err!(TypeError),
        },
        Expr::GtE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (NumV(i1), NumV(i2)) => Ok(BoolV(i1 > i2)),
            _ => err!(TypeError),
        },
        Expr::NegE(e) => match interp(e, env)? {
            NumV(i) => Ok(NumV(-i)),
            _ => err!(TypeError),
        },
        Expr::BoolE(b) => Ok(BoolV(b)),
        Expr::AndE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (BoolV(b1), BoolV(b2)) => Ok(BoolV(b1 && b2)),
            _ => err!(TypeError),
        },
        Expr::OrE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (BoolV(b1), BoolV(b2)) => Ok(BoolV(b1 || b2)),
            _ => err!(TypeError),
        },
        Expr::EqE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (BoolV(b1), BoolV(b2)) => Ok(BoolV(b1 == b2)),
            (NumV(b1), NumV(b2)) => Ok(BoolV(b1 == b2)),
            _ => err!(TypeError),
        },
        Expr::IfE(pred, thn, els) => match interp(pred, env)? {
            BoolV(true) => interp(thn, env),
            BoolV(false) => interp(els, env),
            _ => err!(TypeError),
        },
        Expr::StringE(s) => Ok(Value::StringV(s)),
        Expr::NullE => Ok(NullV),
        Expr::VarE(x) => env.get(&x).cloned().ok_or_else(|| InterpError {
            kind: UndefinedError(x),
            idx: err_idx,
            len: err_len,
            loc: None,
        }),
        Expr::LetE(x, e, body) => {
            let v = interp(e, env)?;
            let next_env = env.update(x, v);
            interp(body, &next_env)
        }
        Expr::MutableE(x, e, body) => {
            let v = interp(e, env)?;
            let v = MutV(Rc::new(RefCell::new(Box::new(v))));
            let next_env = env.update(x, v);
            interp(body, &next_env)
        }
        Expr::MutateE(xe, e) => {
            let v = interp(e, env)?;
            if let MutV(x) = interp(xe, env)? {
                x.replace(Box::new(v));
            }
            Ok(NullV)
        }
        Expr::DerefE(e) => {
            let v = interp(e, env)?;
            match v {
                MutV(cell) => Ok(*cell.borrow().clone()),
                RefV(rc) => Ok((*rc).clone()),
                _ => err!(DerefError(v)),
            }
        }
        Expr::SeqE(e1, e2) => {
            interp(e1, env)?;
            interp(e2, env)
        }
        Expr::FnE(name, x, body) => match name {
            Some(name) => {
                let env_cell = RefCell::new(env.clone());
                let env_rc = unsafe { Rc::from_raw(env_cell.as_ptr()) };
                env_cell.borrow_mut().insert(
                    name.clone(),
                    Value::CloV(Some(name.clone()), x, body, env_rc.clone()),
                );
                Ok(env_rc.get(&name).unwrap().clone())
            }
            None => Ok(CloV(None, x, body, Rc::new(env.clone()))),
        },
        Expr::AppE(f, param) => match (interp(f, env)?, interp(param, env)?) {
            (CloV(_, arg, body, mut local_env), arg_v) => {
                interp(body, &local_env.update(arg, arg_v))
            }
            (PrimV(name), arg_v) => interp_prim(name, vec![arg_v]),
            _ => err!(TypeError),
        },
        Expr::AppPrimE(f, xs) => {
            interp_prim(f, xs.iter().map(|x| env.get(x).cloned().unwrap()).collect())
        }
        Expr::WhileE(pred, body) => {
            // TODO: This seems a little extreme to CLONE the pred and body every time use Rc for exprs???
            loop {
                match interp(pred.clone(), env)? {
                    BoolV(true) => (),
                    BoolV(false) => break,
                    _ => return err!(TypeError),
                };
                interp(body.clone(), env)?;
            }
            Ok(Value::NullV)
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
                    let extended_env: Env = env.clone().union(extension);
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

extern "C" {
    fn srand() -> u32;
}
