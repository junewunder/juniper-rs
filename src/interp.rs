use crate::annotate::Annotated;
use crate::data::*;
use crate::error::*;
use im::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

type DefEnv = HashMap<String, (String, Box<Annotated<Expr>>)>;
pub type InterpResult = std::result::Result<Value, InterpError>;

pub fn interp_program(defs: Vec<Annotated<Defn>>) -> InterpResult {
    let mut env_init: Env = vec![
        ("delay".into(), Value::PrimV("delay".into())),
        ("print".into(), Value::PrimV("print".into())),
    ]
    .into_iter()
    .collect();

    let denv: DefEnv = defs.into_iter().fold(HashMap::new(), interp_fn_defs);

    let env_cell = RefCell::new(env_init);
    let env_rc = unsafe { Rc::from_raw(env_cell.as_ptr()) };
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

fn interp_fn_defs(denv: DefEnv, def: Annotated<Defn>) -> DefEnv {
    use Defn::*;
    match def.unwrap() {
        FnD(name, x, expr) => denv.update(name, (x, expr)),
        PrimD(name, mut xs) => {
            let app = box Expr::AppPrimE(name.clone(), xs.clone());
            let arg0 = xs.remove(0);
            denv.update(
                name,
                (
                    arg0,
                    xs.iter().fold(
                        box Annotated::zero(*app), // TODO: use actual locations here
                        |acc, x| box Annotated::zero(Expr::FnE(None, x.into(), acc)),
                    ),
                ),
            )
        }
        _ => denv,
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
        NumE(i) => Ok(NumV(i)),
        PlusE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (NumV(i1), NumV(i2)) => Ok(NumV(i1 + i2)),
            (StringV(s1), StringV(s2)) => Ok(StringV(s1 + &s2)),
            (StringV(s1), NumV(s2)) => Ok(StringV(s1 + &s2.to_string())),
            (StringV(s1), BoolV(s2)) => Ok(StringV(s1 + &s2.to_string())),
            (StringV(s1), MutV(s2)) => Ok(StringV(s1 + &(*s2.borrow().clone()).to_string())),
            _ => err!(TypeError),
        },
        MinusE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (NumV(i1), NumV(i2)) => Ok(NumV(i1 - i2)),
            _ => err!(TypeError),
        },
        MultE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (NumV(i1), NumV(i2)) => Ok(NumV(i1 * i2)),
            _ => err!(TypeError),
        },
        DivE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (NumV(i1), NumV(i2)) => Ok(NumV(i1 / i2)),
            _ => err!(TypeError),
        },
        LtE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (NumV(i1), NumV(i2)) => Ok(BoolV(i1 < i2)),
            _ => err!(TypeError),
        },
        GtE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (NumV(i1), NumV(i2)) => Ok(BoolV(i1 > i2)),
            _ => err!(TypeError),
        },
        NegE(e) => match interp(e, env)? {
            NumV(i) => Ok(NumV(-i)),
            _ => err!(TypeError),
        },
        BoolE(b) => Ok(BoolV(b)),
        AndE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (BoolV(b1), BoolV(b2)) => Ok(BoolV(b1 && b2)),
            _ => err!(TypeError),
        },
        OrE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (BoolV(b1), BoolV(b2)) => Ok(BoolV(b1 || b2)),
            _ => err!(TypeError),
        },
        EqE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (BoolV(b1), BoolV(b2)) => Ok(BoolV(b1 == b2)),
            (NumV(b1), NumV(b2)) => Ok(BoolV(b1 == b2)),
            _ => err!(TypeError),
        },
        IfE(pred, thn, els) => match interp(pred, env)? {
            BoolV(true) => interp(thn, env),
            BoolV(false) => interp(els, env),
            _ => err!(TypeError),
        },
        StringE(s) => Ok(Value::StringV(s)),
        VarE(x) => env.get(&x).cloned().ok_or_else(|| InterpError {
            kind: UndefinedError(x),
            idx: err_idx,
            len: err_len,
            loc: None,
        }),
        LetE(x, e, body) => {
            let v = interp(e, env)?;
            let next_env = env.update(x, v);
            interp(body, &next_env)
        }
        MutableE(x, e, body) => {
            let v = interp(e, env)?;
            let v = MutV(Rc::new(RefCell::new(Box::new(v))));
            let next_env = env.update(x, v);
            interp(body, &next_env)
        }
        MutateE(xe, e) => {
            let v = interp(e, env)?;
            if let MutV(x) = interp(xe, env)? {
                x.replace(Box::new(v));
            }
            Ok(Null)
        }
        DerefE(e) => {
            let v = interp(e, env)?;
            match v {
                MutV(cell) => Ok(*cell.borrow().clone()),
                _ => err!(DerefError(v)),
            }
        }
        SeqE(e1, e2) => {
            interp(e1, env)?;
            interp(e2, env)
        }
        FnE(name, x, body) => {
            match name {
                Some(name) => {
                    let env_cell = RefCell::new(env.clone());
                    let env_rc = unsafe { Rc::from_raw(env_cell.as_ptr()) };
                    env_cell.borrow_mut().insert(
                        name.clone(),
                        Value::CloV(Some(name.clone()), x, body, env_rc.clone()),
                    );
                    Ok(env_rc.get(&name).unwrap().clone())
                },
                None => Ok(CloV(None, x, body, Rc::new(env.clone())))
            }
        },
        AppE(f, param) => match (interp(f, env)?, interp(param, env)?) {
            (CloV(_, arg, body, mut local_env), arg_v) => {
                interp(body, &local_env.update(arg, arg_v))
            }
            (PrimV(name), arg_v) => interp_prim(name, vec![arg_v]),
            _ => err!(TypeError),
        },
        AppPrimE(f, xs) => {
            interp_prim(f, xs.iter().map(|x| env.get(x).cloned().unwrap()).collect())
        }
        WhileE(pred, body) => {
            // TODO: This seems a little extreme to CLONE the pred and body every time use Rc for exprs???
            loop {
                match interp(pred.clone(), env)? {
                    BoolV(true) => (),
                    BoolV(false) => break,
                    _ => return err!(TypeError),
                };
                interp(body.clone(), env)?;
            }
            Ok(Value::Null)
        }
        _ => err!(TypeError),
    }
}

fn interp_prim(name: String, values: Vec<Value>) -> InterpResult {
    use InterpErrorKind::*;
    use Value::*;
    match name.as_str() {
        "print" => {
            if let Some(v) = values.first() {
                println!("{}", v);
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
                Ok(Null)
            } else {
                Ok(PrimV("delay".into()))
            }
        }
        _ => panic!("no primitive named: {:?}", name),
    }
}
