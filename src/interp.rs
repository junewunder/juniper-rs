use crate::data::*;
use im::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

type DefEnv = HashMap<String, (String, Box<Expr>)>;

pub fn interp_program(defs: Vec<Defn>) -> Option<Value> {
    let mut env_init: Env = vec![
        // ("print".into(), Value::PrimV("print".into()))
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

    // println!("{:?}", env_rc); // TODO: figure out how to not print cyclic pointers
    if let Some(Value::CloV(_, arg, body, env)) = env_rc.get("main".into()) {
        return interp_expr(body.clone(), &env);
    }

    panic!("no <main> method")
}

fn interp_fn_defs(denv: DefEnv, def: Defn) -> DefEnv {
    use Defn::*;
    match def {
        FnD(name, x, expr) => denv.update(name, (x, expr)),
        PrimD(name, mut xs) => {
            let app = box Expr::AppPrimE(name.clone(), xs.clone());
            let arg0 = xs.remove(0);
            denv.update(
                name,
                (
                    arg0,
                    xs.iter().fold(app, |acc, x| box Expr::FnE(x.into(), acc)),
                ),
            )
        }
        _ => denv,
    }
}

pub fn interp_expr(e: Box<Expr>, env: &Env) -> Option<Value> {
    use Expr::*;
    use Value::*;
    // println!("{:?}", env.get("print".into()));
    let interp = interp_expr;
    match *e {
        NumE(i) => Some(NumV(i)),
        PlusE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (NumV(i1), NumV(i2)) => Some(NumV(i1 + i2)),
            (StringV(s1), StringV(s2)) => Some(StringV(s1 + &s2)),
            _ => None,
        },
        MinusE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (NumV(i1), NumV(i2)) => Some(NumV(i1 - i2)),
            _ => None,
        },
        MultE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (NumV(i1), NumV(i2)) => Some(NumV(i1 * i2)),
            _ => None,
        },
        DivE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (NumV(i1), NumV(i2)) => Some(NumV(i1 / i2)),
            _ => None,
        },
        LtE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (NumV(i1), NumV(i2)) => Some(BoolV(i1 < i2)),
            _ => None,
        },
        GtE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (NumV(i1), NumV(i2)) => Some(BoolV(i1 > i2)),
            _ => None,
        },
        NegE(e) => match interp(e, env)? {
            NumV(i) => Some(NumV(-i)),
            _ => None,
        },
        BoolE(b) => Some(BoolV(b)),
        AndE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (BoolV(b1), BoolV(b2)) => Some(BoolV(b1 && b2)),
            _ => None,
        },
        OrE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (BoolV(b1), BoolV(b2)) => Some(BoolV(b1 || b2)),
            _ => None,
        },
        EqE(e1, e2) => match (interp(e1, env)?, interp(e2, env)?) {
            (BoolV(b1), BoolV(b2)) => Some(BoolV(b1 == b2)),
            (NumV(b1), NumV(b2)) => Some(BoolV(b1 == b2)),
            _ => None,
        },
        IfE(pred, thn, els) => match interp(pred, env)? {
            BoolV(true) => interp(thn, env),
            BoolV(false) => interp(els, env),
            _ => None,
        },
        StringE(s) => Some(Value::StringV(s)),
        VarE(x) => env.get(&x).cloned().or_else(|| Some(Null)),
        LetE(x, e, body) => {
            let v = interp(e, env)?;
            let next_env = env.update(x, v);
            interp(body, &next_env)
        }
        SeqE(e1, e2) => match (interp(e1, env), interp(e2, env)) {
            (_, v2) => v2,
        },
        FnE(x, body) => Some(CloV(None, x, body, Rc::new(env.clone()))),
        AppE(f, e) => match (interp(f, env)?, interp(e, env)?) {
            (CloV(_, arg, body, mut local_env), arg_v) => {
                interp(body, &local_env.update(arg, arg_v))
            }
            (PrimV(name), arg_v) => interp_prim(name, vec![arg_v]),
            _ => None,
        },
        AppPrimE(f, xs) => {
            interp_prim(f, xs.iter().map(|x| env.get(x).cloned().unwrap()).collect())
        }
        _ => None,
    }
}

fn interp_prim(name: String, values: Vec<Value>) -> Option<Value> {
    use Value::*;
    match name.as_str() {
        "print" => {
            println!("{:?}", values.get(0));
            Some(PrimV("print".into()))
        }
        _ => Some(Null),
    }
}
