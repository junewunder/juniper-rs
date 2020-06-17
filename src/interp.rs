use crate::annotate::Annotated;
use crate::data::*;
use crate::error::*;
use crate::typecheck;
use im::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
use std::path::PathBuf;

type DefFnEnv = HashMap<String, (String, Box<Annotated<Expr>>)>;
pub type InterpResult = std::result::Result<Value, InterpError>;

pub fn interp_program(filename: PathBuf, defs: Vec<Box<Annotated<Defn>>>) -> Result<Env, InterpError> {
    // println!("HELLLOOOO INTERP PROGRAM", );

    let mut env: Env = vec![
        ("delay".into(), Value::PrimV("delay".into())),
        ("print".into(), Value::PrimV("print".into())),
        ("random".into(), Value::PrimV("random".into())),
        ("round".into(), Value::PrimV("round".into())),
    ]
    .into_iter()
    .collect();

    let env = defs.iter().try_fold(env, |acc, d| interp_imports(filename.clone(), acc, d))?;
    let env = defs.iter().fold(env, interp_scopeless_defs);
    let env_rc = Rc::new(env);
    let mut env_mut = env_rc.clone();
    let env_ptr = unsafe { Rc::get_mut_unchecked(&mut env_mut) };
    let denv: DefFnEnv = defs.iter().fold(HashMap::new(), interp_fn_defs);

    for (name, (arg, body)) in denv.into_iter() {
        env_ptr.insert(
            name.clone(),
            Value::CloV(Some(name), arg, body, env_rc.clone()),
        );
    }

    for def in defs.iter() {
        if let Some((name, value)) = interp_val_def(def, &env_rc) {
            env_ptr.insert(name, value?);
        }
    }

    Ok((*env_rc.clone()).clone())
}

fn interp_imports(filename: PathBuf, env: Env, def: &Box<Annotated<Defn>>) -> Result<Env, InterpError> {
    match def.clone().unwrap() {
        Defn::ImportD(path) => {
            let mut mod_path = PathBuf::from(filename);
            mod_path.pop();
            mod_path.push(path);
            mod_path.set_extension("juni");


            println!("{:?}", mod_path);
            let input = std::fs::read_to_string(mod_path.clone()).expect("Unable to read file");
            let input = input.as_ref();
            let (r, tokbuf) = crate::lex::lex(input).expect("expr failed lexing");
            let (r, ast) = crate::parse::p_defs(tokbuf).expect("expr failed parsing");
            let tenv = typecheck::check_program(mod_path.clone(), ast.clone())?;
            let imported_env = interp_program(mod_path, ast)?;
            Ok(env.union(imported_env))
        }
        _ => Ok(env),
    }
}

fn interp_fn_defs(denv: DefFnEnv, def: &Box<Annotated<Defn>>) -> DefFnEnv {
    match def.clone().unwrap() {
        Defn::VarD(name, mut xs, rt, body) if xs.len() > 0 => {
            let mut ts = typecheck::drill(rt).clone().into_iter().collect::<Vec<_>>();
            let x = xs.remove(0);
            let t = ts.remove(0);
            let mut xt_s = xs.iter().zip(ts);
            let e = xt_s.rev().fold(body, |acc, (x, t)| {
                let next = Expr::FnE(None, x.clone(), t.clone(), acc);
                Box::new(Annotated::zero(next))
            });
            denv.update(name, (x.clone(), e))
        },
        _ => denv,
    }
}

fn interp_val_def(def: &Box<Annotated<Defn>>, env: &Env) -> Option<(String, InterpResult)> {
    match def.clone().unwrap() {
        Defn::VarD(name, xs, rt, body) if xs.len() == 0 => {
            Some((name, interp_expr(body, env)))
        },
        _ => None,
    }
}

fn interp_scopeless_defs(env: Env, def: &Box<Annotated<Defn>>) -> Env {
    use Defn::*;
    use Value::*;
    match def.cloned() {
        StructD(name, _, fields) => {
            env.update(name.clone(), Value::StructV(name.clone(), fields.clone()))
        }
        // PrimD(name, mut xs) => {
        //     let app = box Expr::AppPrimE(name.clone(), xs.clone());
        //     let arg0 = xs.remove(0);
        //     let body = xs.iter().fold(box Annotated::zero(*app), |acc, x| {
        //         box Annotated::zero(Expr::FnE(None, x.into(), acc))
        //     });
        //     env.update(name, CloV(None, arg0, body, Rc::new(HashMap::new())))
        // }
        Defn::EnumD(enum_name, _, variants) => variants.iter().fold(env, |env, (name, ts)| {
            if ts.len() == 0 {
                return env.update(
                    name.clone(),
                    Value::EnumV(enum_name.clone(), name.clone(), Vec::with_capacity(0)),
                );
            }
            let mut xs = ts.clone().iter()
                .enumerate()
                .map(|(i, x)| format!("{}{}", x, i))
                .collect::<Vec<String>>();
            let init_enum_var = Expr::InitEnumVariantE(enum_name.clone(), name.clone(), xs.clone());
            let arg0 = xs.remove(0);
            let body = xs
                .iter()
                .zip(ts)
                .fold(box Annotated::zero(init_enum_var), |acc, (x, t)| {
                    box Annotated::zero(Expr::FnE(None, x.clone(), t.clone(), acc))
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
        Expr::UnitE => Ok(UnitV),
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
            Ok(UnitV)
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
        Expr::FnE(name, x, t, body) => match name {
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
            Ok(Value::UnitV)
        }
        Expr::InitObjectE(fields) => {
            let mut field_vs = HashMap::new();
            for (x, e) in fields.into_iter() {
                field_vs.insert(x, interp(e, env)?);
            }

            Ok(ObjectV(None, field_vs))
        }
        Expr::InitStructE(name, fields) => {
            // TODO: simplify now that this is being checked for in type system
            let allowed_fields = match env.get(&name) {
                Some(StructV(name, fields)) => fields,
                _ => return err!(UndefinedError(name)),
            };
            let mut field_vs = HashMap::new();
            for (x, e) in fields.into_iter() {
                if !allowed_fields.contains_key(&x) {
                    return err!(ExtraFieldError(x));
                }
                field_vs.insert(x, interp(e, env)?);
            }

            let keys: Vec<&String> = field_vs.keys().collect();
            for x in allowed_fields.iter() {
                if !keys.contains(&x.0) {
                    return err!(MissingFieldError(x.0.clone()));
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

            Ok(v.clone()) // previously: Ok(RefV(v))
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
        Expr::TypeAnnotationE(e, t) => interp(e, env),
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
                Ok(UnitV)
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
