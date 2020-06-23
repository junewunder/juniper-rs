use crate::annotate::Annotated;
use crate::data::*;
use crate::error::*;
use crate::typecheck;
use im::HashMap;
use std::cell::RefCell;
use std::rc::Rc;
use std::path::PathBuf;

use crate::compiler::im_languages::*;
use crate::compiler::helper::*;

pub type CompileResult = std::result::Result<Value, CompileError>;

pub fn compile_program(filename: PathBuf, defs: Vec<Box<Annotated<Defn>>>) -> Result<Vec<WatModule>, CompileError> {
    let mut env: Env = HashMap::new();

    // let env = defs.iter().try_fold(env, |acc, d| compile_imports(filename.clone(), acc, d))?;
    // let env = defs.iter().fold(env, compile_scopeless_defs);
    // let env_rc = Rc::new(env);
    // let mut env_mut = env_rc.clone();
    // let env_ptr = unsafe { Rc::get_mut_unchecked(&mut env_mut) };
    // let denv: DefFnEnv = defs.iter().fold(HashMap::new(), compile_fn_defs);
    //
    // for (name, (arg, body)) in denv.into_iter() {
    //     env_ptr.insert(
    //         name.clone(),
    //         Value::CloV(Some(name), arg, body, env_rc.clone()),
    //     );
    // }
    //
    // for def in defs.iter() {
    //     if let Some((name, value)) = compile_val_def(def, &env_rc) {
    //         env_ptr.insert(name, value?);
    //     }
    // }
    //
    // Ok((*env_rc.clone()).clone())

    todo!()
}

fn compile_fn_defs(mut denv: Vec<WatDefn>, def: &Box<Annotated<Defn>>) -> Vec<WatDefn> {
    match def.clone().unwrap() {
        Defn::VarD(name, mut xs, rt, body) if xs.len() > 0 => {
            let mut ts = typecheck::drill(rt).clone().into_iter().collect::<Vec<_>>();
            let result: WatType = ts.last().cloned().unwrap().into();
            let x = xs.remove(0);
            let t = ts.remove(0);
            let mut xt_s = xs.iter().zip(ts);
            let e = xt_s.rev().fold(body, |acc, (x, t)| {
                let next = Expr::FnE(None, x.clone(), t.clone(), acc);
                Box::new(Annotated::zero(next))
            });

            let locals: Vec<(String, WatType)> = uncover_locals(&e).into_iter()
                .map(|(x, t)| (x, t.into()))
                .collect();

            let body = compile_expr(e)?;

            denv.push(WatDefn::Func {
                name: name.clone(),
                params: vec![ (x, WatType::F32) ],
                result,
                locals,
                body,
            });
            denv
        },
        _ => denv,
    }
}

pub fn compile_expr(e: Box<Annotated<Expr>>, tenv: &TEnv) -> CompileResult {
    use Expr::*;
    use CompileErrorKind::*;
    use Value::*;
    let compile = compile_expr;

    let err_idx = e.idx;
    let err_len = e.len;
    let e = e.unwrap();

    macro_rules! err {
        ($variant:expr) => {
            Err(CompileError {
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
        Expr::PlusE(e1, e2) => match (compile(e1, env)?, compile(e2, env)?) {
            (NumV(i1), NumV(i2)) => Ok(NumV(i1 + i2)),
            _ => err!(DynamicTypeError),
        },
        Expr::IfE(pred, thn, els) => match compile(pred, env)? {
            BoolV(true) => compile(thn, env),
            BoolV(false) => compile(els, env),
            _ => err!(DynamicTypeError),
        },
        Expr::VarE(x) => env.get(&x).cloned().ok_or_else(|| CompileError {
            kind: UndefinedError(x),
            idx: err_idx,
            len: err_len,
            loc: None,
        }),
        Expr::LetE(x, e, body) => {
            let v = compile(e, env)?;
            let next_env = env.update(x, v);
            compile(body, &next_env)
        }
        Expr::AppE(f, param) => match (compile(f, env)?, compile(param, env)?) {
            (CloV(_, arg, body, mut local_env), arg_v) => {
                compile(body, &local_env.update(arg, arg_v))
            }
            (PrimV(name), arg_v) => compile_prim(name, vec![arg_v]),
            _ => err!(DynamicTypeError),
        },
        Expr::TypeAnnotationE(e, t) => compile(e, env),
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

fn compile_prim(name: String, values: Vec<Value>) -> CompileResult {
    use CompileErrorKind::*;
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
