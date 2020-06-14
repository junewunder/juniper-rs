use crate::data::*;
use crate::typecheck::*;

pub fn simplify(t: Type, env: &TEnv) -> Type {
    // dbg!(&t);
    match t {
        Type::TypeVar(name) => {
            let t = env.get(&name).cloned().unwrap_or(Type::TypeVar(name.clone()));
            if let Type::TypeVar(other) = t.clone() {
                if name == other { return t }
            }
            simplify(t, env)
        },
        Type::ConcreteT(root, t_val) => {
            let root = simplify(*root, env);
            match root {
                Type::GenericT(root, t_name) => {
                    let env = env.update(t_name, *t_val);
                    simplify(*root, &env)
                },
                _ => Type::ConcreteT(box root, box simplify(*t_val, env))
            }
        },
        Type::GenericT(root, t_name) => {
            let mut env = env.clone();
            env.remove(&t_name); // env.without doesn't work
            let root = simplify(*root, &env);
            if env.contains_key(&t_name) {
                root
            } else {
                Type::GenericT(box root, t_name)
            }
        }
        Type::CloT(i, o) => {
            Type::CloT(box simplify(*i, env), box simplify(*o, env))
        }
        Type::EnumT(name, variants) => {
            let env = env.update(name.clone(), Type::TypeVar(name.clone()));
            let next_variants = variants.into_iter()
                .map(|(k, params)|
                    (k, params.into_iter()
                        .map(|x| simplify(x, &env))
                        .collect::<Vec<_>>())
                )
                .collect::<im::HashMap<_,_>>();
            Type::EnumT(name, next_variants)
        }
        Type::StructT(name, fields) => {
            let env = env.update(name.clone(), Type::TypeVar(name.clone()));
            let next_fields = fields.into_iter()
                .map(|(k, v)| (k, simplify(v, &env)))
                .collect::<im::HashMap<_,_>>();

            Type::StructT(name, next_fields)
        }
        Type::MutT(t) => Type::MutT(box simplify(*t, env)),
        _ => t
    }
}

fn subst_tvar(target: Type, var: String, val: Type) -> Type {
    match target {
        Type::TypeVar(name) if name == var => Type::TypeVar(var),
        Type::GenericT(t, name) if name == var =>{
            Type::GenericT(box subst_tvar(*t, var.clone(), val), var)
        }
        Type::GenericT(t, name) => {
            Type::GenericT(box subst_tvar(*t, var.clone(), val), name)
        }
        Type::ConcreteT(t1, t2) => Type::ConcreteT(
            box subst_tvar(*t1, var.clone(), val.clone()),
            box subst_tvar(*t2, var, val)
        ),
        // Type::MutT(Box<Type>) => _,
        // Type::CloT(Box<Type>, Box<Type>) => _,
        // Type::ObjectT(HashMap<String, Type>) => _,
        // Type::StructT(String, StructFields) => _,
        Type::EnumT(name, variants) => {
            let next_variants = variants.into_iter()
                .map(|(k, params)| {
                    let params = params.into_iter()
                        .map(|x| subst_tvar(x, var.clone(), val.clone()))
                        .collect::<Vec<_>>();
                    (k, params)
                })
                .collect::<im::HashMap<_,_>>();

            Type::EnumT(name, next_variants)
        },
        _ => target,
    }
}

pub fn equiv(env: &TEnv, t1: &Type, t2: &Type) -> bool {
    use Type::*;
    match (t1, t2) {
        (Type::AnyT, _) => true,
        (_, Type::AnyT) => true,
        (TypeVar(lhs), TypeVar(rhs)) if lhs == rhs => true,
        (CloT(i1, o1), CloT(i2, o2)) => {
            equiv(env, &simplify(*i1.clone(), env), &simplify(*i2.clone(), env))
            && equiv(env, &simplify(*o1.clone(), env), &simplify(*o2.clone(), env))
        }
        _ => simplify(t1.clone(), env) == simplify(t2.clone(), env)
    }
}

pub fn peel_generics_defn(env: &TEnv, t: Type) -> (TEnv, Type) {
    match t {
        Type::GenericT(t, name) => {
            let env = env.update(name.clone(), Type::TypeVar(name));
            peel_generics_defn(&env, *t)
        }
        _ => (env.clone(), t)
    }
}

pub fn wrap_generics_defn(defn: Type, xs: Vec<String>) -> Type {
    xs.into_iter().rev().fold(defn, |acc, x| Type::GenericT(box acc, x))
}

pub fn wrap_concrete_defn(defn: Type, xs: Vec<String>) -> Type {
    xs.into_iter().fold(defn, |acc, x| Type::ConcreteT(box acc, box Type::TypeVar(x)))
}

// pub fn generic(root: Type, params: Vec<String>) -> Type {
//     params.into_iter().fold(root, |acc, t| {
//         Type::GenericT(Box::new(acc), t)
//     })
// }


// pub fn drill(t: Type) -> Type {
//     match t {
//         Type::ConcreteT(t1, t2) => drill(*t1),
//         _ => t,
//     }
// }
