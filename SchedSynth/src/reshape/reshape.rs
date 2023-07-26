use crate::ast::ast::Var;
use crate::ast::ast::Func;

pub enum Reshape {
    Split(Func, Var, (Var, Var), i32),
    Fuse(Func, (Var, Var), Var)
}
