use crate::ast::ast::Func;
use crate::ast::ast::Var;
use crate::reshape::reshape::Reshape;

pub enum Backend {
    Halide(),
    Exo()
}

pub trait Target<CommandType>: TargetGenerate<CommandType> + TargetLower<CommandType> {}

pub trait TargetGenerate<CommandType> {
    fn generate() -> String;
    fn extend(commands: Vec<CommandType>);
}

pub trait TargetLower<CommandType> {
    fn to_vectorize(commands: Vec<(Func, Var)>) -> Vec<CommandType>;
    fn to_parallel(commands: Vec<(Func, Var)>) -> Vec<CommandType>;
    fn to_store_at(commands: Vec<(Func, Func, Var)>) -> Vec<CommandType>;
    fn to_compute_at(commands: Vec<(Func, Option<Func>, Option<Var>)>) -> Vec<CommandType>;
    fn to_reorder(commands: Vec<(Func, Var, Var)>) -> Vec<CommandType>;
    fn to_reshape(commands: &Vec<Reshape>) -> Vec<CommandType>;
}
