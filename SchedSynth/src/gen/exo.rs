use crate::gen::target::TargetGenerate;
use crate::gen::target::TargetLower;
use crate::gen::target::Target;
use crate::ast::ast::*;
use crate::reshape::reshape::Reshape;
use crate::ast::ast::Property;
use crate::gen::target::TargetHoles;
use crate::gen::target::Hole;

#[derive(Clone)]
pub struct ExoFunc {
    pub name: String,
}

#[derive(Clone)]
pub struct ExoVar {
    pub name: String
}

#[derive(Clone)]
pub enum ExoCommand {
    Reorder(ExoFunc, ExoVar, ExoVar),
    Split(ExoFunc, ExoVar, ExoVar, ExoVar, i32), // split the var loop into var, var
    Fuse(ExoFunc, ExoVar, ExoVar, ExoVar)
}

#[derive(Clone)]
pub struct ExoProgram {
    pub commands: Vec<ExoCommand>,
    pub funcs: Vec<ExoFunc>,
}

pub struct ExoHole {

}

impl Target for ExoProgram {}
impl TargetGenerate for ExoProgram {
    fn generate(&self) -> String {
        return "".to_string()
    }
}

impl TargetLower for ExoProgram {
    fn to_vectorize(&mut self, commands: Vec<(Func, Var, Property)>) { }
    fn to_parallel(&mut self, commands: Vec<(Func, Var, Property)>) { }
    fn to_store_at(&mut self, commands: Vec<(Func, Func, Var)>) { }
    fn to_unroll(&mut self, commands: Vec<(Func, Var, Property)>) { }
    fn to_compute_at(&mut self, commands: Vec<(Func, Option<Func>, Option<Var>)>) { }
    fn to_reorder(&mut self, commands: Vec<(Func, Var, Var)>) { }
    fn to_reshape(&mut self, commands: &Vec<Reshape>) { }
}

impl TargetHoles for ExoProgram {
    fn get_holes(&self) -> Vec<&dyn Hole> { vec![] }
}

impl ToString for ExoProgram {
    fn to_string(&self) -> String {
        let mut result = String::new();
        for func in &self.funcs {
            // result.push_str(func.name);
        }

        for command in &self.commands {
            let exo_directive = match command {
                ExoCommand::Reorder(func, fvar, tvar) =>
                    format!("{} = reorder({}, \"{} {}\")", func.name, func.name, fvar.name, tvar.name),
                ExoCommand::Split(func, fvar, tvar1, tvar2, factor) =>
                    format!("{} = divide_loop({}, {}, \"{}\", [\"{}\", \"{}\"], perfect=True)", func.name, func.name, factor, fvar.name, tvar1.name, tvar2.name),
                ExoCommand::Fuse(func, fvar1, fvar2, tvar) =>
                    format!("{} = fuse({}, {}, {})\n#TODO --rename tvar", func.name, func.name, fvar1.name, fvar2.name),
            };
            result.push_str(&exo_directive);
        }
        result
    }
}