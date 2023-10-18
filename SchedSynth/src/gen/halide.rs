use crate::options::options::Options;
use crate::gen::target::TargetGenerate;
use crate::gen::target::TargetLower;
use crate::gen::target::Target;
use crate::ast::ast::*;
use crate::reshape::reshape::Reshape;
use crate::ast::ast::Property;

#[derive(Clone)]
pub struct HFunc {
    pub name: String,
    pub update: Option<i32>,
}

#[derive(Clone)]
pub struct HVar {
    pub name: String
}

#[derive(Clone)]
pub enum HalideCommand {
    Vectorize(HFunc, HVar), // HFunc to vectorize
    Parallel(HFunc, HVar), // HFunc to vectorize
    Unroll(HFunc, HVar, i32), // HFunc to unroll, unroll factor.
    Tile(), // HFunc to tile, 
    ComputeAt(HFunc, HFunc, HVar), // Compute func at func at varaiable
    StoreAt(HFunc, HFunc, HVar), // store func at variable
    ComputeRoot(HFunc), // Compute func at func at varaiable
    Reorder(HFunc, (HVar, HVar)), // Reoder <to> hvar, hvar
    Split(HFunc, HVar, (HVar, HVar), i32), // split var into (var, var) with tiling factor i32
    Fuse(HFunc, (HVar, HVar), HVar), // fuse (var, var) into (var)
}

#[derive(Clone)]
pub struct HalideProgram {
    pub commands: Vec<HalideCommand>
}

impl TargetGenerate for HalideProgram {
    fn generate(&self) -> String {
        self.to_string()
    }
}

impl TargetLower for HalideProgram {
    // Conert the the pairs of func and variable into
    // vectorize halide commands.
    fn to_vectorize(&mut self, commands: Vec<(Func, Var, Property)>) {
        // the first far is the func, and the second var is the variable
        // to vectorize (hvar).
        let mut halide_commands = Vec::new();
        for (func, hvar, _) in commands {
            let hfunc = HFunc { name: func.name, update: func.update };
            let hhvar = HVar { name: hvar.name };
            halide_commands.push(HalideCommand::Vectorize(hfunc, hhvar));
        }

        self.commands.append(&mut halide_commands);
    }

    fn to_parallel(&mut self, commands: Vec<(Func, Var, Property)>) {
        let mut halide_commands = Vec::new();
        for (func, hvar, _) in commands {
            let hfunc = HFunc { name: func.name, update: func.update };
            let hhvar = HVar { name: hvar.name };
            halide_commands.push(HalideCommand::Parallel(hfunc, hhvar));
        }
        self.commands.append(&mut halide_commands)
    }

    fn to_unroll(&mut self, commands: Vec<(Func, Var, Property)>) {
        let mut halide_commands = Vec::new();
        for (func, hvar, unroll_property) in commands {
			let factor = match unroll_property {
				Property::Unroll(size) => size,
				_ => panic!("Trying to unroll with non-unroll property!")
			};
            let hfunc = HFunc { name: func.name, update: func.update };
            let hhvar = HVar { name: hvar.name };
            halide_commands.push(HalideCommand::Unroll(hfunc, hhvar, factor));
        }
        self.commands.append(&mut halide_commands)
    }

    fn to_store_at(&mut self, commands: Vec<(Func, Func, Var)>) {
        let mut halide_commands = Vec::new();
        for (func, func2, hvar) in commands {
            let hfunc = HFunc { name: func.name, update: func.update };
            let hfunc2 = HFunc { name: func2.name, update: func2.update };
            let hhvar = HVar { name: hvar.name };
            halide_commands.push(HalideCommand::StoreAt(hfunc, hfunc2, hhvar));
        }
        self.commands.append(&mut halide_commands)
    }

    // turn the var var var into this:
    // ComputeAt(HFunc, HFunc, HVar) // Compute func at func at varaiable
    fn to_compute_at(&mut self, commands: Vec<(Func, Option<Func>, Option<Var>)>) {
        let mut halide_commands = Vec::new();
        for (func, compute_at_func, hvar) in commands {
            let hfunc = HFunc { name: func.name, update: func.update };
            match (compute_at_func, hvar) {
                (Some(compute_at_func), Some(hvar)) => {
                    let hhvar = HVar { name: hvar.name };
                    let hcompute_at_func = HFunc { name: compute_at_func.name, update: func.update };
                    halide_commands.push(HalideCommand::ComputeAt(hfunc, hcompute_at_func, hhvar));
                },
                (None, None) => {
                    // I /think/ that this is only possible this wa.
                    // Not 100% sure what it means with the var set.
                    halide_commands.push(HalideCommand::ComputeRoot(hfunc));
                },
                (None, Some(v)) => panic!("Unexpected variable {} set when processing compute_root", v),
                (Some(v), None) => panic!("Unexpected func {} set when processing compute_root", v)
            }
        }
        self.commands.append(&mut halide_commands)
    }

    // turn the var var var into this:
    // Reorder(HFunc, HVar, HVar) // Compute func at func at varaiable
    fn to_reorder(&mut self, commands: Vec<(Func, Var, Var)>) {
        let mut halide_commands = Vec::new();
        for (func, compute_at_func, hvar) in commands {
            let hfunc = HFunc { name: func.name, update: func.update };
            let hvar1 = HVar { name: compute_at_func.name };
            let hvar2 = HVar { name: hvar.name };
            halide_commands.push(HalideCommand::Reorder(hfunc, (hvar1, hvar2)));
        }
        self.commands.append(&mut halide_commands)
    }

    // the internal finder returns Reshape::Split(Var, (Var, Var)) which needs to be converted
    // into HalideCommand::Split and Reshape::Fuse((Var, Var), Var) which needs to be converted
    // into HalideCommand::Fuse
    fn to_reshape(&mut self, commands: &Vec<Reshape>) {
        let mut halide_commands = Vec::new();
        for command in commands {
            match command {
                // TODO -- figure out how to get the producer name into here.
                Reshape::Split(func, var, (var1, var2), factor) => {
                    let hfunc = HFunc { name: func.name.clone(), update: func.update.clone() };
                    let hvar = HVar { name: var.name.clone() };
                    let hvar1 = HVar { name: var1.name.clone() };
                    let hvar2 = HVar { name: var2.name.clone() };
                    halide_commands.push(HalideCommand::Split(hfunc, hvar, (hvar1, hvar2), factor.clone()));
                },
                Reshape::Fuse(func, (var1, var2), var) => {
                    let hfunc = HFunc { name: func.name.clone(), update: func.update.clone() };
                    let hvar = HVar { name: var.name.clone() };
                    let hvar1 = HVar { name: var1.name.clone() };
                    let hvar2 = HVar { name: var2.name.clone() };
                    halide_commands.push(HalideCommand::Fuse(hfunc, (hvar1, hvar2), hvar));
                },
                Reshape::Reorder(func, (var1, var2)) => {
                    let hfunc = HFunc { name: func.name.clone(), update: func.update.clone() };
                    let hvar1 = HVar { name: var1.name.clone() };
                    let hvar2 = HVar { name: var2.name.clone() };
                    halide_commands.push(HalideCommand::Reorder(hfunc, (hvar1, hvar2)));
                },
            }
        }
        self.commands.append(&mut halide_commands)
    }
}

impl Target for HalideProgram {}

impl ToString for HalideProgram {
    fn to_string(&self) -> String {
        let mut result = String::new();
        for command in &self.commands {
            result.push_str(&command.to_string());
            result.push_str("\n");
        }
        result
    }
}

impl ToString for HFunc {
    fn to_string(&self) -> String {
        match self.update {
            Some(up) => format!("{}.update({})", self.name.clone(), up),
            None => self.name.clone()
        }
    }
}

impl ToString for HVar {
    fn to_string(&self) -> String {
        self.name.clone()
    }
}

impl ToString for HalideCommand {
    fn to_string(&self) -> String {
        match self {
            HalideCommand::Vectorize(func, var) => format!("{}.vectorize({});", func.to_string(),
            var.to_string()),
            HalideCommand::Parallel(func, var) => format!("{}.parallel({});", func.to_string(),
            var.to_string()),
            HalideCommand::Unroll(func, var, factor) => format!("{}.unroll({}, {});", func.to_string(),
            var.to_string(), factor),
            HalideCommand::Tile() => String::from("X.tile()"),
            HalideCommand::ComputeAt(func1, func2, var) => {
                format!("{}.compute_at({}, {});", func1.to_string(), func2.to_string(), var.to_string())
            }
            HalideCommand::StoreAt(func, func2, var) => {
                format!("{}.store_at({}, {});", func.to_string(), func2.to_string(), var.to_string())
            },
            HalideCommand::ComputeRoot(func) => {
                format!("{}.compute_root();", func.to_string())
            }
            // add cases for 
            HalideCommand::Reorder(func1, (var1, var2)) => {
                format!("{}.reorder({}, {});", func1.to_string(), var1.to_string(), var2.to_string())
            }
            HalideCommand::Split(func, var1, (var2, var3), factor) => {
                format!("{}.split({}, {}, {}, {});", func.to_string(), var1.to_string(), var2.to_string(), var3.to_string(), factor.to_string())
            }
            HalideCommand::Fuse(func, (var1, var2), var3) => {
                format!("{}.fuse({}, {}, {});", func.to_string(), var1.to_string(), var2.to_string(), var3.to_string())
            }
        }
    }
}

pub fn generate(_opts: &Options, program: HalideProgram) -> String {
    program.commands.iter().map(|command| command.to_string()).collect::<Vec<String>>().join("\n") + "\n"
}
