use crate::options::options::Options;

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
    Split(ExoFunc, ExoVar, ExoVar, ExoVar), // split the var loop into var, var
}

#[derive(Clone)]
pub struct ExoProgram {
    pub commands: Vec<ExoCommand>,
    pub funcs: Vec<ExoFunc>,
}

impl ToString for ExoProgram {
    fn to_string(&self) -> String {
        let mut result = String::new();
        for func in funcs {
            // result.push_str(func.name);
        }

        for command in commands {
            let exo_directive = match command {
                Reorder(func, fvar, tvar) =>
                    format!("{} = reorder({}, \"{} {}\")", func.name, func.name, fvar.name, tvar.name),
                Split(func, fvar, tvar1, tvar2, factor) =>
                    format!("{} = divide_loop({}, {}, \"{}\", [\"{}\", \"{}\"], perfect=True)", func.name, func.name, fvar.name{}),
                Fuse(func, fvar1, fvar2, tvar) =>
                    format!("{} = fuse({}, {}, {})", func.name, fvar1.name, fvar2.name) +
                    format!("# TODO -- rename to tvar")
            };
            result.push_str(exo_directive);
        }
    }
}
