use crate::options::options::Options;

#[derive(Clone)]
pub struct HFunc {
    pub name: String
}

#[derive(Clone)]
pub struct HVar {
    pub name: String
}

#[derive(Clone)]
pub enum HalideCommand {
    Vectorize(HFunc, HVar), // HFunc to vectorize
    Unroll(HFunc, i32), // HFunc to unroll, unroll factor.
    Tile(), // HFunc to tile, 
    ComputeAt(HFunc, HFunc, HVar), // Compute func at func at varaiable
    Reorder(HFunc, (HVar, HVar)), // Reoder <to> hvar, hvar
    Split(HFunc, HVar, (HVar, HVar), i32), // split var into (var, var) with tiling factor i32
    Fuse(HFunc, (HVar, HVar), HVar), // fuse (var, var) into (var)
}

#[derive(Clone)]
pub struct HalideProgram {
    pub commands: Vec<HalideCommand>
}

impl ToString for HFunc {
    fn to_string(&self) -> String {
        self.name.clone()
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
            HalideCommand::Vectorize(func, var) => format!("{}.vectorize({})", func.to_string(),
            var.to_string()),
            HalideCommand::Unroll(func, factor) => format!("{}.Unroll({})", func.to_string(), factor),
            HalideCommand::Tile() => String::from("X.tile()"),
            HalideCommand::ComputeAt(func1, func2, var) => {
                format!("{}.compute_at({}, {})", func1.to_string(), func2.to_string(), var.to_string())
            }
            // add cases for 
            HalideCommand::Reorder(func1, (var1, var2)) => {
                format!("{}.reorder({}, {})", func1.to_string(), var1.to_string(), var2.to_string())
            }
            HalideCommand::Split(func, var1, (var2, var3), factor) => {
                format!("{}.split({}, {}, {}, {})", func.to_string(), var1.to_string(), var2.to_string(), var3.to_string(), factor.to_string())
            }
            HalideCommand::Fuse(func, (var1, var2), var3) => {
                format!("{}.fuse({}, {}, {})", func.to_string(), var1.to_string(), var2.to_string(), var3.to_string())
            }
        }
    }
}

pub fn generate(_opts: &Options, program: HalideProgram) -> String {
    program.commands.iter().map(|command| command.to_string()).collect::<Vec<String>>().join("\n")
}
