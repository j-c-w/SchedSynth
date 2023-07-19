use crate::options::options::Options;

struct Func {
    name: String
}

struct Variable {
    name: String
}

pub enum HalideCommand {
    Vectorize(Func), // Func to vectorize
    Unroll(Func, i32), // Func to unroll, unroll factor.
    Tile(), // Func to tile, 
    ComputeAt(Func, Func, Variable) // Compute func at func at varaiable
}

impl ToString for Func {
    fn to_string(&self) -> String {
        self.name.clone()
    }
}

impl ToString for Variable {
    fn to_string(&self) -> String {
        self.name.clone()
    }
}

impl ToString for HalideCommand {
    fn to_string(&self) -> String {
        match self {
            HalideCommand::Vectorize(func) => format!("{}.vectorize()", func.to_string()),
            HalideCommand::Unroll(func, factor) => format!("{}.Unroll({})", func.to_string(), factor),
            HalideCommand::Tile() => String::from("X.tile()"),
            HalideCommand::ComputeAt(func1, func2, var) => {
                format!("{}.compute_at({}, {})", func1.to_string(), func2.to_string(), var.to_string())
            }
        }
    }
}

pub fn generate(_opts: &Options, commands: Vec<HalideCommand>) -> String {
    commands.iter().map(|command| command.to_string()).collect::<Vec<String>>().join("\n")
}
