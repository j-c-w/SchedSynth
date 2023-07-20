use crate::gen::halide::HalideProgram;
use crate::gen::halide::HalideCommand;
use crate::gen::halide::HFunc;
use crate::gen::halide::HVar;
use crate::ast::ast::*;
use crate::options::options::Options;

fn to_halide_vectorize(commands: Vec<(Var, Var)>) -> Vec<HalideCommand> {
    // the first far is the func, and the second var is the variable
    // to vectorize (hvar).
    let mut halide_commands = Vec::new();
    for (func, hvar) in commands {
        let hfunc = HFunc { name: func.name };
        let hhvar = HVar { name: hvar.name };
        halide_commands.push(HalideCommand::Vectorize(hfunc, hhvar));
    }
    halide_commands
}

fn synthesize_candidates(opts: &Options, source: &AST, target: &AST) -> Vec<HalideProgram> {
    // Go through the various halide exprs and get the calls for them.
    let vectorize_calls = to_halide_vectorize(crate::ast::ast::get_vectorized(opts, target));
    if opts.debug_synthesizer {
        println!("Got {} vectorize calls", vectorize_calls.len());
    }
    return vec![
        HalideProgram { commands: vectorize_calls }
    ]
}

pub fn synthesize_from_sketch(opts: &Options, source: &AST, target: &AST) ->
HalideProgram {
    let candidates = synthesize_candidates(opts, source, target);
    // todo -- pick best
    return candidates[0].clone()
}
