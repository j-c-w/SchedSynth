use crate::gen::halide::HalideProgram;
use crate::gen::halide::HalideCommand;
use crate::gen::halide::HFunc;
use crate::gen::halide::HVar;
use crate::ast::ast::*;
use crate::options::options::Options;

// Conert the the pairs of func and variable into
// vectorize halide commands.
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

// turn the var var var into this:
// ComputeAt(HFunc, HFunc, HVar) // Compute func at func at varaiable
fn to_halide_compute_at(commands: Vec<(Var, Var, Var)>) -> Vec<HalideCommand> {
    let mut halide_commands = Vec::new();
    for (func, compute_at_func, hvar) in commands {
        let hfunc = HFunc { name: func.name };
        let hcompute_at_func = HFunc { name: compute_at_func.name };
        let hhvar = HVar { name: hvar.name };
        halide_commands.push(HalideCommand::ComputeAt(hfunc, hcompute_at_func, hhvar));
    }
    halide_commands
}

// turn the var var var into this:
// Reorder(HFunc, HVar, HVar) // Compute func at func at varaiable
fn to_halide_reorder(commands: Vec<(Var, Var, Var)>) -> Vec<HalideCommand> {
 let mut halide_commands = Vec::new();
    for (func, compute_at_func, hvar) in commands {
        let hfunc = HFunc { name: func.name };
        let hvar1 = HVar { name: compute_at_func.name };
        let hvar2 = HVar { name: hvar.name };
        halide_commands.push(HalideCommand::Reorder(hfunc, hvar1, hvar2));
    }
    halide_commands
}

fn synthesize_candidates(opts: &Options, source: &AST, target: &AST) -> Vec<HalideProgram> {
    // Go through the various halide exprs and get the calls for them.
    let reorder_calls = to_halide_reorder(crate::ast::ast::find_reorders(opts, source, target));
    let compute_at_calls = to_halide_compute_at(crate::ast::ast::get_compute_at(opts, target));
    let vectorize_calls = to_halide_vectorize(crate::ast::ast::get_vectorized(opts, target));
    if opts.debug_synthesizer {
        println!("Got {} vectorize calls", vectorize_calls.len());
    }

    let mut unambiguous_calls = Vec::new();
    unambiguous_calls.extend(compute_at_calls);
    unambiguous_calls.extend(vectorize_calls);
	unambiguous_calls.extend(reorder_calls);

    return vec![
        HalideProgram { commands: unambiguous_calls }
    ]
}

pub fn synthesize_from_sketch(opts: &Options, source: &AST, target: &AST) ->
HalideProgram {
    let candidates = synthesize_candidates(opts, source, target);
    // todo -- pick best
    return candidates[0].clone()
}
