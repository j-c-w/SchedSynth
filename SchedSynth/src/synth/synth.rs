use crate::gen::halide::HalideProgram;
use crate::gen::halide::HalideCommand;
use crate::gen::halide::HFunc;
use crate::gen::halide::HVar;
use crate::ast::ast::*;
use crate::reshape::reshape::Reshape;
use crate::options::options::Options;

// Conert the the pairs of func and variable into
// vectorize halide commands.
fn to_halide_vectorize(commands: Vec<(Func, Var)>) -> Vec<HalideCommand> {
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

fn to_halide_store_at(commands: Vec<(Func, Var)>) -> Vec<HalideCommand> {
    let mut halide_commands = Vec::new();
    for (func, hvar) in commands {
        let hfunc = HFunc { name: func.name };
        let hhvar = HVar { name: hvar.name };
        halide_commands.push(HalideCommand::StoreAt(hfunc, hhvar));
    }
    halide_commands
}

// turn the var var var into this:
// ComputeAt(HFunc, HFunc, HVar) // Compute func at func at varaiable
fn to_halide_compute_at(commands: Vec<(Func, Option<Func>, Option<Var>)>) -> Vec<HalideCommand> {
    let mut halide_commands = Vec::new();
    for (func, compute_at_func, hvar) in commands {
        let hfunc = HFunc { name: func.name };
        match (compute_at_func, hvar) {
            (Some(compute_at_func), Some(hvar)) => {
                let hhvar = HVar { name: hvar.name };
                let hcompute_at_func = HFunc { name: compute_at_func.name };
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
    halide_commands
}

// turn the var var var into this:
// Reorder(HFunc, HVar, HVar) // Compute func at func at varaiable
fn to_halide_reorder(commands: Vec<(Func, Var, Var)>) -> Vec<HalideCommand> {
 let mut halide_commands = Vec::new();
    for (func, compute_at_func, hvar) in commands {
        let hfunc = HFunc { name: func.name };
        let hvar1 = HVar { name: compute_at_func.name };
        let hvar2 = HVar { name: hvar.name };
        halide_commands.push(HalideCommand::Reorder(hfunc, (hvar1, hvar2)));
    }
    halide_commands
}

// the internal finder returns Reshape::Split(Var, (Var, Var)) which needs to be converted
// into HalideCommand::Split and Reshape::Fuse((Var, Var), Var) which needs to be converted
// into HalideCommand::Fuse
fn to_halide_reshape(commands: &Vec<Reshape>) -> Vec<HalideCommand> {
    let mut halide_commands = Vec::new();
    for command in commands {
        match command {
            // TODO -- figure out how to get the producer name into here.
            Reshape::Split(func, var, (var1, var2), factor) => {
                let hfunc = HFunc { name: func.name.clone() };
                let hvar = HVar { name: var.name.clone() };
                let hvar1 = HVar { name: var1.name.clone() };
                let hvar2 = HVar { name: var2.name.clone() };
                halide_commands.push(HalideCommand::Split(hfunc, hvar, (hvar1, hvar2), factor.clone()));
            },
            Reshape::Fuse(func, (var1, var2), var) => {
                let hfunc = HFunc { name: func.name.clone() };
                let hvar = HVar { name: var.name.clone() };
                let hvar1 = HVar { name: var1.name.clone() };
                let hvar2 = HVar { name: var2.name.clone() };
                halide_commands.push(HalideCommand::Fuse(hfunc, (hvar1, hvar2), hvar));
            },
            Reshape::Reorder(func, (var1, var2)) => {
                let hfunc = HFunc { name: func.name.clone() };
                let hvar1 = HVar { name: var1.name.clone() };
                let hvar2 = HVar { name: var2.name.clone() };
                halide_commands.push(HalideCommand::Reorder(hfunc, (hvar1, hvar2)));
            },
        }
    }
    halide_commands
}

fn synthesize_candidates(opts: &Options, source: &AST, target: &AST, reshapes: &Vec<Reshape>) -> Vec<HalideProgram> {
    // Go through the various halide exprs and get the calls for them.
    let splits = to_halide_reshape(&crate::ast::ast::insert_reorders(opts, reshapes, source, target));
    let compute_at_calls = to_halide_compute_at(crate::ast::ast::get_compute_at(opts, target));
    let store_at_calls = to_halide_store_at(crate::ast::ast::get_store_at(opts, target));
    let vectorize_calls = to_halide_vectorize(crate::ast::ast::get_vectorized(opts, target));
    if opts.debug_synthesizer {
        println!("Got {} vectorize calls", vectorize_calls.len());
    }

    let mut unambiguous_calls = Vec::new();
    unambiguous_calls.extend(splits); // TODO -- splits are ambiguious --- use synthesis to
                                      // find which splitting is the best strategy?
    unambiguous_calls.extend(compute_at_calls);
    unambiguous_calls.extend(store_at_calls);
    unambiguous_calls.extend(vectorize_calls);

    return vec![
        HalideProgram { commands: unambiguous_calls }
    ]
}

pub fn synthesize_from_sketch(opts: &Options, source: &AST, target: &AST, reshapes: &Vec<Reshape>) -> HalideProgram {
    let candidates = synthesize_candidates(opts, source, target, reshapes);
    // todo -- pick best
    return candidates[0].clone()
}
