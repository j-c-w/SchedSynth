use crate::gen::halide::HalideProgram;
use crate::gen::halide::HalideCommand;
use crate::gen::halide::HFunc;
use crate::gen::halide::HVar;
use crate::ast::ast::*;
use crate::reshape::reshape::Reshape;
use crate::options::options::Options;
use crate::runner::runner::evaluate_options;
use crate::gen::target::Target;
use crate::gen::target::Backend;

fn synthesize_candidates(opts: &Options, backend: Backend, source: &AST, target: &AST, reshapes: &Vec<Reshape>) {
    match backend {
        Backend::Halide() => synthesize_candidates_internal(opts, source, target, reshapes),
        Backend::Exo() => synthesize_candidates_internal(opts, source, target, reshapes)
    }
}

fn synthesize_candidates_internal<CommandType, BackendType: Target<CommandType>>(opts: &Options, source: &AST, target: &AST, reshapes:
    &Vec<Reshape>) -> Vec<BackendType> {
    // Go through the various halide exprs and get the calls for them.
    let infered_reshapes = &crate::ast::ast::insert_reorders(opts, reshapes, source, target);
    let infered_reorders = &crate::reshape::reshape::infer_reorders_between(opts, source, target, infered_reshapes);

    let backend = BackendType::new();

    let splits = backend.to_reshape(infered_reshapes);
    let reorders = backend.to_reshape(infered_reorders);
    let compute_at_calls = backend.to_compute_at(crate::ast::ast::get_compute_at(opts, target));
    let store_at_calls = backend.to_store_at(crate::ast::ast::get_store_at(opts, target));
    let vectorize_calls = backend.to_vectorize(crate::ast::ast::get_vectorized(opts, target));
    let parallel_calls = backend.to_parallel(crate::ast::ast::get_parallel(opts, target));
    if opts.debug_synthesizer {
        println!("Got {} vectorize calls", vectorize_calls.len());
    }

    backend.extend(splits); // TODO -- splits are ambiguious --- use synthesis to
                                      // find which splitting is the best strategy?
    backend.extend(reorders);
    backend.extend(compute_at_calls);
    backend.extend(store_at_calls);
    backend.extend(vectorize_calls);
    backend.extend(parallel_calls);

    return vec![
        backend
    ]
}

pub fn synthesize_from_sketch(opts: &Options, source: &AST, target: &AST, reshapes: &Vec<Reshape>) -> HalideProgram {
    let candidates = synthesize_candidates(opts, source, target, reshapes);
    let best_candidate = crate::runner::runner::best_schedule(opts, candidates);

    return best_candidate
}
