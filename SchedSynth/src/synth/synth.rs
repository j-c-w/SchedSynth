use crate::ast::ast::*;
use crate::reshape::reshape::Reshape;
use crate::options::options::Options;
use crate::gen::target::Backend;
use crate::gen::target::TargetLower;
use crate::gen::target::TargetGenerate;

fn synthesize_candidates(opts: &Options, backend_type: Backend, source: &AST, target: &AST, reshapes: &Vec<Reshape>) -> String {
    // Go through the various halide exprs and get the calls for them.
    let infered_reshapes = &crate::ast::ast::insert_reorders(opts, reshapes, source, target);
    let infered_reorders = &crate::reshape::reshape::infer_reorders_between(opts, source, target, infered_reshapes);

    let mut backend = crate::gen::target::newBackend(backend_type);

    backend.to_reshape(infered_reshapes);
    backend.to_reshape(infered_reorders);
    backend.to_compute_at(crate::ast::ast::get_compute_at(opts, target));
    backend.to_store_at(crate::ast::ast::get_store_at(opts, target));
    backend.to_vectorize(crate::ast::ast::get_vectorized(opts, target));
    backend.to_parallel(crate::ast::ast::get_parallel(opts, target));
    backend.to_unroll(crate::ast::ast::get_unroll(opts, target));

    let candidates =  vec![
        backend
    ];

    let best_candidate = crate::runner::runner::best_schedule(opts, candidates);
    best_candidate.generate()
}

pub fn synthesize_from_sketch(opts: &Options, source: &AST, target: &AST, reshapes: &Vec<Reshape>) -> String {
    let best_candidate = synthesize_candidates(opts, opts.backend, source, target, reshapes);
    return best_candidate
}
