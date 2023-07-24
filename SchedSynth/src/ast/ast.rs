use crate::options::options::Options;
use crate::ast::reorder_infer::get_reorders_internal;
use crate::ast::split_infer::find_splits_internal;
use crate::ast::split_infer::Reshape;

#[derive(Clone,Hash,Eq,PartialEq)]
pub struct Var {
    pub name: String
}

#[derive(Clone)]
pub enum Range {
    Between(i32, i32),
    All()
}

#[derive(Clone)]
pub enum AST {
    Produce(Var, Box<AST>),
    Consume(Var, Box<AST>),
    For(Var, Box<AST>, Range),
    Assign(Var),
    Vectorize(Var, Box<AST>, Range),
    Sequence(Vec<AST>)
}


// TODO -- what to return?
pub fn find_splits(opts: &Options, original_ast: &AST, target_ast: &AST) -> Vec<Reshape> {
    find_splits_internal(opts, original_ast, target_ast)
}

pub fn find_reorders(opts: &Options, original_ast: &AST, target_ast: &AST) -> Vec<(Var, Var, Var)> {
    get_reorders_internal(opts, original_ast, target_ast)
}


pub fn get_compute_at(opts: &Options, ast: &AST) -> Vec<(Var, Var, Var)> {
    get_compute_at_internal(opts, ast, &None, &None)
}

// We are looking for this pattern:
// produce ... (outer):
//   for ...:
//     produce (inner):
//       for/vectorized/parallel K:
//     ...
//     consume ...
// and this gives us the compute_at primitives.
// We return Vec<(Var, Var, Var)> --- gives the
// computed function (inner), the computed at function (outer)
// and the index  (K)
fn get_compute_at_internal(opts: &Options, ast: &AST, outer_producer: &Option<Var>, inner_producer: &Option<Var>) -> Vec<(Var, Var, Var)> {
    match ast {
        AST::Produce(var, ast) => {
            let new_inner_producer = Some(var.clone());
            // push the producers through
            get_compute_at_internal(opts, ast, inner_producer, &new_inner_producer)
        },
        AST::Consume(_var, ast) => {
            // although consume comes from compute-at, I don't think it has
            // anything to do with this.
            get_compute_at_internal(opts, ast, outer_producer, inner_producer)
        },
        AST::For(var, subast, _range) | AST::Vectorize(var, subast, _range) => {
            // These are compute loops --- if both producers are set,
            // then we recurse here.
            match (outer_producer, inner_producer) {
                (Some(outer), Some(inner)) => {
                    let res = (outer.clone(), inner.clone(), var.clone());
                    // Recurse and get anything in the inside --- note that
                    // we unset outer --- wehn we encounter a new produce, the
                    // current inner will get pushed into outer.
                    // We have to unset because produce (produce (for (for )))
                    // should only return one compute at, not two.
                    let mut rest = get_compute_at_internal(opts, subast, &None, inner_producer);
                    rest.push(res);
                    rest
                },
                (_, _) => {
                    // do noth ahve a full match (i.e. this is not a compute-at
                    // structure.
                    get_compute_at_internal(opts, subast, outer_producer, inner_producer)
                }
            }
        },
        AST::Assign(_var) => {
            // TODo -- is there anything that we should do in this case?
            vec![]
        },
        AST::Sequence(asts) => {
            let mut res = vec![];
            for ast in asts {
                res.append(&mut get_compute_at_internal(opts, ast, outer_producer, inner_producer));
            }
            res
        }
    }
}

// Gets a list of the the vectorize commands required.
pub fn get_vectorized(opts: &Options, ast: &AST) -> Vec<(Var, Var)> {
    get_vectorized_internal(opts, ast, &None)
}

fn get_vectorized_internal(_opts: &Options, ast: &AST, current_producer: &Option<Var>) -> Vec<(Var, Var)> {
    // recursively walk through the AST and
    // check if there is a vectorize node --- return the producer
    // that contains it, and the variable that is vectorized.
    // when you hit a new produce, reset the producer we are tracking
    match ast {
        AST::Produce(var, ast) => {
            let producer = Some(var.clone());
            get_vectorized_internal(_opts, ast, &producer)
        },
        AST::Consume(_var, ast) => {
			// Do not update the producer for a consume
            get_vectorized_internal(_opts, ast, current_producer)
        },
        AST::Vectorize(var, children, _range) => {
            match current_producer {
                Some(p_name) => {
                    let mut v = get_vectorized_internal(_opts, children, current_producer);
                    v.push((p_name.clone(), var.clone()));
                    v
                },
                None => panic!("Vectorize without corresponding producer")
            }
        },
        AST::For(_, ast, _range) => get_vectorized_internal(_opts, ast, current_producer),
        AST::Assign(_) => Vec::new(),
        AST::Sequence(seq) => {
            // recurse on each element of seq, and join the results into a single vec
            let mut v = Vec::new();
            for e in seq.iter() {
                v.extend(get_vectorized_internal(_opts, e, current_producer));
            };
            v
        }
    }
}
