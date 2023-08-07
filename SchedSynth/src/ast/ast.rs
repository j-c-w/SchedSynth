use crate::options::options::Options;
use crate::ast::reorder_infer::get_reorders_internal;
use crate::ast::reorder_infer::insert_reorders_internal;
use crate::reshape::reshape::Reshape;

#[derive(Clone,Hash,Eq,PartialEq)]
pub struct Var {
    pub name: String
}

#[derive(Clone,Hash,Eq,PartialEq)]
pub struct Func {
    pub name: String,
    pub update: Option<i32>,
}

#[derive(Clone)]
pub enum Range {
    Between(i32, i32),
    All()
}

#[derive(Clone)]
pub enum AST {
    Produce(Func, Box<AST>),
    Consume(Func, Box<AST>),
    For(Var, Box<AST>, Range),
    Assign(Func),
    Vectorize(Var, Box<AST>, Range),
    Parallel(Var, Box<AST>, Range),
    StoreAt(Func),
    Sequence(Vec<AST>)
}

pub trait ASTUtils {
    fn is_loop_type(&self) -> bool;
    fn get_iteration_variable(&self) -> Option<Var>;
    fn get_substruct(&self) -> Option<AST>;
    fn get_iteration_range(&self) -> Option<Range>;
}

impl ASTUtils for AST {
 fn is_loop_type(&self) -> bool {
        match self {
            AST::For(_, _, _) => true,
            AST::Vectorize(_, _, _) => true,
            AST::Parallel(_, _, _) => true,
            _ => false
        }
    }

    fn get_iteration_variable(&self) -> Option<Var> {
        match self {
            AST::For(var, _, _) => Some(var.clone()),
            AST::Vectorize(var, _, _) => Some(var.clone()),
            AST::Parallel(var, _, _) => Some(var.clone()),
            _ => None
        }
    }

    fn get_substruct(&self) -> Option<AST> {
        match self {
            AST::Produce(_, ast) => Some(*ast.clone()),
            AST::Consume(_, ast) => Some(*ast.clone()),
            AST::For(_, ast, _) => Some(*ast.clone()),
            AST::Vectorize(_, ast, _) => Some(*ast.clone()),
            AST::Parallel(_, ast, _) => Some(*ast.clone()),
            AST::Sequence(asts) => {
                if asts.len() == 1 {
                    Some(asts[0].clone())
                } else {
                    None
                }
            },
            _ => None
        }
    }
    fn get_iteration_range(&self) -> Option<Range> {
        match self {
            AST::For(_, _, range) => Some(range.clone()),
            AST::Vectorize(_, _, range) => Some(range.clone()),
            AST::Parallel(_, _, range) => Some(range.clone()),
            _ => None
        }
    }
}

pub fn insert_reorders(opts: &Options, reshapes: &Vec<Reshape>, original_ast: &AST, target_ast: &AST) -> Vec<Reshape> {
    insert_reorders_internal(opts, reshapes, original_ast, target_ast)
}

pub fn find_reorders(opts: &Options, original_ast: &AST, target_ast: &AST) -> Vec<(Func, Var, Var)> {
    get_reorders_internal(opts, original_ast, target_ast)
}

pub fn get_store_at(opts: &Options, ast: &AST) -> Vec<(Func, Func, Var)> {
    let func_lookup_table = crate::ast::analysis::func_table_for(opts, ast);
    let mut res = Vec::new();
    for (func, var) in get_store_at_internal(opts, &None, ast) {
        res.push((func, func_lookup_table.get(&var).unwrap().clone(), var))
    }
    res
}

pub fn get_store_at_internal(opts: &Options, parent_variable: &Option<&Var>, ast: &AST) -> Vec<(Func, Var)> {
    match ast {
        AST::Produce(func, ast) => {
            get_store_at_internal(opts, parent_variable, ast)
        },
        AST::Consume(func, ast) => {
            get_store_at_internal(opts, parent_variable, ast)
        },
        AST::For(var, ast, range) => {
            get_store_at_internal(opts, &Some(&var), ast)
        },
        AST::Assign(func) => {
            vec![]
        },
        AST::Vectorize(var, ast, range) => {
            get_store_at_internal(opts, &Some(&var), ast)
        },
        AST::Parallel(var, ast, range) => {
            get_store_at_internal(opts, &Some(&var), ast)
        },
        AST::StoreAt(func) => {
            match parent_variable {
                Some(parent_variable) => {
                    vec![(func.clone(), (*parent_variable).clone())]
                },
                None => {
                    panic!("unexpected store_at root --- need a return")
                }
            }
        },
        AST::Sequence(asts) => {
            let mut store_ats = vec![];
            for ast in asts {
                store_ats.append(&mut get_store_at_internal(opts, parent_variable, ast));
            }
            store_ats
        }
    }
}

pub fn get_compute_at(opts: &Options, ast: &AST) -> Vec<(Func, Option<Func>, Option<Var>)> {
    get_compute_at_internal(opts, ast, &None, &None, &None)
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
fn get_compute_at_internal(opts: &Options, ast: &AST, outer_producer: &Option<Func>, inner_producer: &Option<Func>, last_variable: &Option<Var>) -> Vec<(Func, Option<Func>, Option<Var>)> {
    // keep track of what should be passed in sub-calls.
    // if the 
    let (new_outer, new_inner): (&Option<Func>, &Option<Func>) = match (outer_producer, inner_producer) {
        (Some(_outer), Some(inner)) => (&None, inner_producer),
        _ => (outer_producer, inner_producer)
    };

    let mut rest = match ast {
        AST::Produce(var, ast) => {
            let new_inner_producer = Some(var.clone());
            // push the producers through
            let mut res = get_compute_at_internal(opts, ast, new_inner, &new_inner_producer, last_variable);
            match (outer_producer, inner_producer) {
                (None, None) => res.push((var.clone(), None, None)), // if neither was set, we want a compute_root
                _ => (),
            };
            res
        },
        AST::Consume(_var, ast) => {
            // although consume comes from compute-at, I don't think it has
            // anything to do with this.
            get_compute_at_internal(opts, ast, new_outer, new_inner, last_variable)
        },
        AST::For(var, subast, _range) | AST::Vectorize(var, subast, _range) | AST::Parallel(var, subast, _range) => {
            get_compute_at_internal(opts, subast, new_outer, new_inner, &Some(var.clone()))
        },
        AST::Assign(_var) => {
            // TODo -- is there anything that we should do in this case?
            vec![]
        },
        AST::StoreAt(_var) => {
            vec![]
        },
        AST::Sequence(asts) => {
            let mut res = vec![];
            for ast in asts {
                res.append(&mut get_compute_at_internal(opts, ast, new_outer, new_inner, last_variable));
            }
            res
        }
    };

    // If the outer and the inner are set, we have a compute-at that
    // is just above this node --- add it to the list.
    // Note that these refer to the outer_producer and inner_producer 
    // /passed/ to this --- i.e. we are really only looking at the parents.
    match (outer_producer, inner_producer) {
        (Some(outer), Some(inner)) => {
            let res = (inner.clone(), Some(outer.clone()), last_variable.clone());
            // Recurse and get anything in the inside --- note that
            // we unset outer --- wehn we encounter a new produce, the
            // current inner will get pushed into outer.
            // We have to unset because produce (produce (for (for )))
            // should only return one compute at, not two.
            rest.push(res);
            rest
        },
        _ => rest
    }
}

// Gets a list of the the vectorize commands required.
pub fn get_vectorized(opts: &Options, ast: &AST) -> Vec<(Func, Var)> {
    get_vectorized_internal(opts, ast, &None)
}

fn get_vectorized_internal(_opts: &Options, ast: &AST, current_producer: &Option<Func>) -> Vec<(Func, Var)> {
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
        AST::Parallel(_, ast, _range) => get_vectorized_internal(_opts, ast, current_producer),
        AST::Assign(_) => Vec::new(),
        AST::StoreAt(_) => Vec::new(),
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

fn get_parallel_internal(_opts: &Options, ast: &AST, current_producer: &Option<Func>) -> Vec<(Func, Var)> {
    // recursively walk through the AST and
    // check if there is a vectorize node --- return the producer
    // that contains it, and the variable that is vectorized.
    // when you hit a new produce, reset the producer we are tracking
    match ast {
        AST::Produce(var, ast) => {
            let producer = Some(var.clone());
            get_parallel_internal(_opts, ast, &producer)
        },
        AST::Consume(_var, ast) => {
			// Do not update the producer for a consume
            get_parallel_internal(_opts, ast, current_producer)
        },
        AST::Vectorize(_var, children, _range) => {
            get_parallel_internal(_opts, children, current_producer)
        },
        AST::For(_, ast, _range) => get_parallel_internal(_opts, ast, current_producer),
        AST::Parallel(var, ast, _range) => {
            match current_producer {
                Some(p_name) => {
                    let mut v = get_parallel_internal(_opts, ast, current_producer);
                    v.push((p_name.clone(), var.clone()));
                    v
                },
                None => panic!("Parallel without corresponding producer")
            }
        },
        AST::Assign(_) => Vec::new(),
        AST::StoreAt(_) => Vec::new(),
        AST::Sequence(seq) => {
            // recurse on each element of seq, and join the results into a single vec
            let mut v = Vec::new();
            for e in seq.iter() {
                v.extend(get_parallel_internal(_opts, e, current_producer));
            };
            v
        }
    }
}

// Gets a list of the the vectorize commands required.
pub fn get_parallel(opts: &Options, ast: &AST) -> Vec<(Func, Var)> {
    get_parallel_internal(opts, ast, &None)
}
