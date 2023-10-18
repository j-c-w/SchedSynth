use crate::options::options::Options;
use crate::ast::reorder_infer::get_reorders_internal;
use crate::ast::reorder_infer::insert_reorders_internal;
use crate::reshape::reshape::Reshape;
use std::collections::HashSet;

#[derive(Clone,Hash,Eq,PartialEq,Debug)]
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
    Consume(Func),
    For(Var, Box<AST>, Range, Vec<Property>),
    Assign(Func),
    StoreAt(Func),
    Sequence(Vec<AST>)
}

#[derive(Clone,Eq)]
pub enum Property {
    Vectorize(),
    Parallel(),
    Unroll(i32)
}

pub trait ASTUtils {
    fn is_loop_type(&self) -> bool;
    fn get_iteration_variable(&self) -> Option<Var>;
    fn get_substruct(&self) -> Option<AST>;
    fn get_iteration_range(&self) -> Option<Range>;
    fn get_properties(&self) -> Vec<Property>;
}

impl ASTUtils for AST {
 fn is_loop_type(&self) -> bool {
        match self {
            AST::For(_, _, _, _) => true,
            _ => false
        }
    }

    fn get_iteration_variable(&self) -> Option<Var> {
        match self {
            AST::For(var, _, _, _) => Some(var.clone()),
            _ => None
        }
    }

    fn get_substruct(&self) -> Option<AST> {
        match self {
            AST::Produce(_, ast) => Some(*ast.clone()),
            AST::Consume(_) => None,
            AST::For(_, ast, _, _) => Some(*ast.clone()),
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
            AST::For(_, _, range, _) => Some(range.clone()),
            _ => None
        }
    }

    fn get_properties(&self) -> Vec<Property> {
        match self {
            AST::For(_, _, _, properties) => properties.clone(),
            _ => vec![]
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
        AST::Consume(func) => {
            vec![]
        },
        AST::For(var, ast, range, properties) => {
            get_store_at_internal(opts, &Some(&var), ast)
        },
        AST::Assign(func) => {
            vec![]
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
    let compute_ats = get_compute_at_internal(opts, ast, &None, &None, &None);
    // Now, filter the compute ats: things that
    // are computed within functions will not
    // be computed at root:
    let mut filtered_compute_ats = Vec::new();
    let mut added = HashSet::new();
    for (fun, atfun, atvar) in &compute_ats {
        match atfun {
            Some(_) => {
                filtered_compute_ats.push((fun.clone(), atfun.clone(), atvar.clone()));
                added.insert(fun.clone());
            }
            None => { },
        }
    }
    // Now, go through and add the compute_root parts
    // for any functions not yet specified.
    for (fun, atfun, atvar) in &compute_ats {
        if !added.contains(fun) {
            filtered_compute_ats.push((fun.clone(), atfun.clone(), atvar.clone()))
        }
    }

    filtered_compute_ats
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
        AST::Consume(_var) => {
            vec![]
        },
        AST::For(var, subast, _range, _properties) => {
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


// Return all the func/var combinations that
// have one of the properties in 'properties'.
// Also return each property that they have.
fn get_loops_with_property(_opts: &Options, ast: &AST, current_producer: &Option<Func>, properties:
    &Vec<Property>) -> Vec<(Func, Var, Property)> {
    // recursively walk through the AST and
    // check if there is a vectorize node --- return the producer
    // that contains it, and the variable that is vectorized.
    // when you hit a new produce, reset the producer we are tracking
    match ast {
        AST::Produce(var, ast) => {
            let producer = Some(var.clone());
            get_loops_with_property(_opts, ast, &producer, properties)
        },
        AST::Consume(_var) => {
            Vec::new()
        },
        AST::For(var, ast, _range, for_properties) => {
            // check if any of properties is in for_properties
            let mut properties_matched = Vec::new();
            for property in properties {
                // TODO -- need to do fuzzy match if property 
                // to check for vectorize regardless of the
                // actual number.
                let matching_properties = fuzzy_property_match(for_properties, &property);
                properties_matched.extend(matching_properties);
            }
            // if it is, we need to vectorize the loop
            let mut sub_results = get_loops_with_property(_opts, ast, current_producer, properties);
            match current_producer {
                Some(p_name) =>
                    for property in properties_matched {
                        sub_results.push((p_name.clone(), var.clone(), property.clone()))
                    }
                None => ()
            };
            // println!("Sub results length is {}", sub_results.len());
            // println!("properties length is {}", properties.len());
            sub_results
        },
        AST::Assign(_) => Vec::new(),
        AST::StoreAt(_) => Vec::new(),
        AST::Sequence(seq) => {
            // recurse on each element of seq, and join the results into a single vec
            let mut v = Vec::new();
            for e in seq.iter() {
                v.extend(get_loops_with_property(_opts, e, current_producer, properties));
            };
            v
        }
    }
}

// Gets a list of the the vectorize commands required.
pub fn get_parallel(opts: &Options, ast: &AST) -> Vec<(Func, Var, Property)> {
    let res = get_loops_with_property(opts, ast, &None, &vec![Property::Parallel()]);
    // println!("Result length is {}", res.len());
    res
}
//
// Gets a list of the the vectorize commands required.
pub fn get_unroll(opts: &Options, ast: &AST) -> Vec<(Func, Var, Property)> {
    get_loops_with_property(opts, ast, &None, &vec![Property::Unroll(0)])
}

// Gets a list of the the vectorize commands required.
pub fn get_vectorized(opts: &Options, ast: &AST) -> Vec<(Func, Var, Property)> {
    get_loops_with_property(opts, ast, &None, &vec![Property::Vectorize()])
}

// Given a property, do a fuzzy match:
// meaning parameter-independent, and return
// the property matched if it exists in
// the list
fn fuzzy_property_match(properties: &Vec<Property>, to_match: &Property) -> Vec<Property> {
    let mut result = Vec::new();
    for prop in properties {
        match (prop, to_match) {
            (Property::Vectorize(), Property::Vectorize()) => result.push(Property::Vectorize()),
            (Property::Parallel(), Property::Parallel()) => result.push(Property::Parallel()),
            (Property::Unroll(orig), Property::Unroll(match_runroll)) =>
                result.push(Property::Unroll(orig.clone())),
            (_, _) => {},
        };
    };
    result
}
