use crate::options::options::Options;
use crate::ast::reorder_infer::get_reorders_internal;
use crate::ast::reorder_infer::insert_reorders_internal;
use crate::reshape::reshape::Reshape;
use std::collections::HashSet;
use std::collections::HashMap;
use crate::shared::range_set::IntegerRangeSet;

#[derive(Clone,Hash,Eq,PartialEq,Debug)]
pub struct Var {
    pub name: String
}

#[derive(Clone,Hash,Eq,PartialEq,Debug)]
pub enum VarOrHole {
    Var(Var),
    Hole()
}

#[derive(Clone,Hash,Eq,PartialEq)]
pub struct Func {
    pub name: String,
    pub update: Option<i32>,
}

#[derive(Clone,Hash,Eq,PartialEq)]
pub struct Buf {
	pub name: String
}

#[derive(Clone,Eq)]
pub enum NumberOrHole {
	Number(i32),
	Hole(IntegerRangeSet)
}

#[derive(Clone)]
pub enum ForRange {
    Between(NumberOrHole, NumberOrHole),
    All()
}

impl PartialEq for NumberOrHole {
    fn eq(&self, other: &NumberOrHole) -> bool {
        match (self, other) {
            (&NumberOrHole::Number(ref a), &NumberOrHole::Number(ref b)) => a == b,
            (&NumberOrHole::Hole(ref a), &NumberOrHole::Hole(ref b)) => a == b,
            _ => false
        }
    }
}

#[derive(Clone)]
pub enum AST {
    Produce(Func, Box<AST>),
    Consume(Func),
    For(VarOrHole, Box<AST>, ForRange, Vec<Property>),
    Assign(Func),
    StoreAt(Func),
    StructuralHole(Box<AST>),
	Prefetch(Buf, VarOrHole, NumberOrHole),
    Sequence(Vec<AST>)
}

#[derive(Clone,Eq)]
pub enum Property {
    Vectorize(),
    Parallel(),
    Unroll(NumberOrHole)
}

pub trait ASTUtils {
    fn is_loop_type(&self) -> bool;
    fn is_main_nest(&self) -> bool; // We use this concept of main nest to distinguish between the loops and properties.
    fn get_iteration_variable(&self) -> Option<Var>;
    fn get_substruct(&self) -> Option<AST>;
    fn get_iteration_range(&self) -> Option<ForRange>;
    fn get_properties(&self) -> Vec<Property>;
    fn get_next_main_var(&self) -> Option<Var>;
    fn size(&self) -> i32;
    fn is_func(&self) -> bool;
    fn has_structural_holes(&self) -> bool;
    fn get_number_of_fixed_holes(&self) -> i32; // get the number of holes between here and the next non-hole.  Only count fixed holes :)
}

pub trait HoleOption<T> {
    fn get(&self) -> Option<T>;
    fn is_hole(&self) -> bool;
}

impl HoleOption<Var> for VarOrHole {
    fn get(&self) -> Option<Var> {
        match self {
            VarOrHole::Var(v) => Some(v.clone()),
            VarOrHole::Hole() => None
        }
    }

    fn is_hole(&self) -> bool {
        match self {
            VarOrHole::Var(_) => false,
            VarOrHole::Hole() => true
        }
    }
}

impl std::fmt::Display for VarOrHole {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            VarOrHole::Var(v) => write!(f, "{}", v),
            VarOrHole::Hole() => write!(f, "_")
        }
    }
}

impl std::fmt::Display for NumberOrHole {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			NumberOrHole::Number(n) => write!(f, "{}", n),
			NumberOrHole::Hole(rs) => write!(f, "{}", rs)
		}
	}
}

impl std::fmt::Display for Buf {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "Buf:{}", self.name)
	}
}

impl ASTUtils for AST {
    fn size(&self) -> i32 {
        match self {
            AST::Produce(func, ast) => 1 + ast.size(),
            AST::Consume(func) => 1,
            AST::For(var, ast, for_range, properties) => 1 + ast.size() + properties.len() as i32,
            AST::Assign(func) => 1,
            AST::StoreAt(func) => 1,
            AST::Prefetch(buf, index, stride) => 1,
            AST::StructuralHole(ast) => 1 + ast.size(),
            AST::Sequence(asts) => asts.iter().map(|ast| ast.size()).sum()
        }
    }

    fn is_func(&self) -> bool {
        match self {
            AST::Produce(_, _) => true,
            _ => false
        }
    }

    fn is_loop_type(&self) -> bool {
        match self {
            AST::For(_, _, _, _) => true,
            _ => false
        }
    }
    
    // everything should have one main nest that goes 
    // in, everything else is the secondary nest.
    // (e..g directives like store_at etc.)
    fn is_main_nest(&self) -> bool {
        match self {
            AST::For(_, _, _, _) => true,
            AST::StructuralHole(_) => true,
            _ => false,
        }
    }

    // Get the number of fixed holes between here
    // and the next non-hole.
    fn get_number_of_fixed_holes(&self) -> i32 {
        match self {
            AST::StructuralHole(ast) => ast.get_number_of_fixed_holes(),
            AST::Sequence(asts) => asts.iter().map(|ast| ast.get_number_of_fixed_holes()).sum(),
            AST::For(v, subast, _, _) => {
                match v {
                    VarOrHole::Var(var) => 0,
                    VarOrHole::Hole() => 1 + subast.get_number_of_fixed_holes()
                }
            },
            AST::Produce(_func, ast) => ast.get_number_of_fixed_holes(),
            AST::Consume(_func) => 0,
            AST::Assign(_func) => 0,
            AST::StoreAt(_func) => 0,
            AST::Prefetch(_buf, var, _) => {
				assert!(!var.is_hole()); // do not support structural holes
				// in prefetches.  THis assert can just be deleted
				// provided that var-filling is implemented
				0
			},
        }
    }

    fn get_next_main_var(&self) -> Option<Var> {
        match self {
            AST::Produce(_, subast) => subast.get_next_main_var(),
            AST::Consume(_) => None,
            AST::For(v, subast, _, _) =>
                match v {
                    VarOrHole::Var(v) => Some(v.clone()),
                    VarOrHole::Hole() => subast.get_next_main_var(),
                },
            AST::Assign(_) => None,
            AST::StoreAt(_) => None,
            AST::Prefetch(_) => None,
            AST::StructuralHole(substr) => substr.get_next_main_var(),
            AST::Sequence(asts) => {
                // Follow mainline
                for ast in asts {
                    if ast.is_main_nest() {
                        return ast.get_next_main_var()
                    }
                }
                // if no main sequence then nothing.
                None
            }
        }
    }

    fn get_iteration_variable(&self) -> Option<Var> {
        match self {
            AST::For(var, _, _, _) =>
                match var {
                    VarOrHole::Var(v) => Some(v.clone()),
                    VarOrHole::Hole() => None
                }
            _ => None
        }
    }

    fn get_substruct(&self) -> Option<AST> {
        match self {
            AST::Produce(_, ast) => Some(*ast.clone()),
            AST::Consume(_) => None,
            AST::For(_, ast, _, _) => Some(*ast.clone()),
            AST::StructuralHole(ast) => Some(*ast.clone()),
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
    fn get_iteration_range(&self) -> Option<ForRange> {
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

    // this should return true if there are any structural hole elements
    // /or/ if there are any variable holes
    fn has_structural_holes(&self) -> bool {
        match self {
            AST::Produce(_f, subast) => subast.has_structural_holes(),
            AST::Consume(_f) => false,
            AST::For(v, subast, _r, _p) => v.is_hole() || subast.has_structural_holes(),
            AST::Assign(_f) => false,
            AST::StoreAt(_f) => false,
            AST::Prefetch(_f) => false,
            AST::StructuralHole(_s) => true,
            AST::Sequence(subasts) => {
                for ast in subasts {
                    if ast.has_structural_holes() {
                        return true;
                    }
                };
                false
            }
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
        AST::Produce(_func, ast) => {
            get_store_at_internal(opts, parent_variable, ast)
        },
        AST::Consume(_func) => {
            vec![]
        },
        AST::For(var, ast, _range, _properties) => match var {
            VarOrHole::Var(v) => get_store_at_internal(opts, &Some(&v), ast),
            VarOrHole::Hole() => panic!("Can't get store_at for a tree with holes") // to get the store at, we need the parent --- we should really fix this to enable store_at to have holes
                                                                                    // also but ATM
                                                                                    // not required
                                                                                    // IIUC.
        },
        AST::Assign(_func) => {
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
		AST::Prefetch(_, _, _) => {
			vec![]
		}
        AST::StructuralHole(ast) => {
            get_store_at_internal(opts, parent_variable, ast)
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
    let compute_ats = get_compute_at_internal(opts, ast, &None, &None);
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

fn get_compute_at_internal(opts: &Options, ast: &AST, producer: &Option<Func>, last_variable:
    &Option<Var>) -> Vec<(Func, Option<Func>, Option<Var>)> {
        match ast {
            AST::Produce(var, ast) => {
                let inner_producer = Some(var.clone());
                get_compute_at_internal(opts, ast, &inner_producer, last_variable)
            },
            AST::Consume(var) => {
                match (producer, last_variable) {
                    (Some(p), Some(lv)) => {
                        vec![(var.clone(), Some(p.clone()), Some(lv.clone()))]
                    },
                    (_, _) =>
                        // Insert a compute-root def?
                        panic!("Consume at top level?")
                }
            },
            AST::For(var, subast, _range, _properties) => match var {
                VarOrHole::Var(v) => get_compute_at_internal(opts, subast, producer, &Some(v.clone())),
                VarOrHole::Hole() => panic!("Can't get compute at from tree without vars")
            },
        AST::Assign(_var) => {
            // TODo -- is there anything that we should do in this case?
            vec![]
        },
        AST::StoreAt(_var) => {
            vec![]
        },
        AST::Prefetch(_var) => {
            vec![]
        },
        AST::StructuralHole(subast) => {
            get_compute_at_internal(opts, subast, producer, last_variable)
        },
        AST::Sequence(asts) => {
            let mut res = vec![];
            for ast in asts {
                res.append(&mut get_compute_at_internal(opts, ast, producer, last_variable));
            }
            res
        }
    }
}

/*
TODO --- need to fix this
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
            res
        },
        AST::Consume(var) => {
            vec![
            match (outer_producer, inner_producer) {
                (None) => (var.clone(), None, None), // if neither was set, we want a compute_root
                _ => (),
            }]
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
*/

fn get_loops_with_property(_opts: &Options, ast: &AST, current_producer: &Option<Func>, properties: &Vec<Property>) -> Vec<(Func, Var, Property)> {
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
                    match var {
                        VarOrHole::Var(v) => {
                            for property in properties_matched {
                                sub_results.push((p_name.clone(), v.clone(), property.clone()))
                            }
                        },
                        VarOrHole::Hole() => panic!("Can't get loop properties on holey structure"),
                    },
                None => ()
            };
            // println!("Sub results length is {}", sub_results.len());
            // println!("properties length is {}", properties.len());
            sub_results
        },
        AST::Assign(_) => Vec::new(),
        AST::StoreAt(_) => Vec::new(),
        AST::Prefetch(_) => Vec::new(),
        AST::StructuralHole(subast) => {
            get_loops_with_property(_opts, subast, &current_producer, properties)
        },
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
    get_loops_with_property(opts, ast, &None, &vec![Property::Unroll(NumberOrHole::Number(0))])
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

// Remove any non-top-level func.  If it's a direct
// child, panic --- shouldn't happen?
fn remove_subfuncs(ast: &AST) -> AST {
    match ast {
        AST::Produce(func, ast) => {
            panic!("Can't remove a non-sequence func");
        },
        AST::Consume(func) => {
            AST::Consume(func.clone())
        },
        AST::For(var, ast, range, props) => {
            AST::For(var.clone(), Box::new(remove_subfuncs(ast)), range.clone(), props.clone())
        },
        AST::Assign(func) => {
            AST::Assign(func.clone())
        },
        AST::StoreAt(func) => {
            AST::StoreAt(func.clone())
        },
		AST::Prefetch(buf, var, stride) => {
			AST::Prefetch(buf.clone(), var.clone(), stride.clone())
		}
        AST::StructuralHole(ast) => {
            AST::StructuralHole(Box::new(remove_subfuncs(ast)))
        },
        AST::Sequence(asts) => {
            let mut newSeq = Vec::new();
            for ast in asts {
                if !ast.is_func() {
                    newSeq.push(remove_subfuncs(ast))
                }
            }
            AST::Sequence(newSeq)
        }
    }
}

pub fn split_ast_into_funcs(ast: &AST) -> HashMap<Func, AST> {
    // split ast into the different funcs within (produce).
    let mut funcs = HashMap::new();
    match ast {
        AST::Produce(func, ast) => {
            funcs.insert(func.clone(), remove_subfuncs(ast));
            funcs.extend(split_ast_into_funcs(&ast));
            funcs
        },
        AST::Consume(_func) => funcs,
        AST::For(_var, ast, _range, _props) => {
            funcs.extend(split_ast_into_funcs(&ast));
            funcs
        },
        AST::Assign(_func) => funcs,
        AST::StoreAt(_func) => funcs,
        AST::Prefetch(_func) => funcs,
        AST::StructuralHole(ast) => {
            funcs.extend(split_ast_into_funcs(&ast));
            funcs
        },
        AST::Sequence(asts) => {
            for ast in asts {
                funcs.extend(split_ast_into_funcs(&ast));
            }
            funcs
        }
    }
}

pub fn get_all_funcnames(ast: &AST) -> Vec<Func> {
    match ast {
        AST::Produce(func, ast) => {
            let mut res = get_all_funcnames(ast);
            res.push(func.clone());
            res
        },
        AST::Consume(func) => {
            vec![func.clone()]
        },
        AST::For(_var, ast, _range, _props) => {
            get_all_funcnames(ast)
        },
        AST::Assign(_func) => {
            vec![]
        },
        AST::StoreAt(_func) => {
            vec![]
        },
        AST::Prefetch(_, _, _) => {
            vec![]
        },
        AST::StructuralHole(ast) => {
            get_all_funcnames(ast)
        },
        AST::Sequence(asts) => {
            let mut res = vec![];
            for ast in asts {
                res.append(&mut get_all_funcnames(ast));
            }
            res
        }
    }
}

pub fn get_prefetch(ast: &AST) -> Vec<(Buf, Var, NumberOrHole)> {
    match ast {
        AST::Produce(func, ast) => {
            get_prefetches(ast);
        },
        AST::Consume(func) => {
            vec![]
        },
        AST::For(_var, ast, _range, _props) => {
            get_prefetches(ast)
        },
        AST::Assign(_func) => {
            vec![]
        },
        AST::StoreAt(_func) => {
            vec![]
        },
        AST::Prefetch(func, var, stride) => {
            vec![(func.clone(), var.clone(), stride.clone())]
        },
        AST::StructuralHole(ast) => {
            get_prefetches(ast)
        },
        AST::Sequence(asts) => {
            let mut res = vec![];
            for ast in asts {
                res.append(&mut get_prefetches(ast));
            }
            res
        }
    }
}
