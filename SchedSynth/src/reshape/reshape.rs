use crate::ast::ast::Var;
use crate::ast::ast::Func;
use crate::ast::ast::Range;
use crate::ast::ast::AST;
use crate::ast::ast::ASTUtils;

use crate::options::options::Options;
use std::collections::HashMap;

use std::fmt;

#[derive(Clone)]
pub enum Reshape {
    Split(Func, Var, (Var, Var), i32),
    Reorder(Func, (Var, Var)),
    Fuse(Func, (Var, Var), Var)
}

#[derive(Clone)]
pub enum OrderingConstraint {
    Nested(Var, Var), // Require that var, var are nested within each other
}

impl fmt::Display for OrderingConstraint {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            OrderingConstraint::Nested(var1, var2) => write!(f, "{} > {}", var1, var2),
        }
    }
}

impl fmt::Display for Reshape {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Reshape::Split(func, var, (outer, inner), factor) => write!(f, "Split({}, {}, ({}, {}), {})", func, var, outer, inner, factor),
            Reshape::Reorder(func, (outer, inner)) => write!(f, "Reorder({}, ({}, {}))", func, outer, inner),
            Reshape::Fuse(func, (outer, inner), fused) => write!(f, "Fuse({}, ({}, {}), {})", func, outer, inner, fused)
        }
    }
}

impl fmt::Debug for Reshape {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Reshape::Split(func, var, (dim1, dim2), factor) => {
                write!(f, "Split({}, {}, ({}, {}), {})", func, var, dim1, dim2, factor)
            },
            Reshape::Reorder(func, (dim1, dim2)) => {
                write!(f, "Reorder({}, ({}, {}))", func, dim1, dim2)
            },
            Reshape::Fuse(func, (dim1, dim2), var) => {
                write!(f, "Fuse({}, ({}, {}), {})", func, dim1, dim2, var)
            }
        }
    }
}

// if can apply, return the ast that this has been applied to.
fn can_apply(ast: &AST, reshape: &Reshape) -> Option<AST> {
    // does the reshape reshape match the ast?
    match reshape {
        Reshape::Split(f, v, (v1, v2), factor) => {
            if ast.is_loop_type() {
                let variable = ast.get_iteration_variable().unwrap(); //loop types must have iter var
                if variable == *v {
                    let inner_loop = ast.get_substruct().unwrap();
                    Some(
                        // TODO -- preserve vect/parallel?
                        AST::For(
                            v1.clone(), Box::new(AST::For(
                                v2.clone(), Box::new(inner_loop), Range::Between(0, factor.clone())
                            )), Range::All()
                        )
                    )
                } else {
                    None
                }
            } else {
                None
            }
        }
        Reshape::Reorder(f, (v1, v2)) => {
            if ast.is_loop_type() {
                let variable_1 = ast.get_iteration_variable().unwrap(); //loop types must have iter var
                if variable_1 == *v1 {
                    let inner_loop = ast.get_substruct().unwrap();
                    let v1_range = ast.get_iteration_range().unwrap();

                    if inner_loop.is_loop_type() {
                        let variable_2 = inner_loop.get_iteration_variable().unwrap();
                        let v2_range = inner_loop.get_iteration_range().unwrap();
                        let inner_inner_loop = inner_loop.get_substruct().unwrap();

                        if variable_2 == *v2 {
                            Some(
                                // TODO -- preserve vect/parallel?
                                AST::For(
                                    v2.clone(), Box::new(AST::For(
                                            v1.clone(), Box::new(inner_inner_loop), v1_range.clone()
                                    )), v2_range.clone()
                                )
                            )
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        }
        Reshape::Fuse(f, (v1, v2), v) => {
            if ast.is_loop_type() {
                let outer_variable = ast.get_iteration_variable().unwrap(); //loop types must have iter var
                let inner_loop = ast.get_substruct().unwrap();

                if inner_loop.is_loop_type() {
                    let inner_variable = inner_loop.get_iteration_variable().unwrap();
                    let inner_inner_loop = inner_loop.get_substruct().unwrap();

                    if outer_variable == *v1 && inner_variable == *v2 {
                        Some(
                            AST::For(
                                v.clone(), Box::new(inner_inner_loop), Range::All()
                            )
                        )
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        }
    }
}

fn apply_reshape(ast: &AST, reshape: &Reshape) -> (AST, bool) {
    match can_apply(ast, reshape) {
        Some(new_ast) => (new_ast, true),
        None => {
            // recurve through the ast and see if we can find somehwere
            // to apply the reshape.
 match ast {
                AST::Produce(func, ast) => {
                    let (new_ast, applied) = apply_reshape(&*ast, reshape);
                    (AST::Produce(func.clone(), Box::new(new_ast)), applied)
                }
                AST::Consume(func, ast) => {
                    let (new_ast, applied) = apply_reshape(&*ast, reshape);
                    (AST::Consume(func.clone(), Box::new(new_ast)), applied)
                }
                AST::For(var, ast, range) => {
                    let (new_ast, applied) = apply_reshape(&*ast, reshape);
                    (AST::For(var.clone(), Box::new(new_ast), range.clone()), applied)
                }
                AST::Assign(func) => {
                    (AST::Assign(func.clone()), false)
                }
                AST::StoreAt(func) => {
                    (AST::StoreAt(func.clone()), false)
                }
                AST::Vectorize(var, ast, range) => {
                    let (new_ast, applied) = apply_reshape(&*ast, reshape);
                    (AST::Vectorize(var.clone(), Box::new(new_ast), range.clone()), applied)
                }
                AST::Sequence(asts) => {
                    let mut new_asts = Vec::new();
                    let mut applied = false;
                    for ast in asts {
                        let (new_ast, applied_in_ast) = apply_reshape(&*ast, reshape);
                        applied = applied | applied_in_ast;
                        new_asts.push(new_ast);
                    }
                    (AST::Sequence(new_asts), applied)
                }
            }
        }
    }
}

fn apply_reshapes(ast: &AST, reshapes: &Vec<Reshape>) -> AST {
    let mut new_ast = ast.clone();
    for reshape in reshapes {
        let (new_ast_internal, applied) = apply_reshape(&new_ast, reshape);
        new_ast = new_ast_internal;

        if !applied {
            println!("Warning: did not apply rule {}", reshape);
        }
    }
    new_ast
}

fn infer_constraints_from(reshape: &Reshape) -> Vec<OrderingConstraint> {
 match reshape {
        Reshape::Split(_func, _var, (_outer, _inner), _factor) => {
            vec![]
        },
        Reshape::Reorder(_func, (outer, inner)) => {
            vec![OrderingConstraint::Nested(inner.clone(), outer.clone())]
        },
        Reshape::Fuse(_func, (outer, inner), _fused) => {
            vec![OrderingConstraint::Nested(outer.clone(), inner.clone())]
        }
    }
}

fn infer_reorder_over(opts: &Options, ast: &AST, constraint: OrderingConstraint) -> Vec<Reshape> {
    if opts.debug_reshape {
        println!("Infering reorders for constraints {}", constraint);
    }
    let func_lookup = crate::ast::analysis::func_table_for(opts, ast);
    let res = match constraint {
        // infer the list of reorder constraints required
        // to support this.
        OrderingConstraint::Nested(outer, inner) => enforce_nested(opts, ast, outer, inner, &func_lookup, false, false)
    };
    if opts.debug_reshape {
        println!("Inferred reorders {:?}", res);
    };
    res
}

fn enforce_nested(opts: &Options, ast: &AST, outer: Var, inner: Var, func_lookup: &HashMap<Var, Func>, found_outer: bool, found_inner: bool) -> Vec<Reshape> {
    // Recursively walk through the AST --- find either the outer, or the inner.
    // Then reorder all the other dimensions to make sure that the outer is outermost,
    // and the inner is innermost.
    if opts.debug_reshape {
        println!("Enforcing nesting {} > {} on ast {}", outer, inner, ast);
    };
    let res = match ast {
        AST::Produce(_func, subast) => {
            enforce_nested(opts, &*subast, outer, inner, func_lookup, found_outer, found_inner)
        },
        AST::Consume(_func, subast) => {
            enforce_nested(opts, &*subast, outer, inner, func_lookup, found_outer, found_inner)
        },
        AST::For(var, subast, _range) | AST::Vectorize(var, subast, _range) => {
            let this_is_inner = *var == inner;
            let this_is_outer = *var == outer;
            if opts.debug_reshape {
                println!("Found inner var: {}, found outer var: {}", this_is_inner | found_inner,
                    this_is_outer | found_outer)
            }
            // breakdown by cases --- if we have seen the inner and outer,
            // we are done -- possibly needing to reorder those if they are in
            // the wrong order.
            if found_outer {
                if this_is_inner {
                    vec![]
                } else {
                    // This is not the inner --- insert a reordering that swaps the outer
                    // and this loop --- and recurse
                    // TODO -- check that both vars have the same parent.
                    let new_reshape = Reshape::Reorder(func_lookup.get(var).unwrap().clone(), (var.clone(),
                    outer.clone()));
                    let mut reorders = enforce_nested(opts, &*subast, outer, inner, func_lookup, found_outer, found_inner);
                    reorders.push(new_reshape);
                    reorders
                }
            } else if found_inner {
                if this_is_outer {
                    // we are done -- but still need to swap inner and outer.
                    vec![Reshape::Reorder(func_lookup.get(var).unwrap().clone(), (outer.clone(),
                    inner.clone()))]
                } else {
                    // we are still looking for the outer --- recurse
                    // and insert a swap for this loop and the inner.
                    let new_reshape = Reshape::Reorder(func_lookup.get(var).unwrap().clone(), (var.clone(),
                    inner.clone()));
                    let mut reorders = enforce_nested(opts, &*subast, outer, inner, func_lookup, found_outer, found_inner);
                    reorders.push(new_reshape);
                    reorders
                }
            } else {
                // have thus far found neither --- recurse (note that this_is_inner and
                // this_is_outer can be passed as args here)
                enforce_nested(opts, &*subast, outer, inner, func_lookup, this_is_outer, this_is_inner)
            }
            // If we have seen just one, need to insert a reorder.  We aim towards
            // moving the outer loop in rather than the inner loop out.

        },
        AST::Assign(_func) => vec![],
        AST::StoreAt(_func) => vec![],
        AST::Sequence(seq) => {
            let mut reorders = vec![];
            for subast in seq {
                let mut new_reorders = enforce_nested(opts, &subast, outer.clone(), inner.clone(), func_lookup, found_outer, found_inner);
                reorders.append(&mut new_reorders);
            }
            reorders
        }
    };
    if opts.debug_reshape {
        println!("Inferred the following reorders: {:?}", res);
    }
    res
}

fn infer_reorders_over(opts: &Options, ast: &AST, order_constraints: &Vec<OrderingConstraint>) -> (AST, Vec<Reshape>) {
    // Go through the ast, and determine every set of reorders required
    match order_constraints.len() {
        // If there are no order constraints, then no reorders are required
        0 => (ast.clone(), vec![]),
        _n => {
            let (head, tail) = order_constraints.split_at(1);

            let mut reshapes = infer_reorder_over(opts, ast, head[0].clone());

            // need to apply the reshapes --- otherwise other constraints may not
            // make sense.
            let new_ast = apply_reshapes(ast, &reshapes);
            let (result_ast, mut rest_of_reshapes) = infer_reorders_over(opts, &new_ast, &tail.to_vec());
            reshapes.append(&mut rest_of_reshapes);
            (result_ast, reshapes)
        }
    }
}

// Aim of this is to go through each reshape, and infer the constraints
// that it imposes on variable ordering.  From that, we can infer
// which reorders we have to use.
pub fn inject_reorders(opts: &Options, ast: &AST, reshapes: &Vec<Reshape>) -> Vec<Reshape> {
    let mut result_reshapes = Vec::new();
    let mut new_ast = ast.clone();
    if opts.debug_reshape {
        println!("Looking for constraints required for reshapes {:?}", reshapes);
    }
    for reshape in reshapes {
        let order_constraints = infer_constraints_from(reshape);
        let (new_ast_intermediate, mut reorders) = infer_reorders_over(opts, &new_ast, &order_constraints);

        result_reshapes.append(&mut reorders); // put the infered reorders
                                               // into a list.
        result_reshapes.push(reshape.clone());
        // apply all the reorders
        let (new_ast_intermediate, _applied) = apply_reshape(&new_ast_intermediate, reshape);
        new_ast = new_ast_intermediate;

        if opts.debug_reshape {
            println!("After reshape {}, have ast {}", reshape, new_ast);
        }
    }

    result_reshapes
}
