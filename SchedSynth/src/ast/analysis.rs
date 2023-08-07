use std::collections::HashMap;
use crate::options::options::Options;
use crate::ast::ast::AST;
use crate::ast::ast::Var;
use crate::ast::ast::Func;
use crate::ast::ast::Range;

/* #[derive(Clone)]
pub enum VariableTree {
    Node(Var, Vec<VariableTree>),
    Leaf()
}

// Build a VariableTree for all the loop/vect variables
// in the ast.
fn variable_tree(opts: &Options, ast: &AST) -> VariableTree {
    let res = variable_tree_internal(opts, ast);
    if res.len() == 0 {
        return VariableTree::Leaf();
    } else if res.len() == 1 {
        return res[0].clone();
    } else {
        // need to figure out what to do in this case --- what should
        // the label for the node be?
        panic!("Unrooted tree")
    }
}

fn variable_tree_internal(opts: &Options, ast: &AST) -> Vec<VariableTree> {
    match ast {
        AST::Produce(var, subast) => {
            let subtrees = variable_tree_internal(opts, subast);
            subtrees
        },
        AST::Consume(var, subast) => {
            let subtrees = variable_tree_internal(opts, subast);
            subtrees
        },
        AST::For(var, subast, range) => {
            let subtrees = variable_tree_internal(opts, subast);
            vec![VariableTree::Node(var, subtrees)]
        },
        AST::Assign(var) => {
            vec![VariableTree::Leaf()]
        },
        AST::Vectorize(var, subast, range) => {
            let subtrees = variable_tree_internal(opts, subast);
            vec![VariableTree::Node(var, subtrees)]
        },
        AST::Sequence(asts) => {
            let mut trees = vec![];
            for ast in asts {
                let subtrees = variable_tree_internal(opts, &ast);
                trees.extend(subtrees);
            }
            trees
        }
    }
} */

// recursively walk through the ast --- for every loop node (vect or for)
// add to the lookup table so we can access the range that that variable
// takes.
fn range_table_for_internal(opts: &Options, ast: &AST, table: &mut HashMap<Var, Range>) {
    match ast {
        AST::Produce(_var, ast) => {
            range_table_for_internal(opts, ast, table)
        },
        AST::Consume(_var, ast) => {
            range_table_for_internal(opts, ast, table)
        },
        AST::For(var, ast, range) => {
            table.insert(var.clone(), range.clone());
            range_table_for_internal(opts, ast, table)
        },
        AST::Assign(_var) => (),
        AST::StoreAt(_var) => (),
        AST::Vectorize(var, ast, range) => {
            table.insert(var.clone(), range.clone());
            range_table_for_internal(opts, ast, table)
        },
        AST::Parallel(var, ast, range) => {
            table.insert(var.clone(), range.clone());
            range_table_for_internal(opts, ast, table)
        },
        AST::Sequence(asts) => {
            for ast in asts {
                range_table_for_internal(opts, ast, table);
            }
        }
    }
}

pub fn range_table_for(opts: &Options, ast: &AST) -> HashMap<Var, Range> {
    let mut map = HashMap::new();
    range_table_for_internal(opts, ast, &mut map);
    map
}

// Each variable should have a parent func it refers to. Using compute_at
// it's possible to have a single varibale with multiple funcs, and this won't
// handle that --- it takes the outermost instance of a variable/func pair.
// keep track of the name of the func producer, and for each for/vectorize
// var that we encounter, add it to the map so we can look up the producer

// if current_producer is none then we throw (?what should we do?)
fn func_table_internal(opts: &Options, ast: &AST, current_producer: &Option<Func>, table: &mut HashMap<Var, Func>) {
    match ast {
        AST::Produce(var, body) => {
            func_table_internal(opts, body, &Some(var.clone()), table);
        },
        AST::Consume(_var, body) => {
            func_table_internal(opts, body, current_producer, table);
        },
        AST::For(var, body, _) => {
            // Note that this probably isn't the binding order we'd really
            // like to have --- but it should be good enough --- really
            // need to make sure we make variables
            if !table.contains_key(var) {
                if let Some(producer) = current_producer {
                    table.insert(var.clone(), producer.clone());
                    if opts.debug_func_table { 
                        println!("Adding {}, {} to table", var.clone(), producer.clone());
                    }
                } else {
                    panic!("No producer for variable {:?}", var.to_string());
                }
            } else {
                // TODO -- want to deal with this warning eventually
                println!("Warning: encountered variable {} more than once", var)
            }
            func_table_internal(opts, body, current_producer, table);
        },
        AST::Assign(_var) => {
            // Should we assert that var is the same as producer?
        },
        AST::StoreAt(_func) => { },
        AST::Vectorize(var, body, _) => {
            // if the variable already exists in the table, then we
            // don't need to do anything
            if !table.contains_key(var) {
                if let Some(producer) = current_producer {
                    table.insert(var.clone(), producer.clone());
                } else {
                    panic!("No producer for variable {:?}", var.to_string());
                }
            } else {
                // TODO -- want to deal with this warning eventually
                println!("Warning: encountered variable {} more than once", var)
            }
            func_table_internal(opts, body, current_producer, table);
        },
        AST::Parallel(var, body, _) => {
            // if the variable already exists in the table, then we
            // don't need to do anything
            if !table.contains_key(var) {
                if let Some(producer) = current_producer {
                    table.insert(var.clone(), producer.clone());
                } else {
                    panic!("No producer for variable {:?}", var.to_string());
                }
            } else {
                // TODO -- want to deal with this warning eventually
                println!("Warning: encountered variable {} more than once", var)
            }
            func_table_internal(opts, body, current_producer, table);
        },
        AST::Sequence(seq) => {
            for ast in seq {
                func_table_internal(opts, ast, current_producer, table);
            }
        }
    }
}

pub fn func_table_for(opts: &Options, ast: &AST) -> HashMap<Var, Func> {
    let mut table = HashMap::new();
    func_table_internal(opts, ast, &None, &mut table);
    table
}
