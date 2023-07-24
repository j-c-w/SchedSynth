use std::collections::HashMap;
use crate::options::options::Options;
use crate::ast::ast::AST;
use crate::ast::ast::Var;
use crate::ast::ast::Range;

// recursively walk through the ast --- for every loop node (vect or for)
// add to the lookup table so we can access the range that that variable
// takes.
fn range_table_for_internal(opts: &Options, ast: &AST, table: &mut HashMap<Var, Range>) {
    match ast {
        AST::Produce(var, ast) => {
            range_table_for_internal(opts, ast, table)
        },
        AST::Consume(var, ast) => {
            range_table_for_internal(opts, ast, table)
        },
        AST::For(var, ast, range) => {
            table.insert(var.clone(), range.clone());
            range_table_for_internal(opts, ast, table)
        },
        AST::Assign(var) => (),
        AST::Vectorize(var, ast, range) => {
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
