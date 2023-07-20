use crate::ast::ast::AST;
use crate::ast::ast::Var;
use crate::sketch_parse::parser::SketchAST;

use std::fmt;

// implement display for Var
impl fmt::Display for Var {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}", self.name)
	}
}

// implement ToString for AST
impl fmt::Display for AST {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            AST::Produce(ref var, ref ast) => write!(f, "produce {} in ({})", var, ast),
            AST::Consume(ref var, ref ast) => write!(f, "consume {} in ({})", var, ast),
            AST::For(ref var, ref ast) => write!(f, "for {} in ({})", var, ast),
            AST::Assign(ref var) => write!(f, "assign {}", var),
            AST::Vectorize(ref var, ref ast) => write!(f, "vectorize {} in ({})", var, ast),
            AST::Sequence(ref ast) => {
                let mut s = String::new();
                for a in ast {
                    s.push_str(&format!("{}", a));
                    s.push_str("; ");
                }
                write!(f, "{}", s)
            }
        }
    }
}

fn variable_to_var(variable: crate::sketch_parse::parser::Variable) -> Var {
    Var { name: variable.name }
}

fn var_to_variable(var: Var) -> crate::sketch_parse::parser::Variable {
    crate::sketch_parse::parser::Variable { name: var.name }
}


pub fn ast_from_sketch_ast(input: SketchAST) -> AST {
    match input {
        SketchAST::Produce(_nesting, var, ast) => {
            AST::Produce(variable_to_var(var), Box::new(ast_from_sketch_ast(*ast)))
        },
        SketchAST::Consume(_nesting, var, ast) => {
            AST::Consume(variable_to_var(var), Box::new(ast_from_sketch_ast(*ast)))
        },
        SketchAST::For(_nesting, var, ast) => {
            AST::For(variable_to_var(var), Box::new(ast_from_sketch_ast(*ast)))
        },
        SketchAST::Assign(_nesting, var) => {
            AST::Assign(variable_to_var(var))
        },
        SketchAST::Vectorize(_nesting, var, children) => {
            AST::Vectorize(variable_to_var(var), Box::new(ast_from_sketch_ast(*children)))
        },
        SketchAST::Sequence(_nesting, asts) => {
            let mut ast_vec = Vec::new();
            for ast in asts {
                ast_vec.push(ast_from_sketch_ast(ast));
            }
            AST::Sequence(ast_vec)
        },
        SketchAST::ASTVariable(_nesting, _var) => {
            panic!("ASTVariable should not be used in AST conversion");
        }
    }
}
