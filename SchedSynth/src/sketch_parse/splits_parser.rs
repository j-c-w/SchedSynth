use pest::Parser;
use pest::iterators::Pair;
use pest_derive::Parser;
use std::fs;

use crate::options::options::Options;
use crate::sketch_parse::parser::Variable;
use crate::reshape::reshape::Reshape;

use crate::ast::ast::Func;
use crate::ast::convert::variable_to_var;

#[derive(Parser)]
#[grammar = "sketch_parse/splits.pest"]
struct SplitsPest;

#[derive(Clone)]
pub enum SplitsAST {
    Split(Variable, (Variable, Variable), i32),
    Fuse((Variable, Variable), Variable),
}

fn splits_ast_to_reshape(ast: SplitsAST) -> Reshape {
    match ast {
        SplitsAST::Split(var, (var1, var2), dim) =>
            Reshape::Split(Func {name: "temp_func".into()}, variable_to_var(var),
            (variable_to_var(var1), variable_to_var(var2)), dim),
        SplitsAST::Fuse((var1, var2), var) =>
            Reshape::Fuse(Func {name: "temp_func".into() }, (variable_to_var(var1),
            variable_to_var(var2)), variable_to_var(var)),
    }
}

fn splits_asts_to_reshapes(asts: Vec<SplitsAST>) -> Vec<Reshape> {
    asts.into_iter().map(splits_ast_to_reshape).collect()
}

impl ToString for SplitsAST
 {
    fn to_string(&self) -> String {
        match self {
            SplitsAST::Split(var, (var1, var2), cost) => {
                format!("Split({}, ({}, {}), {})", var.to_string(), var1.to_string(), var2.to_string(), cost.to_string())
            },
            SplitsAST::Fuse((var1, var2), var) => {
                format!("Fuse(({}, {}), {})", var1.to_string(), var2.to_string(), var.to_string())
            }
        }
    }
}

fn process_ident(opts: &Options, rule: Pair<Rule>) -> Variable {
    match rule.as_rule() {
        Rule::ident => {
            if opts.debug_parser {
                println!("Got an ident");
            }
            let name = rule.as_str();
            Variable{name: name.into()}
        },
        _ => panic!("Unable to process non-ident sequence into variable")
    }
}

fn process(opts: &Options, rule: Pair<Rule>) -> Vec<SplitsAST> {
    match rule.as_rule() {
        Rule::sequence_list => {
            let mut inner = rule.into_inner();

            let mut seq = process(opts, inner.next().unwrap());
            let _ = inner.next(); // whitespace
            let tail = inner.next().unwrap();
            let has_tail = match tail.as_rule() {
                Rule::sequence_list => true,
                Rule::EOI => false,
                _ => panic!("Unexpected last token '{}'", tail)
            };
            if has_tail {
                let mut rest = process(opts, tail);
                seq.append(&mut rest);
                seq
            } else {
                seq
            }
        },
        Rule::sequence => {
            let mut inner = rule.into_inner();
            assert!(inner.len() == 1);
            process(opts, inner.next().unwrap())
        },
        Rule::split => {
            let mut inner = rule.into_inner();

            let split_from = process_ident(opts, inner.next().unwrap());
            let _ = inner.next(); // whitespace
            let _ = inner.next(); // whitespace
            let split_to_1 = process_ident(opts, inner.next().unwrap());
            let _ = inner.next(); // whitespace
            let _ = inner.next(); // whitespace
            let split_to_2 = process_ident(opts, inner.next().unwrap());

            // TODO -- load the split factor
            vec![SplitsAST::Split(split_from, (split_to_1, split_to_2), 0)]
        },
        Rule::fuse => {
            let mut inner = rule.into_inner();

            let fuse_from_1 = process_ident(opts, inner.next().unwrap());
            let _ = inner.next(); // whitespace
            let _ = inner.next(); // whitespace
            let fuse_from_2 = process_ident(opts, inner.next().unwrap());
            let _ = inner.next(); // whitespace
            let _ = inner.next(); // whitespace
            let fuse_to = process_ident(opts, inner.next().unwrap());

            // TODO -- load the split factor
            vec![SplitsAST::Fuse((fuse_from_1, fuse_from_2), fuse_to)]
        },
        _ => panic!("unexpected rule {}", rule.as_str())
    }
}

pub fn parse(opts: &Options, filename: &String) -> Vec<Reshape> {
    let input = fs::read_to_string(filename).expect("unable to read file");
    let mut sequence = SplitsPest::parse(Rule::sequence_list, &input[..]).unwrap();

    let parsed = process(opts, sequence.next().unwrap());
    splits_asts_to_reshapes(parsed)
}
