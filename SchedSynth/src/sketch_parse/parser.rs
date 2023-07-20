use pest::Parser;
use pest::iterators::Pair;
use pest_derive::Parser;
use std::fs;

use crate::options::options::Options;

#[derive(Parser)]
#[grammar = "sketch_parse/grammar.pest"]
struct LoopParser;

#[derive(Clone)]
pub struct Variable {
    pub name: String
}

#[derive(Clone)]
pub enum SketchAST { // nodes have nesting, <other stuff>
    Produce(i32, Variable, Box<SketchAST>), // name, contents
    For(i32, Variable, Box<SketchAST>), // variable name, sub-contents
    Assign(i32, Variable), // variable name
    Vectorize(i32, Variable, Box<SketchAST>), // variable name, sub-contents
    ASTVariable(i32, Variable), // just a plain variable --- not valid on it's own, but is usefule as
                                // an intermediate wrapper.
    Sequence(i32, Vec<SketchAST>) // list of sub-asts
}


// Trait for SketchAST.
trait AST {
    fn children(&self) -> Vec<SketchAST>;
    fn node_type(&self) -> String;
    fn size(&self) -> i32;
}

impl ToString for Variable {
    fn to_string(&self) -> String {
        self.name.clone()
    }
}

impl ToString for SketchAST {
    fn to_string(&self) -> String {
        match self {
            SketchAST::Produce(_, n, subelts) => format!("Produce {} ({})", n.to_string().clone(),
            subelts.to_string()),
            SketchAST::For(_, n, subelts) => format!("For {} ({})", n.to_string().clone(),
            subelts.to_string()),
            SketchAST::Assign(_, n) => format!("{} = ...", n.to_string().clone()),
            SketchAST::Vectorize(_, n, child) => format!("vectorized {} ({})",
            n.to_string().clone(), child.to_string()),
            SketchAST::Sequence(_, subvars) => format!("Sequence({})", subvars.iter().map(|x|
                    x.to_string()).collect::<Vec<String>>().join(",")),
                    SketchAST::ASTVariable(_, name) => format!("Variable {}", name.to_string().clone())
        }
    }

}

impl AST for SketchAST {
    fn children(&self) -> Vec<SketchAST> {
        match self {
            SketchAST::Produce(_nest, _n, children) => {
                let mut res = Vec::new();
                res.push(children.as_ref().clone());
                res
            },
            SketchAST::For(_nest, _n, children) => {
                let mut res = Vec::new();
                res.push(children.as_ref().clone());
                res
            },
            SketchAST::Assign(_nest, _n) => {
                let res = Vec::new();
                res
            },
            SketchAST::Vectorize(_nest, _n, children) => {
                let mut res = Vec::new();
                res.push(children.as_ref().clone());
                res
            }
            SketchAST::Sequence(_nest, children) => children.to_vec(),
            SketchAST::ASTVariable(_nest, _varname) => {
                let res = Vec::new();
                res
            }
        }
    }

    fn node_type(&self) -> String {
        match self {
            SketchAST::Produce(_, _, _) => "Produce".into(),
            SketchAST::For(_, _, _) => "For".into(),
            SketchAST::Assign(_, _) => "Assign".into(),
            SketchAST::Vectorize(_, _, _) => "Vectorize".into(),
            SketchAST::Sequence(_, _) => "Sequence".into(),
            SketchAST::ASTVariable(_, _) => "Variable".into()
        }
    }

    fn size(&self) -> i32 {
        match self {
            SketchAST::Produce(_, _, child) => child.size() + 1,
            SketchAST::For(_, _, child) => child.size() + 1,
            SketchAST::Assign(_, _) => 1,
            SketchAST::Sequence(_, children) => children.iter().map(|child| child.size()).sum(),
            SketchAST::Vectorize(_, _, child) => child.size() + 1,
            SketchAST::ASTVariable(_, _) => 1
        }
    }
}

fn process(opts: &Options, nesting_depth: i32, sequence: Pair<Rule>) -> SketchAST {
    if opts.debug_parser {
        println!("Processing '{}'", sequence.as_str());
    }
    match sequence.as_rule() {
        Rule::sequence_list => {
            if opts.debug_parser {
                println!("Got a sequcnce list");
            }
            let mut inner = sequence.into_inner();
            // For a sequence, get the head, then go through
            // and get the tail.
            // Note that this produces a very flat list --- it doesn't consider
            // how indented things are --- that is handled in a secondary pass.
            let seq = process(opts, nesting_depth, inner.next().unwrap());
            let _ = inner.next().unwrap(); // This is whitespace in both rules.
            let tail = inner.next().unwrap();
            let has_tail = match tail.as_rule() {
                Rule::sequence_list => // This is the first case for sequence_list
                    true,
                Rule::EOI => // This is the econd case of sequence_list
                    false,
                _ => panic!("Unexepced last token '{}'", tail)
            };
            if has_tail {
                // no increase in nesting depth because seq is just a sequence --it's explicit from
                // the indentation anyway.
                let rest = process(opts, nesting_depth, tail); // next seq list
                let mut existing_children = rest.children();
                existing_children.insert(0, seq);
                SketchAST::Sequence(nesting_depth, existing_children)
            } else {
                // just sequnce, EOI
                let mut res_vec = Vec::new();
                res_vec.push(seq);
                SketchAST::Sequence(nesting_depth, res_vec)
            }
        },
        Rule::sequence => {
            if opts.debug_parser {
                println!("Got a sequence");
            }
            let mut inner = sequence.into_inner();

            assert!(inner.len() == 2);

            let nesting = inner.next().unwrap();
            let new_nesting_depth: i32 = nesting.into_inner().len() as i32; // Get the depth of the nest

            // nesting despth is explitict from the indentation.
            let stmt = process(opts, new_nesting_depth, inner.next().unwrap());
            stmt
        },
        Rule::produce => {
            if opts.debug_parser {
                println!("Got a produce");
            }
            let mut inner = sequence.into_inner();

            let _title = inner.next(); // produce
                                       // let _ = inner.next(); // whitespace
            let ident = match process(opts, nesting_depth, inner.next().unwrap()) {
                SketchAST::ASTVariable(_, n) => n,
                _ => panic!("Unexpected non varaiblae")
            }; // identifier.

            // Parser produces un-nested code --- nest it later.
            SketchAST::Produce(nesting_depth, ident,
                Box::new(SketchAST::Sequence(nesting_depth, Vec::new())))
        },
        Rule::pfor => {
            if opts.debug_parser {
                println!("Got a for");
            }
            let mut inner = sequence.into_inner();

            let _title = inner.next(); // for
                                       // let _ = inner.next(); // whitespace
            let ident = match process(opts, nesting_depth, inner.next().unwrap()) {
                SketchAST::ASTVariable(_, n) => n,
                _ => panic!("Unexpected non varaiblae")
            }; // identifier.

            // Parser produces un-nested code --- nest it later.
            SketchAST::For(nesting_depth, ident,
                Box::new(SketchAST::Sequence(nesting_depth, Vec::new())))
        },
        Rule::assignment => {
            if opts.debug_parser {
                println!("Got an assignment");
            }
            let mut inner = sequence.into_inner();

            let ident = match process(opts, nesting_depth, inner.next().unwrap()) {
                SketchAST::ASTVariable(_, n) => n,
                _ => panic!("Unexpected non varaiblae")
            }; // identifier.

            SketchAST::Assign(nesting_depth, ident)
        }
        Rule::vectorize => {
            if opts.debug_parser {
                println!("Got a vectorize");
            }

            let mut inner = sequence.into_inner();
            let _ = inner.next(); // whitespace token
            // let _ = inner.next(); // whitespace
            let ident = match process(opts, nesting_depth, inner.next().unwrap()) {
                SketchAST::ASTVariable(_, n) => n,
                _ => panic!("Unexpected non variable")
            };
            SketchAST::Vectorize(nesting_depth, ident,
                Box::new(SketchAST::Sequence(nesting_depth, Vec::new())))
        }
        Rule::ident => {
            if opts.debug_parser {
                println!("Got an ident");
            }
            let name = sequence.as_str();
            SketchAST::ASTVariable(nesting_depth, Variable{name: name.into()})
        },
        Rule::EOI => {
            if opts.debug_parser {
                println!("Got an EOI");
            }
            SketchAST::Sequence(nesting_depth, Vec::new())
        }
        Rule::indent_nest => {
            panic!("Unreachable?")
        },
        Rule::newline => panic!("Got newline"),
        Rule::whitespace_plus => panic!("whitespace_plus"),
        Rule::whitespace => panic!("whitespace"),
        _ => {
            panic!("Unexpected rule {}", sequence.as_str())
        }
    }
}

// Get the nesting depth
fn get_nest_depth(v: &SketchAST) -> &i32 {
    match v {
        SketchAST::Produce(n, _, _) => n,
        SketchAST::For(n, _, _) => n,
        SketchAST::Assign(n, _) => n,
        SketchAST::Sequence(n, _) => n,
        SketchAST::Vectorize(n, _, _) => n,
        SketchAST::ASTVariable(n, _) => n
    }
}

// set the loop nest contents
fn set_nest(v: &SketchAST, nest: SketchAST) -> SketchAST {
    match v {
        SketchAST::Produce(n, var, current_nest) => {
            assert!(current_nest.size() == 0); // check we aren't deleting anything
            SketchAST::Produce(n.clone(), var.clone(), Box::new(nest))
        },
        SketchAST::For(n, var, current_nest) => {
            assert!(current_nest.size() == 0); // check we aren't deleting anything
            SketchAST::For(n.clone(), var.clone(), Box::new(nest))
        }
        SketchAST::Vectorize(n, var, current_nest) => {
            assert!(current_nest.size() == 0); // check we aren't deleting anything
            SketchAST::Vectorize(n.clone(), var.clone(), Box::new(nest))
        }
        SketchAST::Assign(_n, _var) => panic!("Can't set nest to an assign"),
        SketchAST::Sequence(_n, _nest) => panic!("Can't set nest to a sequence"),
        SketchAST::ASTVariable(_n, _var) => panic!("Can't set nest to a variable")
    }
}

fn nest_rules(rules: Vec<SketchAST>) -> Vec<SketchAST> {
    match rules.len() {
        0 => vec![],
        1 => rules,
        _n => {
            let (head_array, rest) = rules.split_at(1);
            let head = head_array[0].clone();// will always have one element.
            let head_nesting = get_nest_depth(&head);

            // Get the things that belong to head and the things that are at
            // the same level as head.
            // head index is the index where we return to the nesting of this
            // loop --- everything that follows belongs at the same
            // nesting level.
            let head_index = rest.iter().position(|x| get_nest_depth(x) <= head_nesting);
            let (before, after) = match head_index {
                Some(head_index) => rest.split_at(head_index),
                // if no place where we returned to a lesser nesting,
                // there is nothing that comes back above the previous statement.
                None => rest.split_at(rest.len())
            };

            // Recursively build structures in the nest:
            let structured_subnest = nest_rules(before.to_vec());
            // recurviesvley build structures in the tail:
            let mut structured_rest = nest_rules(after.to_vec());

            // Set the sub-slements of this item appropriately:
            let nested = set_nest(&head, SketchAST::Sequence(head_nesting + 1, structured_subnest));
            // And put this at the start of the nest for this level.
            structured_rest.insert(0, nested);
            structured_rest
        }
    }
}

fn nest(parsed: SketchAST) -> SketchAST {
    match parsed {
        SketchAST::Sequence(0, rules) => {
            SketchAST::Sequence(0, nest_rules(rules))
        }
        _ => panic!("Unexepcted top-level statement {}", parsed.to_string())
    }
}

pub fn parse(opts: &Options, filename: &String) -> SketchAST {
    let input = fs::read_to_string(filename).expect("Unable to read file");
    println!("{}", input);
    let mut sequence = LoopParser::parse(Rule::sequence_list, &input[..]).unwrap();
    // Parse into a flat structure.
    let parsed = process(opts, 0, sequence.next().unwrap());
    if opts.debug_parser {
        println!("Parsed {}", parsed.to_string());
    }

    // Go through and properly nest everything.
    let nested = nest(parsed);
    if opts.debug_parser {
        println!("Nested {}", nested.to_string());
    }
    nested
}
