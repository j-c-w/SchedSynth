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
pub enum RangeAST {
    Between(i32, i32), // start, end
    All()
}

#[derive(Clone)]
pub enum SketchAST { // nodes have nesting, <other stuff>
    Produce(i32, Variable, Box<SketchAST>), // name, contents
    Consume(i32, Variable, Box<SketchAST>), // name, contents
    For(i32, Variable, Box<SketchAST>, RangeAST), // variable name, sub-contents, optional range
    Assign(i32, Variable), // variable name
    StoreAt(i32, Variable), // variable name
    Vectorize(i32, Variable, Box<SketchAST>, RangeAST), // variable name, sub-contents, optional range
    Sequence(i32, Vec<SketchAST>), // list of sub-asts
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

impl ToString for RangeAST {
    fn to_string(&self) -> String {
        match self {
            RangeAST::Between(start, end) => format!("{}..{}", start, end),
            RangeAST::All() => String::from("all"),
        }
    }
}

impl ToString for SketchAST {
    fn to_string(&self) -> String {
        match self {
            SketchAST::Produce(_, n, subelts) => format!("Produce {} ({})", n.to_string().clone(),
            subelts.to_string()),
            SketchAST::Consume(_, n, subelts) => format!("Consume {} ({})", n.to_string().clone(),
            subelts.to_string()),
            SketchAST::For(_, n, subelts, range) => format!("For {} in {}: ({})", n.to_string().clone(),
            range.to_string(), subelts.to_string()),
            SketchAST::Assign(_, n) => format!("{} = ...", n.to_string().clone()),
            SketchAST::Vectorize(_, n, child, range) => format!("vectorized {} in {}: ({})",
            n.to_string().clone(), range.to_string(), child.to_string()),
            SketchAST::StoreAt(_, n) => format!("store {} here", n.to_string().clone()),
            SketchAST::Sequence(_, subvars) => format!("Sequence({})", subvars.iter().map(|x|
                    x.to_string()).collect::<Vec<String>>().join(",")),
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
            SketchAST::Consume(_nest, _n, children) => {
                let mut res = Vec::new();
                res.push(children.as_ref().clone());
                res
            },
            SketchAST::For(_nest, _n, children, _range) => {
                let mut res = Vec::new();
                res.push(children.as_ref().clone());
                res
            },
            SketchAST::Assign(_nest, _n) => {
                let res = Vec::new();
                res
            },
            SketchAST::Vectorize(_nest, _n, children, _range) => {
                let mut res = Vec::new();
                res.push(children.as_ref().clone());
                res
            }
            SketchAST::StoreAt(_, n) => {
                vec![]
            }
            SketchAST::Sequence(_nest, children) => children.to_vec(),
        }
    }

    fn node_type(&self) -> String {
        match self {
            SketchAST::Produce(_, _, _) => "Produce".into(),
            SketchAST::Consume(_, _, _) => "Consume".into(),
            SketchAST::For(_, _, _, _) => "For".into(),
            SketchAST::Assign(_, _) => "Assign".into(),
            SketchAST::Vectorize(_, _, _, _) => "Vectorize".into(),
            SketchAST::StoreAt(_, _) => "StoreAt".into(),
            SketchAST::Sequence(_, _) => "Sequence".into(),
        }
    }

    fn size(&self) -> i32 {
        match self {
            SketchAST::Produce(_, _, child) => child.size() + 1,
            SketchAST::Consume(_, _, child) => child.size() + 1,
            SketchAST::For(_, _, child, _) => child.size() + 1,
            SketchAST::Assign(_, _) => 1,
            SketchAST::Sequence(_, children) => children.iter().map(|child| child.size()).sum(),
            SketchAST::Vectorize(_, _, child, _) => child.size() + 1,
            SketchAST::StoreAt(_, _) => 1,
        }
    }
}

fn process_ident(opts: &Options, sequence: Pair<Rule>) -> Variable {
    match sequence.as_rule() {
        Rule::ident => {
            if opts.debug_parser {
                println!("Got an ident");
            }
            let name = sequence.as_str();
            Variable{name: name.into()}
        },
        _ => panic!("Unable to process non-ident sequence '{}' into variable", sequence.as_str())
    }
}

fn process_range(opts: &Options, sequence: Pair<Rule>) -> RangeAST {
    match sequence.as_rule() {
        Rule::optional_range => {
            // whitespace
            let mut inner = sequence.into_inner();

            // we have a rnage
            if inner.len() > 0 {
                // range
                let mut range = inner.next().unwrap().into_inner();

                // println!("Range is {}", range);
                let _ = range.next(); // whitespace
                //let _ = range.next(); // in
                let _ = range.next(); // whitespace
                //let _ = range.next(); // [
                let start = range.next().unwrap().as_str().parse::<i32>().unwrap(); // start
                let _ = range.next(); // whitespace
                // let _ = range.next(); // ,
                let _ = range.next(); // whitespace
                let end = range.next().unwrap().as_str().parse::<i32>().unwrap(); // end
                let res = RangeAST::Between(start, end);

                if opts.debug_parser {
                    println!("Parsed a range of {}", res.to_string());
                }

                res
            } else {
                if opts.debug_parser {
                    println!("Parsed a range of all") 
                }
                // no range
                RangeAST::All()
            }
        },
        _ => panic!("Must pass optinal_range to process_range")
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
            let ident = process_ident(opts, inner.next().unwrap());

            // Parser produces un-nested code --- nest it later.
            SketchAST::Produce(nesting_depth, ident,
                Box::new(SketchAST::Sequence(nesting_depth, Vec::new())))
        },
		Rule::consume => {
			if opts.debug_parser {
                println!("Got a consume");
            }
            let mut inner = sequence.into_inner();

            let _title = inner.next(); // consume
		    // let _ = inner.next(); // whitespace
            let ident = process_ident(opts, inner.next().unwrap());

            // Parser produces un-nested code --- nest it later.
            SketchAST::Consume(nesting_depth, ident,
                Box::new(SketchAST::Sequence(nesting_depth, Vec::new())))
        },
        Rule::pfor => {
            if opts.debug_parser {
                println!("Got a for");
            }
            let mut inner = sequence.into_inner();

            let _title = inner.next(); // for
                                       // let _ = inner.next(); // whitespace
            let ident = process_ident(opts, inner.next().unwrap());
            let range = process_range(opts, inner.next().unwrap());

            // Parser produces un-nested code --- nest it later.
            SketchAST::For(nesting_depth, ident,
                Box::new(SketchAST::Sequence(nesting_depth, Vec::new())),
                range)
        },
        Rule::assignment => {
            if opts.debug_parser {
                println!("Got an assignment");
            }
            let mut inner = sequence.into_inner();

            let ident = process_ident(opts, inner.next().unwrap());

            SketchAST::Assign(nesting_depth, ident)
        }
        Rule::store_at => {
            if opts.debug_parser {
                println!("Got a store_at");
            }
            let mut inner = sequence.into_inner();
            let _ = inner.next(); // whitespace
            let ident = process_ident(opts, inner.next().unwrap());
            SketchAST::StoreAt(nesting_depth, ident)
        }
        Rule::vectorize => {
            if opts.debug_parser {
                println!("Got a vectorize");
            }

            let mut inner = sequence.into_inner();
            let _ = inner.next(); // whitespace token
            // let _ = inner.next(); // whitespace
            let ident = process_ident(opts, inner.next().unwrap());
            let range = process_range(opts, inner.next().unwrap());

            SketchAST::Vectorize(nesting_depth, ident,
                Box::new(SketchAST::Sequence(nesting_depth, Vec::new())),
                range)
        }
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
        SketchAST::Consume(n, _, _) => n,
        SketchAST::For(n, _, _, _) => n,
        SketchAST::Assign(n, _) => n,
        SketchAST::Sequence(n, _) => n,
        SketchAST::Vectorize(n, _, _, _) => n,
        SketchAST::StoreAt(n, _) => n,
    }
}

// set the loop nest contents
fn set_nest(v: &SketchAST, nest: SketchAST) -> SketchAST {
    if nest.size() == 0 {
        v.clone()
    } else {
        match v {
            SketchAST::Produce(n, var, current_nest) => {
                assert!(current_nest.size() == 0); // check we aren't deleting anything
                SketchAST::Produce(n.clone(), var.clone(), Box::new(nest))
            },
            SketchAST::Consume(n, var, current_nest) => {
                assert!(current_nest.size() == 0); // check we aren't deleting anything
                SketchAST::Consume(n.clone(), var.clone(), Box::new(nest))
            },
            SketchAST::For(n, var, current_nest, range) => {
                assert!(current_nest.size() == 0); // check we aren't deleting anything
                SketchAST::For(n.clone(), var.clone(), Box::new(nest), range.clone())
            }
            SketchAST::Vectorize(n, var, current_nest, range) => {
                assert!(current_nest.size() == 0); // check we aren't deleting anything
                SketchAST::Vectorize(n.clone(), var.clone(), Box::new(nest), range.clone())
            },
            SketchAST::StoreAt(_n, _var) => panic!("Can't set nest to a store"),
            SketchAST::Assign(_n, _var) => panic!("Can't set nest to an assign"),
            SketchAST::Sequence(_n, _nest) => panic!("Can't set nest to a sequence"),
        }
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
