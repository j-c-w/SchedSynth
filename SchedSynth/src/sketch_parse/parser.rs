use pest::Parser;
use pest::iterators::Pair;
use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "sketch_parse/grammar.pest"]
struct LoopParser;

#[derive(Clone)]
pub struct Variable {
	name: String
}

#[derive(Clone)]
pub enum SketchAST { // nodes have nesting, <other stuff>
	Produce(i32, Box<Variable>, Box<SketchAST>), // name, contents
	For(i32, Box<Variable>, Box<SketchAST>), // variable name, sub-contents
	Assign(i32, Box<Variable>), // variable name
	Sequence(i32, Vec<SketchAST>) // list of sub-asts
}


// Trait for SketchAST.
trait AST {
    fn children(&self) -> Vec<SketchAST>;
    fn node_type(&self) -> String;
}

impl ToString for Variable {
    fn to_string(&self) -> String {
        self.name.clone()
    }
}

impl ToString for SketchAST {
    fn to_string(&self) -> String {
        match self {
            SketchAST::Produce(_, n, _) => format!("Produce {}", n.to_string().clone()),
            SketchAST::For(_, n, _) => format!("For {}", n.to_string().clone()),
            SketchAST::Assign(_, n) => format!("{} = ...", n.to_string().clone()),
            SketchAST::Sequence(_, _) => format!("Sequence")
        }
    }

}

impl AST for SketchAST {
    fn children(&self) -> Vec<SketchAST> {
        match self {
            SketchAST::Produce(_nest, _n, children) => {
                let mut res = Vec::new();
                res.push(**children);
                res
            },
            SketchAST::For(_nest, _n, children) => {
                let mut res = Vec::new();
                res.push(**children);
                res
            },
            SketchAST::Assign(_nest, _n) => {
                let res = Vec::new();
                res
            },
            SketchAST::Sequence(_nest, children) => children.to_vec()
        }
    }

    fn node_type(&self) -> String {
        match self {
            SketchAST::Produce(_, _, _) => "Produce".into(),
            SketchAST::For(_, _, _) => "For".into(),
            SketchAST::Assign(_, _) => "Assign".into(),
            SketchAST::Sequence(_, _) => "Sequence".into()
        }
    }
}

fn process(sequence: Pair<Rule>) -> SketchAST {
    match sequence.as_rule() {
        Rule::ident => {
            println!("Found ident {:?}", sequence.as_str());
            panic!()
        }
        Rule::sequence_list => {
            let mut inner = sequence.into_inner();

            // For a sequence, get the head, then go through
            // and get the tail.
            // Note that this produces a very flat list --- it doesn't consider
            // how indented things are --- that is handled in a secondary pass.
            let seq = process(inner.next().unwrap());
            if inner.len() > 0 {
                let _ = inner.next(); // Whitespace
                let rest = process(inner.next().unwrap()); // next seq list
                let existing_children = rest.children();
                existing_children.insert(0, seq);
                SketchAST::Sequence(0, existing_children)
            } else {
                let res_vec = Vec::new();
                res_vec.push(seq);
                SketchAST::Sequence(0, res_vec)
            }
        }
        Rule::sequence => {
            let mut _inner = panic!();
        }
        _ => panic!()
    }
}

pub fn parse() {
    let mut sequence = LoopParser::parse(Rule::sequence_list, "produce abd").unwrap();
    process(sequence.next().unwrap());
    for token in sequence.tokens() {
        println!("Token: {:?}", token);
    }
}
