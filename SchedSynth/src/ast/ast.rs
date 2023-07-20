use crate::options::options::Options;

#[derive(Clone)]
pub struct Var {
    pub name: String
}

#[derive(Clone)]
pub enum AST {
    Produce(Var, Box<AST>),
    For(Var, Box<AST>),
    Assign(Var),
    Vectorize(Var, Box<AST>),
    Sequence(Vec<AST>)
}

// Gets a list of the the vectorize commands required.
pub fn get_vectorized(opts: &Options, ast: &AST) -> Vec<(Var, Var)> {
    get_vectorized_internal(opts, ast, &None)
}

fn get_vectorized_internal(_opts: &Options, ast: &AST, current_producer: &Option<Var>) -> Vec<(Var, Var)> {
    // recursively walk through the AST and
    // check if there is a vectorize node --- return the producer
    // that contains it, and the variable that is vectorized.
    // when you hit a new produce, reset the producer we are tracking
    match ast {
        AST::Produce(var, ast) => {
            let producer = Some(var.clone());
            get_vectorized_internal(_opts, ast, &producer)
        },
        AST::Vectorize(var, children) => {
            match current_producer {
                Some(p_name) => {
                    let mut v = get_vectorized_internal(_opts, children, current_producer);
                    v.push((p_name.clone(), var.clone()));
                    v
                },
                None => panic!("Vectorize without corresponding producer")
            }
        },
        AST::For(_, ast) => get_vectorized_internal(_opts, ast, current_producer),
        AST::Assign(_) => Vec::new(),
        AST::Sequence(seq) => {
            // recurse on each element of seq, and join the results into a single vec
            let mut v = Vec::new();
            for e in seq.iter() {
                v.extend(get_vectorized_internal(_opts, e, current_producer));
            };
            v
        }
    }
}
