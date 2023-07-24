use crate::options::options::Options;
use std::collections::HashMap;
use std::collections::HashSet;
use crate::ast::ast::AST;
use crate::ast::ast::Var;

use crate::utils::format::hashset_to_string;

pub enum Reshape {
    Split(Var, Var, (Var, Var), i32), // under producer var, split var into (var, var) by factor
    Fuse(Var, (Var, Var), Var)
}

pub fn find_splits_internal(opts: &Options, original_ast: &AST, target_ast: &AST) -> Vec<Reshape> {
    // First, go through and find variables that are differnet between these two ASTs.
    let original_variables = variables(original_ast);
    let target_variables = variables(target_ast);


    // Now, we need to go through and create plausible mappings between
    // variables.  Mappings can either be to induce fuses, or to induce
    // splits.  Define a transformation as follows:
    //     T(x, y) -> z (join)
    //     T(x) -> (y, z) (split)
    // We go through and infer bounds on the split later.
    

    // Do this in a two-phase manner: first introduce splits to make
    // all the new variables required, then introduce fuses to remove
    // all the unwanted variables.   Use variable name information
    // to determine which variables should be split into what.

    // First get the variables that we have to induce (set diff of target_variables -
    // original_variables):
    let to_induce = target_variables.difference(&original_variables).cloned().collect();
    let splits = induce_splits(opts, original_variables, to_induce);

    // Now, we have all the T(x) -> (y, z).  We want to go through and find the
    // T(x, y) -> z.
    // TODO


    // TODO -- merge any fuses that undo splits.
    let reshapes: Vec<Reshape> = splits
        .iter()
        // TODO -- need to get the correct func.
        .map(|spl| Reshape::Split(Var {name: "default_func".into()}, spl.0.clone(), spl.1.clone(), 0))
        .collect();

    // TODO --- infer the correct factor for each reshape.
    reshapes
}

// Figure out which variables should be split to create variables_to_create
fn induce_splits(opts: &Options, variables_from: HashSet<Var>, variables_to_create: HashSet<Var>) -> HashMap<Var, (Var, Var)> { 
    if opts.debug_split {
        println!("Building variables {} from variables {}", hashset_to_string(&variables_to_create), hashset_to_string(&variables_from));
    }

    // iterate over variables_to_create --- using the odds_variable_is_split_from(v1, v2) to 
    // determine to put v2 into a vec of candidates for v1 (sorted by the odds)
    let mut candidates: HashMap<Var, Vec<(Var, f32)>> = HashMap::new();
    for v1 in &variables_from {
        for v2 in &variables_to_create {
            let odds = odds_variable_is_split_from(v2, v1);
            if opts.debug_split {
                println!("Odds {} is split from {} is {}", v1, v2, odds);
            }
            if odds > 0.0 {
                let entry = candidates.entry(v1.clone()).or_insert(Vec::new());
                entry.push((v2.clone(), odds));
            }
        }
    }

    // insert two temp variables --- with odds 0.5 --- these will later
    // have to be fused.

    // sort the candidates by odds
    for (_, v) in candidates.iter_mut() {
        v.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap());
    }
    
    // Now we need to go through candidates, finding the most
    // likely overall pairings: need each vec to be exactly two elements
    // long --- and no variable to appear in any vec more than once.
    // We use a greedy approach to this here, but should really
    // do a more co-optimized approach.
    let mut splits: HashMap<Var, (Var, Var)> = HashMap::new();
    let mut used_variables: HashSet<Var> = HashSet::new();
    for (split, intos) in candidates.iter() {
        // We need each into variable to appear exactly once.
        let mut into_vars = Vec::new();

        for (invar, _likelyhood) in intos {
            if used_variables.contains(invar) {
                // skip this variable --- already used.
            } else if into_vars.len() < 2 {
                // TODO -- order probably matters here and we should
                // probably be infering the order of variables
                // somwhere.
                into_vars.push(invar);
                used_variables.insert(invar.clone());
            }
        };

        if into_vars.len() == 2 {
            if opts.debug_split {
                println!("Splitting {} into {} and {}", split, into_vars[0], into_vars[1]);
            }
            splits.insert(split.clone(), (into_vars[0].clone(), into_vars[1].clone()));
        } else {
            // Not every variable has to be split /from/.
            if opts.debug_split {
                println!("Variable {} is not being split because only {} candidates were found to split into (2 required)", split, into_vars.len());
            }
        }
    }

    // compute that variables that we were unable to find
    // assignments for:
    let unassigned_variables: HashSet<Var> = variables_to_create.difference(&used_variables).cloned().collect();

    // There are no more variables to be split into 
    if unassigned_variables.len() == 0 {
        splits
    } else {
        // There are still variables that we have to split into. --- recurse and do this.
        if unassigned_variables.len() == variables_to_create.len() {
            // in this call, we didn't manage to split into any of the
            // variables that we wanted to split into --- we are making
            // no progress, so panic.
            // 
            // This panic means that we are guaranteed to terminate as long
            // as unassigned_variables is of finite size.
            panic!("Making no progress creating variables {}", hashset_to_string(&variables_to_create));
        } else {
            // we made progress so recurse and see.
            let new_split_variables = variables_to_create.union(&used_variables).cloned().collect();

            induce_splits(opts, new_split_variables, unassigned_variables)
        }
    }
}


// This is a heuristic function that gives a boolean
// guessing where a variable was split from --- it should probably
// eventually return a float 0-1 to give a rangking
fn odds_variable_is_split_from(variable: &Var, from: &Var) -> f32 {
    // check if from.name is a substring in variable.name
    let from_name: &String = &from.name;
    let variable_name = &variable.name;

    // TODO --- do a better job at this. --- need to properly support the 'dot' notation
    // used in hlaide
    if variable_name.contains(from_name) {
        return 1.0;
    } else {
        return 0.0;
    }
}

// Get all the Vars in the AST by recursively
// walkthing through it.
fn variables(ast: &AST) -> HashSet<Var> {
 let mut vars = HashSet::new();
    match ast {
        AST::Produce(_var, ast) => {
            // dont include func vars
            vars.extend(variables(&*ast));
        },
        AST::Consume(_var, ast) => {
            // dont include func vars
            vars.extend(variables(&*ast));
            vars.extend(variables(&*ast));
        },
        AST::For(var, ast, _range) => {
            vars.insert(var.clone());
            vars.extend(variables(&*ast));
        },
        AST::Assign(_var) => {
            // this is a func var
        },
        AST::Vectorize(var, ast, _range) => {
            vars.insert(var.clone());
            vars.extend(variables(&*ast));
        },
        AST::Sequence(asts) => {
            for ast in asts {
                vars.extend(variables(&ast));
            }
        }
    }
    vars
}
