use crate::ast::ast::Func;
use crate::ast::ast::Var;
use crate::reshape::reshape::Reshape;
use crate::gen::halide::HalideProgram;
use crate::gen::exo::ExoProgram;
use crate::ast::ast::Property;
use core::iter::Map;
use std::collections::HashMap;
use std::collections::HashSet;
use crate::shared::range_set::Range;
use crate::shared::range_set::IntegerRangeSet;
use crate::shared::range_set::TotalOrderRange;

#[derive(PartialEq,Clone,Copy)]
pub enum Backend {
    Halide(),
    Exo()
}

#[derive(Clone)]
pub enum BackendInstance {
    Halide(HalideProgram),
    Exo(ExoProgram)
}

impl Target for BackendInstance { }

impl TargetGenerate for BackendInstance {
    fn generate(&self) -> String {
        match self {
            BackendInstance::Halide(hp) => hp.generate(),
            BackendInstance::Exo(ep) => ep.generate()
        }
    }
}

impl TargetLower for BackendInstance {
    fn to_vectorize(&mut self, commands: Vec<(Func, Var, Property)>) {
        match self {
            BackendInstance::Halide(hp) => hp.to_vectorize(commands),
            BackendInstance::Exo(ep) => ep.to_vectorize(commands)
        }
    }

    fn to_unroll(&mut self, commands: Vec<(Func, Var, Property)>) {
        match self {
            BackendInstance::Halide(hp) => hp.to_unroll(commands),
            BackendInstance::Exo(ep) => ep.to_unroll(commands)
        }
    }

    fn to_parallel(&mut self, commands: Vec<(Func, Var, Property)>) {
        match self {
            BackendInstance::Halide(hp) => hp.to_parallel(commands),
            BackendInstance::Exo(ep) => ep.to_parallel(commands)
        }
    }

    fn to_store_at(&mut self, commands: Vec<(Func, Func, Var)>) {
        match self {
            BackendInstance::Halide(hp) => hp.to_store_at(commands),
            BackendInstance::Exo(ep) => ep.to_store_at(commands)
        }
    }

    fn to_compute_at(&mut self, commands: Vec<(Func, Option<Func>, Option<Var>)>) {
        match self {
            BackendInstance::Halide(hp) => hp.to_compute_at(commands),
            BackendInstance::Exo(ep) => ep.to_compute_at(commands)
        }
    }

    fn to_reorder(&mut self, commands: Vec<(Func, Var, Var)>) {
        match self {
            BackendInstance::Halide(hp) => hp.to_reorder(commands),
            BackendInstance::Exo(ep) => ep.to_reorder(commands)
        }
    }

    fn to_reshape(&mut self, commands: &Vec<Reshape>) {
        match self {
            BackendInstance::Halide(hp) => hp.to_reshape(commands),
            BackendInstance::Exo(ep) => ep.to_reshape(commands)
        }
    }
}

impl TargetHoles for BackendInstance {
    fn get_holes(&self) -> Vec<Box<dyn Hole>> {
        match self {
            BackendInstance::Halide(hp) => hp.get_holes(),
            BackendInstance::Exo(ep) => ep.get_holes()
        }
    }
}

pub fn newBackend(typ: Backend) -> BackendInstance {
    match typ {
        Backend::Exo() => BackendInstance::Exo(ExoProgram { commands: Vec::new(), funcs: Vec::new()
        }),
        Backend::Halide() => BackendInstance::Halide(HalideProgram { commands: Vec::new() })
    }
}

pub trait Target: TargetGenerate + TargetLower + TargetHoles + Clone {}
pub trait CommandType: ToString {}

pub trait TargetGenerate {
    fn generate(&self) -> String;
}

pub trait TargetHoles {
    fn get_holes(&self) -> Vec<Box<dyn Hole>>;
}

pub trait Hole {
    fn to_opentuner(&self) -> String;
    fn get_name(&self) -> String;
    // TODO -- need a function to fill holes by name.
}

pub enum HoleValue {
    IntHole(i32)
}

#[derive(Clone)]
pub enum HoleOption<T> {
    Hole(String, HashSet<T>),
    IntHole(String, IntegerRangeSet),
    Value(T)
}

impl<T: std::fmt::Display> Hole for HoleOption<T> {
 fn to_opentuner(&self) -> String {
        match self {
            &HoleOption::Hole(ref name, ref set) => {
                let mut s = String::new();
                s.push_str(&format!("param.{} = hp.Choice('{}', [", name, name));
                for (i, v) in set.iter().enumerate() {
                    s.push_str(&format!("{}", v));
                    if i < set.len() - 1 {
                        s.push_str(", ");
                    }
                }
                s.push_str("])");
                s
            },
            &HoleOption::IntHole(ref name, ref range) => {
                let mut s = String::new();
                s.push_str(&format!("param.{} = hp.UniformInteger('{}', {}, {})", name, name, range.min_elt(), range.max_elt()));
                s
            },
            &HoleOption::Value(ref v) => {
                format!("{}", v)
            }
        }
    }

    fn get_name(&self) -> String {
        match self {
            &HoleOption::Hole(ref name, _) => name.clone(),
            &HoleOption::IntHole(ref name, _) => name.clone(),
            &HoleOption::Value(_) => panic!("Cannot get name of a value")
        }
    }
}

pub fn is_hole<T>(opt: &HoleOption<T>) -> bool {
    match opt {
        HoleOption::Hole(_, _) => true,
        HoleOption::IntHole(_, _) => true,
        _ => false
    }
}

impl <T: std::fmt::Display> std::fmt::Display for HoleOption<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HoleOption::Hole(name, _) => write!(f, "Hole({})", name),
            HoleOption::IntHole(name, _) => write!(f, "Hole({})", name),
            HoleOption::Value(value) => write!(f, "{}", value)
        }
    }
}

pub struct HoleBindingMap {
    pub map: HashMap<String, HoleValue>,
}

pub trait TargetLower {
    fn to_vectorize(&mut self, commands: Vec<(Func, Var, Property)>);
    fn to_unroll(&mut self, commands: Vec<(Func, Var, Property)>);
    fn to_parallel(&mut self, commands: Vec<(Func, Var, Property)>);
    fn to_store_at(&mut self, commands: Vec<(Func, Func, Var)>);
    fn to_compute_at(&mut self, commands: Vec<(Func, Option<Func>, Option<Var>)>);
    fn to_reorder(&mut self, commands: Vec<(Func, Var, Var)>);
    fn to_reshape(&mut self, commands: &Vec<Reshape>);
}
