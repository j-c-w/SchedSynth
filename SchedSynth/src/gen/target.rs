use crate::ast::ast::Func;
use crate::ast::ast::Var;
use crate::reshape::reshape::Reshape;
use crate::gen::halide::HalideProgram;
use crate::gen::exo::ExoProgram;
use crate::ast::ast::Property;

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
    fn get_holes(&self) -> Vec<&dyn Hole> {
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
    fn get_holes(&self) -> Vec<&dyn Hole>;
}

pub trait Hole {
    fn to_opentuner(&self) -> String;
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
