use crate::gen::halide::HalideCommand;
use crate::sketch_parse::parser::SketchAST;
use crate::options::options::Options;

pub fn synthesize_from_sketch(_opts: &Options, source: SketchAST, target: SketchAST) -> Vec<HalideCommand> {
    Vec::new()
}
