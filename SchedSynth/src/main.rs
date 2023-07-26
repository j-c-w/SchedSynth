mod ast;
mod gen;
mod options;
mod sketch_parse;
mod synth;
mod utils;
mod reshape;

fn main() {
    let options = options::options::parse_options();

    let sketch_original = sketch_parse::parser::parse(&options, &options.source);
    let sketch_target = sketch_parse::parser::parse(&options, &options.target);

    let reshapes = sketch_parse::splits_parser::parse(&options, &options.reshapes);

    // lower to ast
    let ast_original = ast::convert::ast_from_sketch_ast(sketch_original);
    let ast_target = ast::convert::ast_from_sketch_ast(sketch_target);

    let synthed_option = synth::synth::synthesize_from_sketch(&options, &ast_original, &ast_target, &reshapes);
    let output = gen::halide::generate(&options, synthed_option);

    println!("Output: {}", output);
}
