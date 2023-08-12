mod ast;
mod gen;
mod options;
mod sketch_parse;
mod synth;
mod utils;
mod reshape;
mod runner;

fn main() {
    let options = options::options::parse_options();

    let sketch_original = sketch_parse::parser::parse(&options, &options.source);
    let sketch_target = sketch_parse::parser::parse(&options, &options.target);


    // lower to ast
    let ast_original = ast::convert::ast_from_sketch_ast(sketch_original);
    let ast_target = ast::convert::ast_from_sketch_ast(sketch_target);

    // Get the reshapes
    let reshapes = sketch_parse::splits_parser::parse(&options, &ast_original, &options.reshapes);

    let synthed_option = synth::synth::synthesize_from_sketch(&options, &ast_original, &ast_target, &reshapes);
    let output = gen::halide::generate(&options, synthed_option);

    std::fs::write(options.dest, output.clone()).expect("File write failed");

    println!("Output: {}", output);
}
