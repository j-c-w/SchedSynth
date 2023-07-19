mod sketch_parse;
mod gen;
mod synth;
mod options;

fn main() {
    let options = options::options::parse_options();

    let original = sketch_parse::parser::parse(&options, &options.source);
    let target = sketch_parse::parser::parse(&options, &options.target);

    let synthed_option = synth::synth::synthesize_from_sketch(&options, original, target);
    let output = gen::halide::generate(&options, synthed_option);
    println!("{}", output);
}
