use clap::{App, Arg};

pub struct Options {
	// Keep track of whether struct is initilzied --- apparently
	// the static way of keeping this around doesn't work in
	// multi-threaded programs, so keep this field around for
	// debugging.
	pub initialized: bool,

	// positional args -- these are files.
	pub source: String,
	pub target: String,

	// debug flags
	pub debug_parser: bool,
	pub debug_synthesizer: bool,
}

pub fn parse_options() -> Options {
    let args = App::new("SchedSynth")
        .version("1.0")
        .about("Synthesize Halide schedules from skeletons")
        .arg(
            Arg::with_name("source")
                .help("Source file")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::with_name("target")
                .help("Target file")
                .required(true)
                .index(2),
        )
        .arg(
            Arg::new("debug-parser")
            .long("debug-parser")
            .help("debug the skeleton parser")
        )
		.arg(
			Arg::new("debug-synthesizer")
			.long("debug-synthesizer")
			.help("debug the synthesizer")
		).get_matches();

    // initialize to defaults
    let opts: Options = Options {
        initialized: true,

        source: args.value_of("source").unwrap().into(),
        target: args.value_of("target").unwrap().into(),

        debug_parser: args.is_present("debug-parser"),
		debug_synthesizer: args.is_present("debug-synthesizer"),
    };

    return opts;
}
