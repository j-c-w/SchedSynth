use crate::options::options::Options;
use std::process::Command;
use std::fs::File;
use std::time::Instant;
use std::io::Read;
use std::io::Write;
use crate::gen::target::Target;

#[derive(Clone)]
pub struct ExecutionResult {
    pub execution_time: f32,
    pub exit_status: i32,
}

pub fn best_schedule<T: Target>(opts: &Options, schedules: Vec<T>) -> T {
    if schedules.len() == 0 {
        panic!("Trying to get best schedule form empty list");
    } else if schedules.len() == 1 {
        return schedules[0].clone();
    } else {
        let string_schedules = schedules.iter().map(|x| x.generate()).collect();
        let rankings = evaluate_options(opts, string_schedules);
        let best_program = get_best_ranking(&rankings);
        schedules[best_program as usize].clone()
    }
}

// Get the best ranking execution result
fn get_best_ranking(exec_results: &Vec<ExecutionResult>) -> i32 {
    let mut best_ranking = -1;
    // Set as FP infinity
    let mut best_time: f32 = std::f32::INFINITY;

    for (i, result) in exec_results.iter().enumerate() {
        if result.execution_time < best_time {
            best_ranking = i as i32;
            best_time = result.execution_time;
        }
    }

    if best_ranking < 0 {
        panic!("Error: all executions failed!");
    }
    best_ranking
}

pub fn evaluate_options(opts: &Options, schedules: Vec<String>) -> Vec<ExecutionResult> {
    let mut results = Vec::new();
    let mut number = 0;

    for schedule in schedules {
        let temp_file = format!("{}.c", number);
        create_runnable(opts, opts.halide_program.clone(), temp_file.clone(), schedule);
        let runnable = build_runnable(opts, temp_file.clone());
        let result = execute_runnable(opts, runnable);
        results.push(result.clone());

        number = number + 1;
    }

    results
}

// Read in the file in template_file,
// then write it out into opts.execution_dir with
// the SCHED_CONTENT string replaced with the schedule
// Return the (unique
fn create_runnable(opts: &Options, template_file: String, target_file: String, schedule: String) {
    // check if opts.execution_dir is a directory -- if not, create it.
    let execution_dir = std::path::PathBuf::from(&opts.execution_dir);
    if !execution_dir.exists() {
        std::fs::create_dir_all(&execution_dir).expect("Failed to create execution directory");
    }

    let mut template_contents = String::new();
    let target_file = format!("{}/{}", opts.execution_dir.clone(), target_file);

    let mut file = File::open(&template_file).expect(format!("Unable to open template file {}.  Use --halide-program to specify a template that can be filled by the scheduler", template_file).as_str());
    file.read_to_string(&mut template_contents).expect("Unable to read template file");
    let new_contents = template_contents.replace("SCHED_CONTENT", &schedule);
    let mut file = File::create(&target_file).expect("Unable to create target file");
    file.write_all(new_contents.as_bytes()).expect("Unable to write to target file");
}

// Build the file C file using the Halide build command g++ test.c -I<opts.halide_dir>/include -L<opts.halide_dir>lib -lHalide -lpthread -ldl
fn build_runnable(opts: &Options, target_file: String) -> String {
    let target_file = format!("{}/{}", opts.execution_dir.clone(), target_file);
    let mut halide_include_dir = opts.halide_dir.clone();
    halide_include_dir.push_str("include");
    let mut halide_lib_dir = opts.halide_dir.clone();
    halide_lib_dir.push_str("lib");
    let mut command = String::from("g++ ");
    command.push_str(&target_file);
    command.push_str(" -I");
    command.push_str(&halide_include_dir);
    command.push_str(" -L");
    command.push_str(&halide_lib_dir);
    command.push_str(" -lHalide -lpthread -ldl");
    command.push_str(" -o");
    command.push_str(&target_file);
    command.push_str(".o");
    let output = Command::new("sh")
        .arg("-c")
        .arg(&command)
        .output()
        .expect("Failed to execute build command");
    if opts.debug_execution {
        println!("Compile output: {:?}", output);
    }

    return format!("{}.o", target_file);
}

fn execute_runnable(opts: &Options, executable_name: String) -> ExecutionResult {
    // TODO -- parse the output of the halide runner rather tahan
    // using the time measurement here.
    let start_time = Instant::now();

    let output = Command::new(executable_name)
        .output()
        .expect("failed to execute process");

    let end_time = Instant::now();

    let execution_time = end_time.duration_since(start_time).as_secs_f32();

    if opts.debug_execution {
        println!("Execution Output: {:?}", output);
    }

    ExecutionResult {
        execution_time: execution_time,
        // get the i32 from ExitStatus
        exit_status: output.status.code().unwrap_or(-1),
    }
}
