# Build instructions:

`nix-shell` fetches all dependencies.  Requires nix (see nixos.org to install).

Then, we need some extra pyton packages.  Run:

python -m virtualenv env
source env/bin/activate

pip install pulp


Then, go into `SchedSynth` and run `cargo build`


# Usage:

The tool takes four arguments: a file containing the original loop structure, a file containing the target loop structure, a file containing the split information and the output file.

For example, we could have the following files:

Original:

def f:
  for x:
    for y:
      compute

Target:

def f:
  parallel y:
    for x1:
	  vectorized x2:
	    compute

Splits:
x->x1,x2,4

And then we can do:

`cargo run Original Target Splits Output

# Examples and paper reproduction:

The examples from the paper are in scripts/apps.

The format is: <benchmark_name>_{in,out,splits} to indicate the original, the target and the required splits.

For example, to run the blur example, we can run (from the scripts/apps) folder:

`cargo run blur_in blur_out blur_splits output`
