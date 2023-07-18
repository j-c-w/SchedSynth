{ pkgs ? import<nixpkgs> {} }:

with pkgs;
mkShell {
	buildInputs = [ rustc cargo ];
	SHELL_NAME = "SchedSynth";
}
