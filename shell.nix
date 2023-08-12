{ pkgs ? import<nixpkgs> {} }:

with pkgs;
mkShell rec {
	buildInputs = [ gcc rustc cargo zlib.dev zlib ];
	SHELL_NAME = "SchedSynth";

	shellHook = ''
		export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath buildInputs}:$LD_LIBRARY_PATH"
		export LD_LIBRARY_PATH="${pkgs.stdenv.cc.cc.lib.outPath}/lib:$LD_LIBRARY_PATH"
	'';

}
