{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, pretty, stdenv }:
      mkDerivation {
        pname = "language-c-pedantic";
        version = "0.0.1.0";
        src = ./.;
        libraryHaskellDepends = [ base pretty ];
        description = "A library for pedantic C; created with code generation in mind. It has support for the C preprocessor as well.";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
