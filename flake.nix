{
  description = "type-safe text/string formatting with a simple interface";

  inputs = {
    nixpkgs.url      = github:nixos/nixpkgs/be44bf67; # nixos-22.05 2022-10-15
    build-utils.url  = github:sixears/flake-build-utils/r1.0.0.13;

    base0t.url        = github:sixears/base0t/r0.0.1.12;
    has-callstack.url = github:sixears/has-callstack/r1.0.1.16;
    more-unicode.url  = github:sixears/more-unicode/r0.0.17.11;
    number.url        = github:sixears/number/r1.1.2.13;
  };

  outputs = { self, nixpkgs, build-utils
            , base0t, has-callstack, more-unicode, number }:
    build-utils.lib.hOutputs self nixpkgs "tfmt" {
      unbreak = hpkgs: { inherit (hpkgs) text-format; };
      # we're pinned to GHC 8 for now, because we're using text-format-0.3.2,
      # which has an upper bound on `base` of < 4.15 - ghc 9 uses >= 4.15.1
      ghc = p: p.ghc8107;

      callPackage = { mkDerivation, lib, mapPkg, system
                    , base, containers, data-textual, formatting, lens, parsers
                    , prettyprinter, process, tasty, tasty-hunit
                    , template-haskell, text, text-format, text-printer, time
                    , trifecta
                    }:
        let
          pkg = build-utils.lib.flake-def-pkg system;
        in
          mkDerivation {
            pname = "tfmt";
            version = "0.2.7.17";
            src = ./.;
            libraryHaskellDepends = [
              base containers data-textual formatting lens parsers prettyprinter
              process tasty tasty-hunit template-haskell text text-format
              text-printer time trifecta
            ] ++ map (p: pkg p) [ base0t has-callstack more-unicode number ] ;

            testHaskellDepends = [ base tasty ];
            description = "type-safe text/string formatting with a simple interface";
            license = lib.licenses.mit;
          };

    };
}
