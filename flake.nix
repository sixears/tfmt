{
  description = "type-safe text/string formatting with a simple interface";

  inputs = {
    nixpkgs.url      = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url  = "github:sixears/flake-build-utils/r1.0.0.6";

    base0t.url        = "github:sixears/base0t/r0.0.1.4";
    has-callstack.url = "github:sixears/has-callstack/r1.0.1.8";
    more-unicode.url  = "github:sixears/more-unicode/r0.0.17.5";
    number.url        = "github:sixears/number/r1.1.2.6";
  };

  outputs = { self, nixpkgs, flake-utils, build-utils
            , base0t, has-callstack, more-unicode, number }:
    build-utils.lib.hOutputs self nixpkgs "tfmt" {
      deps = {
        inherit base0t has-callstack more-unicode number;
      };
      unbreak = hpkgs: { inherit (hpkgs) text-format; };
      # we're pinned to GHC 8 for now, because we're using text-format-0.3.2,
      # which has an upper bound on `base` of < 4.15 - ghc 9 uses >= 4.15.1
      ghc = p: p.ghc8107;
    };
}
