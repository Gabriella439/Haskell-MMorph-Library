# You can build this repository using Nix by running:
#
#     $ nix-build -A mmorph release.nix
#
# You can also open up this repository inside of a Nix shell by running:
#
#     $ nix-shell -A mmorph.env release.nix
#
# ... and then Nix will supply the correct Haskell development environment for
# you
let
  config = { };

  overlay = pkgsNew: pkgsOld: {
    haskellPackages = pkgsOld.haskellPackages.override (old: {
      overrides =
        pkgsNew.lib.composeExtensions
          (old.overrides or (_: _: {}))
          (pkgsNew.haskell.lib.packageSourceOverrides {
            mmorph = ./.;
          });
    });
  };

  pkgs = import <nixpkgs> { inherit config; overlays = [ overlay ]; };

in
  { mmorph = pkgs.haskellPackages.mmorph;
  }
