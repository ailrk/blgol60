{
  description = "blgol60";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, flake-utils, ... }@inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              blgol60 = (hfinal.callPackage ./default.nix {});
            };
        };
        blgol60 =
          with final.haskell.lib.compose;
          overrideCabal (drv: {
            enableSeparateDataOutput = false;
          }) (justStaticExecutables final.haskellPackages.blgol60);
      };

      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          hspkgs = pkgs.haskellPackages;
        in
        {
          devShells = rec {
            default = blgol60-shell;
            blgol60-shell = pkgs.callPackage ./shell.nix { hspkgs = hspkgs; };
          };

          packages = rec {
            default = blgol60;
            blgol60 = pkgs.blgol60;
          };
        };
    in
    { inherit overlay; } // flake-utils.lib.eachDefaultSystem perSystem;
}
