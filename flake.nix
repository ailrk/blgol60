{
  description = "blgol60";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskell.packages.ghc928;
        packageName = "blgol60";
        commands = {
          # limit the amount of memory can be used when running a program
          limitmem = pkgs.writeShellScriptBin "limitmem" ''
            LIMIT=$1
            shift
            systemd-run --user --scope -p MemoryMax="$LIMIT" -p MemorySwapMax=0 -- $@
          '';
        };

      in
      {
        packages = {
          ${packageName} =
            pkgs.haskell.lib.dontCheck (haskellPackages.callCabal2nix packageName self rec { });

          default = self.packages.${system}.${packageName};
        };

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            haskell-language-server
            fourmolu
            ghcid
            cabal-install
            pkgs.treefmt
            pkgs.nixpkgs-fmt
            commands.limitmem
            ghc
          ];
          inputFrom = builtins.attrValues self.packages.${system};
        };
      });
}
