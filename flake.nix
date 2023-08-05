{
  inputs = {
    systems.url = "github:nix-systems/default";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
 
  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } {
    systems = import inputs.systems;
    imports = [
      inputs.haskell-flake.flakeModule
    ];
    perSystem = { self', system, lib, config, pkgs, ... }: {
      haskellProjects.default = {
        devShell.hlsCheck.enable = false;
        autoWire = [];
      };
      packages.default = config.haskellProjects.default.outputs.packages.techtonica.package;
      devShells.default = pkgs.mkShell {
        inputsFrom = [
          config.haskellProjects.default.outputs.devShell
        ];
      };
    };
  };
}

