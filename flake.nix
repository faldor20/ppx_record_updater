{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flakelight.url = "github:nix-community/flakelight";
  };

  outputs =
    { flakelight, ... }@inputs:
    flakelight ./. {
      inherit inputs;

      # default devshell
      devShell.packages =
        pkgs: with pkgs; [
          ocaml
          dune_3
          ocamlPackages.utop
          ocamlPackages.ocaml-lsp
          ocamlPackages.odoc
        ];

      # Define the main package
      package =
        {
          ocamlPackages,
          lib,
          defaultMeta,
        }:
        ocamlPackages.buildDunePackage {
          pname = "ppx_record_updater";
          version = "0.1.0";

          src = ./.;

          buildInputs = with ocamlPackages; [
            ppxlib
            ppx_deriving
          ];

          meta = defaultMeta // {
            description = "A PPX deriver that generates an update function and type for dynamically performing partial updates on a record.";
          };
        };

    };
}
