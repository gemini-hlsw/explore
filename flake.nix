{
  inputs = {
    typelevel-nix.url = "github:typelevel/typelevel-nix";
    nixpkgs.follows = "typelevel-nix/nixpkgs";
    flake-utils.follows = "typelevel-nix/flake-utils";
    sops-nix.url = "github:Mic92/sops-nix";
    sops-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, typelevel-nix, sops-nix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs-x86_64 = import nixpkgs { system = "x86_64-darwin"; };
        scala-cli-overlay = final: prev: { scala-cli = pkgs-x86_64.scala-cli; };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ typelevel-nix.overlays.default scala-cli-overlay];
        };

        # SOPS secrets
        secretValues = {
          FONTAWESOME_NPM_AUTH_TOKEN = "SOPS_ENCRYPTED_FONTAWESOME_NPM_AUTH_TOKEN";
          GPP_SLACK_WEBHOOK_URL = "SOPS_ENCRYPTED_GPP_SLACK_WEBHOOK_URL";
        };
      in
      {
        devShell = pkgs.devshell.mkShell {
          imports = [ typelevel-nix.typelevelShell ];
          packages = [
            pkgs.nodePackages.typescript-language-server
            pkgs.nodePackages.vscode-langservers-extracted
            pkgs.nodePackages.prettier
            pkgs.nodePackages.typescript
            pkgs.nodePackages.graphqurl
            pkgs.hasura-cli
            pkgs.sops
            pkgs.age
            pkgs.yq
            pkgs.direnv
          ];
          typelevelShell = {
            nodejs.enable = true;
            jdk.package = pkgs.jdk21;
          };
          env = [
            {
              name = "NODE_OPTIONS";
              value = "--max-old-space-size=8192";
            }
            {
              name = "FONTAWESOME_NPM_AUTH_TOKEN";
              value = secretValues.FONTAWESOME_NPM_AUTH_TOKEN;
            }
            {
              name = "GPP_SLACK_WEBHOOK_URL";
              value = secretValues.GPP_SLACK_WEBHOOK_URL;
            }
          ];
        };
      }

    );
}
