{
  inputs.flake-utils.url = "github:numtide/flake-utils";

  description = "Bootstrappable GHC experiments";

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let pkgs = nixpkgs.legacyPackages."${system}"; in
    {
      devShells.default = pkgs.mkShell {
        packages = [ pkgs.hugs pkgs.happy ];
      };
    });

