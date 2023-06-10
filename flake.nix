{
  inputs.flake-utils.url = "github:numtide/flake-utils";

  description = "A very basic flake";

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let pkgs = nixpkgs.legacyPackages."${system}"; in
    {
      devShells.default = pkgs.mkShell {
        packages = [ pkgs.hugs ];
      };
    });
}
