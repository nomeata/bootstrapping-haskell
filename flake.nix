{
  inputs.flake-utils.url = "github:numtide/flake-utils";

  description = "Bootstrappable GHC experiments";

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages."${system}";
      hugs = pkgs.hugs.overrideAttrs(old: {
        src = ./hugs98-Sep2006;
      });
    in
    {
      packages = {
        inherit hugs;
      };
      devShells.default = pkgs.mkShell {
        packages = [ pkgs.hugs pkgs.happy ];
      };
    });
}
