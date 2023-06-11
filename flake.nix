{
  inputs.flake-utils.url = "github:numtide/flake-utils";

  description = "Bootstrappable GHC experiments";

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages."${system}";
      hugs = pkgs.hugs.overrideAttrs(old: {
        src = ./hugs98-Sep2006;
      });
      # This GCC version is too old for 64-bit support, or many platforms
      # so we force it to build on a compatible system. Sorry if you can't
      # cross-compile this!
      gcc295 = (import ./gcc-2.95.3.nix { pkgs = nixpkgs.legacyPackages.i686-linux; });
    in
    {
      packages = {
        inherit hugs gcc295;
      };
      devShells.default = pkgs.mkShell {
        packages = [ hugs pkgs.happy gcc295 ];
      };
    });
}
