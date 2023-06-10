{
  # nixpkgs from Nov 2014
  # with some fixes that I failed to include via overrides
  inputs.nixpkgs-old.url = github:nomeata/nixpkgs/haskell-boostrap;
  inputs.nixpkgs-old.flake = false;

  description = "A very basic flake";

  outputs = { self, nixpkgs, nixpkgs-old }:
    let
      system = "x86_64-linux";
      system32 = "i686-linux";

      pkgs = nixpkgs.legacyPackages.${system};
      pkgs32 = import nixpkgs-old {
        system = system32;
      };

      nhcsrc = fetchTarball {
        url = "http://www.haskell.org/nhc98/nhc98src-1.22.tar.gz";
        sha256 = "sha256:1kx2agd6fdx7y9g36v7qhlkq1iaag9g0jyx701h3kyyh8kvmvav5";
      };


      nhc = pkgs32.stdenv.mkDerivation {
        name = "nhc";
        src = nhcsrc;
        postUnpack = ''
          # find -name \*.hc -print -delete
        '';
        preConfigure = ''
          STRIP=true
          echo '#!/bin/sh' > script/harch.inst
          echo 'echo "ix86-Linux"'  >> script/harch.inst
        '';
      };

      hugs = pkgs32.hugs;
    in
    {
      packages.${system} = {
        gcc = pkgs32.gcc;
        nhc = nhc;
        mpc = pkgs32.mpc;
      };
      devShells.${system}.default = pkgs.mkShell {
        packages = [ hugs ];
        nhcsrc = nhcsrc;
        # inherit nhc;
      };

    };
}
