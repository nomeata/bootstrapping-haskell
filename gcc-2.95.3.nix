# As a derivative work of the Guix project, this file is subject to copyright
# under the GPLv3, with the following copyright information:
#
# ;;; GNU Guix --- Functional package management for GNU
# ;;; Copyright © 2012-2023 Ludovic Courtès <ludo@gnu.org>
# ;;; Copyright © 2014 Andreas Enge <andreas@enge.fr>
# ;;; Copyright © 2012 Nikita Karetnikov <nikita@karetnikov.org>
# ;;; Copyright © 2014, 2015, 2017 Mark H Weaver <mhw@netris.org>
# ;;; Copyright © 2017, 2018, 2019, 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
# ;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
# ;;; Copyright © 2018, 2019, 2020, 2021, 2022, 2023 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
# ;;; Copyright © 2019-2022 Marius Bakke <marius@gnu.org>
# ;;; Copyright © 2020, 2022 Timothy Sample <samplet@ngyro.com>
# ;;; Copyright © 2020 Guy Fleury Iteriteka <gfleury@disroot.org>
# ;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
# ;;; Copyright © 2021 Chris Marusich <cmmarusich@gmail.com>
# ;;; Copyright © 2021 Julien Lepiller <julien@lepiller.eu>
# ;;; Copyright © 2022 Ricardo Wurmus <rekado@elephly.net>
# ;;; Copyright © 2022 Ekaitz Zarraga <ekaitz@elenq.tech>
# ;;;
# ;;; This file is part of GNU Guix.
# ;;;
# ;;; GNU Guix is free software; you can redistribute it and/or modify it
# ;;; under the terms of the GNU General Public License as published by
# ;;; the Free Software Foundation; either version 3 of the License, or (at
# ;;; your option) any later version.
# ;;;
# ;;; GNU Guix is distributed in the hope that it will be useful, but
# ;;; WITHOUT ANY WARRANTY; without even the implied warranty of
# ;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# ;;; GNU General Public License for more details.
# ;;;
# ;;; You should have received a copy of the GNU General Public License
# ;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

{ pkgs, ... }:

let
  gcc-boot-patch = ./gcc-boot-2.95.3.patch;

  config-cache = pkgs.writeText "config.cache" ''
    ac_cv_c_float_format='IEEE (little-endian)'
  '';

  bash = pkgs.bash;
  binutils = pkgs.binutils;
  tcc = pkgs.tinycc;
in

# This is a very old version of GCC, used for its compatibility with the equally
# old C code generated by GHC and used in STGHugs.
#
# This derivation is cribbed from Guix, where it is used as part of their bootstrap
# pipeline. For this reason, it doesn't get built with a modern GCC (which I doubt is
# even possible), but instead with a copy of tinycc without a glibc.
#
# Not everything in this file is super deliberate. The main reasons features are
# specified here are
#  * they make it work, or
#  * they were part of the original Guix manifest, and they don't make it not work

pkgs.stdenv.mkDerivation rec {
  name = "gcc295";
  version = "2.95.3";

  src = builtins.fetchurl {
    # Taken from the mirror list randomly. A mirror URL doesn't work here
    # for some reason, but this should be a mirror:// to not always hit this
    # endpoint
    url = "https://ftp.mpi-inf.mpg.de/mirrors/gnu/mirror/gcc.gnu.org/pub/gcc/releases/gcc-2.95.3/gcc-core-${version}.tar.gz";
    sha256 = "1xvfy4pqhrd5v2cv8lzf63iqg92k09g6z9n2ah6ndd4h17k1x0an";
  };

  postUnpackPhase = ''
    patch --force -p1 -i ${gcc-boot-patch}
    cp ${config-cache} $src
  '';

  nativeBuildInputs = [ binutils tcc ];

  configureFlags = [
    "--enable-static"
    "--disable-shared"
    "--disable-werror"
    "--build=i686-unknown-linux-gnu"
    "--host=i686-unknown-linux-gnu"
    "--prefix=$out"
  ];

  configurePhase = ''
    export CONFIG_SHELL=${bash}/bin/bash
    export CFLAGS="-Dinhibit_libc"
    export CC="tcc"
    export CC_FOR_BUILD="$CC"
    export CPP="tcc -E"
    export C_INCLUDE_PATH="include lib/gcc-lib/i686-unknown-linux-gnu/2.95.3/include"
    export LIBRARY_PATH="lib"

    ./configure ${builtins.concatStringsSep " " configureFlags}

    rm -r "texinfo"
    touch 'gcc/cpp.info' 'gcc/gcc.info'
  '';

  makeFlags = [
    "AR=ar"
    "RANLIB=ranlib"
    "LIBGCC2_INCLUDES='-I${tcc}/include'"
    "LANGUAGES=c"
    "BOOT_LDFLAGS='-B${tcc}/lib/'"
  ];

  preInstallPhase = ''
    makeFlagsArray += (CC="tcc -static OLDCC="tcc -static" CC_FOR_BUILD="tcc -static")
  '';

  postInstallPhase = ''
    mkdir -p tmp
    push tmp

    GCCDIR="$out/lib/gcc-lib/i686-unknown-linux-gnu/2.95.3"

    ar x "../gcc/libgcc2.a"
    ar x "${tcc}/lib/libtcc1.a"
    find . -name "*.o" -exec ar r "$GCCDIR/libgcc.a" {} \;
    cp "gcc/libgcc2.a" "$out/lib/libgcc2.a"
    cp "${tcc}/lib/libtcc1.a" "$out/lib/libtcc1.a"
    ar x "${tcc}/lib/libtcc1.a"
    ar x "${tcc}/lib/libc.a"
    ar r "$GCCDIR/libc.a" "libc.o" "libtcc1.o"
  '';
}
