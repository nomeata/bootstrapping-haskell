# RPM spec file for GHC
#
# Copyright [1998..2000] Manuel M. T. Chakravarty <chak@acm.org>
# Thanks to Zoltan Vorosbaranyi <vbzoli@vbzo.li> for suggestions in 
# earlier versions and Pixel <pixel@mandrakesoft.com> for coding tips.
#
# This file is subject to the same free software license as GHC.

# Values for 8.2 and 1 are set by the `configure' script.  
# SNAP releases are CVS snapshots.  Official releases should replace SNAP by
# an appropriate release numbers (they are usually numbered starting from 1).

%define version    8.2
%define patchlevel 1
%define release    SNAP
%define prefix     /usr

Summary: Glasgow Haskell Compilation system
Name: ghc
Version: %{version}
Release: %{release}
Copyright: BSD style w/o adv. clause
Group: Development/Languages
Source: http://haskell.org/ghc/dist/%{version}/ghc-%{version}-src.tar.gz
URL: http://haskell.org/ghc/
BuildRoot: /var/tmp/ghc-%{version}-%{release}-root
Requires: gmp-devel
Provides: haskell
BuildRequires: happy >= 1.6, ghc, stylesheets

%description
The Glorious Glasgow Haskell Compilation System (GHC) is a robust,
fully-featured, optimising compiler for the functional programming
language Haskell 98.  GHC compiles Haskell to either native code or
C. It implements numerous experimental language extensions to Haskell,
including concurrency, a foreign language interface, several
type-system extensions, exceptions, and so on. GHC comes with a
generational garbage collector, a space and time profiler, and a
comprehensive set of libraries.  This package includes HTML and PS
versions of the SGML-based documentation for GHC.  They are also available 
online at http://www.haskell.org/ghc/.

Haskell 98 is "the" standard lazy functional programming language.
More info plus the language definition is at http://www.haskell.org/.

%package prof
Summary: Profiling libraries for GHC
Group: Development/Libraries
Requires: ghc = %{PACKAGE_VERSION}

%description prof
Profiling libraries for Glorious Glasgow Haskell Compilation System (GHC).
They should be installed when GHC's profiling subsystem is needed.

%changelog

* Thu Jun 22 2000 Sven Panne
- removed explicit usage of hslibs/docs, it belongs to ghc/docs/set

* Sun Apr 23 2000 Manuel Chakravarty
- revised for ghc 4.07; added suggestions from Pixel <pixel@mandrakesoft.com>
- added profiling package

* Tue Dec 7 1999 Manuel Chakravarty
- version for use from CVS

* Thu Sep 16 1999 Manuel Chakravarty
- modified for GHC 4.04, patchlevel 1 (no more 62 tuple stuff); minimises use
  of patch files - instead emits a build.mk on-the-fly

* Sat Jul 31 1999 Manuel Chakravarty
- modified for GHC 4.04

* Wed Jun 30 1999 Manuel Chakravarty
- some more improvements from vbzoli

* Fri Feb 26 1999 Manuel Chakravarty
- modified for GHC 4.02

* Thu Dec 24 1998 Zoltan Vorosbaranyi 
- added BuildRoot
- files located in /usr/local/bin, /usr/local/lib moved to /usr/bin, /usr/lib

* Tue Jul 28 1998 Manuel Chakravarty
- original version

%prep
%setup -n fptools

# generate our own `build.mk'
#
# * this is a kludge
#
cat >mk/build.mk <<END
GhcLibWays = p
#SRC_HAPPY_OPTS += -agc # useful from Happy 1.7 onwards
SRC_HAPPY_OPTS += -c
END


%build
./configure --prefix=%{prefix} --libdir=%{prefix}/lib/ghc-%{version}
make boot
make -C glafp-utils sgmlverb
make all
make -C docs ps html
make -C ghc/docs ps html

%install
# compress the non-html docs
#
for j in docs ghc/docs; do
  dir=`pwd`
  cd $j
  for i in ps dvi sgml vsgml verb idx; do
    find . -name '*.'$i -exec gzip -9 '{}' ';' -print
  done
  cd $dir
done
rm -rf $RPM_BUILD_ROOT
make prefix=$RPM_BUILD_ROOT%{prefix}\
     libdir=$RPM_BUILD_ROOT%{prefix}/lib/ghc-%{version} install

# generate the file list for lib/ _excluding_ all files needed for profiling
# only
#
# * generating file lists in a BUILD_ROOT spec is a bit tricky: the file list
#   has to contain complete paths, _but_ without the BUILD_ROOT, we also do
#   _not_ want have directory names in the list; furthermore, we have to make
#   sure that any leading / is removed from %{prefix}/lib, as find has to 
#   interpret the argument as a relative path; however, we have to include the
#   leading / again in the final file list (otherwise, rpm complains)
# * isn't there an easier way to do all this?
#
dir=`pwd`
cd $RPM_BUILD_ROOT
libdir=`echo %{prefix}/lib | sed 's|^/||'`
find $libdir ! -type d ! -name '*.p_hi' ! -name '*_p.a' -print | sed 's|^|/|'\
     >$dir/rpm-noprof-lib-files
cd $dir

%clean
rm -rf $RPM_BUILD_ROOT

%files -f rpm-noprof-lib-files
%doc docs/
%doc ghc/ANNOUNCE ghc/README ghc/docs/
%{prefix}/bin/*

%files prof
%{prefix}/lib/ghc-%{version}/imports/*/*.p_hi
%{prefix}/lib/ghc-%{version}/libHS*_p.a
