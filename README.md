# Bootstrapping Haskell

This repository contains attempts at bootstrapping Haskell. These attempts so
far ar feeble, sporadic and very incomplete, do not expect too much.

## What?

Bootstrapping here means: Building the Haskell ecosystem, in particular GHC,
only from sources, i.e. not relying on any binaries or other generated files.

## Why?

See <https://bootstrappable.org/>.

## Current state

(PRs updating this section are very welcome!)

The most serious work bootstrapping GHC are done by Ricardo Wurmus, also known
as rekado. His approach is to identify a version GHC that is

 * new enough to successively build newer versions, but
 * old enough to somehow be built using Hugs.

Hugs is the most complete Haskell implementation that's _not_ in Haskell (it is
written in C).

These posts of him are relevant and worth reading:

* Jan 2017: https://elephly.net/posts/2017-01-09-bootstrapping-haskell-part-1.html

* Feb 2022: https://elephly.net/paste/1644020383.html

* Feb 2022: Lots of interesting discussion on the guix #bootstrappable IRC channel, starting at
  <https://logs.guix.gnu.org/bootstrappable/2022-02-01.log> until `2022-02-07`.

  Worth noting:

  - GHC-4 patch avoiding lvalue casts in C-code: https://elephly.net/paste/1643877096.patch.html

* May 2022: Ricard writes

  > > Also, I wonder if its really important to use STGHugs instead of normal
  > > Hugs. Your motivation was that STGHugs can load and provide the base
  > > library that the GHC code expects, while Hugs has a different prelude,
  > > right? I wonder how big the differences are and whether one can write a
  > > compatibility module maybe. It might be easier to stay within the
  > > purely interpreted world to run ghc?
  >
  > The differences are significant.  STGHugs is the only *known* way to run
  > GHC with Hugs, because that’s what older GHCs were made to do.  We can’t
  > use combined mode, though,
  >
  > A different route is to interpret GHC with a modern Hugs, but I couldn’t
  > get that to work, because the lower levels of GHC are closely
  > intertwined with its RTS, which is not written in Haskell.  It was easy
  > to overcome other compatibility problems:
  >
  > - recursive module dependencies: split the modules or introduce a new
  >   module that imports A and B.
  > - module size: just make them smaller
  > - non-standard type names: use the standard type names instead, etc
  >
  > But severing the old GHC’s dependency on a runtime system while
  > interpreting the Haskell part with Hugs: that didn’t work and I don’t
  > see an obvious way forward there.

Summary of some relevant facts:

 * Guix has a build chain from GHC 4.08 to 6.10.4 and from 7.8.4 to 9.2.5

   ([according to Simon Tournier](https://www.joachim-breitner.de/blog/802-More_thoughts_on_a_bootstrappable_GHC#comment_1), if someone can contribute a link to the actual code, that would be helpful.)

   The gap can probably be bridged, and just needs doing.

 * [Guix can build GHC 4.08](https://packages.guix.gnu.org/packages/ghc/4.08.2/)
   using the pre-generated C code.

 * It seems that GHC uses GHC-internals of the RTS in the “base library”, so it might be hard to build using Hugs' base library.

 * Hugs has a mode that can use the GHC RTS (aka STGHugs)

 * The RTS system of GHC 4.08 can be built using GCC 2.95

## Next steps?

Here are some possible next steps:

 * Nixify some of the work done in Guix, to allow more people to contribute
   easily, in particular building GHC 4.08 using the pre-generated C code.

 * Build (and nixify) STGHugs, reproducing what rekardo did.

 * Using GHC 4.08, instrument the build system to find out the precise command
   lines and sources used to generate the pre-generated C code, and check if we
   get identical output. This drills down to precisely the execution of GHC
   that we need to reproduce.

 * Try load the GHC source (or parts of it) into STGHugs, and find obstacles such as

   - missing features in Hugs (recursive modules...)
   - different ideas about syntax

   and find ways to fix them, such as

   - removing code from GHC if it is not needed to generate C output
   - modifying the GHC source files to be more compatible with Hugs
   - adding features to Hugs

 * GHC relies on Haskell-code-generating tools for parsing (alex and happy).
   Worth identifying their command line during the GHC build, and investigating

   - if these tools can be run using Hugs
   - if the output can be interpreted by Hugs, and if not, if they have a hugs-compatible mode


## How to help?

Just play around, dump your attempts in this repo, keep this file up-to-date.

## More links

I wrote blog posts with thoughts about boostrapping GHC, but not much there that isn't in this file already:

* https://www.joachim-breitner.de/blog/748-Thoughts_on_bootstrapping_GHC
* https://www.joachim-breitner.de/blog/802-More_thoughts_on_a_bootstrappable_GHC
