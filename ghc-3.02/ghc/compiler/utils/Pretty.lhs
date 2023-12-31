*********************************************************************************
*										*
*	John Hughes's and Simon Peyton Jones's Pretty Printer Combinators	*
*										*
*		based on "The Design of a Pretty-printing Library"		*
*		in Advanced Functional Programming, 				*
*		Johan Jeuring and Erik Meijer (eds), LNCS 925 			*
* 		http://www.cs.chalmers.se/~rjmh/Papers/pretty.ps		*
*										*
*		Heavily modified by Simon Peyton Jones, Dec 96			*
*										*
*********************************************************************************

Version 2.0	24 April 1997
  * Made empty into a left unit for <> as well as a right unit;
    it is also now true that
	nest k empty = empty
    which wasn't true before.

  * Fixed an obscure bug in sep that occassionally gave very wierd behaviour

  * Added $+$

  * Corrected and tidied up the laws and invariants

======================================================================
Relative to John's original paper, there are the following new features:

1.  There's an empty document, "empty".  It's a left and right unit for 
    both <> and $$, and anywhere in the argument list for
    sep, hcat, hsep, vcat, fcat etc.

    It is Really Useful in practice.

2.  There is a paragraph-fill combinator, fsep, that's much like sep,
    only it keeps fitting things on one line until itc can't fit any more.

3.  Some random useful extra combinators are provided.  
	<+> puts its arguments beside each other with a space between them,
	    unless either argument is empty in which case it returns the other


	hcat is a list version of <>
	hsep is a list version of <+>
	vcat is a list version of $$

	sep (separate) is either like hsep or like vcat, depending on what fits

	cat  is behaves like sep,  but it uses <> for horizontal conposition
	fcat is behaves like fsep, but it uses <> for horizontal conposition

	These new ones do the obvious things:
		char, semi, comma, colon, space,
		parens, brackets, braces, 
		quotes, doubleQuotes
	
4.	The "above" combinator, $$, now overlaps its two arguments if the
	last line of the top argument stops before the first line of the second begins.
	For example:  text "hi" $$ nest 5 "there"
	lays out as
			hi   there
	rather than
			hi
			     there

	There are two places this is really useful

	a) When making labelled blocks, like this:
		Left ->   code for left
	        Right ->  code for right
		LongLongLongLabel ->
			  code for longlonglonglabel
	   The block is on the same line as the label if the label is
	   short, but on the next line otherwise.

	b) When laying out lists like this:
		[ first
		, second
		, third
		]
	   which some people like.  But if the list fits on one line
	   you want [first, second, third].  You can't do this with
	   John's original combinators, but it's quite easy with the
	   new $$.

	The combinator $+$ gives the original "never-overlap" behaviour.

5.	Several different renderers are provided:
		* a standard one
		* one that uses cut-marks to avoid deeply-nested documents 
			simply piling up in the right-hand margin
		* one that ignores indentation (fewer chars output; good for machines)
		* one that ignores indentation and newlines (ditto, only more so)

6.	Numerous implementation tidy-ups
	Use of unboxed data types to speed up the implementation



\begin{code}
module Pretty (
	Doc, 		-- Abstract
	Mode(..), TextDetails(..),

	empty, nest,

	text, char, ptext,
	int, integer, float, double, rational,
	parens, brackets, braces, quotes, doubleQuotes,
	semi, comma, colon, space, equals,
	lparen, rparen, lbrack, rbrack, lbrace, rbrace,

	(<>), (<+>), hcat, hsep, 
	($$), ($+$), vcat, 
	sep, cat, 
	fsep, fcat, 

	hang, punctuate,
	
--	renderStyle,		-- Haskell 1.3 only
	render, fullRender
  ) where

#include "HsVersions.h"

import FastString
import GlaExts

-- Don't import Util( assertPanic ) because it makes a loop in the module structure

infixl 6 <> 
infixl 6 <+>
infixl 5 $$, $+$
\end{code}



*********************************************************
*							*
\subsection{CPP magic so that we can compile with both GHC and Hugs}
*							*
*********************************************************

The library uses unboxed types to get a bit more speed, but these CPP macros
allow you to use either GHC or Hugs.  To get GHC, just set the CPP variable
	__GLASGOW_HASKELL__

\begin{code}

#if defined(__GLASGOW_HASKELL__)


-- Glasgow Haskell

-- Disable ASSERT checks; they are expensive!
#define LOCAL_ASSERT(x)

#define INT	Int#
#define MINUS	-#
#define NEGATE  negateInt#
#define PLUS	+#
#define GR	>#
#define GREQ	>=#
#define LT	<#
#define DIV	`quotInt#`


#define SHOW	Show
#define MAXINT	maxBound

#else

-- Standard Haskell

#define LOCAL_ASSERT(x)

#define INT	Int
#define IBOX(x)	x
#define MINUS	-
#define NEGATE  negate
#define PLUS	+
#define GR	>
#define GREQ	>=
#define LT	<
#define DIV	`quot`
#define ILIT(x) x

#define SHOW	Show
#define MAXINT	maxBound

#endif

\end{code}


*********************************************************
*							*
\subsection{The interface}
*							*
*********************************************************

The primitive @Doc@ values

\begin{code}
empty  			  :: Doc
text			  :: String -> Doc 
char  			  :: Char -> Doc

semi, comma, colon, space, equals	       :: Doc
lparen, rparen, lbrack, rbrack, lbrace, rbrace :: Doc

parens, brackets, braces  :: Doc -> Doc 
quotes, doubleQuotes	  :: Doc -> Doc

int	 :: Int -> Doc
integer  :: Integer -> Doc
float	 :: Float -> Doc
double	 :: Double -> Doc
rational :: Rational -> Doc
\end{code}

Combining @Doc@ values

\begin{code}
(<>)   :: Doc -> Doc -> Doc	-- Beside
hcat   :: [Doc] -> Doc		-- List version of <>
(<+>)  :: Doc -> Doc -> Doc	-- Beside, separated by space
hsep   :: [Doc] -> Doc		-- List version of <+>

($$)   :: Doc -> Doc -> Doc 	-- Above; if there is no
				-- overlap it "dovetails" the two
vcat   :: [Doc] -> Doc		-- List version of $$

cat    :: [Doc] -> Doc		-- Either hcat or vcat
sep    :: [Doc] -> Doc		-- Either hsep or vcat
fcat   :: [Doc] -> Doc		-- ``Paragraph fill'' version of cat
fsep   :: [Doc] -> Doc		-- ``Paragraph fill'' version of sep

nest   :: Int -> Doc -> Doc	-- Nested
\end{code}

GHC-specific ones.

\begin{code}
hang :: Doc -> Int -> Doc -> Doc
punctuate :: Doc -> [Doc] -> [Doc]	-- punctuate p [d1, ... dn] = [d1 <> p, d2 <> p, ... dn-1 <> p, dn]
\end{code}

Displaying @Doc@ values. 

\begin{code}
instance SHOW Doc where
  showsPrec prec doc cont = showDoc doc cont

render     :: Doc -> String		-- Uses default style
fullRender :: Mode
	   -> Int			-- Line length
	   -> Float			-- Ribbons per line
	   -> (TextDetails -> a -> a)	-- What to do with text
	   -> a				-- What to do at the end
	   -> Doc
	   -> a				-- Result

{-	When we start using 1.3 
renderStyle  :: Style -> Doc -> String
data Style = Style { lineLength     :: Int,	-- In chars
		     ribbonsPerLine :: Float,	-- Ratio of ribbon length to line length
		     mode :: Mode
	     }
style :: Style		-- The default style
style = Style { lineLength = 100, ribbonsPerLine = 2.5, mode = PageMode }
-}

data Mode = PageMode 		-- Normal 
	  | ZigZagMode		-- With zig-zag cuts
	  | LeftMode		-- No indentation, infinitely long lines
	  | OneLineMode		-- All on one line

\end{code}


*********************************************************
*							*
\subsection{The @Doc@ calculus}
*							*
*********************************************************

The @Doc@ combinators satisfy the following laws:
\begin{verbatim}
Laws for $$
~~~~~~~~~~~
<a1>	(x $$ y) $$ z	= x $$ (y $$ z)
<a2>	empty $$ x	= x
<a3>	x $$ empty 	= x

	...ditto $+$...

Laws for <>
~~~~~~~~~~~
<b1>	(x <> y) <> z	= x <> (y <> z)
<b2>	empty <> x	= empty
<b3>	x <> empty	= x

	...ditto <+>...

Laws for text
~~~~~~~~~~~~~
<t1>	text s <> text t	= text (s++t)
<t2>	text "" <> x		= x, if x non-empty

Laws for nest
~~~~~~~~~~~~~
<n1>	nest 0 x		= x
<n2>	nest k (nest k' x)	= nest (k+k') x
<n3>	nest k (x <> y)		= nest k z <> nest k y
<n4>	nest k (x $$ y)		= nest k x $$ nest k y
<n5>	nest k empty		= empty
<n6>	x <> nest k y		= x <> y, if x non-empty

** Note the side condition on <n6>!  It is this that
** makes it OK for empty to be a left unit for <>.

Miscellaneous
~~~~~~~~~~~~~
<m1>	(text s <> x) $$ y = text s <> ((text "" <> x)) $$ 
					 nest (-length s) y)

<m2>	(x $$ y) <> z = x $$ (y <> z)
	if y non-empty


Laws for list versions
~~~~~~~~~~~~~~~~~~~~~~
<l1> 	sep (ps++[empty]++qs)   = sep (ps ++ qs)
	...ditto hsep, hcat, vcat, fill...

<l2>	nest k (sep ps) = sep (map (nest k) ps)
	...ditto hsep, hcat, vcat, fill...

Laws for oneLiner
~~~~~~~~~~~~~~~~~
<o1>	oneLiner (nest k p) = nest k (oneLiner p)
<o2>	oneLiner (x <> y)   = oneLiner x <> oneLiner y 
\end{verbatim}


You might think that the following verion of <m1> would
be neater:
\begin{verbatim}
<3 NO>	(text s <> x) $$ y = text s <> ((empty <> x)) $$ 
					 nest (-length s) y)
\end{verbatim}
But it doesn't work, for if x=empty, we would have
\begin{verbatim}
	text s $$ y = text s <> (empty $$ nest (-length s) y)
		    = text s <> nest (-length s) y
\end{verbatim}



*********************************************************
*							*
\subsection{Simple derived definitions}
*							*
*********************************************************

\begin{code}
semi  = char ';'
colon = char ':'
comma = char ','
space = char ' '
equals = char '='
lparen = char '('
rparen = char ')'
lbrack = char '['
rbrack = char ']'
lbrace = char '{'
rbrace = char '}'

int	 n = text (show n)
integer  n = text (show n)
float    n = text (show n)
double   n = text (show n)
--ORIG: rational n = text (show n)
rational n = text (show (fromRationalX n)) -- _showRational 30 n)

quotes p	= char '`' <> p <> char '\''
doubleQuotes p	= char '"' <> p <> char '"'
parens p	= char '(' <> p <> char ')'
brackets p	= char '[' <> p <> char ']'
braces p	= char '{' <> p <> char '}'


hcat = foldr (<>)  empty
hsep = foldr (<+>) empty
vcat = foldr ($$)  empty

hang d1 n d2 = sep [d1, nest n d2]

punctuate p []     = []
punctuate p (d:ds) = go d ds
		   where
		     go d [] = [d]
		     go d (e:es) = (d <> p) : go e es
\end{code}


*********************************************************
*							*
\subsection{The @Doc@ data type}
*							*
*********************************************************

A @Doc@ represents a {\em set} of layouts.  A @Doc@ with
no occurrences of @Union@ or @NoDoc@ represents just one layout.
\begin{code}
data Doc
 = Empty				-- empty
 | NilAbove Doc				-- text "" $$ x
 | TextBeside TextDetails INT Doc	-- text s <> x	
 | Nest INT Doc				-- nest k x
 | Union Doc Doc			-- ul `union` ur
 | NoDoc				-- The empty set of documents
 | Beside Doc Bool Doc			-- True <=> space between
 | Above  Doc Bool Doc			-- True <=> never overlap

type RDoc = Doc		-- RDoc is a "reduced Doc", guaranteed not to have a top-level Above or Beside


reduceDoc :: Doc -> RDoc
reduceDoc (Beside p g q) = beside p g (reduceDoc q)
reduceDoc (Above  p g q) = above  p g (reduceDoc q)
reduceDoc p		 = p


data TextDetails = Chr  Char
		 | Str  String
		 | PStr FAST_STRING
space_text = Chr ' '
nl_text    = Chr '\n'
\end{code}

Here are the invariants:
\begin{itemize}
\item
The argument of @NilAbove@ is never @Empty@. Therefore
a @NilAbove@ occupies at least two lines.

\item
The arugment of @TextBeside@ is never @Nest@.

\item 
The layouts of the two arguments of @Union@ both flatten to the same string.

\item 
The arguments of @Union@ are either @TextBeside@, or @NilAbove@.

\item
The right argument of a union cannot be equivalent to the empty set (@NoDoc@).
If the left argument of a union is equivalent to the empty set (@NoDoc@),
then the @NoDoc@ appears in the first line.

\item 
An empty document is always represented by @Empty@.
It can't be hidden inside a @Nest@, or a @Union@ of two @Empty@s.

\item 
The first line of every layout in the left argument of @Union@
is longer than the first line of any layout in the right argument.
(1) ensures that the left argument has a first line.  In view of (3),
this invariant means that the right argument must have at least two
lines.
\end{itemize}

\begin{code}
	-- Arg of a NilAbove is always an RDoc
nilAbove_ p = LOCAL_ASSERT( ok p ) NilAbove p
	    where
	      ok Empty = False
	      ok other = True

	-- Arg of a TextBeside is always an RDoc
textBeside_ s sl p = TextBeside s sl (LOCAL_ASSERT( ok p ) p)
		   where
		     ok (Nest _ _) = False
		     ok other      = True

	-- Arg of Nest is always an RDoc
nest_ k p = Nest k (LOCAL_ASSERT( ok p ) p)
	  where
	    ok Empty = False
	    ok other = True

	-- Args of union are always RDocs
union_ p q = Union (LOCAL_ASSERT( ok p ) p) (LOCAL_ASSERT( ok q ) q)
	   where
	     ok (TextBeside _ _ _) = True
	     ok (NilAbove _)       = True
	     ok (Union _ _)        = True
	     ok other	           = False
\end{code}


Notice the difference between
	* NoDoc (no documents)
	* Empty (one empty document; no height and no width)
	* text "" (a document containing the empty string;
		   one line high, but has no width)



*********************************************************
*							*
\subsection{@empty@, @text@, @nest@, @union@}
*							*
*********************************************************

\begin{code}
empty = Empty

char  c = textBeside_ (Chr c) 1# Empty
text  s = case length   s of {IBOX(sl) -> textBeside_ (Str s)  sl Empty}
ptext s = case _LENGTH_ s of {IBOX(sl) -> textBeside_ (PStr s) sl Empty}

nest IBOX(k)  p = mkNest k (reduceDoc p)	-- Externally callable version

-- mkNest checks for Nest's invariant that it doesn't have an Empty inside it
mkNest k       (Nest k1 p) = mkNest (k PLUS k1) p
mkNest k       NoDoc       = NoDoc
mkNest k       Empty       = Empty
mkNest ILIT(0) p	   = p			-- Worth a try!
mkNest k       p 	   = nest_ k p

-- mkUnion checks for an empty document
mkUnion Empty q = Empty
mkUnion p q     = p `union_` q
\end{code}

*********************************************************
*							*
\subsection{Vertical composition @$$@}
*							*
*********************************************************


\begin{code}
p $$  q = Above p False q
p $+$ q = Above p True q

above :: Doc -> Bool -> RDoc -> RDoc
above (Above p g1 q1)  g2 q2 = above p g1 (above q1 g2 q2)
above p@(Beside _ _ _) g  q  = aboveNest (reduceDoc p) g ILIT(0) (reduceDoc q)
above p g q		     = aboveNest p	       g ILIT(0) (reduceDoc q)

aboveNest :: RDoc -> Bool -> INT -> RDoc -> RDoc
-- Specfication: aboveNest p g k q = p $g$ (nest k q)

aboveNest NoDoc               g k q = NoDoc
aboveNest (p1 `Union` p2)     g k q = aboveNest p1 g k q `union_` 
	      		              aboveNest p2 g k q
	  		        
aboveNest Empty               g k q = mkNest k q
aboveNest (Nest k1 p)         g k q = nest_ k1 (aboveNest p g (k MINUS k1) q)
	      		          -- p can't be Empty, so no need for mkNest
	  		        
aboveNest (NilAbove p)        g k q = nilAbove_ (aboveNest p g k q)
aboveNest (TextBeside s sl p) g k q = textBeside_ s sl rest
				    where
				      k1   = k MINUS sl
				      rest = case p of
						Empty -> nilAboveNest g k1 q
						other -> aboveNest  p g k1 q
\end{code}

\begin{code}
nilAboveNest :: Bool -> INT -> RDoc -> RDoc
-- Specification: text s <> nilaboveNest g k q 
--		= text s <> (text "" $g$ nest k q)

nilAboveNest g k Empty       = Empty	-- Here's why the "text s <>" is in the spec!
nilAboveNest g k (Nest k1 q) = nilAboveNest g (k PLUS k1) q

nilAboveNest g k q 	     | (not g) && (k GR ILIT(0))	-- No newline if no overlap
			     = textBeside_ (Str (spaces k)) k q
			     | otherwise			-- Put them really above
			     = nilAbove_ (mkNest k q)
\end{code}


*********************************************************
*							*
\subsection{Horizontal composition @<>@}
*							*
*********************************************************

\begin{code}
p <>  q = Beside p False q
p <+> q = Beside p True  q

beside :: Doc -> Bool -> RDoc -> RDoc
-- Specification: beside g p q = p <g> q
 
beside NoDoc               g q   = NoDoc
beside (p1 `Union` p2)     g q   = (beside p1 g q) `union_` (beside p2 g q)
beside Empty               g q   = q
beside (Nest k p)          g q   = nest_ k (beside p g q)	-- p non-empty
beside p@(Beside p1 g1 q1) g2 q2 
	   {- (A `op1` B) `op2` C == A `op1` (B `op2` C)  iff op1 == op2 
				                 [ && (op1 == <> || op1 == <+>) ] -}
         | g1 == g2              = beside p1 g1 (beside q1 g2 q2)
         | otherwise             = beside (reduceDoc p) g2 q2
beside p@(Above _ _ _)     g q   = beside (reduceDoc p) g q
beside (NilAbove p)        g q   = nilAbove_ (beside p g q)
beside (TextBeside s sl p) g q   = textBeside_ s sl rest
			       where
				  rest = case p of
					   Empty -> nilBeside g q
					   other -> beside p g q
\end{code}

\begin{code}
nilBeside :: Bool -> RDoc -> RDoc
-- Specification: text "" <> nilBeside g p 
--		= text "" <g> p

nilBeside g Empty      = Empty	-- Hence the text "" in the spec
nilBeside g (Nest _ p) = nilBeside g p
nilBeside g p	       | g         = textBeside_ space_text ILIT(1) p
		       | otherwise = p
\end{code}

*********************************************************
*							*
\subsection{Separate, @sep@, Hughes version}
*							*
*********************************************************

\begin{code}
-- Specification: sep ps  = oneLiner (hsep ps)
--			   `union`
--			    vcat ps

sep = sepX True		-- Separate with spaces
cat = sepX False	-- Don't

sepX x []     = empty
sepX x (p:ps) = sep1 x (reduceDoc p) ILIT(0) ps


-- Specification: sep1 g k ys = sep (x : map (nest k) ys)
--			      = oneLiner (x <g> nest k (hsep ys))
--				`union` x $$ nest k (vcat ys)

sep1 :: Bool -> RDoc -> INT -> [Doc] -> RDoc
sep1 g NoDoc 		   k ys = NoDoc
sep1 g (p `Union` q)       k ys = sep1 g p k ys
				  `union_`
				  (aboveNest q False k (reduceDoc (vcat ys)))

sep1 g Empty               k ys = mkNest k (sepX g ys)
sep1 g (Nest n p)          k ys = nest_ n (sep1 g p (k MINUS n) ys)

sep1 g (NilAbove p)        k ys = nilAbove_ (aboveNest p False k (reduceDoc (vcat ys)))
sep1 g (TextBeside s sl p) k ys = textBeside_ s sl (sepNB g p (k MINUS sl) ys)

-- Specification: sepNB p k ys = sep1 (text "" <> p) k ys
-- Called when we have already found some text in the first item
-- We have to eat up nests

sepNB g (Nest _ p)  k ys  = sepNB g p k ys

sepNB g Empty k ys        = oneLiner (nilBeside g (reduceDoc rest))
				`mkUnion` 
			    nilAboveNest False k (reduceDoc (vcat ys))
			  where
			    rest | g	     = hsep ys
				 | otherwise = hcat ys

sepNB g p k ys		  = sep1 g p k ys
\end{code}

*********************************************************
*							*
\subsection{@fill@}
*							*
*********************************************************

\begin{code}
fsep = fill True
fcat = fill False

-- Specification: 
--   fill []  = empty
--   fill [p] = p
--   fill (p1:p2:ps) = oneLiner p1 <#> nest (length p1) 
--				       	    (fill (oneLiner p2 : ps))
--	   	       `union`
--			p1 $$ fill ps

fill g []     = empty
fill g (p:ps) = fill1 g (reduceDoc p) ILIT(0) ps


fill1 :: Bool -> RDoc -> INT -> [Doc] -> Doc
fill1 g NoDoc	 	    k ys = NoDoc
fill1 g (p `Union` q)       k ys = fill1 g p k ys
				   `union_`
				   (aboveNest q False k (fill g ys))

fill1 g Empty               k ys = mkNest k (fill g ys)
fill1 g (Nest n p)          k ys = nest_ n (fill1 g p (k MINUS n) ys)

fill1 g (NilAbove p)        k ys = nilAbove_ (aboveNest p False k (fill g ys))
fill1 g (TextBeside s sl p) k ys = textBeside_ s sl (fillNB g p (k MINUS sl) ys)

fillNB g (Nest _ p)  k ys  = fillNB g p k ys
fillNB g Empty k []        = Empty
fillNB g Empty k (y:ys)    = nilBeside g (fill1 g (oneLiner (reduceDoc y)) k1 ys)
			     `mkUnion` 
			     nilAboveNest False k (fill g (y:ys))
			   where
			     k1 | g 	    = k MINUS ILIT(1)
				| otherwise = k

fillNB g p k ys		   = fill1 g p k ys
\end{code}


*********************************************************
*							*
\subsection{Selecting the best layout}
*							*
*********************************************************

\begin{code}
best :: Mode
     -> Int		-- Line length
     -> Int		-- Ribbon length
     -> RDoc
     -> RDoc		-- No unions in here!

best OneLineMode IBOX(w) IBOX(r) p
  = get p
  where
    get Empty               = Empty
    get NoDoc               = NoDoc
    get (NilAbove p)        = nilAbove_ (get p)
    get (TextBeside s sl p) = textBeside_ s sl (get p)
    get (Nest k p)          = get p		-- Elide nest
    get (p `Union` q)       = first (get p) (get q)

best mode IBOX(w) IBOX(r) p
  = get w p
  where
    get :: INT		-- (Remaining) width of line
        -> Doc -> Doc
    get w Empty               = Empty
    get w NoDoc               = NoDoc
    get w (NilAbove p)        = nilAbove_ (get w p)
    get w (TextBeside s sl p) = textBeside_ s sl (get1 w sl p)
    get w (Nest k p)          = nest_ k (get (w MINUS k) p)
    get w (p `Union` q)       = nicest w r (get w p) (get w q)

    get1 :: INT		-- (Remaining) width of line
         -> INT		-- Amount of first line already eaten up
         -> Doc		-- This is an argument to TextBeside => eat Nests
         -> Doc		-- No unions in here!

    get1 w sl Empty               = Empty
    get1 w sl NoDoc               = NoDoc
    get1 w sl (NilAbove p)        = nilAbove_ (get (w MINUS sl) p)
    get1 w sl (TextBeside t tl p) = textBeside_ t tl (get1 w (sl PLUS tl) p)
    get1 w sl (Nest k p)          = get1 w sl p
    get1 w sl (p `Union` q)       = nicest1 w r sl (get1 w sl p) 
				                   (get1 w sl q)

nicest w r p q = nicest1 w r ILIT(0) p q
nicest1 w r sl p q | fits ((w `minn` r) MINUS sl) p = p
		   | otherwise 	                 = q

fits :: INT	-- Space available
     -> Doc
     -> Bool	-- True if *first line* of Doc fits in space available
 
fits n p    | n LT ILIT(0) = False
fits n NoDoc               = False
fits n Empty               = True
fits n (NilAbove _)        = True
fits n (TextBeside _ sl p) = fits (n MINUS sl) p

minn x y | x LT y    = x
	 | otherwise = y
\end{code}

@first@ and @nonEmptySet@ are similar to @nicest@ and @fits@, only simpler.
@first@ returns its first argument if it is non-empty, otherwise its second.

\begin{code}
first p q | nonEmptySet p = p 
	  | otherwise     = q

nonEmptySet NoDoc           = False
nonEmptySet (p `Union` q)      = True
nonEmptySet Empty	       = True
nonEmptySet (NilAbove p)       = True		-- NoDoc always in first line
nonEmptySet (TextBeside _ _ p) = nonEmptySet p
nonEmptySet (Nest _ p)         = nonEmptySet p
\end{code}

@oneLiner@ returns the one-line members of the given set of @Doc@s.

\begin{code}
oneLiner :: Doc -> Doc
oneLiner NoDoc               = NoDoc
oneLiner Empty               = Empty
oneLiner (NilAbove p)        = NoDoc
oneLiner (TextBeside s sl p) = textBeside_ s sl (oneLiner p)
oneLiner (Nest k p)          = nest_ k (oneLiner p)
oneLiner (p `Union` q)       = oneLiner p
\end{code}



*********************************************************
*							*
\subsection{Displaying the best layout}
*							*
*********************************************************


\begin{code}
{-
renderStyle Style{mode, lineLength, ribbonsPerLine} doc 
  = fullRender mode lineLength ribbonsPerLine doc ""
-}

render doc       = showDoc doc ""
showDoc doc rest = fullRender PageMode 100 1.5 string_txt rest doc

string_txt (Chr c)   s  = c:s
string_txt (Str s1)  s2 = s1 ++ s2
string_txt (PStr s1) s2 = _UNPK_ s1 ++ s2
\end{code}

\begin{code}

fullRender OneLineMode _ _ txt end doc = easy_display space_text txt end (reduceDoc doc)
fullRender LeftMode    _ _ txt end doc = easy_display nl_text    txt end (reduceDoc doc)

fullRender mode line_length ribbons_per_line txt end doc
  = display mode line_length ribbon_length txt end best_doc
  where 
    best_doc = best mode hacked_line_length ribbon_length (reduceDoc doc)

    hacked_line_length, ribbon_length :: Int
    ribbon_length = round (fromInt line_length / ribbons_per_line)
    hacked_line_length = case mode of { ZigZagMode -> MAXINT; other -> line_length }

display mode IBOX(page_width) IBOX(ribbon_width) txt end doc
  = case page_width MINUS ribbon_width of { gap_width ->
    case gap_width DIV ILIT(2) of { shift ->
    let
    	lay k (Nest k1 p)  = lay (k PLUS k1) p
    	lay k Empty        = end
    
    	lay k (NilAbove p) = nl_text `txt` lay k p
    
    	lay k (TextBeside s sl p)
	    = case mode of
		    ZigZagMode |  k GREQ gap_width
			       -> nl_text `txt` (
				  Str (multi_ch shift '/') `txt` (
				  nl_text `txt` (
				  lay1 (k MINUS shift) s sl p)))

			       |  k LT ILIT(0)
			       -> nl_text `txt` (
				  Str (multi_ch shift '\\') `txt` (
				  nl_text `txt` (
				  lay1 (k PLUS shift) s sl p )))

		    other -> lay1 k s sl p
    
    	lay1 k s sl p = Str (indent k) `txt` (s `txt` lay2 (k PLUS sl) p)
    
    	lay2 k (NilAbove p)        = nl_text `txt` lay k p
    	lay2 k (TextBeside s sl p) = s `txt` (lay2 (k PLUS sl) p)
    	lay2 k (Nest _ p)          = lay2 k p
    	lay2 k Empty               = end
    in
    lay ILIT(0) doc
    }}

cant_fail = error "easy_display: NoDoc"
easy_display nl_text txt end doc 
  = lay doc cant_fail
  where
    lay NoDoc 		    no_doc = no_doc
    lay (Union p q)	    no_doc = {- lay p -} (lay q cant_fail)		-- Second arg can't be NoDoc
    lay (Nest k p)   	    no_doc = lay p no_doc
    lay Empty        	    no_doc = end
    lay (NilAbove p) 	    no_doc = nl_text `txt` lay p cant_fail	-- NoDoc always on first line
    lay (TextBeside s sl p) no_doc = s `txt` lay p no_doc

indent n | n GREQ ILIT(8) = '\t' : indent (n MINUS ILIT(8))
	 | otherwise	  = spaces n

multi_ch ILIT(0) ch = ""
multi_ch n       ch = ch : multi_ch (n MINUS ILIT(1)) ch

spaces ILIT(0) = ""
spaces n       = ' ' : spaces (n MINUS ILIT(1))
\end{code}

Doesn't really belong here..

-----------------------------------
\begin{code}
-- from Lennart
fromRationalX :: (RealFloat a) => Rational -> a

fromRationalX r =
	let
	    h = ceiling (huge `asTypeOf` x)
	    b = toInteger (floatRadix x)
	    x = fromRat 0 r
	    fromRat e0 r' =
		let d = denominator r'
		    n = numerator r'
		in  if d > h then
		       let e = integerLogBase b (d `div` h) + 1
		       in  fromRat (e0-e) (n % (d `div` (b^e)))
		    else if abs n > h then
		       let e = integerLogBase b (abs n `div` h) + 1
		       in  fromRat (e0+e) ((n `div` (b^e)) % d)
		    else
		       scaleFloat e0 (fromRational r')
	in  x

-- Compute the discrete log of i in base b.
-- Simplest way would be just divide i by b until it's smaller then b, but that would
-- be very slow!  We are just slightly more clever.
integerLogBase :: Integer -> Integer -> Int
integerLogBase b i =
     if i < b then
	0
     else
	-- Try squaring the base first to cut down the number of divisions.
	let l = 2 * integerLogBase (b*b) i

	    doDiv :: Integer -> Int -> Int
	    doDiv j k = if j < b then k else doDiv (j `div` b) (k+1)
	in
	doDiv (i `div` (b^l)) l


------------

-- Compute smallest and largest floating point values.
{-
tiny :: (RealFloat a) => a
tiny =
	let (l, _) = floatRange x
	    x = encodeFloat 1 (l-1)
	in  x
-}

huge :: (RealFloat a) => a
huge =
	let (_, u) = floatRange x
	    d = floatDigits x
	    x = encodeFloat (floatRadix x ^ d - 1) (u - d)
	in  x
\end{code}
