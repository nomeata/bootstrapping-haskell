%
% (c) The GRASP Project, Glasgow University, 1992-1996
%
\section[Outputable]{Classes for pretty-printing}

Defines classes for pretty-printing and forcing, both forms of
``output.''

\begin{code}
module Outputable (
	Outputable(..),			-- Class

	PprStyle, 
	getPprStyle, withPprStyle, pprDeeper,
	codeStyle, ifaceStyle, userStyle, debugStyle, asmStyle,
	ifPprDebug, ifNotPprForUser,

	SDoc, 		-- Abstract
	interppSP, interpp'SP, pprQuotedList,
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
	speakNth, speakNTimes,

	showSDoc, printSDoc, printErrs, printDump, 
	printForC, printForAsm, printForIface,
	pprCols,

	-- error handling
	pprPanic, pprPanic#, pprError, pprTrace, assertPprPanic,
	panic, panic#, assertPanic
    ) where

#include "HsVersions.h"

import IO		( Handle, hPutChar, hPutStr, stderr, stdout )
import CmdLineOpts	( opt_PprStyle_All, opt_PprStyle_Debug, opt_PprUserLength )
import FastString
import qualified Pretty
import Pretty		( Doc, Mode(..), TextDetails(..), fullRender )
import Util		( panic, assertPanic, panic# )
import GlaExts		( trace )
\end{code}


%************************************************************************
%*									*
\subsection{The @PprStyle@ data type}
%*									*
%************************************************************************

\begin{code}
data PprStyle
  = PprUser Depth		-- Pretty-print in a way that will
				-- make sense to the ordinary user;
				-- must be very close to Haskell
				-- syntax, etc.

  | PprDebug			-- Standard debugging output

  | PprInterface		-- Interface generation

  | PprCode CodeStyle		-- Print code; either C or assembler


data CodeStyle = CStyle		-- The format of labels differs for C and assembler
	       | AsmStyle

data Depth = AllTheWay
           | PartWay Int	-- 0 => stop
\end{code}

Orthogonal to the above printing styles are (possibly) some
command-line flags that affect printing (often carried with the
style).  The most likely ones are variations on how much type info is
shown.

The following test decides whether or not we are actually generating
code (either C or assembly), or generating interface files.

%************************************************************************
%*									*
\subsection{The @SDoc@ data type}
%*									*
%************************************************************************

\begin{code}
type SDoc = PprStyle -> Doc

withPprStyle :: PprStyle -> SDoc -> SDoc
withPprStyle sty d sty' = d sty

pprDeeper :: SDoc -> SDoc
pprDeeper d (PprUser (PartWay 0)) = Pretty.text "..."
pprDeeper d (PprUser (PartWay n)) = d (PprUser (PartWay (n-1)))
pprDeeper d other_sty             = d other_sty

getPprStyle :: (PprStyle -> SDoc) -> SDoc
getPprStyle df sty = df sty sty
\end{code}

\begin{code}
codeStyle :: PprStyle -> Bool
codeStyle (PprCode _)	  = True
codeStyle _		  = False

asmStyle :: PprStyle -> Bool
asmStyle (PprCode AsmStyle)  = True
asmStyle other               = False

ifaceStyle :: PprStyle -> Bool
ifaceStyle PprInterface	  = True
ifaceStyle other	  = False

debugStyle :: PprStyle -> Bool
debugStyle PprDebug	  = True
debugStyle other	  = False

userStyle ::  PprStyle -> Bool
userStyle (PprUser _) = True
userStyle other       = False
\end{code}

\begin{code}
ifNotPprForUser :: SDoc -> SDoc	-- Returns empty document for User style
ifNotPprForUser d sty@(PprUser _) = Pretty.empty
ifNotPprForUser d sty             = d sty

ifPprDebug :: SDoc -> SDoc	  -- Empty for non-debug style
ifPprDebug d sty@PprDebug = d sty
ifPprDebug d sty	  = Pretty.empty
\end{code}

\begin{code}
printSDoc :: SDoc -> PprStyle -> IO ()
printSDoc d sty = printDoc PageMode stdout (d sty)

-- I'm not sure whether the direct-IO approach of printDoc
-- above is better or worse than the put-big-string approach here
printErrs :: SDoc -> IO ()
printErrs doc = printDoc PageMode stderr (final_doc user_style)
	      where
		final_doc = doc $$ text ""
		user_style = mkUserStyle (PartWay opt_PprUserLength)

printDump :: SDoc -> IO ()
printDump doc = printDoc PageMode stderr (final_doc PprDebug)
	      where
		final_doc = doc $$ text ""


-- printForC, printForAsm doe what they sound like
printForC :: Handle -> SDoc -> IO ()
printForC handle doc = printDoc LeftMode handle (doc (PprCode CStyle))

printForAsm :: Handle -> SDoc -> IO ()
printForAsm handle doc = printDoc LeftMode handle (doc (PprCode AsmStyle))

-- printForIface prints all on one line for interface files.
-- It's called repeatedly for successive lines
printForIface :: Handle -> SDoc -> IO ()
printForIface handle doc = printDoc OneLineMode handle (doc PprInterface)


-- showSDoc just blasts it out as a string
showSDoc :: SDoc -> String
showSDoc d = show (d (mkUserStyle AllTheWay))

mkUserStyle depth |  opt_PprStyle_Debug 
	  	  || opt_PprStyle_All = PprDebug
	          |  otherwise        = PprUser depth
\end{code}

\begin{code}
empty sty      = Pretty.empty
text s sty     = Pretty.text s
char c sty     = Pretty.char c
ptext s sty    = Pretty.ptext s
int n sty      = Pretty.int n
integer n sty  = Pretty.integer n
float n sty    = Pretty.float n
double n sty   = Pretty.double n
rational n sty = Pretty.rational n

parens d sty       = Pretty.parens (d sty)
braces d sty       = Pretty.braces (d sty)
brackets d sty     = Pretty.brackets (d sty)
quotes d sty       = Pretty.quotes (d sty)
doubleQuotes d sty = Pretty.doubleQuotes (d sty)

semi sty   = Pretty.semi
comma sty  = Pretty.comma
colon sty  = Pretty.colon
equals sty = Pretty.equals
space sty  = Pretty.space
lparen sty = Pretty.lparen
rparen sty = Pretty.rparen
lbrack sty = Pretty.lbrack
rbrack sty = Pretty.rbrack
lbrace sty = Pretty.lbrace
rbrace sty = Pretty.rbrace

nest n d sty    = Pretty.nest n (d sty)
(<>) d1 d2 sty  = (Pretty.<>)  (d1 sty) (d2 sty)
(<+>) d1 d2 sty = (Pretty.<+>) (d1 sty) (d2 sty)
($$) d1 d2 sty  = (Pretty.$$)  (d1 sty) (d2 sty)
($+$) d1 d2 sty = (Pretty.$+$) (d1 sty) (d2 sty)

hcat ds sty = Pretty.hcat [d sty | d <- ds]
hsep ds sty = Pretty.hsep [d sty | d <- ds]
vcat ds sty = Pretty.vcat [d sty | d <- ds]
sep ds sty  = Pretty.sep  [d sty | d <- ds]
cat ds sty  = Pretty.cat  [d sty | d <- ds]
fsep ds sty = Pretty.fsep [d sty | d <- ds]
fcat ds sty = Pretty.fcat [d sty | d <- ds]

hang d1 n d2 sty   = Pretty.hang (d1 sty) n (d2 sty)

punctuate :: SDoc -> [SDoc] -> [SDoc]
punctuate p []     = []
punctuate p (d:ds) = go d ds
		   where
		     go d [] = [d]
		     go d (e:es) = (d <> p) : go e es
\end{code}


%************************************************************************
%*									*
\subsection[Outputable-class]{The @Outputable@ class}
%*									*
%************************************************************************

\begin{code}
class Outputable a where
	ppr :: a -> SDoc
\end{code}

\begin{code}
instance Outputable Bool where
    ppr True  = ptext SLIT("True")
    ppr False = ptext SLIT("False")

instance Outputable Int where
   ppr n = int n

instance (Outputable a) => Outputable [a] where
    ppr xs = brackets (fsep (punctuate comma (map ppr xs)))

instance (Outputable a, Outputable b) => Outputable (a, b) where
    ppr (x,y) =
      hang (hcat [lparen, ppr x, comma]) 4 ((<>) (ppr y) rparen)

-- ToDo: may not be used
instance (Outputable a, Outputable b, Outputable c) => Outputable (a, b, c) where
    ppr (x,y,z) =
      parens (sep [ (<>) (ppr x) comma,
		      (<>) (ppr y) comma,
		      ppr z ])
\end{code}


%************************************************************************
%*									*
\subsection{Other helper functions}
%*									*
%************************************************************************

\begin{code}
pprCols = (100 :: Int) -- could make configurable

printDoc :: Mode -> Handle -> Doc -> IO ()
printDoc mode hdl doc
  = fullRender mode pprCols 1.5 put done doc
  where
    put (Chr c)  next = hPutChar hdl c >> next 
    put (Str s)  next = hPutStr  hdl s >> next 
    put (PStr s) next = hPutFS   hdl s >> next 

    done = hPutChar hdl '\n'
\end{code}


\begin{code}
interppSP  :: Outputable a => [a] -> SDoc
interppSP  xs = hsep (map ppr xs)

interpp'SP :: Outputable a => [a] -> SDoc
interpp'SP xs = hsep (punctuate comma (map ppr xs))

pprQuotedList :: Outputable a => [a] -> SDoc
-- [x,y,z]  ==>  `x', `y', `z'
pprQuotedList xs = hsep (punctuate comma (map (quotes . ppr) xs))
\end{code}




%************************************************************************
%*									*
\subsection{Printing numbers verbally}
%*									*
%************************************************************************

@speakNth@ converts an integer to a verbal index; eg 1 maps to
``first'' etc.

\begin{code}
speakNth :: Int -> SDoc

speakNth 1 = ptext SLIT("first")
speakNth 2 = ptext SLIT("second")
speakNth 3 = ptext SLIT("third")
speakNth 4 = ptext SLIT("fourth")
speakNth 5 = ptext SLIT("fifth")
speakNth 6 = ptext SLIT("sixth")
speakNth n = hcat [ int n, text st_nd_rd_th ]
  where
    st_nd_rd_th | n_rem_10 == 1 = "st"
		| n_rem_10 == 2 = "nd"
		| n_rem_10 == 3 = "rd"
		| otherwise     = "th"

    n_rem_10 = n `rem` 10
\end{code}

\begin{code}
speakNTimes :: Int {- >=1 -} -> SDoc
speakNTimes t | t == 1 	   = ptext SLIT("once")
              | t == 2 	   = ptext SLIT("twice")
              | otherwise  = int t <+> ptext SLIT("times")
\end{code}

%************************************************************************
%*									*
\subsection[Utils-errors]{Error handling}
%*									*
%************************************************************************

\begin{code}
pprPanic heading pretty_msg = panic (show (doc PprDebug))
			    where
			      doc = text heading <+> pretty_msg

pprError heading pretty_msg = error (heading++ " " ++ (show pretty_msg))

pprTrace heading pretty_msg = trace (show (doc PprDebug))
			    where
			      doc = text heading <+> pretty_msg

pprPanic# heading pretty_msg = panic# (show (doc PprDebug))
			     where
			       doc = text heading <+> pretty_msg

assertPprPanic :: String -> Int -> SDoc -> a
assertPprPanic file line msg
  = panic (show (doc PprDebug))
  where
    doc = sep [hsep[text "ASSERT failed! file", 
		 	   text file, 
			   text "line", int line], 
		    msg]
\end{code}
