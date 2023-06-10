\documentclass{article}

\usepackage{verbatim}

\title{Generating Hypertext Haskell Documentation}

\author{Jakob Axelsson\\
Dept. of Computer and Information Science\\
Link\"{o}ping University\\
S-581 83 Link\"{o}ping, SWEDEN\\
Email: jakax@ida.liu.se}
\date{February 4, 1997}

\newenvironment{smallverb}{\small\verbatim}{\endverbatim\normalsize}
\newcommand{\smalltt}[1]{\texttt{\small#1}}
\newenvironment{code}{\small\verbatim}{\endverbatim\normalsize}

\begin{document}
\maketitle

\section{Introduction}
%---------------------

Complex Haskell programs often consist of a large number of modules,
which might have complex interrelations between each
other. Developing, maintaining, and understanding such programs can be
troublesome using standard tools like text editors, mainly due to the
large number of files involved in the project, and the difficulty to
locate the definition of a particular data type or function.  Often,
there is also insufficient documentation to aid this task, and there
is seldom enough time to keep the documentation up to date with the
changes of an ongoing development project. On the other hand, an
experienced Haskell programmer can get most of the information he
needs from the module interface descriptions, and by making these
easily accessible and browsable, the program development becomes much
more convenient.

This report describes a Haskell program called ``HaskellDoc'', for
generating such a documentation automatically from the interface files
of a Haskell project. The output is in HTML format, which makes it
readable using standard World Wide Web browsers. The program has been
developed for use with the Chalmers Haskell B compiler (HBC), based on
version 1.3 of the language report. Although the source code for
HaskellDoc strictly follows the language standard, it is not
guaranteed to work with the interface files of other Haskell compilers
as input since the file format is undefined in the language report
(though the program can probably be easily modified for use with other
compilers).


\section{Usage}
%--------------

The program is very easy to use. Simply go to the directory where the
source code of the modules are placed, and compile the project using
HBC by giving the command \smalltt{hbcmake Mod}\footnote{Probably, you
  will also want to pass the flag \smalltt{-fno-syn-expand} to
  \smalltt{hbcmake}, to stop it from expanding type synonyms in the
  interface file, e.g. replacing \smalltt{String} by
  \smalltt{[Char]}.} (assuming throughout this report that the main
module is located in a file called \smalltt{Mod}). The reason the
project has to be compiled is to ensure that the module interface
files have been generated. Then use the command \smalltt{HaskellDoc
  Mod Dir}, where \smalltt{Dir} is the directory in which the
generated documentation should be placed. After that, the
documentation should be viewable by visiting the URL
\smalltt{file:Dir/Mod.html}.


\section{An example}
%----------------------

In the following sections, the construction of the program will be
described.  To facilitate the understanding of the program, an example
is given of a small Haskell project which could serve as input.
Assume that the main module is contained in the file
\smalltt{"Mod.hs"}:

\begin{smallverb}
module Main (main) where

import Mod1 (Point (..), distance)

p1, p2 :: Point
p1 = P 1.0 2.0
p2 = P 3.0 4.0

main = print (distance p1 p2)
\end{smallverb}

\noindent The imported module is contained in the file \smalltt{"Mod1.hs"}:

\begin{smallverb}
module Mod1 (Point (..), distance) where

data Point = P Float Float

distance :: Point -> Point -> Float
distance (P x y)(P x' y') = sqrt (diff2 x x' + diff2 y y')

diff2 :: Float -> Float -> Float
diff2 x x' = (x - x') ^ 2
\end{smallverb}

\noindent After compilation, the interface file \smalltt{"Mod.hi"} contains 
(with unimportant comments abbreviated):

\begin{smallverb}
interface Main where {
{-# IMPORTING Mod1 #-}
main :: _LibIO.IO ()   {- ... -}
}
\end{smallverb}

\noindent and \smalltt{"Mod1.hi"} contains:

\begin{smallverb}
interface Mod1 where {
data Point = P Prelude.Float Prelude.Float;
distance :: Point -> Point -> Prelude.Float   {- ... -};
instance Prelude.Eval Point
}
\end{smallverb}


\section{The \texttt{main} function}
%-----------------------------------

The program imports some functions from the standard Haskell libraries:

\begin{code}
import Char (isAlpha, isSpace)
import Directory (getDirectoryContents, getCurrentDirectory)
import List (sort)
import System (getArgs)
\end{code}

The \smalltt{main} function first fetches the command line arguments,
and then finds all the \smalltt{.hi} files in the current
directory. Using the function \smalltt{importedModules} (see next
section) the list of modules in the current directory which are used
(directly or indirectly) by the main module are determined. From this
list, three files are generated in the destination directory:

\begin{description}
\item[\smalltt{ModIndex.html}] is a list of module names, with links to 
their definitions;
\item[\smalltt{ModModules.html}] contains pretty-printed versions
of all the module interfaces;
\item[\smalltt{Mod.html}] contains two HTML frames in which the above 
two files are shown.
\end{description}

\noindent The \smalltt{main} function is defined as follows:

\begin{code}
main :: IO ()
main = 
  do (mainModule, destination) <- decodeArgs
     dirContents <- getDirectoryContents "."
     let interfaceFiles = filter ((".hi" ==) . dropWhile (/= '.')) 
                                 dirContents
     projectModules <- importedModules interfaceFiles [mainModule]
     let modules = mainModule : sort (filter (/= mainModule) 
                                             projectModules)
     writeFile (destination ++ ".html")(generateFrames mainModule)
     writeFile (destination ++ "Index.html")
               (generateIndex mainModule modules)
     moduleText <- accumulate (map (generateModule modules) modules)
     writeFile (destination ++ "Modules.html")
               ("<HTML>\n<BODY>\n" ++ concat moduleText ++ 
                "<P>This documentation was generated using " ++
                "<A HREF=\"http://www.ida.liu.se/~jakax\">" ++
                "Jakob Axelsson</A>'s HaskellDoc program." ++
                "\n</P>\n</BODY>\n</HTML>")
  where
    decodeArgs = 
      do args <- getArgs
         if length args /= 2 then 
             error "Usage: HaskellDoc MainModule DestinationDir"
           else
             return (args !! 0, args !! 1 ++ "/" ++ args !! 0)
\end{code}


\section{Determining the set of modules}
%---------------------------------------

The set of modules in the project can be determined by traversing a
graph where the nodes are modules, and the arcs are import declarations.
The set of modules to be documented are those nodes in the graph that
are reachable from the main module, and who can be found in the current
directory (this is to avoid documenting standard libraries, etc.).

The function \smalltt{importedModules} takes two arguments: the first
is a list of strings containing the names of all interface files in
the current directory which have not yet been examined, and the second
is a list of module names which are reachable from some module which
has previously been determined to be in the project but which might
not yet have been visited. The initial call is
\smalltt{importedModules is ["Mod"]}, where \smalltt{is} is the list
of interface files. Whenever a new module is reached, it is checked if
it has already been visited, or if it does not exist in the current
directory; in these cases, the search is continued with the next
module remaining to be visited. Otherwise, its interface file is
opened to check what modules it imports, and these are added to the
list of modules remaining to visit. When no more modules remain, the
result is the list of visited modules.

\begin{code}
importedModules :: [String] -> [String] -> IO [String]
importedModules _ [] = return []
importedModules is (mod:mods) | (mod ++ ".hi") `notElem` is = 
  importedModules is mods 
importedModules is (mod:mods) =
  do s <- readFile (mod ++ ".hi")
     imods <- importedModules [i | i <- is, i /= mod ++ ".hi"]
                              (mods ++ importList (lexModule s))
     return (mod:imods)
  where
    importList [] = []
    importList ("{":"-#":"IMPORTING":ss) = 
      filter (/= ",") (takeWhile (/= "#-") ss)
    importList (s:ss) = importList ss
\end{code}

\noindent The function \smalltt{lexModule} is used to break up the module 
interface file into lexemes:

\begin{code}
lexModule :: String -> [String]
lexModule s = case lex s of
                [("", "")] -> []
                [(s,  ss)] -> s : lexModule ss
\end{code}

\section{Generating output}
%--------------------------

In this section, the functions for generating the output are
presented.  They build strings from their arguments, and since they
might look somewhat complex, it is shown for each of the output functions
what their results look like when applied to the example described
above.


\subsection{The frames}
%----------------------

The file \smalltt{"Mod.html"}, which is the main file of the documentation,
defines two frames, where the module index and interface list will
be shown. It also sets the document title:

\begin{code}
generateFrames :: String -> String
generateFrames mainModule =
  "<HTML>\n<HEAD>\n<TITLE>" ++ mainModule ++ 
  " module documentation</TITLE>\n</HEAD>\n" ++
  "<FRAMESET COLS=\"140,*\">\n" ++
  "<FRAME SRC=\"" ++ mainModule ++ "Index.html\">\n" ++
  "<FRAME SRC=\"" ++ mainModule ++ "Modules.html\" NAME=\"modules\">\n" ++
  "</FRAMESET>\n</HTML>\n"
\end{code}

\noindent The output generated from the example will be in the file
\smalltt{Mod.html}:

\begin{smallverb}
<HTML>
<HEAD>
<TITLE>Mod module documentation</TITLE>
</HEAD>
<FRAMESET COLS="140,*">
<FRAME SRC="ModIndex.html">
<FRAME SRC="ModModules.html" NAME="modules">
</FRAMESET>
</HTML>
\end{smallverb}


\subsection{The module index}
%----------------------------

The module index contains a list of all the modules, and by clicking
on one of the modules, its definition appears in the interface list frame:

\begin{code}
generateIndex :: String -> [String] -> String
generateIndex mainModule projectModules =
    "<HTML>\n<BODY>\n" ++ concat (map indexLine projectModules) ++ 
    "</BODY>\n</HTML>\n"
  where 
    indexLine mod = 
      "<A HREF=\"" ++ mainModule ++ "Modules.html#" ++ mod ++
      "\" TARGET=\"modules\">" ++ mod ++ "</A><BR>\n"
\end{code}

\noindent The output generated from the example will be in the file
\smalltt{ModIndex.html}:

\begin{smallverb}
<HTML>
<BODY>
<A HREF="ModModules.html#Mod" TARGET="modules">Mod</A><BR>
<A HREF="ModModules.html#Mod1" TARGET="modules">Mod1</A><BR>
</BODY>
</HTML>
\end{smallverb}


\subsection{The module interfaces}
%---------------------------------

For each module in the project, the function \smalltt{generateModule} is
called to format the interface. The interface list will be one long list
where the different modules are separated by horizontal lines. A heading
with the module name is inserted at the beginning of each module, and
this heading is tagged so that it can be reached by the link from the
index. There is also a link from the heading to the source code file
of the module:

\begin{code}
generateModule :: [String] -> String -> IO String
generateModule projectModules moduleName =
  do mod <- readFile (moduleName ++ ".hi")
     dir <- getCurrentDirectory
     return ("<A HREF=\"file:" ++ dir ++ "/" ++ moduleName ++ ".hs\"" ++ 
             "NAME=\"" ++ moduleName ++ "\"><H1>" ++ moduleName ++ 
             "</H1></A>\n<PRE>\n" ++ generateDeclarations mod ++
             "\n</PRE>\n<HR>\n")
\end{code}

\noindent The generation of the interface declarations is slightly complicated.
The function (which is best read backwards) first removes all comments,
and the file heading and the ending brace. Then the tokens are formatted
and finally the declarations are given an appropriate form:

\begin{code}
  where
    generateDeclarations = 
      formatDeclarations . 
      formatTokens projectModules .
      init . tail . dropWhile (/= "{") . 
      removeComments . lexModule
\end{code}

\noindent The comments are removed using the function \smalltt{removeComments}:

\begin{code}
    removeComments []            = []
    removeComments ("{":"-#":ss) = removeComments (afterComment ss)
    removeComments (s:ss)        = s : removeComments ss

    afterComment ("#-":"}":ss)   = ss
    afterComment (_:ss)          = afterComment ss
\end{code}

\noindent HBC generates qualified names in the interface file, and 
to make the description less verbose, these are removed. But for items
which come from another module within the project, a HTML link is
inserted to that module. Also, spaces are inserted around certain
tokens:

\begin{code}
    formatTokens _ [] = []
    formatTokens mods (m:".":n:ss) =
      adjustSpace mods
        (if m `elem` mods then
           "<A HREF=\"#" ++ m ++ "\">" ++ n ++ "</A>"
         else
           n) ss
    formatTokens mods (",":ss)  = ", "   : formatTokens mods ss
    formatTokens mods ("=":ss)  = " = "  : formatTokens mods ss
    formatTokens mods ("->":ss) = " -> " : formatTokens mods ss
    formatTokens mods ("=>":ss) = " => " : formatTokens mods ss
    formatTokens mods ("::":ss) = " :: " : formatTokens mods ss
    formatTokens mods (s:ss) = adjustSpace mods s ss

    adjustSpace mods n (s:ss) | (isId n && isId s) ||
                                (isId n && head s `elem` "([") ||
                                (last n `elem` "])" && isId s) =
        (n ++ " ") : formatTokens mods (s:ss)
      where 
        isId ('<':_) = True       -- Id with hypertext link
        isId (s:_)   = isAlpha s
    adjustSpace mods n ss = n  : formatTokens mods ss
\end{code}

\noindent Class and data declarations need some formatting to look good,
and keywords are marked in a special font:

\begin{code}
    formatDeclarations ("data ":s) = keyword "data " ++ format s where
      format []           = ""
      format (";":s)      = "<BR>\n"        ++ formatDeclarations s
      format (" = ":s)    = " =<BR>    "     ++ format s
      format ("|":s)      = "<BR>  | "      ++ format s
      format ("{":s)      = " {<BR>      "  ++ format s
      format (", ":s)     = ",<BR>      "   ++ format s
      format ("}":s)      = "<BR>    }"     ++ format s
      format (c:s)        = c               ++ format s
    formatDeclarations ("class ":s) = keyword "class " ++ format s where
      format []           = ""
      format (";":s)      = "<BR>\n"        ++ formatDeclarations s
      format ("where":s)  = keyword "where" ++ " {" ++ format s
      format ("{":s)      = "<BR>"          ++ formatDeclarations ("\n":s)
      format (c:s)        = c               ++ format s
    formatDeclarations ("instance ":s) = 
      keyword "instance " ++ defaultFormat s
    formatDeclarations ("type ":s) = keyword "type " ++ defaultFormat s
    formatDeclarations s = defaultFormat s

    defaultFormat []      = ""
    defaultFormat (";":s) = "<BR>\n"        ++ formatDeclarations s
    defaultFormat ("}":s) = "<BR>}\n"       ++ formatDeclarations s
    defaultFormat (c:s)   = c               ++ defaultFormat s

    keyword s = "<B>" ++ s ++ "</B>"
\end{code}

\noindent (End of function \smalltt{generateModule}.) 
The output generated from the example will be in the file
\smalltt{ModModules.html} (where \smalltt{...} is the
path to the source file):

\begin{smallverb}
<HTML>
<BODY>
<A HREF="file:/.../Mod.hs"NAME="Mod"><H1>Mod</H1></A>
<PRE>
main :: _IO ()
</PRE>
<HR>
<A HREF="file:/.../Mod1.hs"NAME="Mod1"><H1>Mod1</H1></A>
<PRE>
<B>data </B>Point=<BR>    P Float Float<BR>
distance :: Point -> Point -> Float<BR>
<B>instance </B>Eval Point
</PRE>
<HR>
<P>This documentation was generated using 
<A HREF="http://www.ida.liu.se/~jakax">Jakob Axelsson</A>'s 
HaskellDoc program.
</P>
</BODY>
</HTML>
\end{smallverb}

\section{Known problems}
%-----------------------

The formatting of class declarations and complex data declarations does
not always come out as nicely as would be desired. Better security and
more control options for the output would also be nice. But above all,
it would be very helpful for constructors of the kind of tools described
in this report, if the syntax of interface files would be reintroduced
into the language standard.

\end{document}
