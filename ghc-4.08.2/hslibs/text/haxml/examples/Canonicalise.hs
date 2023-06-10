module Main where

import System (getArgs)
import IO

import XmlParse     (xmlParse)
import XmlHtmlParse (htmlParse)
import XmlPP        (document)
import XmlLib       (fix2Args)
import Pretty       (render)
import IsSuffixOf
#if !defined(__HASKELL98__)
import HPutStrLn
#endif

-- This is just a trivial application that reads an XML document from
-- a file (or stdin) and writes it back to another file (or stdout).
-- It demonstrates the behaviour of the parser and pretty-printer,
-- including any shortcomings they may have.

main =
  fix2Args >>= \(inf,outf)->
  ( if inf=="-" then getContents
    else readFile inf )            >>= \content->
  ( if outf=="-" then return stdout
    else openFile outf WriteMode ) >>= \o->
  let parse = if ".html" `isSuffixOf` inf || ".htm" `isSuffixOf` inf
              then htmlParse else xmlParse
  in
  ( hPutStrLn o . render . document . parse) content

