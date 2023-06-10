-----------------------------------------------------------------------------
$Id: RegexString.lhs,v 1.4 2000/01/14 10:25:19 simonmar Exp $

A simple high-level interface to Regex

(c) Simon Marlow 1997-1999

Modified 1999 by Ian Jackson to fix an apparent fatal bug (?!)  and to
provide matchRegexAll.
-----------------------------------------------------------------------------

> module RegexString (Regex, mkRegex, matchRegex, matchRegexAll) where

> import Regex
> import PackedString
> import Array
> import GlaExts

> type Regex = PatBuffer
> 
> mkRegex :: String -> Regex
> mkRegex s = unsafePerformPrimIO (
>         re_compile_pattern (packString s) False False)
> 
> matchRegex :: Regex -> String -> Maybe [String]
> matchRegex = matchRegexIB matches
> 
> matchRegexAll :: Regex -> String ->
>         Maybe ( String, -- $`
>                 String, -- $&
>                 String, -- $'
>                 String, -- $+
>                 [String] -- $1..
>               )
> matchRegexAll = matchRegexIB matchesAll
>
> matchRegexIB interpretBy p s = unsafePerformPrimIO (
>         re_match p str 0 True >>= \m ->
>         case m of
>                 Nothing -> return Nothing
>                 Just m  -> return (Just (interpretBy m str))
>         )
>    where
>         str = packString s
>
> matches (REmatch arr _ _ _ _) s = map (getFromBounds s) (elems arr)
>
> matchesAll rm@(REmatch _ before entire after lastbracket) s =
>         ( g before, g entire, g after, g lastbracket, matches rm s )
>    where
>         g = getFromBounds s
>
> getFromBounds s (beg,end) = unpackPS (substrPS s beg (end-1))
