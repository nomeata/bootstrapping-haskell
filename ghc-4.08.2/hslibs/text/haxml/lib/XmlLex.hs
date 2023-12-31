module XmlLex
  ( xmlLex	-- :: String -> [Token]
  , xmlReLex    -- :: String -> Posn -> [Token]
  , Posn(..)
  , TokenT(..)
  , Token
  , Special(..)
  , Section(..)
  ) where

-- This is a hand-written lexer for tokenising the text of an XML
-- document so that it is ready for parsing.  It attaches position
-- information in (line,column) format to every token.  The main
-- entry point is xmlLex.  A secondary entry point, xmlReLex, is
-- provided for when the parser needs to stuff a string back onto
-- the front of the text and re-tokenise it (typically when expanding
-- macros).
--
-- As one would expect, the lexer is essentially a small finite
-- state machine.


#if !defined(__HASKELL98__)
#define isAlphaNum isAlphanum
#endif

import Char

data Where = InTag | NotInTag
    deriving (Eq)

type Token = (Posn, TokenT)

data Posn = Pn Int Int		-- line and column
        deriving (Eq)

instance Show Posn where
      showsPrec p (Pn l c) = showString "line " . shows l .
                             showString " col " . shows c

data TokenT =
      TokCommentOpen		--     <!--
    | TokCommentClose		--     -->
    | TokPIOpen			--     <?
    | TokPIClose		--     ?>
    | TokSectionOpen		--     <![
    | TokSectionClose		--     ]]>
    | TokSection Section	--     CDATA INCLUDE IGNORE etc
    | TokSpecialOpen		--     <!
    | TokSpecial Special	--     DOCTYPE ELEMENT ATTLIST etc
    | TokEndOpen		--     </
    | TokEndClose		--     />
    | TokAnyOpen		--     <
    | TokAnyClose		--     >
    | TokSqOpen			--     [
    | TokSqClose		--     ]
    | TokEqual			--     =
    | TokQuery			--     ?
    | TokStar			--     *
    | TokPlus			--     +
    | TokAmp			--     &
    | TokSemi			--     ;
    | TokHash			--     #
    | TokBraOpen		--     (
    | TokBraClose		--     )
    | TokPipe			--     |
    | TokPercent		--     %
    | TokComma			--     ,
    | TokQuote			--     '' or ""
    | TokName      String	--     begins with letter
    | TokFreeText  String	--     any character data
    | TokNull			--     fake token
    deriving (Eq,Show)

data Special =
      DOCTYPEx
    | ELEMENTx
    | ATTLISTx
    | ENTITYx
    | NOTATIONx
    deriving (Eq,Show)
data Section =
      CDATAx
    | INCLUDEx
    | IGNOREx
    deriving (Eq,Show)

--trim, revtrim :: String -> String
--trim    = f . f         where f = reverse . dropWhile isSpace
--revtrim = f.reverse.f   where f = dropWhile isSpace
revtrim = reverse . dropWhile (=='\n')

emit :: TokenT -> Posn -> Token
emit tok p = forcep p `seq` (p,tok)

forcep (Pn n m) = m `seq` n

lexerror :: String -> Posn -> a
lexerror s p = error ("Lexical error at "++show p++": "++s++"\n")

addcol :: Int -> Posn -> Posn
addcol n (Pn r c) = Pn r (c+n)

newline, tab :: Posn -> Posn
newline (Pn r c) = Pn (r+1) 0
tab     (Pn r c) = Pn r (((c`div`8)+1)*8)

white :: Char -> Posn -> Posn
white ' '  = addcol 1
white '\n' = newline
white '\r' = id
white '\t' = tab
white '\xa0' = addcol 1

skip :: Int -> Posn -> String -> (Posn->String->[Token]) -> [Token]
skip n p s k = k (addcol n p) (drop n s)

blank :: ([Where]->Posn->String->[Token]) -> [Where]-> Posn-> String-> [Token]
blank k    (InTag:_) p [] = lexerror "unexpected EOF in tag" p
blank k          _   p [] = []
blank k      w p (' ': s) = blank k w (addcol 1 p) s
blank k      w p ('\t':s) = blank k w (tab p) s
blank k      w p ('\n':s) = blank k w (newline p) s
blank k      w p ('\r':s) = blank k w  p s
blank k   w p ('\xa0': s) = blank k w (addcol 1 p) s
blank k      w p    s     = k w p s

prefixes :: String -> String -> Bool
[]     `prefixes`   ys   = True
(x:xs) `prefixes` (y:ys) = x==y && xs `prefixes` ys
(x:xs) `prefixes`   []   = error "unexpected EOF in prefix"

accumulateUntil (c:cs) tok acc pos  p  [] k =
    lexerror ("unexpected EOF while looking for "++c:cs++" after "++show pos) p
accumulateUntil (c:cs) tok acc pos  p (s:ss) k
    | c==s && cs `prefixes` ss  = emit (TokFreeText (reverse acc)) pos:
                                  emit tok p: skip (length cs) p ss k
    | isSpace s  = accumulateUntil (c:cs) tok (s:acc) pos (white s p) ss k
    | otherwise  = accumulateUntil (c:cs) tok (s:acc) pos (addcol 1 p) ss k

----
xmlLex :: String -> [Token]
xmlLex = xmlAny [] (Pn 1 0)

xmlReLex :: Posn -> String -> [Token]
xmlReLex = xmlAny []

--xmltop :: Posn -> String -> [Token]
--xmltop p [] = []
--xmltop p s
--    | "<?"   `prefixes` s  = emit TokPIOpen p:      next 2 (xmlPI [InTag])
--    | "<!--" `prefixes` s  = emit TokCommentOpen p: next 4 (xmlComment [])
--    | "<!"   `prefixes` s  = emit TokSpecialOpen p: next 2 (xmlSpecial [InTag])
--    | otherwise            = lexerror "expected <?xml?> or <!DOCTYPE>" p
--  where next n k = skip n p s k

xmlPI      w p s = xmlName p s (blank xmlPIEnd w)
xmlPIEnd   w p s = accumulateUntil "?>"  TokPIClose "" p p s
                                                      (blank xmlAny (tail w))
xmlComment w p s = accumulateUntil "-->" TokCommentClose "" p p s
                                                             (blank xmlAny w)

-- Note: the order of the clauses in xmlAny is very important.
-- Some matches must precede the NotInTag test, the rest must follow it.
xmlAny :: [Where] -> Posn -> String -> [Token]
xmlAny    (InTag:_)  p [] = lexerror "unexpected EOF inside tag" p
xmlAny          _    p [] = []
xmlAny w p s@('<':ss)
    | "?"   `prefixes` ss = emit TokPIOpen p:      skip 2 p s (xmlPI (InTag:w))
    | "!--" `prefixes` ss = emit TokCommentOpen p: skip 4 p s (xmlComment w)
    | "!["  `prefixes` ss = emit TokSectionOpen p: skip 3 p s (xmlSection w)
    | "!"   `prefixes` ss = emit TokSpecialOpen p:
                                              skip 2 p s (xmlSpecial (InTag:w))
    | "/"   `prefixes` ss = emit TokEndOpen p: 
                                             skip 2 p s (xmlTag (InTag:tail w))
    | otherwise           = emit TokAnyOpen p:
                                         skip 1 p s (xmlTag (InTag:NotInTag:w))
xmlAny (_:_:w) p s@('/':ss)
    | ">"   `prefixes` ss = emit TokEndClose p: skip 2 p s (xmlAny w)
xmlAny w p ('&':ss) = emit TokAmp p:      accumulateUntil ";" TokSemi "" p
                                                     (addcol 1 p) ss (xmlAny w)
xmlAny w@(NotInTag:_) p s = xmlContent "" w p p s
xmlAny w p ('>':ss) = emit TokAnyClose p:       xmlAny (tail w) (addcol 1 p) ss
xmlAny w p ('[':ss) = emit TokSqOpen p:   blank xmlAny (InTag:w) (addcol 1 p) ss
xmlAny w p (']':ss) = emit TokSqClose p:  blank xmlAny (tail w) (addcol 1 p) ss
xmlAny w p ('(':ss) = emit TokBraOpen p:  blank xmlAny (InTag:w) (addcol 1 p) ss
xmlAny w p (')':ss) = emit TokBraClose p: blank xmlAny (tail w) (addcol 1 p) ss
xmlAny w p ('=':ss) = emit TokEqual p:    blank xmlAny w (addcol 1 p) ss
xmlAny w p ('*':ss) = emit TokStar p:     blank xmlAny w (addcol 1 p) ss
xmlAny w p ('+':ss) = emit TokPlus p:     blank xmlAny w (addcol 1 p) ss
xmlAny w p ('?':ss) = emit TokQuery p:    blank xmlAny w (addcol 1 p) ss
xmlAny w p ('|':ss) = emit TokPipe p:     blank xmlAny w (addcol 1 p) ss
xmlAny w p ('%':ss) = emit TokPercent p:  blank xmlAny w (addcol 1 p) ss
xmlAny w p (';':ss) = emit TokSemi p:     blank xmlAny w (addcol 1 p) ss
xmlAny w p (',':ss) = emit TokComma p:    blank xmlAny w (addcol 1 p) ss
xmlAny w p ('#':ss) = emit TokHash p:     blank xmlAny w (addcol 1 p) ss
xmlAny w p ('"':ss) = emit TokQuote p:    accumulateUntil "\"" TokQuote "" p
                                                     (addcol 1 p) ss (xmlAny w)
xmlAny w p ('\'':ss) = emit TokQuote p:   accumulateUntil "'" TokQuote "" p
                                                     (addcol 1 p) ss (xmlAny w)
xmlAny w p s
    | isSpace (head s)     = blank xmlAny w p s
    | isAlphaNum (head s)  = xmlName p s (blank xmlAny w)
    | otherwise            = lexerror "unrecognised token" p

xmlTag w p s = xmlName p s (blank xmlAny w)

xmlSection = blank xmlSection0
  where
    xmlSection0 w p s
      | "CDATA["   `prefixes` s  = emit (TokSection CDATAx) p:    k w p s 6
      | "INCLUDE[" `prefixes` s  = emit (TokSection INCLUDEx) p:  k w p s 8
      | "IGNORE["  `prefixes` s  = emit (TokSection IGNOREx) p:   k w p s 7
      | otherwise = lexerror ("expected CDATA, IGNORE, or INCLUDE") p
    k w p s n =
      let p0 = addcol n p in
      accumulateUntil "]]>" TokSectionClose "" p0 p0 (drop n s) (blank xmlAny w)

xmlSpecial w p s
    | "DOCTYPE"  `prefixes` s = emit (TokSpecial DOCTYPEx) p: k 7
    | "ELEMENT"  `prefixes` s = emit (TokSpecial ELEMENTx) p: k 7
    | "ATTLIST"  `prefixes` s = emit (TokSpecial ATTLISTx) p: k 7
    | "ENTITY"   `prefixes` s = emit (TokSpecial ENTITYx) p:  k 6
    | otherwise = lexerror "expected DOCTYPE, ELEMENT, ENTITY, or ATTLIST" p
  where k n = skip n p s (blank xmlAny w)

xmlName p (s:ss) k
    | isAlphaNum s || s==':' || s=='_'  = gatherName (s:[]) p (addcol 1 p) ss k
    | otherwise                         = lexerror "expected name" p
  where
    gatherName acc pos p [] k =
        emit (TokName (reverse acc)) pos: k p []
    --  lexerror ("unexpected EOF in name at "++show pos) p
    gatherName acc pos p (s:ss) k
        | isAlphaNum s || s `elem` ".-_:"
                      = gatherName (s:acc) pos (addcol 1 p) ss k
        | otherwise   = emit (TokName (reverse acc)) pos: k p (s:ss)

xmlContent acc w pos p [] = if all isSpace acc then []
                            else lexerror "unexpected EOF between tags" p
xmlContent acc w pos p (s:ss)
    | elem s "<&"    = if all isSpace acc then xmlAny w p (s:ss)
                       else emit (TokFreeText (revtrim acc)) pos: xmlAny w p (s:ss)
    | isSpace s      = xmlContent (s:acc) w pos (white s p) ss
    | otherwise      = xmlContent (s:acc) w pos (addcol 1 p) ss



--ident :: (String->TokenT) ->
--          Posn -> String -> [String] ->
--         (Posn->String->[String]->[Token]) -> [Token]
--ident tok p s ss k =
--    let (name,s0) = span (\c-> isAlphaNum c || c `elem` "`-_#.'/\\") s
--    in emit (tok name) p: skip (length name) p s ss k
