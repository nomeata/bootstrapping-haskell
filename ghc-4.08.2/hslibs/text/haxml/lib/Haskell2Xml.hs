module Haskell2Xml
  ( Haskell2Xml(..)
  , HType(..)
  , Constr(..)
  , toXml, toDTD, fromXml
  , readXml, writeXml
  , hReadXml, hWriteXml
  , mkElem , mkElemC
  , showConstr
  , module XmlTypes
  , isPrefixOf
  ) where

-- The class Haskell2Xml is a replacement for Read and Show: it provides
-- textual conversions for your Haskell data values - use the packaged
-- version of DrIFT to derive this class for your own datatypes, then
-- include this module where you want to use the facilities.
--
-- The functions toXml and fromXml convert a value to and from a generic
-- internal representation of an XML document (including a DTD).
-- The functions readXml and writeXml do the conversion to and from
-- the given filenames.
-- The functions hReadXml and hWriteXml do the conversion to and from
-- the given file handles.
-- (See the type signatures.)

import IO

import XmlTypes
import Pretty   (render)
import XmlParse (xmlParse)
import qualified XmlPP as PP
#if defined(__HASKELL98__)
import List(intersperse,isPrefixOf,isSuffixOf)
import Char (ord)
#else
import List(intersperse)
import IsPrefixOf
import IsSuffixOf
ord :: Char -> Int
ord = fromEnum
#endif


data HType =
      Maybe HType
    | List HType
    | Tuple [HType]
    | Prim String String
    | String
    | Defined String [HType] [Constr]
    deriving (Show)

instance Eq HType where
    (Maybe x)  == (Maybe y)  =  x==y
    (List x)   == (List y)   =  x==y
    (Tuple xs) == (Tuple ys) =  xs==ys
    (Prim x _) == (Prim y _) =  x==y
    String     == String     =  True
    (Defined n xs _) == (Defined m ys _)  =  n==m && xs==ys
    _          == _          =  False

data Constr = Constr String [HType] [HType]
    deriving (Eq,Show)


attval :: (Read a) => AttValue -> a
attval (AttValue [Left s]) = read s

mkAttr :: String -> String -> Attribute
mkAttr n v = (n, AttValue [Left v])

atoi :: String -> Int
atoi = foldl (\x y-> x*10 + ord y - ord '0') 0

atoI :: String -> Integer
atoI = foldl (\x y-> x*10 + toInteger (ord y) - toInteger (ord '0')) 0

class Haskell2Xml a where
    toHType      :: a -> HType
    toContents   :: a -> [Content]
    fromContents :: [Content] -> (a,[Content])

    -- The following is used only to coerce lists of Char into String.
    xToChar      :: a -> Char
    xToChar       = error "Haskell2Xml.xToChar used in error"
    xFromChar    :: Char -> a
    xFromChar     = error "Haskell2Xml.xFromChar used in error"

instance Haskell2Xml Bool where
    toHType   _    = Prim "Bool" "bool"
    toContents b   = [CElem (Elem "bool" [mkAttr "value" (show b)] [])]
    fromContents (CElem (Elem "bool" [("value",b)] []):cs) = (attval b, cs)
    fromContents (_:cs) = fromContents cs

instance Haskell2Xml Int where
    toHType   _    = Prim "Int" "int"
    toContents i   = [CElem (Elem "int" [mkAttr "value" (show i)] [])]
    fromContents (CElem (Elem "int" [("value",(AttValue [Left s]))] []):cs) = (atoi s, cs)
    fromContents (_:cs) = fromContents cs

instance Haskell2Xml Integer where
    toHType   _    = Prim "Integer" "integer"
    toContents i   = [CElem (Elem "integer" [mkAttr "value" (show i)] [])]
    fromContents (CElem (Elem "integer" [("value",(AttValue [Left s]))] []):cs) = (atoI s, cs)
    fromContents (_:cs) = fromContents cs

instance Haskell2Xml Char where
    -- NOT in a string
    toHType   _    = Prim "Char" "char"
    toContents c   = [CElem (Elem "char" [mkAttr "value" [c]] [])]
    fromContents (CElem (Elem "char" [("value",(AttValue [Left [c]]))] []):cs) = (c, cs)
    fromContents (_:cs) = fromContents cs

    -- Only defined for Char and no other types:
    xToChar   = id
    xFromChar = id

instance Haskell2Xml a => Haskell2Xml [a] where
    toHType xs     = case toHType x of
                       (Prim "Char" _) -> String
                       _ -> List (toHType x)
                   where   (x:_) = xs
    toContents xs  = case toHType x of
                       (Prim "Char" _) ->
                            [mkElem "string" [CString False (map xToChar xs)]]
                       _ -> [mkElem xs (concatMap toContents xs)]
                   where   (x:_) = xs
    fromContents (CString _ s:cs) =
                   (map xFromChar s,cs)
    fromContents (CElem (Elem _ [] [CString _ s]):cs) =
                   (map xFromChar s,cs) --fromContents cs 
    fromContents (CElem (Elem _ [] xs):cs) =
                   (fst (scanElements xs), cs)
                   where
                 --scanElements :: Haskell2Xml a => [Content] -> ([a],[Content])
                   scanElements [] = ([],[])
                   scanElements els =
                     (\(x,els0)-> (\(xs,els1)-> (x:xs,els1))
                                  (scanElements els0))
                       (fromContents els)
                   --let (x,els0)  = fromContents els
                   --    (xs,els1) = scanElements els0
                   --in (x:xs, els1) 

instance (Haskell2Xml a, Haskell2Xml b) => Haskell2Xml (a,b) where
    toHType p        = Tuple [toHType a, toHType b]   where   (a,b) = p
    toContents (a,b) = toContents a ++ toContents b
    fromContents cs  = --let  (x,cs1) = fromContents cs
                       --     (y,cs2) = fromContents cs1
                       --in ((x,y),cs2)
                       (\(x,cs1)-> (\(y,cs2)-> ((x,y),cs2)) (fromContents cs1))
                         (fromContents cs)

instance (Haskell2Xml a) => Haskell2Xml (Maybe a) where
    toHType m      = Maybe (toHType x)   where   (Just x) = m
    toContents m   = [mkElem m (maybe [] toContents m)]
    fromContents (CElem (Elem _ [] []):cs) =
                     (Nothing,cs)
    fromContents (CElem (Elem _ [] cs0):cs) =
                   --let (x,[]) = fromContents cs0 in (Just x,cs)
                     (\(x,[])-> (Just x, cs)) (fromContents cs0)

instance (Haskell2Xml a, Haskell2Xml b) => Haskell2Xml (Either a b) where
    toHType m  = Defined "Either" [hx, hy]
                         [Constr "Left" [hx] [hx] ,Constr "Right" [hy] [hy]]
               where   (Left x)  = m
                       (Right y) = m
                       hx = toHType x
                       hy = toHType y
    fromContents (CElem (Elem constr [] cs):etc)
        | "Left" `isPrefixOf` constr =
            (\(aa,_)-> (Left aa, etc)) (fromContents cs)
        | "Right" `isPrefixOf` constr =
            (\(ab,_)-> (Right ab, etc)) (fromContents cs)
    toContents v@(Left aa) =
        [mkElemC (showConstr 0 (toHType v)) (toContents aa)]
    toContents v@(Right ab) =
        [mkElemC (showConstr 1 (toHType v)) (toContents ab)]


instance Haskell2Xml () where
    toHType _      = Prim "unit" "unit"
    toContents ()  = [CElem (Elem "unit" [] [])]
    fromContents (CElem (Elem "unit" [] []):cs) = ((),cs)  


mkElem x cs  = CElem (Elem (flat (toHType x) "") [] cs)
mkElemC x cs = CElem (Elem x [] cs)


toDTD :: HType -> DocTypeDecl
toDTD ht =
  DTD (toplevel ht) Nothing (reverse (h2d True [] [] [ht]))
  where
    toplevel ht@(Defined _ _ _) = flat ht "-XML"
    toplevel ht@_               = flat ht ""
    c0 = False
    h2d :: Bool -> [HType] -> [Constr] -> [HType] -> [MarkupDecl]
    -- toplevel?   history    history   remainingwork     result
    h2d c history chist []       = []
    h2d c history chist (ht:hts) =
      if ht `elem` history then h2d c0 history chist hts
      else
        case ht of
          Maybe ht0  -> declelem ht: h2d c0 (ht:history) chist (ht0:hts)
          List ht0   -> declelem ht: h2d c0 (ht:history) chist (ht0:hts)
          Tuple hts0 -> (c ? (declelem ht:))
                                     (h2d c0 history chist (hts0++hts))
          Prim s t   -> declprim ht ++ h2d c0 (ht:history) chist hts
          String     -> declstring:    h2d c0 (ht:history) chist hts
          Defined s _ cs ->
               let hts0 = concatMap grab cs in
               (c ? (decltopelem ht:)) (declmacro ht chist)
               ++ h2d c0 (ht:history) (cs++chist) (hts0++hts)
    declelem ht =
      Element (ElementDecl (flat ht "") (ContentSpec (outerHtExpr ht)))
    decltopelem ht =	-- hack to avoid peref at toplevel
      Element (ElementDecl (flat ht "-XML") (ContentSpec (innerHtExpr ht None)))
    declmacro ht@(Defined _ _ cs) chist =
      Entity (EntityPEDecl (PEDecl (flat ht "") (PEDefEntityValue ev))):
      concatMap (declConstr chist) cs
      where ev = EntityValue [EVString (render (PP.cp (outerHtExpr ht)))]
    declConstr chist c@(Constr s fv hts)
      | c `notElem` chist =
          [Element (ElementDecl (cflat c "") (ContentSpec (constrHtExpr c)))]
      | otherwise = []
    declprim (Prim s t) =
      [ Element (ElementDecl t EMPTY)
      , AttList (AttListDecl t [AttDef "value" StringType REQUIRED])]
    declstring =
      Element (ElementDecl "string" (Mixed PCDATA))
    grab (Constr _ _ hts) = hts

(?) :: Bool -> (a->a) -> (a->a)
b ? f | b     = f
      | not b = id


flat :: HType -> ShowS
flat (Maybe ht)       = showString "maybe-" . flat ht
flat (List ht)        = showString "list-" . flat ht
flat (Tuple hts)      = showString "tuple" . shows (length hts) .
                        showChar '-' .
                        foldr1 (.) (intersperse (showChar '-') (map flat hts))
flat (Prim s t)       = showString t
flat String           = showString "string"
flat (Defined s fv _) = showString s . ((length fv > 0) ? (showChar '-')) .
                        foldr (.) id (intersperse (showChar '-') (map flat fv))
cflat :: Constr -> ShowS
cflat (Constr s fv _) = showString s . ((length fv > 0) ? (showChar '-')) .
                        foldr (.) id (intersperse (showChar '-') (map flat fv))

outerHtExpr :: HType -> CP
outerHtExpr (Maybe ht)      = innerHtExpr ht Query
outerHtExpr (List ht)       = innerHtExpr ht Star
outerHtExpr (Defined s fv cs) =
    Choice (map (\c->TagName (cflat c "") None) cs) None
outerHtExpr ht              = innerHtExpr ht None

innerHtExpr :: HType -> Modifier -> CP
innerHtExpr (Prim s t)  m = TagName t m
innerHtExpr (Tuple hts) m = Seq (map (\c-> innerHtExpr c None) hts) m
innerHtExpr ht@(Defined s hts cs) m = -- CPPE (flat ht "") (outerHtExpr ht)
                                      TagName ('%': flat ht ";") m
							-- ***HACK!!!***
innerHtExpr ht m = TagName (flat ht "") m

constrHtExpr (Constr s fv [])  = TagName "EMPTY" None	-- ***HACK!!!***
constrHtExpr (Constr s fv hts) = innerHtExpr (Tuple hts) None



---------------------------
-- Exported user functions.
---------------------------

toXml :: Haskell2Xml a => a -> Document
toXml value =
  let ht = toHType value in
  Document (Prolog Nothing (Just (toDTD ht)))
           emptyST
           (case (ht, toContents value) of
             (Tuple _, cs) -> (Elem (flat ht "") [] cs)
             (Defined _ _ _, cs) -> (Elem (flat ht "-XML") [] cs)
             (_,[CElem e]) -> e )

fromXml :: Haskell2Xml a => Document -> a
fromXml (Document _ _ e@(Elem n _ cs))
  | "tuple" `isPrefixOf` n = fst (fromContents cs)
  | "-XML"  `isSuffixOf` n = fst (fromContents cs)
  | otherwise = fst (fromContents [CElem e])


readXml  :: Haskell2Xml a => FilePath -> IO a
readXml fp = do
    f <- openFile fp ReadMode 
    content <- hGetContents f
    --hClose f
    return (fromXml (xmlParse content))

writeXml :: Haskell2Xml a => FilePath -> a -> IO ()
writeXml fp v = do
    f <- openFile fp WriteMode 
    (hPutStrLn f . render . PP.document . toXml) v
    hClose f

hReadXml  :: Haskell2Xml a => Handle -> IO a
hReadXml f = do
    content <- hGetContents f
    return (fromXml (xmlParse content))

hWriteXml :: Haskell2Xml a => Handle -> a -> IO ()
hWriteXml f v = (hPutStrLn f . render . PP.document . toXml) v


showConstr n (Defined _ _ cs) = cflat (cs!!n) ""
showConstr n _ = error "no constructors for builtin types"

-- END
