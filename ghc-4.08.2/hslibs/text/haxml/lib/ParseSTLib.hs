{-----------------------------------------------------------------------------

** Extended to allow a symbol table/state to be threaded through the monad.
** Extended to allow a parameterised token type, rather than just strings.
** Extended to allow error-reporting.
** (Extensions: Malcolm.Wallace@cs.york.ac.uk)

                 A LIBRARY OF MONADIC PARSER COMBINATORS

                              29th July 1996

                 Graham Hutton               Erik Meijer
            University of Nottingham    University of Utrecht

This Haskell 1.3 script defines a library of parser combinators, and is taken
from sections 1-6 of our article "Monadic Parser Combinators".  Some changes
to the library have been made in the move from Gofer to Haskell:

   * Do notation is used in place of monad comprehension notation;

   * The parser datatype is defined using "newtype", to avoid the overhead
     of tagging and untagging parsers with the P constructor.

-----------------------------------------------------------------------------}

module ParseSTLib
   (Parser(..), item, papply, (+++), {-sat,-} tok, many, many1,
    sepby, sepby1, chainl, chainl1, chainr, chainr1, ops, bracket, elserror,
    stupd, stquery, stget, reparse
   ) where

import Char
import Monad

infixr 5 +++

#if defined(__HASKELL98__)
#define FMAP fmap
#define MZERO mzero
#define MPLUS `mplus`
#else
#define FMAP map
#define MZERO zero
#define MPLUS ++
#endif

--- The parser monad ---------------------------------------------------------

newtype Parser s t a   = P (s -> [t] -> [(a,s,[t])])

instance Functor (Parser s t) where
   -- fmap         :: (a -> b) -> (Parser s t a -> Parser s t b)
   FMAP f (P p)     = P (\st inp -> [(f v, s, out) | (v,s,out) <- p st inp])

instance Monad (Parser s t) where
   -- return      :: a -> Parser s t a
   return v        = P (\st inp -> [(v,st,inp)])

   -- >>=         :: Parser s t a -> (a -> Parser s t b) -> Parser s t b
   (P p) >>= f     = P (\st inp -> concat [papply (f v) s out | (v,s,out) <- p st inp])

#if defined(__HASKELL98__)
   -- fail        :: String -> Parser s t a
   fail _          = P (\st inp -> [])
#endif

#if defined(__HASKELL98__)
instance MonadPlus (Parser s t) where
#else
instance MonadZero (Parser s t) where
#endif
   -- mzero            :: Parser s t a
   MZERO                = P (\st inp -> [])

#if !defined(__HASKELL98__)
instance MonadPlus (Parser s t) where
#endif
   -- mplus            :: Parser s t a -> Parser s t a -> Parser s t a
   (P p) MPLUS (P q)    = P (\st inp -> (p st inp ++ q st inp))

--- Other primitive parser combinators ---------------------------------------

item              :: Parser s t t
item               = P (\st inp -> case inp of
                                   []     -> []
                                   (x:xs) -> [(x,st,xs)])

force             :: Parser s t a -> Parser s t a
force (P p)        = P (\st inp -> let xs = p st inp
                                       h = head xs in
                                   h `seq` (h: tail xs))

first             :: Parser s t a -> Parser s t a
first (P p)        = P (\st inp -> case p st inp of
                                   []     -> []
                                   (x:xs) -> [x])

papply               :: Parser s t a -> s -> [t] -> [(a,s,[t])]
papply (P p) st inp   = p st inp


--- Derived combinators ------------------------------------------------------

(+++)             :: Parser s t a -> Parser s t a -> Parser s t a
p +++ q            = first (p MPLUS q)

sat               :: (t -> Bool) -> Parser s (p,t) t
sat p              = do {(_,x) <- item; if p x then return x else MZERO}

tok               :: Eq t => t -> Parser s (p,t) t
tok t              = do {x <- item; if t==snd x then return t else MZERO}

many              :: Parser s t a -> Parser s t [a]
many p             = many1 p +++ return []
--many p           = force (many1 p +++ return [])

many1             :: Parser s t a -> Parser s t [a]
many1 p            = do {x <- p; xs <- many p; return (x:xs)}

sepby             :: Parser s t a -> Parser s t b -> Parser s t [a]
p `sepby` sep      = (p `sepby1` sep) +++ return []

sepby1            :: Parser s t a -> Parser s t b -> Parser s t [a]
p `sepby1` sep     = do {x <- p; xs <- many (do {sep; p}); return (x:xs)}

chainl            :: Parser s t a -> Parser s t (a->a->a) -> a -> Parser s t a
chainl p op v      = (p `chainl1` op) +++ return v

chainl1           :: Parser s t a -> Parser s t (a->a->a) -> Parser s t a
p `chainl1` op     = do {x <- p; rest x}
                     where
                        rest x = do {f <- op; y <- p; rest (f x y)}
                                 +++ return x

chainr            :: Parser s t a -> Parser s t (a->a->a) -> a -> Parser s t a
chainr p op v      = (p `chainr1` op) +++ return v

chainr1           :: Parser s t a -> Parser s t (a->a->a) -> Parser s t a
p `chainr1` op     = do {x <- p; rest x}
                     where
                        rest x = do {f <- op; y <- p `chainr1` op; return (f x y)}
                                 +++ return x

ops               :: [(Parser s t a, b)] -> Parser s t b
ops xs             = foldr1 (+++) [do {p; return op} | (p,op) <- xs]

bracket           :: (Show p,Show t) =>
                     Parser s (p,t) a -> Parser s (p,t) b ->
                               Parser s (p,t) c -> Parser s (p,t) b
bracket open p close = do {open;
                           x <- p;
                           close; -- `elserror` "improperly matched construct";
                           return x}

elserror          :: (Show p,Show t) =>
                     Parser s (p,t) a -> String -> Parser s (p,t) a
p `elserror` s     = p +++
                     (P (\st inp->
                         case inp of
                           [] -> error "Parse error: unexpected EOF\n"
                           ((p,t):_) ->
                                 error ("Parse error at "++show p++": "++s++"\n"++
                                        "    actual token found: "++show t)))

stupd      :: (s->s) -> Parser s t ()
stupd f     = P (\st inp-> {-let newst = f st in newst `seq`-} [((), f st, inp)])

stquery    :: (s->a) -> Parser s t a
stquery f   = P (\st inp-> [(f st, st, inp)])

stget      :: Parser s t s
stget       = P (\st inp-> [(st, st, inp)])

reparse    :: [t] -> Parser s t ()
reparse ts  = P (\st inp-> [((), st, ts++inp)])

-----------------------------------------------------------------------------
