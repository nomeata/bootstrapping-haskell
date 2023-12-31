--!!! Examples from the Hugs 1.3c documentation

-- Multiparameter type classes
class Collection c a where
    empty  :: c a
    insert :: a -> c a -> c a
    enum   :: c a -> [a]

-- Some instances (not included in docs)
instance Collection [] a where
    empty  = []
    insert = (:)
    enum   = id

-- need a newtype to avoid conflict with [] instance
newtype Set a = MkSet [a]

instance Eq a => Collection Set a where
    empty           = MkSet []
    insert x s@(MkSet xs)
      | x `elem` xs = s 
      | otherwise   = MkSet (x:xs)
    enum (MkSet xs) = xs

-- less restrictions on contexts (a few random examples)
data Tree a = Branch [Tree a]
            | Leaf a

instance (Eq [Tree a], Eq a) => Eq (Tree a) where
  (Branch xs) == (Branch ys) = xs == ys
  (Leaf x)    == (Leaf y)    = x == y

instance Eq a => Eq (Bool -> a) where
  f == g = all (\x -> f x == g x) [False,True]

instance Num a => Num (String,[a]) where
  (s,xs) + (t,ys) = (s++"+"++t,zipWith (+) xs ys)
  negate (s,xs)  = ("-"++s, map negate xs)

-- It's ok to give a more restrictive context that the one inferred
f  :: Eq a => [a] -> Bool
f x = x==[]

-- polymorphic recursion
p  :: Eq a => a -> Bool
p x = x==x && p [x]

-- polymorphic mutual recursion
p'  :: Eq a => a -> Bool
p' x = x==x && q' [x]

q' x = x==x && p' [x]

-- Rank 2 polymorphism
amazed :: (forall a. a -> a) -> (Bool,Char)
amazed i = (i True, i 'a')

twice    :: (forall b. b -> f b) -> a -> f (f a)
twice f   = f . f

eg1 = amazed (let i x = x in i)
eg2 = amazed (\x -> x)
eg3 = amazed (id . id . id . id)
eg4 = amazed (id id id id id)

-- Rank 2 polymorphism in data constructors
data Monad1 m = MkMonad1 {
                 unit1 :: (forall a. a -> m a),
                 bind1 :: (forall a, b. m a -> (a -> m b) -> m b)
                }

data Monad2 m = MkMonad2 (forall a. a -> m a)
                         (forall a, b. m a -> (a -> m b) -> m b)

listMonad1 = MkMonad1 {unit1 = \x->[x],
                       bind1 = \x f -> concat (map f x)}

listMonad2 = MkMonad2 (\x->[x]) (\x f -> concat (map f x))

listMonad1 :: Monad1 []
listMonad2 :: Monad2 []

eg5 = \(x::Int) -> x                  -- :: Int -> Int
eg6 = \(x::a) (xs::[a]) -> xs ++ [x]  -- :: a -> [a] -> [a].

f' (x::a) = let g   :: a -> [a]
                g y  = [x,y]
            in  g x


pair         :: t -> s -> (t,s)
pair x (y::t) = (x,y::t)

data Appl = MkAppl (a -> Int) a (a -> a)

good1 (MkAppl f x i) = f x
good2 (MkAppl f x i) = map f (iterate i x)

-- Using scoped type variables with existentials
good3 (MkAppl f (x::a) i) = map f (iterate i x :: [a])

