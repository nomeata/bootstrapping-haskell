\haskell:module{
  \Name{HtmlBlockTable}
  \Version{0.2}
  \Description{
	Provides support for building abstract (block) tables.}
  \License{
        The Haskell Html Library is Copyright &copy;
        Andy Gill, and the Oregon Graduate Institute of Science and
        Technology, 1999, All rights reserved, and is distributed as
        free software under the license in the file "License", which
	is included in the distribution.}
  \Author{
	\A[HREF="http://www.cse.ogi.edu/~andy"]{Andy Gill}}
  \Restrictions{
	This works with all Haskell 98 compilers.
  }
  \Tested{Hugs98, GHC 4.03}
}

\begin{code}
module HtmlBlockTable (

-- Datatypes:

      BlockTable,             -- abstract

-- Contruction Functions: 

      single,
      above,
      beside,

-- Investigation Functions: 

      getMatrix,
      showsTable,
      showTable,

      ) where

infixr 4 `beside`
infixr 3 `above`
\end{code}

These combinators can be used to build formated 2D tables.
The specific target useage is for HTML table generation.

\pre{
   Examples of use:

  	> table1 :: BlockTable String
  	> table1 = single "Hello"	+-----+
					|Hello|
	  This is a 1x1 cell		+-----+
	  Note: single has type
	 
		single :: a -> BlockTable a
	
	  So the cells can contain anything.
	
	> table2 :: BlockTable String
	> table2 = single "World"	+-----+
					|World|
					+-----+


	> table3 :: BlockTable String
	> table3 = table1 %-% table2	+-----%-----+
					|Hello%World|
	 % is used to indicate		+-----%-----+
	 the join edge between
	 the two Tables.  

	> table4 :: BlockTable String
	> table4 = table3 %/% table2	+-----+-----+
					|Hello|World|
	  Notice the padding on the	%%%%%%%%%%%%%
	  smaller (bottom) cell to	|World      |
	  force the table to be a	+-----------+
	  rectangle.

	> table5 :: BlockTable String
	> table5 = table1 %-% table4	+-----%-----+-----+
					|Hello%Hello|World|
	  Notice the padding on the	|     %-----+-----+
	  leftmost cell, again to	|     %World      |
	  force the table to be a	+-----%-----------+
	  rectangle.
 
   Now the table can be rendered with processTable, for example:
	Main> processTable table5
	[[("Hello",(1,2)),
	  ("Hello",(1,1)),
	  ("World",(1,1))],
	 [("World",(2,1))]] :: [[([Char],(Int,Int))]]
	Main> 
}

\h3{Contruction Functions} 

Perhaps one day I'll write the Show instance
to show boxes aka the above ascii renditions.

\begin{code}
instance (Show a) => Show (BlockTable a) where
      showsPrec p = showsTable

type TableI a = [[(a,(Int,Int))]] -> [[(a,(Int,Int))]]

data BlockTable a = Table (Int -> Int -> TableI a) Int Int
\end{code}

You can create a (1x1) table entry

\begin{code}
single :: a -> BlockTable a
single a = Table (\ x y z -> [(a,(x+1,y+1))] : z) 1 1
\end{code}

You can compose tables, horizonally and vertically

\begin{code}
above  :: BlockTable a -> BlockTable a -> BlockTable a
beside :: BlockTable a -> BlockTable a -> BlockTable a

t1 `above` t2 = trans (combine (trans t1) (trans t2) (.))

t1 `beside` t2 = combine t1 t2 (\ lst1 lst2 r ->
    let
      -- Note this depends on the fact that
      -- that the result has the same number
      -- of lines as the y dimention; one list
      -- per line. This is not true in general
      -- but is always true for these combinators.
      -- I should assert this!
      -- I should even prove this.
      beside (x:xs) (y:ys) = (x ++ y) : beside xs ys
      beside (x:xs) []     = x        : xs ++ r
      beside []     (y:ys) = y        : ys ++ r
      beside []     []     =                  r
    in
      beside (lst1 []) (lst2 []))
\end{code}

trans flips (transposes) over the x and y axis of
the table. It is only used internally, and typically
in pairs, ie. (flip ... munge ... (un)flip).

\begin{code}
trans :: BlockTable a -> BlockTable a
trans (Table f1 x1 y1) = Table (flip f1) y1 x1

combine :: BlockTable a 
      -> BlockTable b 
      -> (TableI a -> TableI b -> TableI c) 
      -> BlockTable c
combine (Table f1 x1 y1) (Table f2 x2 y2) comb = Table new_fn (x1+x2) max_y
    where
      max_y = max y1 y2
      new_fn x y =
         case compare y1 y2 of
          EQ -> comb (f1 0 y)             (f2 x y)
          GT -> comb (f1 0 y)             (f2 x (y + y1 - y2))
          LT -> comb (f1 0 (y + y2 - y1)) (f2 x y)
\end{code}

\h2{Investigation Functions}

This is the other thing you can do with a Table;
turn it into a 2D list, tagged with the (x,y)
sizes of each cell in the table.

\begin{code}
getMatrix :: BlockTable a -> [[(a,(Int,Int))]]
getMatrix (Table r _ _) = r 0 0 []
\end{code}

You can also look at a table
\begin{code}
showsTable :: (Show a) => BlockTable a -> ShowS
showsTable table = shows (getMatrix table)

showTable :: (Show a) => BlockTable a -> String
showTable table = showsTable table ""
\end{code}
