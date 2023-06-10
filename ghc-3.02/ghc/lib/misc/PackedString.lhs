%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1996
%
\section{Packed strings}

This sits on top of the sequencing/arrays world, notably @ByteArray#@s.

Glorious hacking (all the hard work) by Bryan O'Sullivan.

\begin{code}
module PackedString (
        PackedString,      -- abstract

         -- Creating the beasts
	packString,          -- :: [Char] -> PackedString
	packStringST,        -- :: [Char] -> ST s PackedString
        packCBytesST,        -- :: Int -> Addr -> ST s PackedString

	byteArrayToPS,       -- :: ByteArray Int -> PackedString
	unsafeByteArrayToPS, -- :: ByteArray a   -> Int -> PackedString

	psToByteArray,       -- :: PackedString  -> ByteArray Int
	psToByteArrayST,     -- :: PackedString  -> ST s (ByteArray Int)

	unpackPS,    -- :: PackedString -> [Char]
{-LATER:
	hPutPS,      -- :: Handle -> PackedString -> IO ()
        putPS,       -- :: FILE -> PackedString -> PrimIO () -- ToDo: more sensible type
	getPS,       -- :: FILE -> Int -> PrimIO PackedString
-}
	nilPS,       -- :: PackedString
	consPS,      -- :: Char -> PackedString -> PackedString
	headPS,      -- :: PackedString -> Char
	tailPS,      -- :: PackedString -> PackedString
	nullPS,      -- :: PackedString -> Bool
	appendPS,    -- :: PackedString -> PackedString -> PackedString
	lengthPS,    -- :: PackedString -> Int
          {- 0-origin indexing into the string -}
	indexPS,     -- :: PackedString -> Int -> Char
	mapPS,       -- :: (Char -> Char) -> PackedString -> PackedString
	filterPS,    -- :: (Char -> Bool) -> PackedString -> PackedString
	foldlPS,     -- :: (a -> Char -> a) -> a -> PackedString -> a
	foldrPS,     -- :: (Char -> a -> a) -> a -> PackedString -> a
	takePS,      -- :: Int -> PackedString -> PackedString
	dropPS,      -- :: Int -> PackedString -> PackedString
	splitAtPS,   -- :: Int -> PackedString -> (PackedString, PackedString)
	takeWhilePS, -- :: (Char -> Bool) -> PackedString -> PackedString
	dropWhilePS, -- :: (Char -> Bool) -> PackedString -> PackedString
	spanPS,      -- :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
	breakPS,     -- :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
	linesPS,     -- :: PackedString -> [PackedString]

	wordsPS,     -- :: PackedString -> [PackedString]
	reversePS,   -- :: PackedString -> PackedString
	splitPS,     -- :: Char -> PackedString -> [PackedString]
	splitWithPS, -- :: (Char -> Bool) -> PackedString -> [PackedString]
	joinPS,      -- :: PackedString -> [PackedString] -> PackedString
	concatPS,    -- :: [PackedString] -> PackedString
	elemPS,      -- :: Char -> PackedString -> Bool

	 {-
           Pluck out a piece of a PS start and end
	   chars you want; both 0-origin-specified
         -}
	substrPS,    -- :: PackedString -> Int -> Int -> PackedString

	comparePS,

 	  -- Converting to C strings
	packCString#, 
	unpackCString#, unpackCString2#, unpackAppendCString#, unpackFoldrCString#,
	unpackCString
    ) where

import GlaExts
import PrelBase ( showList__ ) -- ToDo: better
import Addr

import PrelArr  ( StateAndMutableByteArray#(..) , StateAndByteArray#(..) )
import PrelST
import ST
import IOExts   ( unsafePerformIO )

import Ix
import Char (isSpace)

\end{code}

%************************************************************************
%*									*
\subsection{@PackedString@ type declaration}
%*									*
%************************************************************************

\begin{code}
data PackedString
  = PS	ByteArray#  -- the bytes
	Int#	    -- length (*not* including NUL at the end)
	Bool	    -- True <=> contains a NUL
  | CPS	Addr#	    -- pointer to the (null-terminated) bytes in C land
	Int#	    -- length, as per strlen
		    -- definitely doesn't contain a NUL

instance Eq PackedString where
    x == y  = compare x y == EQ
    x /= y  = compare x y /= EQ

instance Ord PackedString where
    compare = comparePS
    x <= y  = compare x y /= GT
    x <	 y  = compare x y == LT
    x >= y  = compare x y /= LT
    x >	 y  = compare x y == GT
    max x y = case (compare x y) of { LT -> y ; EQ -> x ; GT -> x }
    min x y = case (compare x y) of { LT -> x ; EQ -> x ; GT -> y }

--instance Read PackedString: ToDo

instance Show PackedString where
    showsPrec p ps r = showsPrec p (unpackPS ps) r
    showList = showList__ (showsPrec 0) 
\end{code}


%************************************************************************
%*									*
\subsection{@PackedString@ instances}
%*									*
%************************************************************************

We try hard to make this go fast:
\begin{code}
comparePS :: PackedString -> PackedString -> Ordering

comparePS (PS  bs1 len1 has_null1) (PS  bs2 len2 has_null2)
  | not has_null1 && not has_null2
  = unsafePerformIO (
    _ccall_ strcmp ba1 ba2  >>= \ (I# res) ->
    return (
    if      res <#  0# then LT
    else if res ==# 0# then EQ
    else		    GT
    ))
  where
    ba1 = ByteArray (0, I# (len1 -# 1#)) bs1
    ba2 = ByteArray (0, I# (len2 -# 1#)) bs2

comparePS (PS  bs1 len1 has_null1) (CPS bs2 len2)
  | not has_null1
  = unsafePerformIO (
    _ccall_ strcmp ba1 ba2  >>= \ (I# res) ->
    return (
    if      res <#  0# then LT
    else if res ==# 0# then EQ
    else		    GT
    ))
  where
    ba1 = ByteArray (0, I# (len1 -# 1#)) bs1
    ba2 = A# bs2

comparePS (CPS bs1 len1) (CPS bs2 len2)
  = unsafePerformIO (
    _ccall_ strcmp ba1 ba2  >>= \ (I# res) ->
    return (
    if      res <#  0# then LT
    else if res ==# 0# then EQ
    else		    GT
    ))
  where
    ba1 = A# bs1
    ba2 = A# bs2

comparePS a@(CPS _ _) b@(PS _ _ has_null2)
  | not has_null2
  = -- try them the other way 'round
    case (comparePS b a) of { LT -> GT; EQ -> EQ; GT -> LT }

comparePS ps1 ps2 -- slow catch-all case (esp for "has_null" True)
  = looking_at 0#
  where
    end1 = lengthPS# ps1 -# 1#
    end2 = lengthPS# ps2 -# 1#

    looking_at char#
      = if char# ># end1 then
	   if char# ># end2 then -- both strings ran out at once
	      EQ
	   else	-- ps1 ran out before ps2
	      LT
	else if char# ># end2 then
	   GT	-- ps2 ran out before ps1
	else
	   let
	      ch1 = indexPS# ps1 char#
	      ch2 = indexPS# ps2 char#
	   in
	   if ch1 `eqChar#` ch2 then
	      looking_at (char# +# 1#)
	   else if ch1 `ltChar#` ch2 then LT
				     else GT
\end{code}


%************************************************************************
%*									*
\subsection{Constructor functions}
%*									*
%************************************************************************

Easy ones first.  @packString@ requires getting some heap-bytes and
scribbling stuff into them.

\begin{code}
nilPS :: PackedString
nilPS = CPS ""# 0#

consPS :: Char -> PackedString -> PackedString
consPS c cs = packString (c : (unpackPS cs)) -- ToDo:better

packString :: [Char] -> PackedString
packString str = runST (packStringST str)

packStringST :: [Char] -> ST s PackedString
packStringST str =
  let len = length str  in
  packNCharsST len str

packNCharsST :: Int -> [Char] -> ST s PackedString
packNCharsST len@(I# length#) str =
  {- 
   allocate an array that will hold the string
   (not forgetting the NUL byte at the end)
  -}
 new_ps_array (length# +# 1#) >>= \ ch_array ->
   -- fill in packed string from "str"
 fill_in ch_array 0# str   >>
   -- freeze the puppy:
 freeze_ps_array ch_array >>= \ (ByteArray _ frozen#) ->
 let has_null = byteArrayHasNUL# frozen# length# in
 return (PS frozen# length# has_null)
 where
  fill_in :: MutableByteArray s Int -> Int# -> [Char] -> ST s ()
  fill_in arr_in# idx [] =
   write_ps_array arr_in# idx (chr# 0#) >>
   return ()

  fill_in arr_in# idx (C# c : cs) =
   write_ps_array arr_in# idx c	 >>
   fill_in arr_in# (idx +# 1#) cs

byteArrayToPS :: ByteArray Int -> PackedString
byteArrayToPS (ByteArray ixs@(_, ix_end) frozen#) =
 let
  n# = 
   case (
	 if null (range ixs)
	  then 0
	  else ((index ixs ix_end) + 1)
        ) of { I# x -> x }
 in
 PS frozen# n# (byteArrayHasNUL# frozen# n#)

unsafeByteArrayToPS :: ByteArray a -> Int -> PackedString
unsafeByteArrayToPS (ByteArray _ frozen#) (I# n#)
  = PS frozen# n# (byteArrayHasNUL# frozen# n#)

psToByteArray	 :: PackedString -> ByteArray Int
psToByteArray (PS bytes n has_null)
  = ByteArray (0, I# (n -# 1#)) bytes

psToByteArray (CPS addr len#)
  = let
	len		= I# len#
	byte_array_form = packCBytes len (A# addr)
    in
    case byte_array_form of { PS bytes _ _ ->
    ByteArray (0, len - 1) bytes }
\end{code}

%************************************************************************
%*									*
\subsection{Destructor functions (taking @PackedStrings@ apart)}
%*									*
%************************************************************************

\begin{code}
-- OK, but this code gets *hammered*:
-- unpackPS ps
--   = [ indexPS ps n | n <- [ 0::Int .. lengthPS ps - 1 ] ]

unpackPS :: PackedString -> [Char]
unpackPS (PS bytes len has_null)
 = unpack 0#
 where
    unpack nh
      | nh >=# len  = []
      | otherwise   = C# ch : unpack (nh +# 1#)
      where
	ch = indexCharArray# bytes nh

unpackPS (CPS addr len)
  = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'# = []
      | otherwise	   = C# ch : unpack (nh +# 1#)
      where
	ch = indexCharOffAddr# addr nh
\end{code}

Output a packed string via a handle:

\begin{code}
{- LATER:
hPutPS :: Handle -> PackedString -> IO ()
hPutPS handle ps = 
 let
  len = 
   case ps of
    PS  _ len _ -> len
    CPS _ len   -> len
 in
 if len ==# 0# then
    return ()
 else
    _readHandle handle				    >>= \ htype ->
    case htype of 
      _ErrorHandle ioError ->
	  _writeHandle handle htype		    >>
          failWith ioError
      _ClosedHandle ->
	  _writeHandle handle htype		    >>
	  failWith (IllegalOperation "handle is closed")
      _SemiClosedHandle _ _ ->
	  _writeHandle handle htype		    >>
	  failWith (IllegalOperation "handle is closed")
      _ReadHandle _ _ _ ->
	  _writeHandle handle htype		    >>
	  failWith (IllegalOperation "handle is not open for writing")
      other -> 
          _getBufferMode other			    >>= \ other ->
          (case _bufferMode other of
            Just LineBuffering ->
		writeLines (_filePtr other)
            Just (BlockBuffering (Just size)) ->
	        writeBlocks (_filePtr other) size
            Just (BlockBuffering Nothing) ->
	        writeBlocks (_filePtr other) ``BUFSIZ''
            _ -> -- Nothing is treated pessimistically as NoBuffering
	        writeChars (_filePtr other) 0#
	  )    					    >>= \ success ->
	    _writeHandle handle (_markHandle other) >>
          if success then
              return ()
          else
              _constructError "hPutStr"   	    >>= \ ioError ->
	      failWith ioError

  where
    pslen = lengthPS# ps

    writeLines :: Addr -> IO Bool
    writeLines = writeChunks ``BUFSIZ'' True 

    writeBlocks :: Addr -> Int -> IO Bool
    writeBlocks fp size = writeChunks size False fp
 
     {-
       The breaking up of output into lines along \n boundaries
       works fine as long as there are newlines to split by.
       Avoid the splitting up into lines altogether (doesn't work
       for overly long lines like the stuff that showsPrec instances
       normally return). Instead, we split them up into fixed size
       chunks before blasting them off to the Real World.

       Hacked to avoid multiple passes over the strings - unsightly, but
       a whole lot quicker. -- SOF 3/96
     -}

    writeChunks :: Int -> Bool -> Addr -> IO Bool
    writeChunks (I# bufLen) chopOnNewLine fp =
     newCharArray (0,I# bufLen) >>= \ arr@(MutableByteArray _ arr#) ->
     let
      shoveString :: Int# -> Int# -> IO Bool
      shoveString n i 
       | i ==# pslen =   -- end of string
	   if n ==# 0# then
	      return True
	   else
             _ccall_ writeFile arr fp (I# n) >>= \rc ->
             return (rc==0)
       | otherwise =
	   (\ (S# s#) ->
              case writeCharArray# arr# n (indexPS# ps i) s# of
	        s1# -> 
		   {- Flushing lines - should we bother? -}
		  (if n ==# bufLen then
                     _ccall_ writeFile arr fp (I# (n +# 1#)) >>= \rc ->
	             if rc == 0 then
	                shoveString 0# (i +# 1#)
	              else
	                return False
                   else
                      shoveString (n +# 1#) (i +# 1#)) (S# s1#))
     in
     shoveString 0# 0#

    writeChars :: Addr -> Int# -> IO Bool
    writeChars fp i 
      | i ==# pslen = return True
      | otherwise  =
	_ccall_ filePutc fp (ord (C# (indexPS# ps i)))  >>= \ rc ->
        if rc == 0 then
	    writeChars fp (i +# 1#)
	else
	    return False

---------------------------------------------

putPS :: _FILE -> PackedString -> IO ()
putPS file ps@(PS bytes len has_null)
  | len ==# 0#
  = return ()
  | otherwise
  = let
	byte_array = ByteArray (0, I# (len -# 1#)) bytes
    in
    _ccall_ fwrite byte_array (1::Int){-size-} (I# len) file
					>>= \ (I# written) ->
    if written ==# len then
	return ()
    else
	error "putPS: fwrite failed!\n"

putPS file (CPS addr len)
  | len ==# 0#
  = return ()
  | otherwise
  = _ccall_ fputs (A# addr) file >>= \ (I# _){-force type-} ->
    return ()
\end{code}

The dual to @_putPS@, note that the size of the chunk specified
is the upper bound of the size of the chunk returned.

\begin{code}
getPS :: _FILE -> Int -> IO PackedString
getPS file len@(I# len#)
 | len# <=# 0# = return nilPS -- I'm being kind here.
 | otherwise   =
    -- Allocate an array for system call to store its bytes into.
   new_ps_array len#      >>= \ ch_arr ->
   freeze_ps_array ch_arr >>= \ (ByteArray _ frozen#) ->
   let
    byte_array = ByteArray (0, I# len#) frozen#
   in
   _ccall_ fread byte_array (1::Int) len file >>= \  (I# read#) ->
   if read# ==# 0# then -- EOF or other error
      error "getPS: EOF reached or other error"
   else
     {-
       The system call may not return the number of
       bytes requested. Instead of failing with an error
       if the number of bytes read is less than requested,
       a packed string containing the bytes we did manage
       to snarf is returned.
     -}
     let
      has_null = byteArrayHasNUL# frozen# read#
     in 
     return (PS frozen# read# has_null)
END LATER -}
\end{code}

%************************************************************************
%*									*
\subsection{List-mimicking functions for @PackedStrings@}
%*									*
%************************************************************************

First, the basic functions that do look into the representation;
@indexPS@ is the most important one.

\begin{code}
lengthPS   :: PackedString -> Int
lengthPS ps = I# (lengthPS# ps)

{-# INLINE lengthPS# #-}

lengthPS# (PS  _ i _) = i
lengthPS# (CPS _ i)   = i

{-# INLINE strlen# #-}

strlen# :: Addr# -> Int
strlen# a
  = unsafePerformIO (
    _ccall_ strlen (A# a)  >>= \ len@(I# _) ->
    return len
    )

byteArrayHasNUL# :: ByteArray# -> Int#{-length-} -> Bool
byteArrayHasNUL# bs len
  = unsafePerformIO (
    _ccall_ byteArrayHasNUL__ ba (I# len)  >>= \ (I# res) ->
    return (
    if res ==# 0# then False else True
    ))
  where
    ba = ByteArray (0, I# (len -# 1#)) bs

-----------------------

indexPS :: PackedString -> Int -> Char
indexPS ps (I# n) = C# (indexPS# ps n)

{-# INLINE indexPS# #-}

indexPS# (PS bs i _) n
  = --ASSERT (n >=# 0# && n <# i)	-- error checking: my eye!  (WDP 94/10)
    indexCharArray# bs n

indexPS# (CPS a _) n
  = indexCharOffAddr# a n
\end{code}

Now, the rest of the functions can be defined without digging
around in the representation.

\begin{code}
headPS :: PackedString -> Char
headPS ps
  | nullPS ps = error "headPS: head []"
  | otherwise  = C# (indexPS# ps 0#)

tailPS :: PackedString -> PackedString
tailPS ps
  | len <=# 0# = error "tailPS: tail []"
  | len ==# 1# = nilPS
  | otherwise  = substrPS# ps 1# (len -# 1#)
  where
    len = lengthPS# ps

nullPS :: PackedString -> Bool
nullPS (PS  _ i _) = i ==# 0#
nullPS (CPS _ i)   = i ==# 0#

{- (ToDo: some non-lousy implementations...)

    Old : _appendPS xs  ys = packString (unpackPS xs ++ unpackPS ys)

-}
appendPS :: PackedString -> PackedString -> PackedString
appendPS xs ys
  | nullPS xs = ys
  | nullPS ys = xs
  | otherwise  = concatPS [xs,ys]

{- OLD: mapPS f xs = packString (map f (unpackPS xs)) -}

mapPS :: (Char -> Char) -> PackedString -> PackedString {-or String?-}
mapPS f xs = 
  if nullPS xs then
     xs
  else
     runST (
       new_ps_array (length +# 1#)         >>= \ ps_arr ->
       whizz ps_arr length 0#              >>
       freeze_ps_array ps_arr	           >>= \ (ByteArray _ frozen#) ->
       let has_null = byteArrayHasNUL# frozen# length in
       return (PS frozen# length has_null))
  where
   length = lengthPS# xs

   whizz :: MutableByteArray s Int -> Int# -> Int# -> ST s ()
   whizz arr# n i 
    | n ==# 0#
      = write_ps_array arr# i (chr# 0#) >>
	return ()
    | otherwise
      = let
	 ch = indexPS# xs i
	in
	write_ps_array arr# i (case f (C# ch) of { (C# x) -> x})     >>
	whizz arr# (n -# 1#) (i +# 1#)

filterPS :: (Char -> Bool) -> PackedString -> PackedString {-or String?-}
filterPS pred ps = 
  if nullPS ps then
     ps
  else
     {-
      Filtering proceeds as follows:
      
       * traverse the list, applying the pred. to each element,
	 remembering the positions where it was satisfied.

	 Encode these positions using a run-length encoding of the gaps
	 between the matching positions. 
 
       * Allocate a MutableByteArray in the heap big enough to hold
         all the matched entries, and copy the elements that matched over.

      A better solution that merges the scan&copy passes into one,
      would be to copy the filtered elements over into a growable
      buffer. No such operation currently supported over
      MutableByteArrays (could of course use malloc&realloc)
      But, this solution may in the case of repeated realloc's
      be worse than the current solution.
     -}
     runST (
       let
        (rle,len_filtered) = filter_ps (len# -# 1#) 0# 0# []
	len_filtered#      = case len_filtered of { I# x# -> x#}
       in
       if len# ==# len_filtered# then 
         {- not much filtering as everything passed through. -}
         return ps
       else if len_filtered# ==# 0# then
	 return nilPS
       else
         new_ps_array (len_filtered# +# 1#) >>= \ ps_arr ->
         copy_arr ps_arr rle 0# 0#          >>
         freeze_ps_array ps_arr	            >>= \ (ByteArray _ frozen#) ->
         let has_null = byteArrayHasNUL# frozen# len_filtered# in
         return (PS frozen# len_filtered# has_null))
  where
   len# = lengthPS# ps

   matchOffset :: Int# -> [Char] -> (Int,[Char])
   matchOffset off [] = (I# off,[])
   matchOffset off (C# c:cs) =
    let
     x    = ord# c
     off' = off +# x
    in
    if x==# 0# then -- escape code, add 255#
       matchOffset off' cs
    else
       (I# off', cs)

   copy_arr :: MutableByteArray s Int -> [Char] -> Int# -> Int# -> ST s ()
   copy_arr arr# [_] _ _ = return ()
   copy_arr arr# ls  n i =
     let
      (x,ls') = matchOffset 0# ls
      n'      = n +# (case x of { (I# x#) -> x#}) -# 1#
      ch      = indexPS# ps n'
     in
     write_ps_array arr# i ch                >>
     copy_arr arr# ls' (n' +# 1#) (i +# 1#)

   esc :: Int# -> Int# -> [Char] -> [Char]
   esc v 0# ls = (C# (chr# v)):ls
   esc v n  ls = esc v (n -# 1#) (C# (chr# 0#):ls)

   filter_ps :: Int# -> Int# -> Int# -> [Char] -> ([Char],Int)
   filter_ps n hits run acc
    | n <# 0# = 
        let
	 escs = run `quotInt#` 255#
	 v    = run `remInt#`  255#
        in
       (esc (v +# 1#) escs acc, I# hits)
    | otherwise
       = let
          ch = indexPS# ps n
          n' = n -# 1#
	 in
         if pred (C# ch) then
	    let
	     escs = run `quotInt#` 255#
	     v    = run `remInt#`  255#
	     acc' = esc (v +# 1#) escs acc
	    in
	    filter_ps n' (hits +# 1#) 0# acc'
	 else
	    filter_ps n' hits (run +# 1#) acc


foldlPS :: (a -> Char -> a) -> a -> PackedString -> a
foldlPS f b ps 
 = if nullPS ps then
      b 
   else
      whizzLR b 0#
   where
    len = lengthPS# ps

    --whizzLR :: a -> Int# -> a
    whizzLR b idx
     | idx ==# len = b
     | otherwise   = whizzLR (f b (C# (indexPS# ps idx))) (idx +# 1#)
 

foldrPS :: (Char -> a -> a) -> a -> PackedString -> a
foldrPS f b ps  
 = if nullPS ps then
      b
   else
      whizzRL b len
   where
    len = lengthPS# ps

    --whizzRL :: a -> Int# -> a
    whizzRL b idx
     | idx <# 0# = b
     | otherwise = whizzRL (f (C# (indexPS# ps idx)) b) (idx -# 1#)

takePS :: Int -> PackedString -> PackedString
takePS (I# n) ps 
  | n ==# 0#   = nilPS
  | otherwise  = substrPS# ps 0# (n -# 1#)

dropPS	:: Int -> PackedString -> PackedString
dropPS (I# n) ps
  | n ==# len = ps
  | otherwise = substrPS# ps n  (lengthPS# ps -# 1#)
  where
    len = lengthPS# ps

splitAtPS :: Int -> PackedString -> (PackedString, PackedString)
splitAtPS  n ps  = (takePS n ps, dropPS n ps)

takeWhilePS :: (Char -> Bool) -> PackedString -> PackedString
takeWhilePS pred ps
  = let
	break_pt = char_pos_that_dissatisfies
			(\ c -> pred (C# c))
			ps
			(lengthPS# ps)
			0#
    in
    if break_pt ==# 0# then
       nilPS
    else
       substrPS# ps 0# (break_pt -# 1#)

dropWhilePS :: (Char -> Bool) -> PackedString -> PackedString
dropWhilePS pred ps
  = let
	len	 = lengthPS# ps
	break_pt = char_pos_that_dissatisfies
			(\ c -> pred (C# c))
			ps
			len
			0#
    in
    if len ==# break_pt then
       nilPS
    else
       substrPS# ps break_pt (len -# 1#)

elemPS :: Char -> PackedString -> Bool
elemPS (C# ch) ps
  = let
	len	 = lengthPS# ps
	break_pt = first_char_pos_that_satisfies
			(`eqChar#` ch)
			ps
			len
			0#
    in
    break_pt <# len

char_pos_that_dissatisfies :: (Char# -> Bool) -> PackedString -> Int# -> Int# -> Int#

char_pos_that_dissatisfies p ps len pos
  | pos >=# len		= pos -- end
  | p (indexPS# ps pos) = -- predicate satisfied; keep going
			  char_pos_that_dissatisfies p ps len (pos +# 1#)
  | otherwise		= pos -- predicate not satisfied

first_char_pos_that_satisfies :: (Char# -> Bool) -> PackedString -> Int# -> Int# -> Int#
first_char_pos_that_satisfies p ps len pos
  | pos >=# len		= pos -- end
  | p (indexPS# ps pos) = pos -- got it!
  | otherwise		= first_char_pos_that_satisfies p ps len (pos +# 1#)

-- ToDo: could certainly go quicker
spanPS :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
spanPS  p ps = (takeWhilePS p ps, dropWhilePS p ps)

breakPS :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
breakPS p ps = spanPS (not . p) ps

linesPS :: PackedString -> [PackedString]
linesPS ps = splitPS '\n' ps

wordsPS :: PackedString -> [PackedString]
wordsPS ps = splitWithPS isSpace ps

reversePS :: PackedString -> PackedString
reversePS ps =
  if nullPS ps then -- don't create stuff unnecessarily. 
     ps
  else
    runST (
      new_ps_array (length +# 1#)    >>= \ arr# -> -- incl NUL byte!
      fill_in arr# (length -# 1#) 0# >>
      freeze_ps_array arr#	     >>= \ (ByteArray _ frozen#) ->
      let has_null = byteArrayHasNUL# frozen# length in
      return (PS frozen# length has_null))
 where
  length = lengthPS# ps
  
  fill_in :: MutableByteArray s Int -> Int# -> Int# -> ST s ()
  fill_in arr_in# n i =
   let
    ch = indexPS# ps n
   in
   write_ps_array arr_in# i ch		         >>
   if n ==# 0# then
      write_ps_array arr_in# (i +# 1#) (chr# 0#) >>
      return ()
   else
      fill_in arr_in# (n -# 1#) (i +# 1#)
     
concatPS :: [PackedString] -> PackedString
concatPS [] = nilPS
concatPS pss
  = let
	tot_len# = case (foldr ((+) . lengthPS) 0 pss) of { I# x -> x }
	tot_len	 = I# tot_len#
    in
    runST (
    new_ps_array (tot_len# +# 1#)   >>= \ arr# -> -- incl NUL byte!
    packum arr# pss 0#		    >>
    freeze_ps_array arr#	    >>= \ (ByteArray _ frozen#) ->

    let has_null = byteArrayHasNUL# frozen# tot_len# in
	  
    return (PS frozen# tot_len# has_null)
    )
  where
    packum :: MutableByteArray s Int -> [PackedString] -> Int# -> ST s ()

    packum arr [] pos
      = write_ps_array arr pos (chr# 0#) >>
	return ()
    packum arr (ps : pss) pos
      = fill arr pos ps 0# (lengthPS# ps)  >>= \ (I# next_pos) ->
	packum arr pss next_pos

    fill :: MutableByteArray s Int -> Int# -> PackedString -> Int# -> Int# -> ST s Int

    fill arr arr_i ps ps_i ps_len
     | ps_i ==# ps_len
       = return (I# (arr_i +# ps_len))
     | otherwise
       = write_ps_array arr (arr_i +# ps_i) (indexPS# ps ps_i) >>
	 fill arr arr_i ps (ps_i +# 1#) ps_len

------------------------------------------------------------
joinPS :: PackedString -> [PackedString] -> PackedString
joinPS filler pss = concatPS (splice pss)
 where
  splice []  = []
  splice [x] = [x]
  splice (x:y:xs) = x:filler:splice (y:xs)

-- ToDo: the obvious generalisation
{-
  Some properties that hold:

  * splitPS x ls = ls'   
      where False = any (map (x `elemPS`) ls')
            False = any (map (nullPS) ls')

    * all x's have been chopped out.
    * no empty PackedStrings in returned list. A conseq.
      of this is:
           splitPS x nilPS = []
         

  * joinPS (packString [x]) (_splitPS x ls) = ls

-}

splitPS :: Char -> PackedString -> [PackedString]
splitPS (C# ch) = splitWithPS (\ (C# c) -> c `eqChar#` ch)

splitWithPS :: (Char -> Bool) -> PackedString -> [PackedString]
splitWithPS pred ps =
 splitify 0#
 where
  len = lengthPS# ps
  
  splitify n 
   | n >=# len = []
   | otherwise =
      let
       break_pt = 
         first_char_pos_that_satisfies
	    (\ c -> pred (C# c))
	    ps
	    len
	    n
      in
      if break_pt ==# n then -- immediate match, no substring to cut out.
         splitify (break_pt +# 1#)
      else 
         substrPS# ps n (break_pt -# 1#): -- leave out the matching character
         splitify (break_pt +# 1#)
\end{code}

%************************************************************************
%*									*
\subsection{Local utility functions}
%*									*
%************************************************************************

The definition of @_substrPS@ is essentially:
@take (end - begin + 1) (drop begin str)@.

\begin{code}
substrPS :: PackedString -> Int -> Int -> PackedString
substrPS ps (I# begin) (I# end) = substrPS# ps begin end

substrPS# ps s e
  | s <# 0# || e <# s
  = error "substrPS: bounds out of range"

  | s >=# len || result_len# <=# 0#
  = nilPS

  | otherwise
  = runST (
	new_ps_array (result_len# +# 1#) >>= \ ch_arr -> -- incl NUL byte!
	fill_in ch_arr 0#	         >>
	freeze_ps_array ch_arr	         >>= \ (ByteArray _ frozen#) ->

	let has_null = byteArrayHasNUL# frozen# result_len# in
	  
	return (PS frozen# result_len# has_null)
    )
  where
    len = lengthPS# ps

    result_len# = (if e <# len then (e +# 1#) else len) -# s
    result_len  = I# result_len#

    -----------------------
    fill_in :: MutableByteArray s Int -> Int# -> ST s ()

    fill_in arr_in# idx
      | idx ==# result_len#
      = write_ps_array arr_in# idx (chr# 0#) >>
	return ()
      | otherwise
      = let
	    ch = indexPS# ps (s +# idx)
	in
	write_ps_array arr_in# idx ch	     >>
	fill_in arr_in# (idx +# 1#)
\end{code}

(Very :-) ``Specialised'' versions of some CharArray things...

\begin{code}
new_ps_array	:: Int# -> ST s (MutableByteArray s Int)
write_ps_array	:: MutableByteArray s Int -> Int# -> Char# -> ST s () 
freeze_ps_array :: MutableByteArray s Int -> ST s (ByteArray Int)

new_ps_array size = ST $ \ s# ->
    case newCharArray# size s#  of { StateAndMutableByteArray# s2# barr# ->
    STret s2# (MutableByteArray bot barr#)}
  where
    bot = error "new_ps_array"

write_ps_array (MutableByteArray _ barr#) n ch = ST $ \ s# ->
    case writeCharArray# barr# n ch s#	of { s2#   ->
    STret s2# ()}

-- same as unsafeFreezeByteArray
freeze_ps_array (MutableByteArray ixs arr#) = ST $ \ s# ->
    case unsafeFreezeByteArray# arr# s# of { StateAndByteArray# s2# frozen# ->
    STret s2# (ByteArray ixs frozen#) }
\end{code}


%*********************************************************
%*							*
\subsection{Packing and unpacking C strings}
%*							*
%*********************************************************

\begin{code}
unpackCString :: Addr -> [Char]

-- Calls to the next four are injected by the compiler itself, 
-- to deal with literal strings
packCString#	     :: [Char]          -> ByteArray#
unpackCString#       :: Addr#           -> [Char]
unpackCString2#      :: Addr# -> Int#   -> [Char]
unpackAppendCString# :: Addr# -> [Char] -> [Char]
unpackFoldrCString#  :: Addr# -> (Char  -> a -> a) -> a -> a 

packCString# str = case (packString str) of { PS bytes _ _ -> bytes }

unpackCString a@(A# addr) = 
 if a == ``NULL'' then
    []
 else
    unpackCString# addr

unpackCString# addr
  = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'# = []
      | otherwise	   = C# ch : unpack (nh +# 1#)
      where
	ch = indexCharOffAddr# addr nh

unpackCString2# addr len
  -- This one is called by the compiler to unpack literal strings with NULs in them; rare.
  = unpackPS (packCBytes (I# len) (A# addr))

unpackAppendCString# addr rest
  = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'# = rest
      | otherwise	   = C# ch : unpack (nh +# 1#)
      where
	ch = indexCharOffAddr# addr nh

unpackFoldrCString# addr f z 
  = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'# = z
      | otherwise	   = C# ch `f` unpack (nh +# 1#)
      where
	ch = indexCharOffAddr# addr nh


cStringToPS	 :: Addr  -> PackedString
cStringToPS (A# a#) =	-- the easy one; we just believe the caller
 CPS a# len
 where
  len = case (strlen# a#) of { I# x -> x }

packBytesForC :: [Char] -> ByteArray Int
packBytesForC str = psToByteArray (packString str)

psToByteArrayST :: [Char] -> ST s (ByteArray Int)
psToByteArrayST str =
  packStringST str	>>= \ (PS bytes n has_null) -> 
   --later? ASSERT(not has_null)
  return (ByteArray (0, I# (n -# 1#)) bytes)

packNBytesForCST :: Int -> [Char] -> ST s (ByteArray Int)
packNBytesForCST len str =
  packNCharsST len str	>>= \ (PS bytes n has_null) -> 
  return (ByteArray (0, I# (n -# 1#)) bytes)

packCBytes :: Int -> Addr -> PackedString
packCBytes len addr = runST (packCBytesST len addr)

packCBytesST :: Int -> Addr -> ST s PackedString
packCBytesST len@(I# length#) (A# addr) =
  {- 
    allocate an array that will hold the string
    (not forgetting the NUL byte at the end)
  -}
  new_ps_array (length# +# 1#)  >>= \ ch_array ->
   -- fill in packed string from "addr"
  fill_in ch_array 0#   >>
   -- freeze the puppy:
  freeze_ps_array ch_array >>= \ (ByteArray _ frozen#) ->
  let has_null = byteArrayHasNUL# frozen# length# in
  return (PS frozen# length# has_null)
  where
    fill_in :: MutableByteArray s Int -> Int# -> ST s ()

    fill_in arr_in# idx
      | idx ==# length#
      = write_ps_array arr_in# idx (chr# 0#) >>
	return ()
      | otherwise
      = case (indexCharOffAddr# addr idx) of { ch ->
	write_ps_array arr_in# idx ch >>
	fill_in arr_in# (idx +# 1#) }

\end{code}
