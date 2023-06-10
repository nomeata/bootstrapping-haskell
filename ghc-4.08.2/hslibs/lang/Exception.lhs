% -----------------------------------------------------------------------------
% $Id: Exception.lhs,v 1.11.2.1 2000/06/19 09:41:11 simonmar Exp $
%
% (c) The GRAP/AQUA Project, Glasgow University, 1998
%

The External API for exceptions.  The functions provided in this
module allow catching of exceptions in the IO monad.

\begin{code}
module Exception (
	Exception(..),		-- instance Eq, Ord, Show, Typeable
	ArithException(..),	-- instance Eq, Ord, Show, Typeable
	ArrayException(..),	-- instance Eq, Ord, Show, Typeable
	AsyncException(..),	-- instance Eq, Ord, Show, Typeable

	tryAll,    -- :: a    -> IO (Either Exception a)
	tryAllIO,  -- :: IO a -> IO (Either Exception a)
	try,	   -- :: (Exception -> Maybe b) -> a    -> IO (Either b a)
	tryIO,	   -- :: (Exception -> Maybe b) -> IO a -> IO (Either b a)

	catchAll,  -- :: a    -> (Exception -> IO a) -> IO a
	catchAllIO,-- :: IO a -> (Exception -> IO a) -> IO a
	catch,     -- :: (Exception -> Maybe b) -> a    -> (b -> IO a) -> IO a
	catchIO,   -- :: (Exception -> Maybe b) -> IO a -> (b -> IO a) -> IO a

	-- Exception predicates

	justIoErrors,		-- :: Exception -> Maybe IOError
	justArithExceptions, 	-- :: Exception -> Maybe ArithException
	justErrors,		-- :: Exception -> Maybe String
	justDynExceptions,	-- :: Exception -> Maybe Dynamic
	justAssertions,		-- :: Exception -> Maybe String
	justAsyncExceptions, 	-- :: Exception -> Maybe AsyncException

	-- Throwing exceptions

	throw,		-- :: Exception -> a
#ifndef __HUGS__
	-- for now
	raiseInThread,	-- :: ThreadId -> Exception -> a
#endif

	-- Dynamic exceptions

	throwDyn, 	-- :: Typeable ex => ex -> b
	catchDyn, 	-- :: Typeable ex => IO a -> (ex -> IO a) -> IO a
	
	-- Async exception control

        blockAsyncExceptions,   -- :: IO a -> IO a
        unblockAsyncExceptions, -- :: IO a -> IO a

	-- Assertions

	-- for now
	assert,		-- :: Bool -> a -> a

	-- Utilities
		
	finally, 	-- :: IO a -> IO b -> IO b

	bracket,  	-- :: IO a -> (a -> IO b) -> (a -> IO c) -> IO ()
	bracket_, 	-- :: IO a -> IO b -> IO c -> IO ()
  ) where

#ifndef __HUGS__
import Prelude hiding (catch)
import PrelGHC (catch#, assert)
import PrelException hiding (try, catch, bracket, bracket_)
import PrelConc	( raiseInThread )
import PrelIOBase ( IO(..) )
#else
import Prelude hiding ( catch )
import PrelPrim	( catchException 
		, Exception(..)
		, throw
		, ArithException(..)
		, AsyncException(..)
		, assert
		)
#endif

import Dynamic	( Dynamic, toDyn, fromDynamic, Typeable(..)
		, TyCon, mkTyCon, mkAppTy
		)
\end{code}

-----------------------------------------------------------------------------
Catching exceptions

PrelException defines 'catchException' for us.

\begin{code}
catchAll  :: a    -> (Exception -> IO a) -> IO a
catchAll a handler = catchException (a `seq` return a) handler

catchAllIO :: IO a -> (Exception -> IO a) -> IO a
catchAllIO =  catchException

catch :: (Exception -> Maybe b) -> a -> (b -> IO a) -> IO a
catch p a handler = catchAll a handler'
  where handler' e = case p e of 
			Nothing -> throw e
			Just b  -> handler b

catchIO :: (Exception -> Maybe b) -> IO a -> (b -> IO a) -> IO a
catchIO p a handler = catchAllIO a handler'
  where handler' e = case p e of 
			Nothing -> throw e
			Just b  -> handler b
\end{code}

-----------------------------------------------------------------------------
'try' and variations.

\begin{code}
tryAll :: a    -> IO (Either Exception a)
tryAll a = catchException (a `seq` return (Right a)) (\e -> return (Left e))

tryAllIO :: IO a -> IO (Either Exception a)
tryAllIO a = catchAllIO (a >>= \ v -> return (Right v))
			(\e -> return (Left e))

try :: (Exception -> Maybe b) -> a -> IO (Either b a)
try p a = do
  r <- tryAll a
  case r of
	Right v -> return (Right v)
	Left  e -> case p e of
			Nothing -> throw e
			Just b  -> return (Left b)

tryIO :: (Exception -> Maybe b) -> IO a -> IO (Either b a)
tryIO p a = do
  r <- tryAllIO a
  case r of
	Right v -> return (Right v)
	Left  e -> case p e of
			Nothing -> throw e
			Just b  -> return (Left b)
\end{code}

-----------------------------------------------------------------------------
Dynamic exception types.  Since one of the possible kinds of exception
is a dynamically typed value, we can effectively have polymorphic
exceptions.

throwDyn will raise any value as an exception, provided it is in the
Typeable class (see Dynamic.lhs).  

catchDyn will catch any exception of a given type (determined by the
handler function).  Any raised exceptions that don't match are
re-raised.

\begin{code}
throwDyn :: Typeable exception => exception -> b
throwDyn exception = throw (DynException (toDyn exception))

catchDyn :: Typeable exception => IO a -> (exception -> IO a) -> IO a
catchDyn m k = catchException m handle
  where handle ex = case ex of
  			   (DynException dyn) ->
		  	  	case fromDynamic dyn of
				    Just exception  -> k exception
				    Nothing -> throw ex
			   _ -> throw ex
\end{code}

-----------------------------------------------------------------------------
Exception Predicates

\begin{code}
justIoErrors		:: Exception -> Maybe IOError
justArithExceptions 	:: Exception -> Maybe ArithException
justErrors		:: Exception -> Maybe String
justDynExceptions	:: Exception -> Maybe Dynamic
justAssertions		:: Exception -> Maybe String
justAsyncExceptions 	:: Exception -> Maybe AsyncException

justIoErrors (IOException e) = Just e
justIoErrors _ = Nothing

justArithExceptions (ArithException e) = Just e
justArithExceptions _ = Nothing

justErrors (ErrorCall e) = Just e
justErrors _ = Nothing

justAssertions (AssertionFailed e) = Just e
justAssertions _ = Nothing

justDynExceptions (DynException e) = Just e
justDynExceptions _ = Nothing

justAsyncExceptions (AsyncException e) = Just e
justAsyncExceptions _ = Nothing
\end{code}

-----------------------------------------------------------------------------
Some Useful Functions

\begin{code}
bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after thing =
  blockAsyncExceptions (do
    a <- before 
    r <- catchAllIO 
	   (unblockAsyncExceptions (thing a))
	   (\e -> do { after a; throw e })
    after a
    return r
 )
   
-- finally is an instance of bracket, but it's quite common
-- so we give the specialised version for efficiency.
finally :: IO a -> IO b -> IO a
a `finally` sequel =
  blockAsyncExceptions (do
    r <- catchAllIO 
	     (unblockAsyncExceptions a)
	     (\e -> do { sequel; throw e })
    sequel
    return r
  )

bracket_ :: IO a -> IO b -> IO c -> IO c
bracket_ before after thing = bracket before (const after) (const thing)

#ifdef __HUGS__
-- For Hugs, we do not implemented blocking (yet!)
blockAsyncExceptions :: IO a -> IO a
blockAsyncExceptions a = a
unblockAsyncExceptions :: IO a -> IO a
unblockAsyncExceptions a = a
#endif

\end{code}

-----------------------------------------------------------------------------
Typeable instances

\begin{code}
exceptionTc :: TyCon
exceptionTc = mkTyCon "Exception"

instance Typeable Exception where
  typeOf _ = mkAppTy exceptionTc []

iOErrorTc :: TyCon
iOErrorTc = mkTyCon "IOError"

instance Typeable IOError where
  typeOf _ = mkAppTy iOErrorTc []

arithExceptionTc :: TyCon
arithExceptionTc = mkTyCon "ArithException"

instance Typeable ArithException where
  typeOf _ = mkAppTy arithExceptionTc []

arrayExceptionTc :: TyCon
arrayExceptionTc = mkTyCon "ArrayException"

instance Typeable ArrayException where
  typeOf _ = mkAppTy arrayExceptionTc []

asyncExceptionTc :: TyCon
asyncExceptionTc = mkTyCon "AsyncException"

instance Typeable AsyncException where
  typeOf _ = mkAppTy asyncExceptionTc []
\end{code}
