%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1996
%
\section[Readline]{GNU Readline Library Bindings}

This module attempts to provide a better line based editing facility
for Haskell programmers by providing access to the GNU Readline
library.  Related to this are bindings for the GNU History library
which can be found in History (at some point in the future :-).

Original version by Darren Moffat
Heavily modified in 1999 by Sven Panne <Sven.Panne@informatik.uni-muenchen.de>

NOTE: This binding is still *very* incomplete...  Volunteers?

\begin{code}
{-# OPTIONS -#include <readline/readline.h> -#include <readline/history.h> #-}

module Readline
	( KeyCode
	, CallbackFunction

	, initialize		-- :: IO ()
	, readline		-- :: String -> IO (Maybe String)
	, addHistory		-- :: String -> IO ()
	, bindKey		-- :: KeyCode -> CallbackFunction -> IO ()
	, addDefun		-- :: String  -> CallbackFunction -> Maybe KeyCode -> IO ()

	, getReadlineName	-- :: IO String
	, setReadlineName	-- :: String -> IO ()
	, getLineBuffer		-- :: IO String
	, setLineBuffer		-- :: String -> IO ()
	, getPoint		-- :: IO Int
	, setPoint		-- :: Int -> IO ()
	, getEnd		-- :: IO Int
	, setEnd		-- :: Int -> IO ()
	, getMark		-- :: IO Int
	, setMark		-- :: Int -> IO ()
	, setDone		-- :: Bool -> IO ()
	, setPendingInput	-- :: KeyCode -> IO ()
	, getPrompt		-- :: IO String
	, getTerminalName	-- :: IO String
	, inStream		-- :: Handle
	, outStream		-- :: Handle
	) where

import Char	( ord, chr )
import Foreign	( Storable(..), Addr, nullAddr, mallocElems, free, alloca )
import IO	( Handle )
import IOExts	( IORef, newIORef, readIORef, writeIORef,
		  unsafePerformIO, freeHaskellFunctionPtr )
import Monad	( unless, zipWithM_ )
import Posix	( intToFd, fdToHandle )
import System	( getProgName )

-- SUP: Haskell has closures and I've got no clue about the return value,
--      so a better type for the callbacks is probably
--      Int {- Numeric Arg -} -> IO ()

type KeyCode = Char

type CallbackFunction = 
    (Int ->			-- Numeric Argument
     KeyCode ->			-- KeyCode of pressed Key
     IO Int)                    -- What's this?
\end{code}

%***************************************************************************
%*                                                                         *
\subsection[Readline-Functions]{Main Readline Functions}
%*                                                                         *
%***************************************************************************
\begin{code}

initialize :: IO ()
initialize = do
  n <- marshalString ""
  _casm_ ``rl_readline_name = %0;'' n
  setReadlineName =<< getProgName
  b <- marshalString ""
  _casm_ ``rl_line_buffer = %0;'' b

readline :: String		-- Prompt String
	 -> IO (Maybe String)	-- Just returned line or Nothing if EOF
readline prompt =  do
   lAddr <- inString readlineAux prompt
   if lAddr == nullAddr
      then return Nothing
      else do line <- unmarshalString lAddr
              free lAddr
              return (Just line)

foreign import "readline" readlineAux :: Addr -> IO Addr

addHistory :: String		-- String to enter in history
           -> IO ()
addHistory = inString add_history

foreign import unsafe add_history :: Addr -> IO ()

bindKey :: KeyCode		-- Key to Bind to
	-> CallbackFunction	-- Function to exec on execution
	-> IO ()
bindKey key cback = do
   cbAddr <- mkCallback (\n k -> cback n (chr k))
   ok     <- rl_bind_key (ord key) cbAddr
   if ok /= 0 then wrongKeyCode else addCbackEntry key cbAddr

foreign export dynamic mkCallback :: (Int -> Int -> IO Int) -> IO Addr
foreign import unsafe rl_bind_key :: Int -> Addr -> IO Int

addDefun :: String		-- Function Name
         -> CallbackFunction	-- Function to call
	 -> Maybe KeyCode	-- Key to bind to
	 -> IO ()
addDefun name cback mbKey = do
   cbAddr <- mkCallback (\n k -> cback n (chr k))
   -- ATTENTION: Memory leak due to silly readline behaviour (rl_add_defun does
   -- *not* make a copy of the function name!
   addr   <- marshalString name
   ok     <- rl_add_defun addr cbAddr (maybe (-1) ord mbKey)
   unless (ok == 0) wrongKeyCode

foreign import unsafe rl_add_defun :: Addr -> Addr -> Int -> IO Int

-- Don't know how this should ever happen with KeyCode = Char
wrongKeyCode :: IO ()
wrongKeyCode = ioError (userError "Invalid ASCII Key Code, must be in range 0..255")

-- Global hacking for freeing callbacks

{-# notInline theCbackTable #-}
theCbackTable :: IORef [(KeyCode,Addr)]
theCbackTable = unsafePerformIO (newIORef [])

addCbackEntry :: KeyCode -> Addr -> IO ()
addCbackEntry key cbAddr = do
   cbackTable <- readIORef theCbackTable
   maybe (return ()) freeHaskellFunctionPtr (lookup key cbackTable)
   writeIORef theCbackTable
              ((key,cbAddr) : [ entry | entry@(k,_) <- cbackTable, k /= key ])
\end{code}

%***************************************************************************
%*                                                                         *
\subsection[Readline-Globals]{Global Readline Variables}
%*                                                                         *
%***************************************************************************

These are the global variables required by the readline lib. Need to
find a way of making these read/write from the Haskell side.  Should
they be in the IO Monad, should they be Mutable Variables?

\begin{code}
getReadlineName :: IO String
getReadlineName = unmarshalString =<< _casm_ ``%r = rl_readline_name;''

setReadlineName :: String -> IO ()
setReadlineName str = do
   free =<< _casm_ ``%r = rl_readline_name;''
   addr <- marshalString str
   _casm_ ``rl_readline_name = %0;'' addr

getLineBuffer :: IO String
getLineBuffer = unmarshalString =<< _casm_ ``%r = rl_line_buffer;''
				
setLineBuffer :: String -> IO ()
setLineBuffer str = do
   free =<< _casm_ ``%r = rl_line_buffer;''
   addr <- marshalString str
   _casm_ ``rl_line_buffer = %0;'' addr

getPoint :: IO Int
getPoint = _casm_ ``%r = rl_point;''

setPoint :: Int -> IO ()
setPoint point = _casm_ ``rl_point = %0;'' point
	 
getEnd :: IO Int
getEnd = _casm_ ``%r = rl_end;''

setEnd :: Int -> IO ()
setEnd end = _casm_ ``rl_end = %0;'' end

getMark :: IO Int
getMark = _casm_ ``%r = rl_mark;''

setMark :: Int -> IO ()
setMark mark = _casm_ ``rl_mark = %0;'' mark

setDone :: Bool -> IO ()
setDone done = _casm_ ``rl_done = %0;'' (if done then 0::Int else 1)

setPendingInput :: KeyCode -> IO ()
setPendingInput key = _casm_ ``rl_pending_input = %0;'' key

getPrompt :: IO String
getPrompt = unmarshalString =<<  _casm_ ``%r = rl_prompt;''

getTerminalName :: IO String
getTerminalName = unmarshalString =<< _casm_ ``%r = rl_terminal_name;''

inStream :: Handle
inStream  = unsafePerformIO (fdToHandle (intToFd ``fileno(rl_instream)''))

outStream :: Handle
outStream = unsafePerformIO (fdToHandle (intToFd ``fileno(rl_outstream)''))
\end{code}

%***************************************************************************
%*                                                                         *
\subsection[Readline-Util]{Miscellaneous utility functions}
%*                                                                         *
%***************************************************************************

\begin{code}
pokeString :: Addr -> String -> IO ()
pokeString buf str = do
   zipWithM_ (pokeElemOff buf) [ 0 .. ] (str ++ "\0")

inString :: (Addr -> IO a) -> String -> IO a
inString act str = alloca (length str + 1)
                          (\addr -> do pokeString addr str
                                       act addr)

marshalString :: String -> IO Addr
marshalString str = do
   let numElements = length str
   buf <- mallocElems (head str) (numElements+1)
   pokeString buf str
   return buf

unmarshalString :: Addr -> IO String
unmarshalString buf = loop 0 []
   where loop idx accu = do
            c <- peekElemOff buf idx
            if c == '\0'
               then return $ reverse accu
               else loop (idx+1) (c:accu)
\end{code}

A simple test:

import Readline(initialize, bindKey, addHistory, readline)
import Monad(unless)

main :: IO ()
main = do initialize
          bindKey '\^X' (\nargc kc -> do print (nargc,kc); return 0)
          loop
   where loop = maybe (putStrLn "Qapla'!")
                      (\reply -> do unless (null reply) $ do
                                       addHistory reply
                                       putStrLn (reply ++ "...   pItlh!")
                                    loop) =<< readline "nuqneH, ghunwI'? "
