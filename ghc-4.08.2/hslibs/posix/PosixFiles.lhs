%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1996
%
\section[PosixFiles]{Haskell 1.3 POSIX File and Directory Operations}

\begin{code}
module PosixFiles (

    -- Directory streams
    DirStream,
    openDirStream, closeDirStream,
    readDirStream, rewindDirStream,

    -- set/get process' working directory.
    getWorkingDirectory, changeWorkingDirectory,

    -- File modes/permissions
    FileMode,
    nullFileMode,
    ownerReadMode, ownerWriteMode, ownerExecuteMode, ownerModes,
    groupReadMode, groupWriteMode, groupExecuteMode, groupModes,
    otherReadMode, otherWriteMode, otherExecuteMode, otherModes,
    setUserIDMode, setGroupIDMode,
    stdFileMode,   accessModes,

    unionFileModes, intersectFileModes,

    -- File operations on descriptors
    stdInput, stdOutput, stdError,
    OpenMode(..),
    OpenFileFlags(..), defaultFileFlags,
    openFd, createFile,

    -- other file&directory operations
    setFileCreationMask,
    createLink, removeLink,
    createDirectory, removeDirectory,
    createNamedPipe,
    rename,

    -- FileStatus
    FileStatus,
    getFileStatus, getFdStatus,
    fileExist,
    fileAccess,
    setFileMode,

    fileMode,
    fileID,         FileID,
    deviceID,       DeviceID,
    linkCount,
    fileOwner, fileGroup,
    fileSize,
    accessTime,     modificationTime, statusChangeTime,
    isDirectory,    isCharacterDevice,
    isBlockDevice,  isRegularFile,
    isNamedPipe,

    setOwnerAndGroup,  -- chown (might be restricted)
    setFileTimes,      -- set access and modification time
    touchFile,         -- set access and modification time to current time.

    -- run-time limit & POSIX feature testing
    PathVar(..),
    getPathVar,
    getFileVar

    ) where

import PrelST
import ST
import PrelIOBase
import IO
import IOExts		( unsafePerformIO )
import CString		( packString, allocChars, freeze, strcpy )
import Addr		( Addr, nullAddr, Word(..) )
import CCall
import PrelBase	hiding( append )
import ByteArray

import PosixErr
import PosixUtil
import Directory	( removeDirectory,  -- re-use its code
			  getCurrentDirectory,
			  setCurrentDirectory
			)

\end{code}

%************************************************************
%*                                                          *
\subsection[DirStream]{POSIX Directory streams}
%*                                                          *
%************************************************************

Accessing directories is done in POSIX via @DIR@ streams, with
operations for opening, closing, reading and rewinding the current
pointer in a directory.

{\bf Note:} The standard interface @Directory@ provides the
operation @getDirectoryContents@ which returns the directory contents of a
specified file path, which supplants some of the raw @DirStream@ operations
defined here.

\begin{code}

newtype DirStream = DirStream Addr
instance CCallable   DirStream
instance CReturnable DirStream

openDirStream :: FilePath -> IO DirStream
openDirStream name = do
    dirp <- _ccall_ opendir (packString name)
    if dirp /= nullAddr
       then return (DirStream dirp)
       else syserr "openDirStream"

readDirStream :: DirStream -> IO String
readDirStream dirp = do
    setErrorCode noError
    dirent <- _ccall_ readdir dirp
    if dirent /= nullAddr
       then do
	    str <- _casm_ ``%r = ((struct dirent *)%0)->d_name;'' dirent
	    name <- strcpy str
	    return name
       else do
	     errno <- getErrorCode
	     if errno == noError
		then ioError (IOError Nothing EOF "readDirStream" "EOF")
		else syserr "readDirStream"

rewindDirStream :: DirStream -> IO ()
rewindDirStream dirp = do
    _ccall_ rewinddir dirp
    return ()

closeDirStream :: DirStream -> IO ()
closeDirStream dirp = do
    rc <- _ccall_ closedir dirp
    if rc == (0::Int)
       then return ()
       else syserr "closeDirStream"

{-
 Renamings of functionality provided via Directory interface,
 kept around for b.wards compatibility and for having more POSIXy
 names
-}
getWorkingDirectory :: IO FilePath
getWorkingDirectory = getCurrentDirectory

changeWorkingDirectory :: FilePath -> IO ()
changeWorkingDirectory name = setCurrentDirectory name
\end{code}

%************************************************************
%*                                                          *
\subsection[FileMode]{POSIX File modes}
%*                                                          *
%************************************************************

The abstract type @FileMode@ and constants and operators for manipulating the
file modes defined by POSIX.

\begin{code}

newtype FileMode = FileMode Word   deriving Eq
instance CCallable FileMode
instance CReturnable FileMode

nullFileMode :: FileMode
nullFileMode = FileMode ``0''

ownerReadMode :: FileMode
ownerReadMode = FileMode ``S_IRUSR''

ownerWriteMode :: FileMode
ownerWriteMode = FileMode ``S_IWUSR''

ownerExecuteMode :: FileMode
ownerExecuteMode = FileMode ``S_IXUSR''

groupReadMode :: FileMode
groupReadMode = FileMode ``S_IRGRP''

groupWriteMode :: FileMode
groupWriteMode = FileMode ``S_IWGRP''

groupExecuteMode :: FileMode
groupExecuteMode = FileMode ``S_IXGRP''

otherReadMode :: FileMode
otherReadMode = FileMode ``S_IROTH''

otherWriteMode :: FileMode
otherWriteMode = FileMode ``S_IWOTH''

otherExecuteMode :: FileMode
otherExecuteMode = FileMode ``S_IXOTH''

setUserIDMode :: FileMode
setUserIDMode = FileMode ``S_ISUID''

setGroupIDMode :: FileMode
setGroupIDMode = FileMode ``S_ISGID''

stdFileMode :: FileMode
stdFileMode = FileMode ``(S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH)''

ownerModes :: FileMode
ownerModes = FileMode ``S_IRWXU''

groupModes :: FileMode
groupModes = FileMode ``S_IRWXG''

otherModes :: FileMode
otherModes = FileMode ``S_IRWXO''

accessModes :: FileMode
accessModes = FileMode ``(S_IRWXU|S_IRWXG|S_IRWXO)''

unionFileModes :: FileMode -> FileMode -> FileMode
unionFileModes (FileMode (W# m1)) (FileMode (W# m2)) = FileMode (W# (m1 `or#` m2))

intersectFileModes :: FileMode -> FileMode -> FileMode
intersectFileModes (FileMode (W# m1)) (FileMode (W# m2)) = FileMode (W# (m1 `and#` m2))

\end{code}

%************************************************************
%*                                                          *
\subsection[FileDescriptor]{POSIX File descriptors}
%*                                                          *
%************************************************************

File descriptors (formerly @Channel@s) are the lowest level
handles to file objects.

\begin{code}
stdInput, stdOutput, stdError :: Fd
stdInput   = intToFd 0
stdOutput  = intToFd 1
stdError   = intToFd 2

data OpenMode = ReadOnly | WriteOnly | ReadWrite

data OpenFileFlags =
 OpenFileFlags {
    append    :: Bool,
    exclusive :: Bool,
    noctty    :: Bool,
    nonBlock  :: Bool,
    trunc     :: Bool
 }

defaultFileFlags :: OpenFileFlags
defaultFileFlags =
 OpenFileFlags {
    append    = False,
    exclusive = False,
    noctty    = False,
    nonBlock  = False,
    trunc     = False
  }

openFd :: FilePath
       -> OpenMode
       -> Maybe FileMode -- Just x => O_CREAT, Nothing => must exist
       -> OpenFileFlags
       -> IO Fd
openFd name how maybe_mode (OpenFileFlags append exclusive noctty nonBlock truncate) = do
    fd <- _ccall_ open (packString name) flags mode_w
    if fdToInt fd /= ((-1)::Int)
       then return fd
       else syserr "openFd"
  where
    mode_w = case maybe_mode of { Nothing -> ``0'' ; Just x -> x }
    flags  = W# (creat# `or#` flags# `or#` how#)

    or (W# x#) (W# y#) = W# (x# `or#` y#)

    (W# flags#) =
       (if append    then ``O_APPEND''   else zero) `or`
       (if exclusive then ``O_EXCL''     else zero) `or`
       (if noctty    then ``O_NOCTTY''   else zero) `or`
       (if nonBlock  then ``O_NONBLOCK'' else zero) `or`
       (if truncate  then ``O_TRUNC''    else zero)

    zero = W# (int2Word# 0#)

    creat# =
     case (case maybe_mode of {
              Nothing -> zero ;
	      Just _ -> ``O_CREAT'' }) of {
      W# x -> x }

    how#  =
     case
      (case how of { ReadOnly  -> ``O_RDONLY'';
                     WriteOnly -> ``O_WRONLY'';
		     ReadWrite -> ``O_RDWR''}) of {
      W# x -> x }

createFile :: FilePath -> FileMode -> IO Fd
createFile name mode =
    _ccall_ creat (packString name) mode >>= \fd ->
    if fdToInt fd /= ((-1)::Int)
       then return fd
       else syserr "createFile"

setFileCreationMask :: FileMode -> IO FileMode
setFileCreationMask mask =  _ccall_ umask mask

createLink :: FilePath -> FilePath -> IO ()
createLink name1 name2 = do
    rc <- _ccall_ link (packString name1) (packString name2)
    if rc == (0::Int)
       then return ()
       else syserr "createLink"

createDirectory :: FilePath -> FileMode -> IO ()
createDirectory name mode = do -- NB: diff signature from LibDirectory one!
    rc  <- _ccall_ mkdir (packString name) mode
    if rc == (0::Int)
       then return ()
       else syserr "createDirectory"

createNamedPipe :: FilePath -> FileMode -> IO ()
createNamedPipe name mode = do
    rc   <-_ccall_ mkfifo (packString name) mode
    if rc == (0::Int)
       then return ()
       else syserr "createNamedPipe"

removeLink :: FilePath -> IO ()
removeLink name = do
    rc   <-_ccall_ unlink (packString name)
    if rc == (0::Int)
       then return ()
       else syserr "removeLink"

rename :: FilePath -> FilePath -> IO ()
rename name1 name2 = do
    rc    <- _ccall_ rename (packString name1) (packString name2)
    if rc == (0::Int)
       then return ()
       else syserr "rename"

type FileStatus = ByteArray Int
type FileID = Int
type DeviceID = Int

fileMode :: FileStatus -> FileMode
fileMode stat = unsafePerformIO $
    _casm_ ``%r = ((struct stat *)%0)->st_mode;'' stat

fileID :: FileStatus -> FileID
fileID stat = unsafePerformIO $
    _casm_ ``%r = ((struct stat *)%0)->st_ino;'' stat

deviceID :: FileStatus -> DeviceID
deviceID stat = unsafePerformIO $
    _casm_ ``%r = ((struct stat *)%0)->st_dev;'' stat

linkCount :: FileStatus -> LinkCount
linkCount stat = unsafePerformIO $
    _casm_ ``%r = ((struct stat *)%0)->st_nlink;'' stat

fileOwner :: FileStatus -> UserID
fileOwner stat = unsafePerformIO $
    _casm_ ``%r = ((struct stat *)%0)->st_uid;'' stat

fileGroup :: FileStatus -> GroupID
fileGroup stat = unsafePerformIO $
    _casm_ ``%r = ((struct stat *)%0)->st_gid;'' stat

fileSize :: FileStatus -> FileOffset
fileSize stat = unsafePerformIO $
    _casm_ ``%r = ((struct stat *)%0)->st_size;'' stat

accessTime :: FileStatus -> EpochTime
accessTime stat = unsafePerformIO $
    _casm_ ``%r = ((struct stat *)%0)->st_atime;'' stat

modificationTime :: FileStatus -> EpochTime
modificationTime stat = unsafePerformIO $
    _casm_ ``%r = ((struct stat *)%0)->st_mtime;'' stat

statusChangeTime :: FileStatus -> EpochTime
statusChangeTime stat = unsafePerformIO $
    _casm_ ``%r = ((struct stat *)%0)->st_ctime;'' stat

isDirectory :: FileStatus -> Bool
isDirectory stat = unsafePerformIO $
    _casm_ ``%r = S_ISDIR(((struct stat *)%0)->st_mode);'' stat >>= \ rc ->
    return (rc /= (0::Int))

isCharacterDevice :: FileStatus -> Bool
isCharacterDevice stat = unsafePerformIO $
    _casm_ ``%r = S_ISCHR(((struct stat *)%0)->st_mode);'' stat >>= \ rc ->
    return (rc /= (0::Int))

isBlockDevice :: FileStatus -> Bool
isBlockDevice stat = unsafePerformIO $
    _casm_ ``%r = S_ISBLK(((struct stat *)%0)->st_mode);'' stat >>= \ rc ->
    return (rc /= (0::Int))

isRegularFile :: FileStatus -> Bool
isRegularFile stat = unsafePerformIO $
    _casm_ ``%r = S_ISREG(((struct stat *)%0)->st_mode);'' stat >>= \ rc ->
    return (rc /= (0::Int))

isNamedPipe :: FileStatus -> Bool
isNamedPipe stat = unsafePerformIO $
    _casm_ ``%r = S_ISFIFO(((struct stat *)%0)->st_mode);'' stat >>= \ rc ->
    return (rc /= (0::Int))

getFileStatus :: FilePath -> IO FileStatus
getFileStatus name = do
    bytes <- allocChars ``sizeof(struct stat)''
    rc    <- _casm_ ``%r = stat(%0,(struct stat *)%1);'' (packString name) bytes
    if rc == (0::Int)
       then do
	    stat <- freeze bytes
	    return stat
       else syserr "getFileStatus"

getFdStatus :: Fd -> IO FileStatus
getFdStatus fd = do
    bytes <- allocChars ``sizeof(struct stat)''
    rc    <- _casm_ ``%r = fstat(%0,(struct stat *)%1);'' fd bytes
    if rc == (0::Int)
       then do
	    stat <- freeze bytes
	    return stat
       else syserr "getFdStatus"

fileAccess :: FilePath -> Bool -> Bool -> Bool -> IO Bool
fileAccess name read write exec = do
    rc   <- _ccall_ access (packString name) flags
    return (rc == (0::Int))
  where
    flags  = I# (word2Int# (read# `or#` write# `or#` exec#))
    read#  = case (if read  then ``R_OK'' else ``0'') of { W# x -> x }
    write# = case (if write then ``W_OK'' else ``0'') of { W# x -> x }
    exec#  = case (if exec  then ``X_OK'' else ``0'') of { W# x -> x }

fileExist :: FilePath -> IO Bool
fileExist name = do
    rc   <- _ccall_ access (packString name) (``F_OK''::Int)
    return (rc == (0::Int))

setFileMode :: FilePath -> FileMode -> IO ()
setFileMode name mode = do
    rc   <- _ccall_ chmod (packString name) mode
    if rc == (0::Int)
       then return ()
       else syserr "setFileMode"

setOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO ()
setOwnerAndGroup name uid gid = do
    rc   <- _ccall_ chown (packString name) uid gid
    if rc == (0::Int)
       then return ()
       else syserr "setOwnerAndGroup"

setFileTimes :: FilePath -> EpochTime -> EpochTime -> IO ()
setFileTimes name atime mtime = do
    rc   <- _casm_ ``do {struct utimbuf ub; ub.actime = (time_t) %0;
		         ub.modtime = (time_t) %1;
		         %r = utime(%2, &ub);} while(0);'' atime mtime 
							  (packString name)
    if rc == (0::Int)
       then return ()
       else syserr "setFileTimes"

{- Set access and modification time to current time -}
touchFile :: FilePath -> IO ()
touchFile name = do
    rc   <- _ccall_ utime (packString name) nullAddr
    if rc == (0::Int)
       then return ()
       else syserr "touchFile"

data PathVar = LinkLimit                     {- _PC_LINK_MAX         -}
             | InputLineLimit                {- _PC_MAX_CANON        -}
             | InputQueueLimit               {- _PC_MAX_INPUT        -}
	     | FileNameLimit                 {- _PC_NAME_MAX         -}
             | PathNameLimit                 {- _PC_PATH_MAX         -}
	     | PipeBufferLimit               {- _PC_PIPE_BUF         -}
             | SetOwnerAndGroupIsRestricted  {- _PC_CHOWN_RESTRICTED -}
	     | FileNamesAreNotTruncated      {- _PC_NO_TRUNC         -}

getPathVar :: PathVar -> FilePath -> IO Limit
getPathVar v name =
   (case v of
      LinkLimit       -> pathconf ``_PC_LINK_MAX''
      InputLineLimit  -> pathconf ``_PC_MAX_CANON''
      InputQueueLimit -> pathconf ``_PC_MAX_INPUT''
      FileNameLimit   -> pathconf ``_PC_NAME_MAX''
      PathNameLimit   -> pathconf ``_PC_PATH_MAX''
      PipeBufferLimit -> pathconf ``_PC_PIPE_BUF''
      SetOwnerAndGroupIsRestricted -> pathconf ``_PC_CHOWN_RESTRICTED''
      FileNamesAreNotTruncated     -> pathconf ``_PC_NO_TRUNC'') name

pathconf :: Int -> FilePath -> IO Limit
pathconf n name = do
  rc   <- _ccall_ pathconf (packString name) n
  if rc /= ((-1)::Int)
     then return rc
     else do
	  errno <-  getErrorCode
	  if errno == invalidArgument
	     then ioError (IOError Nothing NoSuchThing "getPathVar" "no such path limit or option")
	     else syserr "PosixFiles.getPathVar"


getFileVar :: PathVar -> Fd -> IO Limit
getFileVar v fd =
    (case v of
      LinkLimit       -> fpathconf (``_PC_LINK_MAX''::Int)
      InputLineLimit  -> fpathconf (``_PC_MAX_CANON''::Int)
      InputQueueLimit -> fpathconf ``_PC_MAX_INPUT''
      FileNameLimit   -> fpathconf ``_PC_NAME_MAX''
      PathNameLimit   -> fpathconf ``_PC_PATH_MAX''
      PipeBufferLimit -> fpathconf ``_PC_PIPE_BUF''
      SetOwnerAndGroupIsRestricted -> fpathconf ``_PC_CHOWN_RESTRICTED''
      FileNamesAreNotTruncated -> fpathconf ``_PC_NO_TRUNC'') fd

fpathconf :: Int -> Fd -> IO Limit
fpathconf n fd = do
 rc <- _ccall_ fpathconf fd n
 if rc /= ((-1)::Int)
    then return rc
    else do
	 errno <-  getErrorCode
	 if errno == invalidArgument
	    then ioError (IOError Nothing NoSuchThing "getFileVar" "no such path limit or option")
	    else syserr "getFileVar"

\end{code}
