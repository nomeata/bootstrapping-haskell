/* -----------------------------------------------------------------------------
 * $Id: stgio.h,v 1.19.2.4 2000/08/07 16:00:59 rrt Exp $
 *
 * (c) The GRASP/AQUA Project, Glasgow University, 1994-1999
 *
 * Helper code for GHC's IO subsystem.
 *
 * ---------------------------------------------------------------------------*/

#ifndef STGIO_H
#define STGIO_H

#include "StgDLL.h"  /* for DLL_IMPORT_STDLIB */

#include "stgerror.h"
#include "fileObject.h"

/* Function prototypes for the I/O subsytem...
 */

/* closeFile.c */
StgAddr allocMemory__ (StgInt);

/* closeFile.c */
StgInt closeFile (StgForeignPtr,StgInt);

/* createDirectory.c */
StgInt createDirectory (StgByteArray);

/* directoryAux.c */
StgAddr openDir__         (StgByteArray);
StgAddr readDir__         (StgAddr);
StgAddr get_dirent_d_name (StgAddr);
StgWord get_stat_st_mode  (StgAddr);
StgInt64 get_stat_st_mtime(StgAddr);
void     set_stat_st_mtime(StgByteArray, StgByteArray);
StgInt  sizeof_stat       (void);
StgInt  prim_stat         (StgAddr,StgAddr);
StgInt  const_F_OK        (void);
StgWord const_S_IRUSR 	  (void);
StgWord const_S_IWUSR 	  (void);
StgWord const_S_IXUSR 	  (void);
StgInt  prim_S_ISDIR  	  (StgWord);
StgInt  prim_S_ISREG  	  (StgWord);

/* echoAux.c */
StgInt setTerminalEcho (StgForeignPtr, StgInt);
StgInt getTerminalEcho (StgForeignPtr);
StgInt isTerminalDevice (StgForeignPtr);

/* env.c */
char *	strDup		(const char *);
int	setenviron	(char **);
int	copyenv		(void);
int	_setenv		(char *);
int	delenv		(char *);

/* errno.c */
DLL_IMPORT_STDLIB extern	int ghc_errno;
DLL_IMPORT_STDLIB extern	int ghc_errtype;
DLL_IMPORT_STDLIB extern	char* ghc_errstr;

void	cvtErrno(void);
void	stdErrno(void);
void    convertErrno(void);
StgAddr getErrStr__(void);
StgInt  getErrNo__(void);
StgInt  getErrType__(void);

/* execvpe.c */
int	execvpe (char *, char **, char **);

/* fileEOF.c */
StgInt	fileEOF (StgForeignPtr);
/* fileGetc.c */
StgInt	fileGetc (StgForeignPtr);

/* fileLookAhead.c */
StgInt	fileLookAhead (StgForeignPtr);
StgInt	ungetChar (StgForeignPtr,StgChar);

/* fileObject.c */
void    setBufFlags (StgForeignPtr, StgInt);
void    setBufWPtr  (StgForeignPtr, StgInt);
StgInt  getBufWPtr  (StgForeignPtr);
void    setBuf      (StgForeignPtr, StgAddr, StgInt);
StgAddr getBuf      (StgForeignPtr);
StgAddr getWriteableBuf (StgForeignPtr);
StgAddr getBufStart (StgForeignPtr,StgInt);
StgInt  getBufSize  (StgForeignPtr);
void    setFilePtr  (StgForeignPtr, StgAddr);
StgAddr getFilePtr  (StgForeignPtr);
void    setConnectedTo  (StgForeignPtr, StgForeignPtr, StgInt);
void    setPushbackBufSize (StgInt);
StgInt  getPushbackBufSize (void);
void    setNonBlockingIOFlag__ (StgForeignPtr);
void    clearNonBlockingIOFlag__ (StgForeignPtr);
void    setConnNonBlockingIOFlag__ (StgForeignPtr);
void    clearConnNonBlockingIOFlag__ (StgForeignPtr);
StgInt  getFileFd  (StgForeignPtr);
StgInt  getConnFileFd  (StgForeignPtr);
StgInt  fill_up_line_buffer(IOFileObject*);

/* filePosn.c */
StgInt	getFilePosn (StgForeignPtr);
StgInt	setFilePosn (StgForeignPtr, StgInt, StgByteArray);

/* filePutc.c */
StgInt	filePutc    (StgForeignPtr, StgChar);

/* fileSize.c */
StgInt	fileSize    (StgForeignPtr, StgByteArray);
StgInt	fileSize_int64 (StgForeignPtr, StgByteArray);

/* flushFile.c */
StgInt	flushFile   (StgForeignPtr);
StgInt	flushBuffer (StgForeignPtr);
StgInt	flushReadBuffer (StgForeignPtr);
void	flushConnectedBuf (StgForeignPtr);

/* freeFile.c */
void freeStdFile (StgAddr);
void freeStdFileObject (StgAddr);
void freeFileObject (StgAddr);

StgAddr ref_freeStdFileObject (void);
StgAddr ref_freeFileObject    (void);

/* getBufferMode.c */
StgInt	getBufferMode (StgForeignPtr);

/* getClockTime.c */
StgInt	getClockTime (StgByteArray, StgByteArray);
StgInt  prim_getClockTime(StgByteArray, StgByteArray);

/* getCPUTime.c */
StgInt getCPUTime (StgByteArray);
StgInt clockTicks(void);

/* getCurrentDirectory.c */
StgAddr getCurrentDirectory(void);

/* getDirectoryContents.c */
StgAddr getDirectoryContents (StgByteArray);

/* getLock.c */
int     lockFile    (int, int, int);
int     unlockFile  (int);
StgInt	getLock	    (StgInt, StgInt);

/* inputReady.c */
StgInt	inputReady  (StgForeignPtr, StgInt);

/* openFile.c */
IOFileObject* openFile    (StgByteArray, StgInt, StgInt);
IOFileObject* openFd      (StgInt, StgInt, StgInt);
IOFileObject* openStdFile (StgInt, StgInt);

/* progargs.c */
StgAddr get_prog_argv(void);
StgInt  get_prog_argc(void);

/* readFile.c */
StgInt	readBlock (StgForeignPtr);
StgInt	readChunk (StgForeignPtr,StgAddr,StgInt,StgInt);
StgInt	readLine  (StgForeignPtr);
StgInt	readChar  (StgForeignPtr);

/* removeDirectory.c */
StgInt removeDirectory (StgByteArray);

/* removeFile.c */
StgInt removeFile (StgByteArray);

/* renameDirectory.c */
StgInt renameDirectory (StgByteArray, StgByteArray);

/* renameFile.c */
StgInt renameFile (StgByteArray, StgByteArray);

/* seekFile.c */
StgInt	seekFile  (StgForeignPtr, StgInt, StgInt, StgByteArray);
StgInt	seekFile_int64 (StgForeignPtr, StgInt, StgInt64);
StgInt	seekFileP (StgForeignPtr);

/* setBinaryMode.c */
StgInt	setBinaryMode__ (StgForeignPtr, StgInt);

/* setBuffering.c */
StgInt	setBuffering (StgForeignPtr, StgInt);
StgInt  const_BUFSIZ (void);

/* setCurrentDirectory.c */
StgInt setCurrentDirectory (StgByteArray);

/* showTime.c */
StgInt showTime (StgInt, StgByteArray, StgInt, StgByteArray);

/* system.c */
StgInt	systemCmd (StgByteArray);

/* timezone.c */
StgInt  get_tm_sec       ( StgAddr );
StgInt  get_tm_min       ( StgAddr );
StgInt  get_tm_hour      ( StgAddr );
StgInt  get_tm_mday      ( StgAddr );
StgInt  get_tm_mon       ( StgAddr );
StgInt  get_tm_year      ( StgAddr );
StgInt  get_tm_wday      ( StgAddr );
StgInt  get_tm_yday      ( StgAddr );
StgInt  get_tm_isdst     ( StgAddr );
StgAddr prim_ZONE        ( StgAddr );
StgInt  prim_GMTOFF      ( StgAddr );
void    prim_SETZONE     ( StgAddr, StgAddr );
StgInt  sizeof_word      ( void ); 
StgInt  sizeof_struct_tm ( void );
StgInt  sizeof_time_t    ( void );
char*   get_ZONE         ( StgAddr );

/* toLocalTime.c */
StgInt toLocalTime (StgInt, StgByteArray, StgByteArray);
StgInt prim_toLocalTime ( StgInt64,StgByteArray );

/* toUTCTime.c */
StgInt toUTCTime (StgInt, StgByteArray, StgByteArray);
StgInt prim_toUTCTime ( StgInt64,StgByteArray );

/* toClockSec.c */
StgInt toClockSec (StgInt, StgInt, StgInt, StgInt, StgInt, StgInt, StgInt, StgInt, StgByteArray);

/* writeError.c */
StgAddr addrOf_ErrorHdrHook(void);
void    writeErrString__ (StgAddr, StgByteArray, StgInt);

/* writeFile.c */
StgInt	writeBuf  (StgForeignPtr, StgAddr, StgInt, StgInt);
StgInt	writeBufBA  (StgForeignPtr, StgByteArray, StgInt, StgInt);
StgInt	writeFileObject (StgForeignPtr, StgInt);
StgInt	writeBuffer (StgForeignPtr, StgInt);
StgInt  write_ (StgForeignPtr ptr, StgAddr buf, StgInt len);

#endif /* ! STGIO_H */


