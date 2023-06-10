# -----------------------------------------------------------------------------=
# $Id: Makefile,v 1.15 2000/03/28 11:19:03 rrt Exp $
#
# (c) The GHC Team 1997-1999
#

TOP=..
include $(TOP)/mk/boilerplate.mk

HSLIB = posix
HSLIB_DEPS = lang
HSLIB_HAS_CBITS = YES

# basically nothing for hugs, until we convert these libraries to use the FFI
NOT_FOR_HUGS = Posix.lhs PosixDB.lhs PosixErr.lhs PosixFiles.lhs PosixIO.lhs \
	       PosixProcEnv.lhs PosixTTY.lhs PosixUtil.lhs

SRC_HC_OPTS += -recomp -cpp -fglasgow-exts -fvia-C -Rghc-timing -I$(GHC_IO_INCLUDE_DIR) -I$(GHC_INCLUDE_DIR)

PosixUtil_HC_OPTS     ='-\#include"cbits/libposix.h"' -monly-3-regs
PosixDB_HC_OPTS       ='-\#include"cbits/libposix.h"'
PosixErr_HC_OPTS      ='-\#include"cbits/libposix.h"'
PosixFiles_HC_OPTS    ='-\#include"cbits/libposix.h"'
PosixIO_HC_OPTS       ='-\#include"cbits/libposix.h"'
PosixProcEnv_HC_OPTS  ='-\#include"cbits/libposix.h"'
PosixProcPrim_HC_OPTS ='-\#include"cbits/libposix.h"'
PosixTTY_HC_OPTS      ='-\#include"cbits/libposix.h"' -monly-2-regs
Posix_HC_OPTS         ='-\#include"cbits/libposix.h"'

include $(TOP)/mk/target.mk

