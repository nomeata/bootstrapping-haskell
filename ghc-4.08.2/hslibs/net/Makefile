# -----------------------------------------------------------------------------=
# $Id: Makefile,v 1.6 2000/03/30 14:17:36 rrt Exp $
#
# (c) The GHC Team 1997-1999
#

TOP = ..
include $(TOP)/mk/boilerplate.mk

HSLIB = net
HSLIB_DEPS = lang text
HSLIB_HAS_CBITS = YES

# basically nothing for hugs, until we convert these libraries to use the FFI
NOT_FOR_HUGS = BSD.lhs Socket.lhs SocketPrim.lhs URI.hs

SRC_HC_OPTS += -recomp -cpp -fglasgow-exts -fvia-C -Rghc-timing -I$(GHC_IO_INCLUDE_DIR) -I$(GHC_INCLUDE_DIR)
SRC_BLD_DLL_OPTS += -lwsock32

BSD_HC_OPTS          += -optc-DNON_POSIX_SOURCE
Socket_HC_OPTS       += -optc-DNON_POSIX_SOURCE
SocketPrim_HC_OPTS   += -optc-DNON_POSIX_SOURCE

include $(TOP)/mk/target.mk
