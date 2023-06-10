# -----------------------------------------------------------------------------=
# $Id: Makefile,v 1.7 2000/05/12 14:59:16 panne Exp $
#
# (c) The GHC Team 1997-1999
#

TOP = ..
include $(TOP)/mk/boilerplate.mk

HSLIB = lang
HSLIB_DEPS =
HSLIB_HAS_CBITS = YES

SRCS_FROM_SUBDIRS = monads

SRC_HC_OPTS += -recomp -cpp -fglasgow-exts -fvia-C -Rghc-timing -I$(GHC_INCLUDE_DIR)

Int_HC_OPTS          += -H20m -monly-3-regs
Word_HC_OPTS         += -H20m -monly-3-regs
IOExts_HC_OPTS	     += -I$(GHC_IO_INCLUDE_DIR)
Dynamic_HC_OPTS	     += $(MAGIC_HSCPP_OPTS)

include $(TOP)/mk/target.mk
