# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.6 2000/01/05 14:10:25 simonmar Exp $
#
# (c) The GHC Team, 1999
#

TOP=../..
include $(TOP)/mk/boilerplate.mk

CC = $(HC)

SRC_CC_OPTS += -I$(GHC_INCLUDE_DIR) -I$(GHC_IO_INCLUDE_DIR) -Wall

LIBRARY=libHSposix_cbits.a
LIBOBJS=$(C_OBJS)
INSTALL_LIBS += $(LIBRARY)

include $(TOP)/mk/target.mk
