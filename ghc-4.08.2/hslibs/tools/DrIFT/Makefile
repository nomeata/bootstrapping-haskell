# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.5 2000/05/05 11:10:18 panne Exp $
#
# (c) The GHC Team, 2000
#

TOP = ../..
include $(TOP)/mk/boilerplate.mk

HS_PROG = DrIFT
INSTALL_PROGS += $(HS_PROG)
SRC_HC_OPTS += -cpp -fglasgow-exts -package text

CLEAN_FILES += Main.hi

include $(TOP)/mk/target.mk
