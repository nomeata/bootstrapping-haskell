#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.10 2000/01/31 15:12:07 simonmar Exp $
#

TOP=.
include $(TOP)/mk/boilerplate.mk

#
# subdir dependencies:
#	everything needs utils
#	includes/ needs driver (to easily c.pile mkNativeGen.c)
#	make depend except in {utils,driver} needs includes	
#	RTS and compiler need includes
#

#
# Order is important! driver/ has to come before includes/ which
# again has to come before the rest.
#
# If we're booting from .hc files, swap the order
# we descend into compiler/ and lib/
#
ifeq "$(GhcWithHscBuiltViaC)" "NO"
SUBDIRS = utils driver includes rts docs compiler lib
else
SUBDIRS = utils driver includes rts docs lib compiler
endif

# Easier to copy
ifeq "$(BIN_DIST)" "1"
SUBDIRS := $(filter-out docs,$(SUBDIRS))
endif

ifeq ($(IncludeTestDirsInBuild),YES)
SUBDIRS += tests
endif

SRC_DIST_FILES += configure

include $(TOP)/mk/target.mk
