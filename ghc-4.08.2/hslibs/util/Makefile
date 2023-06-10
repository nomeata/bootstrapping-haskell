# -----------------------------------------------------------------------------
# $Id: Makefile,v 1.11.2.1 2001/01/10 18:02:38 rrt Exp $
#
# Makefile for miscellaneous libraries.
#

TOP = ..
include $(TOP)/mk/boilerplate.mk

HSLIB = util
HSLIB_DEPS = lang concurrent posix
HSLIB_HAS_CBITS = YES
SRCS_FROM_SUBDIRS = check

NOT_FOR_HUGS = Readline.lhs Memo.lhs Select.lhs

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
  NOT_FOR_GHC += Select.lhs
  HSLIB_DEPS := $(filter-out posix, $(HSLIB_DEPS))
endif

# Remove Readline.lhs if it is not wanted or headers are not available or on mingwin.
ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
  ifeq "$(GhcLibsWithReadline)" "YES"
    ifneq "$(ReadlineIncludePath)" ""
      SRC_HC_OPTS += -I$(ReadlineIncludePath)
    endif
    STUBOBJS += Readline_stub.$(way_)o
    CLEAN_FILES += Readline_stub.[ch] $(STUBOBJS)
  else
    NOT_FOR_GHC += Readline.lhs
  endif
else
  NOT_FOR_GHC += Readline.lhs
endif

CC = $(HC)
SRC_CC_OPTS += -I$(GHC_INCLUDE_DIR) -I$(GHC_IO_INCLUDE_DIR)
SRC_HC_OPTS += -recomp -cpp -fglasgow-exts -fvia-C

include $(TOP)/mk/target.mk

