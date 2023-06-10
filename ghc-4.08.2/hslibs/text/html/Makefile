# -----------------------------------------------------------------------------=
# $Id: Makefile,v 1.2 2000/05/12 11:48:31 rrt Exp $
# (c) The GHC Team 1997-1999

TOP = ..
include $(TOP)/mk/boilerplate.mk

WAYS=$(GhcLibWays)

#-----------------------------------------------------------------------------
# 	Setting the standard variables
#

LIBRARY = libHShtml$(_way).a
LIBOBJS = $(HS_OBJS)

#-----------------------------------------------------------------------------
# 	Setting the GHC compile options

SRC_HC_OPTS += -recomp -cpp -fglasgow-exts -fvia-C -Rghc-timing $(GhcLibHcOpts)

ifneq "$(way)" "dll"
SRC_HC_OPTS += -static
endif

#
# Profiling options
WAY_p_HC_OPTS += -GPrelude
WAY_mr_HC_OPTS += -GPrelude

#
# Object and interface files have suffixes tagged with their ways
#
ifneq "$(way)" ""
SRC_HC_OPTS += -hisuf $(way_)hi
endif

#-----------------------------------------------------------------------------
# 	Dependency generation

SRC_MKDEPENDHS_OPTS += -optdep-w $(MAGIC_HSCPP_OPTS)

#-----------------------------------------------------------------------------
# 	Win32 DLL setup

DLL_NAME = HShtml.dll
DLL_IMPLIB_NAME = libHShtml_imp.a
SRC_BLD_DLL_OPTS += --export-all --output-def=HShtml.def DllVersionInfo.o
SRC_BLD_DLL_OPTS += -lwinmm -lHSrts_imp -lHScbits_imp -lHS_imp -lgmp -L. -L../../rts/gmp -L../../rts -L../std -L../std/cbits

ifeq "$(way)" "dll"
all :: DllVersionInfo.o
endif

#-----------------------------------------------------------------------------
# 	Installation; need to install .hi files as well as libraries
#
# The interface files are put inside the $(libdir), since they
# might (potentially) be platform specific..
#
# override is used here because for binary distributions, datadir is
# set on the command line. sigh.
#
override datadir:=$(libdir)/imports/html

#
# Files to install from here
# 
INSTALL_LIBS  += $(LIBRARY)
INSTALL_DATAS += $(HS_IFACES)

ifeq "$(EnableWin32DLLs)" "YES"
INSTALL_PROGS += $(DLL_NAME)
ifneq "$(way)" "dll"
INSTALL_LIBS += $(patsubst %.a, %_imp.a, $(LIBRARY))
endif
endif

include $(TOP)/mk/target.mk
