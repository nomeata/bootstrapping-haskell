# -----------------------------------------------------------------------------
# $Id: target.mk,v 1.16.2.1 2000/09/04 16:31:31 rrt Exp $
#
# hslibs/target.mk
#

# We need to set TOP to be the TOP that the next level up expects!
# The TOP variable is reset after the inclusion of the fptools
# boilerplate, so we stash TOP away first:
HSLIBS_TOP := $(TOP)
TOP:=$(TOP)/..

# -----------------------------------------------------------------------------
# Makefiles in the subdirectories should set the following variables:
# 
#   HSLIB		the (short) name of the top-level library category 
#			we're building.  eg. lang, net, text etc.
#
#   HSLIB_DEPS  	other library categories that this category depends on.
#
#   HSLIB_HAS_CBITS     YES if there is a cbits subdirectory
#
#   SRCS_FROM_SUBDIRS	pull out *.hs and *.lhs sources from these subdirectories
#			under the current directory.
#
#   NOT_FOR_GHC		exclude source files from certain compilers.
#   NOT_FOR_NHC
#   NOT_FOR_HBC
#   NOT_FOR_HUGS

# -----------------------------------------------------------------------------

# If we have a cbits subdir, add it to SUBDIRS

ifeq "$(HSLIB_HAS_CBITS)" "YES"
SUBDIRS += cbits
endif

# Filter source file list depending on which compiler we're using

ifeq "$(HsLibsFor)" "ghc"
SRCS := $(filter-out $(NOT_FOR_GHC), $(SRCS))
HC = $(GHC_INPLACE)
MKDEPENDHS = $(HC)
endif

ifeq "$(HsLibsFor)" "hugs"
SRCS := $(filter-out $(NOT_FOR_HUGS), $(SRCS))
MKDEPENDHS = $(GHC)
endif

ifeq "$(HsLibsFor)" "nhc"
SRCS := $(filter-out $(NOT_FOR_NHC), $(SRCS))
HC = $(NHC)
MKDEPENDHS = $(GHC)
endif

ifeq "$(HsLibsFor)" "hbc"
SRCS := $(filter-out $(NOT_FOR_HBC), $(SRCS))
HC = $(HBC)
MKDEPENDHS = $(GHC)
endif

# -----------------------------------------------------------------------------
# Building libraries for GHC

ifeq "$(HsLibsFor)" "ghc"

ifeq "$(HSLIB)" ""

# No library, we are actually building the tools
SRC_HC_OPTS += $(GhcLibToolsHcOpts)

else

LIBRARY = libHS$(HSLIB)$(_way).a
LIBOBJS = $(HS_OBJS)

WAYS=$(GhcLibWays)

SRCS += $(wildcard $(patsubst %, %/*hs, $(SRCS_FROM_SUBDIRS)))
SRC_HC_OPTS += $(patsubst %, -i%, $(SRCS_FROM_SUBDIRS))

MAGIC_HSCPP_OPTS=-DBEGIN_FOR_GHC='-}' -DEND_FOR_GHC='{-' -DBEGIN_FOR_HUGS='{-' -DEND_FOR_HUGS='-}'
SRC_MKDEPENDHS_OPTS += -optdep-w $(MAGIC_HSCPP_OPTS)

ifneq "$(way)" "dll"
SRC_HC_OPTS += -static

# Filter out cbits from SUBDIRS for ways other than the default and dll
ifneq "$(way)" ""
SUBDIRS := $(filter-out cbits, $(SUBDIRS))
endif

endif

#
# Object and interface files have suffixes tagged with their ways
#
ifneq "$(way)" ""
SRC_HC_OPTS += -hisuf $(way_)hi
endif

SRC_HC_OPTS += $(GhcLibHcOpts)
ifeq "$(StripObjs)" "YES"
SRC_HC_OPTS += -strip-objs
endif

DLL_NAME = HS$(HSLIB).dll
DLL_IMPLIB_NAME = libHS$(HSLIB)_imp.a
SRC_BLD_DLL_OPTS += --export-all --output-def=HS$(HSLIB).def DllVersionInfo.o
SRC_BLD_DLL_OPTS += -lwinmm -lHSrts_imp -lHSstd_cbits_imp -lHSstd_imp -lgmp -L. -L$(GHC_RUNTIME_DIR)/gmp -L$(GHC_RUNTIME_DIR) -L$(GHC_LIB_DIR)/std -L$(GHC_LIB_DIR)/std/cbits
SRC_BLD_DLL_OPTS += $(patsubst %,-lHS%_imp, $(HSLIB_DEPS))
SRC_BLD_DLL_OPTS += $(patsubst %,-L../%, $(HSLIB_DEPS))
ifeq "$(HSLIB_HAS_CBITS)" "YES"
SRC_BLD_DLL_OPTS += -lHS$(HSLIB)_cbits_imp -Lcbits
endif

ifeq "$(way)" "dll"
all :: DllVersionInfo.o
SplitObjs = NO 
endif

# add syslib dependencies and current package name
SRC_HC_OPTS += $(patsubst %, -package %, $(HSLIB_DEPS)) -package-name $(HSLIB)

# Installation; need to install .hi files as well as libraries
#
# The interface files are put inside the $(libdir), since they
# might (potentially) be platform specific..
#
# override is used here because for binary distributions, datadir is
# set on the command line. sigh.
#

override datadir:=$(libdir)/imports/$(HSLIB)
INSTALL_LIBS  += $(LIBRARY)
INSTALL_DATAS += $(HS_IFACES)

ifeq "$(EnableWin32DLLs)" "YES"
INSTALL_PROGS += $(DLL_NAME)
ifneq "$(way)" "dll"
INSTALL_LIBS  += $(patsubst %.a, %_imp.a, $(LIBRARY))
endif
endif

endif # HSLIB /= ""

ifeq "$(GhcWithHscBuiltViaC)" "YES"

# When booting from .hc files, remove the suffix rule for 
# .l?hs -> .o, so that the .hc -> .o is used instead.
# Also disable the generation of the .hc files, even if
# the .l?hs files are newer than the .hc ones.
%.$(way_)o  : %.lhs
%.$(way_)o  : %.hs
%.$(way_)hc : %.lhs
%.$(way_)hc : %.hs

# There's no need to compute dependencies when booting from .hc files
#
MKDEPENDHS_SRCS =

endif # GhcWithHscBuiltViaC == YES

endif # HsLibsFor = ghc

# -----------------------------------------------------------------------------
# Building libraries for Hugs

# we CPP all the haskell sources and pop them in a mirror hierarchy
# under hugs-srcs/.

ifeq "$(HsLibsFor)" "hugs"

HUGS_SRC_DIR = hugs-srcs
HUGS_SRC_DIRS = $(HUGS_SRC_DIR) $(patsubst %, $(HUGS_SRC_DIR)/%, $(SRCS_FROM_SUBDIRS))

SRCS += $(wildcard $(patsubst %, %/*hs, $(SRCS_FROM_SUBDIRS)))
HUGS_SRCS = $(patsubst %, $(HUGS_SRC_DIR)/%, $(SRCS))

all :: $(HUGS_SRC_DIRS) $(HUGS_SRCS)

$(HUGS_SRC_DIRS) ::
	$(MKDIRHIER) $@

HUGS_DEFINES = -D__HUGS__

$(HUGS_SRC_DIR)/%.hs : %.hs
	$(RAWCPP) $(HUGS_DEFINES) -o $(HUGS_SRC_DIR)/$*.hs - < $<

$(HUGS_SRC_DIR)/%.lhs : %.lhs
	$(RAWCPP) $(HUGS_DEFINES) -o $(HUGS_SRC_DIR)/$*.lhs - < $<

INSTALL_DATAS = $(HUGS_SRCS)

endif

# -----------------------------------------------------------------------------

include $(TOP)/mk/target.mk

# Reset TOP
TOP:=$(HSLIBS_TOP)
