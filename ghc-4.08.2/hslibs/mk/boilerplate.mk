# ##############################################################################
# $Id: boilerplate.mk,v 1.6 2000/03/19 18:26:50 rrt Exp $
#
# Hslibs boilerplate.mk
# (c) The GHC Team 1999
# #############################################################################


# Begin by slurping in the boilerplate from one level up.
# Remember, TOP is the top level of the innermost level
# (FPTOOLS_TOP is the fptools top)

# We need to set TOP to be the TOP that the next level up expects!
# The TOP variable is reset after the inclusion of the fptools
# boilerplate, so we stash TOP away first:
HSLIBS_TOP := $(TOP)
TOP:=$(TOP)/..

include $(TOP)/mk/boilerplate.mk

# Reset TOP
TOP:=$(HSLIBS_TOP)

# -----------------------------------------------------------------
# Everything after this point
# augments or overrides previously set variables.
# -----------------------------------------------------------------

-include $(TOP)/mk/paths.mk
-include $(TOP)/mk/opts.mk
-include $(TOP)/mk/version.mk
-include $(TOP)/mk/suffix.mk

# Urk! ToDo.
GHC_INCLUDE_DIR=$(FPTOOLS_TOP)/ghc/includes
GHC_LIB_DIR=$(FPTOOLS_TOP)/ghc/lib
GHC_IO_INCLUDE_DIR=$(GHC_LIB_DIR)/std/cbits
GHC_RUNTIME_DIR=$(FPTOOLS_TOP)/ghc/rts