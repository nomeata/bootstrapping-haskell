# -----------------------------------------------------------------------------=
# $Id: Makefile,v 1.4 2000/04/02 21:03:45 andy Exp $
#
# (c) The GHC Team 1997-1999
#

TOP = ..
include $(TOP)/mk/boilerplate.mk

HSLIB = data
HSLIB_DEPS = lang util

SRCS_FROM_SUBDIRS = edison edison/Assoc edison/Coll edison/Seq

SRC_HC_OPTS += -recomp -cpp -fglasgow-exts -Rghc-timing -I$(GHC_INCLUDE_DIR)

SRC_HC_OPTS += -fallow-undecidable-instances -funbox-strict-fields

include $(TOP)/mk/target.mk
