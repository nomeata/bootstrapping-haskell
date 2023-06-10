# -----------------------------------------------------------------------------=
# $Id: Makefile,v 1.2 1999/12/07 16:04:14 simonmar Exp $
#
# (c) The GHC Team 1997-1999
#

TOP = ..
include $(TOP)/mk/boilerplate.mk

HSLIB = concurrent
HSLIB_DEPS = lang

# until Hugs's concurrency support is up to scratch...
NOT_FOR_HUGS = Channel.lhs Concurrent.lhs Merge.lhs ChannelVar.lhs Parallel.lhs \
	       SampleVar.lhs Semaphore.lhs

SRC_HC_OPTS += -recomp -cpp -fglasgow-exts -fvia-C -Rghc-timing -I$(GHC_INCLUDE_DIR)

include $(TOP)/mk/target.mk

