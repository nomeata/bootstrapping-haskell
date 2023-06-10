#
# Makefile for hslibs subdir
#
TOP=..
include $(TOP)/mk/boilerplate.mk

# Module names are different from filenames in some of these files, so
# define the module names explicitly so that we install the right bits.
HS_MODULES = \
 Adams_Bashforth_Approx \
 Adams_Predictor_Corrector_Approx \
 Choleski_Factorization \
 Crout_Reduction \
 Cubic_Spline \
 Fixed_Point_Approx \
 Gauss_Seidel_Iteration \
 Hermite_Interpolation \
 Horner \
 Jacobi_Iteration \
 LLDecompMethod\
 Least_Squares_Fit \
 Matrix_Ops \
 Neville_Iterated_Interpolation \
 Newton_Cotes \
 Newton_Interpolatory_Divided_Difference \
 Newton_Raphson_Approx \
 Runge_Kutta_Approx \
 SOR_Iteration \
 Secant_Approx \
 SetMap \
 Steffensen_Approx \
 Taylor_Approx \
 Vector_Ops

HS_SRCS= $(wildcard src/*.hs)
LIBRARY=libHScontrib$(_way).a
LIBOBJS = $(HS_OBJS)
HS_IFACES = $(foreach mod, $(HS_MODULES), src/$(mod).$(way_)hi)

SRC_HC_OPTS += -recomp -isrc -cpp -fvia-C -fglasgow-exts -dcore-lint -Rghc-timing

src/SetMap_HC_OPTS=-H10m
src/Cubic_Spline_HC_OPTS=-H10m

#-----------------------------------------------------------------------------
# 	Installation; need to install .hi files as well as libraries
#
# The interface files are put inside the $(libdir), since they
# might (potentially) be platform specific..
#
# override is used here because for binary distributions, datadir is
# set on the command line. sigh.
#
override datadir:=$(libdir)/hslibs/contrib/imports

#
# Files to install from here
# 
INSTALL_LIBS  += $(LIBRARY)
INSTALL_DATAS += $(HS_IFACES)

include $(TOP)/mk/target.mk
