#-----------------------------------------------------------------------------
# $Id: Makefile,v 1.4 1997/03/19 20:44:55 simonpj Exp $

TOP=../..
include $(TOP)/mk/boilerplate.mk
# There's only one `way' for gmp, empty WAYS variable right away.
WAYS=

GMP_OBJS = $(MPZ_OBJS) $(MPQ_OBJS) $(MPN_OBJS) $(IMPL_OBJS) mp_bases.o

IMPL_SRCS = memory.c mp_set_fns.c _mpz_set_str.c _mpz_get_str.c \
  mpz_realloc.c mp_clz_tab.c alloca.c
IMPL_OBJS = memory.o mp_set_fns.o _mpz_set_str.o _mpz_get_str.o \
  mpz_realloc.o mp_clz_tab.o alloca.o

MPZ_SRCS = mpz_init.c mpz_set.c mpz_set_ui.c mpz_set_si.c mpz_set_str.c \
  mpz_iset.c mpz_iset_ui.c mpz_iset_si.c mpz_iset_str.c mpz_clear.c \
  mpz_get_ui.c mpz_get_si.c mpz_get_str.c mpz_size.c mpz_sizeinb.c \
  mpz_add.c mpz_add_ui.c mpz_sub.c mpz_sub_ui.c mpz_mul.c mpz_mul_ui.c \
  mpz_div.c mpz_div_ui.c mpz_mod.c mpz_mod_ui.c mpz_dm.c mpz_dm_ui.c \
  mpz_mdiv.c mpz_mmod.c mpz_mdm.c mpz_mdiv_ui.c mpz_mmod_ui.c mpz_mdm_ui.c \
  mpz_gcd.c mpz_gcdext.c mpz_sqrt.c mpz_sqrtrem.c mpz_powm.c mpz_powm_ui.c \
  mpz_cmp.c mpz_cmp_ui.c mpz_cmp_si.c mpz_mul_2exp.c mpz_div_2exp.c \
  mpz_mod_2exp.c mpz_abs.c mpz_neg.c mpz_com.c mpz_and.c mpz_ior.c \
  mpz_inp_raw.c mpz_inp_str.c mpz_out_raw.c mpz_out_str.c \
  mpz_perfsqr.c mpz_random.c mpz_random2.c mpz_pow_ui.c \
  mpz_clrbit.c mpz_fac_ui.c mpz_pprime_p.c
MPZ_OBJS = mpz_init.o mpz_set.o mpz_set_ui.o mpz_set_si.o mpz_set_str.o \
  mpz_iset.o mpz_iset_ui.o mpz_iset_si.o mpz_iset_str.o mpz_clear.o \
  mpz_get_ui.o mpz_get_si.o mpz_get_str.o mpz_size.o mpz_sizeinb.o \
  mpz_add.o mpz_add_ui.o mpz_sub.o mpz_sub_ui.o mpz_mul.o mpz_mul_ui.o \
  mpz_div.o mpz_div_ui.o mpz_mod.o mpz_mod_ui.o mpz_dm.o mpz_dm_ui.o \
  mpz_mdiv.o mpz_mmod.o mpz_mdm.o mpz_mdiv_ui.o mpz_mmod_ui.o mpz_mdm_ui.o \
  mpz_gcd.o mpz_gcdext.o mpz_sqrt.o mpz_sqrtrem.o mpz_powm.o mpz_powm_ui.o \
  mpz_cmp.o mpz_cmp_ui.o mpz_cmp_si.o mpz_mul_2exp.o mpz_div_2exp.o \
  mpz_mod_2exp.o mpz_abs.o mpz_neg.o mpz_com.o mpz_and.o mpz_ior.o \
  mpz_inp_raw.o mpz_inp_str.o mpz_out_raw.o mpz_out_str.o \
  mpz_perfsqr.o mpz_random.o mpz_random2.o mpz_pow_ui.o \
  mpz_clrbit.o mpz_fac_ui.o mpz_pprime_p.o

MPQ_SRCS = mpq_init.c mpq_set.c mpq_set_ui.c mpq_set_si.c \
  mpq_set_num.c mpq_set_den.c mpq_get_num.c mpq_get_den.c \
  mpq_add.c mpq_sub.c mpq_mul.c mpq_div.c \
  mpq_clear.c mpq_cmp.c mpq_inv.c mpq_neg.c
MPQ_OBJS = mpq_init.o mpq_set.o mpq_set_ui.o mpq_set_si.o \
  mpq_set_num.o mpq_set_den.o mpq_get_num.o mpq_get_den.o \
  mpq_add.o mpq_sub.o mpq_mul.o mpq_div.o \
  mpq_clear.o mpq_cmp.o mpq_inv.o mpq_neg.o

MPN_SRCS = mpn_add.c mpn_sub.c mpn_cmp.c mpn_mul.c mpn_div.c mpn_dm_1.c \
  mpn_mod_1.c mpn_lshift.c mpn_rshift.c mpn_rshiftci.c mpn_sqrt.c
MPN_OBJS = mpn_add.o mpn_sub.o mpn_cmp.o mpn_mul.o mpn_div.o mpn_dm_1.o \
  mpn_mod_1.o mpn_lshift.o mpn_rshift.o mpn_rshiftci.o mpn_sqrt.o

#
# Compiling gmp with gcc-2.7.2 *without* -O tripped up a bug in the
# code that sets up and stores the regs around an __asm__ (mpn_mul.c : add_ssaaaa).
# Enabling -O flushed out the problem, so dropping the -O is not advised.
#
SRC_CC_OPTS += -I. -O

all depend :: gmp-mparam.h
libgmp.a :: stamp-stddefh

LIBRARY = libgmp.a
LIBOBJS = $(GMP_OBJS)
DESTDIR = $(INSTLIBDIR_GHC)
C_SRCS  = $(IMPL_SRCS) $(MPZ_SRCS) $(MPQ_SRCS) $(MPN_SRCS)

#
# Install gmp
# 
INSTALL_LIBS += $(LIBRARY)

CLEAN_FILES += test-with-stddefh.c stamp-stddefh cre-mparam gmp-mparam.h mp_bases.c cre-conv-tab

SRC_H_FILES+=$(wildcard *.h) COPYING ChangeLog INSTALL Makefile.original README TODO VERSION gmp.texi mpn_mul_classic.c-EXTRA
# ToDo: something to make the doc

# from here on, it is magic from the original Makefile

# If you cross compile on a machine with the same sizes of the integral
# types ("int", "long int", "short int", and "char") define this as the
# local compiler.  Otherwise, you need look for the uses of LOCAL_CC below,
# and handle those cases manually.

LOCAL_CC = $(CC)

stamp-stddefh:
	rm -f stddef.h
	rm -f test-stddefh.c
	(	echo '#include <stddef.h>'				;\
		echo 'main(){size_t foo=sizeof(size_t);exit(0);}'	;\
	) > test-stddefh.c
	@if $(LOCAL_CC) $(CFLAGS) test-stddefh.c -c 2> /dev/null	;\
	then true							;\
	else								\
	  echo 'This machine has no "stddef.h".  Creating a minimal in ./';\
	  $(LOCAL_CC) $(CFLAGS) cre-stddefh.c -o cre-stddefh		;\
	  ./cre-stddefh > stddef.h					;\
	fi
	rm -f test-stddefh.o
	touch stamp-stddefh

mp_bases.c: cre-conv-tab
	./cre-conv-tab > tmp-$@
	mv tmp-$@ $@
cre-conv-tab: cre-conv-tab.c gmp.h gmp-impl.h gmp-mparam.h longlong.h
	$(LOCAL_CC) $(CFLAGS) `if [ x$(firstword $^) = x ];	\
			       then echo cre-conv-tab.c;	\
			       else echo $(firstword $^); fi` -o $@ -lm

gmp-mparam.h: cre-mparam
	./cre-mparam > tmp-$@
	mv tmp-$@ $@
cre-mparam: cre-mparam.c stamp-stddefh gmp.h
	$(LOCAL_CC) $(CFLAGS) cre-mparam.c -o $@

include $(TOP)/mk/target.mk
