dnl $Id: aclocal.m4,v 1.22 1998/04/10 12:38:38 simonm Exp $
dnl 
dnl Extra autoconf macros for the Glasgow fptools
dnl

dnl 
dnl Are we running under the GNU libc?  Need -D_GNU_SOURCE to get 
dnl caddr_t and such.
dnl 
AC_DEFUN(AC_GNU_LIBC,
[AC_CACHE_CHECK([GNU libc], ac_cv_gnu_libc,
[AC_EGREP_CPP(yes,
[#include <features.h>
#ifdef __GLIBC__
yes
#endif
], ac_cv_gnu_libc=yes, ac_cv_gnu_libc=no)])
if test "$ac_cv_gnu_libc" = yes; then
  AC_DEFINE(_GNU_SOURCE)
fi
])

dnl
dnl Has timezone the type time_t or long (HP-UX 10.20 apparently
dnl has `long'..)
dnl 
AC_DEFUN(AC_TYPE_TIMEZONE,
[AC_CACHE_CHECK([type of timezone], ac_cv_type_timezone,
[AC_TRY_COMPILE([#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

extern time_t timezone;
],
[int i;], ac_cv_type_timezone=time_t, ac_cv_type_timezone=long)])
AC_DEFINE_UNQUOTED(TYPE_TIMEZONE, $ac_cv_type_timezone)
])

dnl *** Is altzone available? ***
dnl 
AC_DEFUN(AC_ALTZONE,
[AC_CACHE_CHECK([altzone], ac_cv_altzone,
[AC_TRY_LINK([#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif
], [return altzone;], 
ac_cv_altzone=yes, ac_cv_altzone=no)])
if test "$ac_cv_altzone" = yes; then
  AC_DEFINE(HAVE_ALTZONE)
fi
])

dnl ** check for leading underscores in symbol names
dnl 
dnl Test for determining whether symbol names have a leading
dnl underscore.
dnl 
dnl We assume that they _haven't_ if anything goes wrong.
dnl
AC_DEFUN(AC_UNDERSCORE,
[AC_CHECK_LIB(elf, nlist, LIBS="-lelf $LIBS")dnl
AC_CACHE_CHECK([leading underscore in symbol names], ac_cv_lead_uscore,

dnl
dnl Hack!: nlist() under Digital UNIX insist on there being an _,
dnl but symbol table listings show none. What is going on here?!?
dnl
changequote(<<, >>)dnl
<<
case $HostPlatform in
alpha-dec-osf*) ac_cv_lead_uscore='no';;
*) >>
changequote([, ])dnl
AC_TRY_RUN([#ifdef HAVE_NLIST_H
#include <nlist.h>
changequote(<<, >>)dnl
<<
struct nlist xYzzY[] = {{"_xYzzY", 0},{0}};
#endif

main(argc, argv)
int argc;
char **argv;
{
#ifdef HAVE_NLIST_H
    if(nlist(argv[0], xYzzY) == 0 && xYzzY[0].n_value != 0)
        exit(0);>>
changequote([, ])dnl
#endif
    exit(1);
}], ac_cv_lead_uscore=yes, ac_cv_lead_uscore=no, ac_cv_lead_uscore=NO)
;;
esac);
LeadingUnderscore=`echo $ac_cv_lead_uscore | sed 'y/yesno/YESNO/'`
AC_SUBST(LeadingUnderscore)
case $LeadingUnderscore in
YES) AC_DEFINE(LEADING_UNDERSCORE);;
esac
])

dnl
dnl Check for Happy and version.
dnl
AC_DEFUN(AC_HAPPY,
[AC_PATH_PROG(HappyCmd,happy)
AC_CACHE_CHECK([for version of happy], ac_cv_happy_version,
[if test x"$HappyCmd" != x; then
   ac_cv_happy_version="`$HappyCmd -v |
changequote(, )dnl
			  grep 'Happy Version' | sed -e 's/Happy Version \([^ ]*\).*/\1/g'`" ;
changequote([, ])dnl
else
   ac_cv_happy_version="";
fi;
if expr "$ac_cv_happy_version" "<" 1.4 > /dev/null 2>&1; then
   echo
   echo "Happy version 1.4 or later is required to compile GHC."
   exit 1;
fi;
])
HappyVersion=$ac_cv_happy_version;
AC_SUBST(HappyVersion)
])

dnl
dnl What's the best way of doing context diffs?
dnl
dnl (NB: NeXTStep thinks diff'ing a file against itself is "trouble")
dnl
AC_DEFUN(AC_PROG_DIFF,
[AC_CACHE_CHECK([for ok way to do context diffs], ac_cv_context_diffs,
[echo foo > conftest1
echo foo > conftest2
if diff -C 1 conftest1 conftest2 > /dev/null 2>&1 ; then
    ac_cv_context_diffs='diff -C 1'
else
    if diff -c1 conftest1 conftest2 > /dev/null 2>&1 ; then
        ac_cv_context_diffs='diff -c1'
    else
        echo "Can't figure out how to do context diffs."
        echo "Neither \`diff -C 1' nor \`diff -c1' works."
        exit 1
    fi
fi
rm -f conftest1 conftest2
])
ContextDiffCmd=$ac_cv_context_diffs
AC_SUBST(ContextDiffCmd)
])

dnl
dnl Finding the Right Yacc
dnl
AC_DEFUN(AC_PROG_YACCY,
[AC_PROG_YACC
if test "$YACC" = "yacc"; then
   AC_CACHE_CHECK([if it is an OK yacc], ac_cv_prog_yacc,
   [AC_CHECK_PROG(WhatCmd, what, what, :)
    $WhatCmd $YACC > conftest.out
    if egrep 'y1\.c 1\..*SMI' conftest.out >/dev/null 2>&1; then
        echo "I don't trust your $YaccCmd; it looks like an old Sun yacc"
        if test -f /usr/lang/yacc; then
           echo "I'm going to use /usr/lang/yacc instead"
           ac_cv_prog_yacc=/usr/lang/yacc
        else
           echo "I'm assuming the worst...no parser generator at all"
           ac_cv_prog_yacc=:
        fi
    elif egrep 'y1\.c.*Revision: 4\.2\.6\.3.*DEC' conftest.out >/dev/null 2>&1; then
        echo "I don't trust your $YaccCmd; it looks like a lame DEC yacc"
        echo "I'm assuming the worst...no parser generator at all"
        ac_cv_prog_yacc=:
    else
	ac_cv_prog_yacc=$YACC
    fi
    rm -fr conftest*
])
else
    ac_cv_prog_yacc=$YACC
fi
YaccCmd=$ac_cv_prog_yacc
AC_SUBST(YaccCmd)
])

dnl *** Checking for ar and its arguments + whether we need ranlib.
dnl
dnl ArCmd and RANLIB are AC_SUBST'ed
dnl 
AC_DEFUN(AC_PROG_AR_AND_RANLIB,
[AC_PATH_PROG(ArCmd,ar)
if test -z "$ArCmd"; then
    echo "You don't seem to have ar in your PATH...I have no idea how to make a library"
    exit 1;
fi
if $ArCmd clqs conftest.a >/dev/null 2>/dev/null; then
    ArCmd="$ArCmd clqs"
    NeedRanLib=''
elif $ArCmd cqs conftest.a >/dev/null 2>/dev/null; then
    ArCmd="$ArCmd cqs"
    NeedRanLib=''
elif $ArCmd clq conftest.a >/dev/null 2>/dev/null; then
    ArCmd="$ArCmd clq"
    NeedRanLib='YES'
elif $ArCmd cq conftest.a >/dev/null 2>/dev/null; then
    ArCmd="$ArCmd cq"
    NeedRanLib='YES'
elif $ArCmd cq conftest.a 2>&1 | grep 'no archive members specified' >/dev/null 2>/dev/null; then
    ArCmd="$ArCmd cq"
    NeedRanLib='YES'
else
    echo "I can't figure out how to use your $ArCmd"
    exit 1
fi
rm -rf conftest*
test -n "$ArCmd" && test -n "$verbose" && echo "        setting ArCmd to $ArCmd"
AC_SUBST(ArCmd)
if test -z "$NeedRanLib"; then
    RANLIB=':'
    test -n "$verbose" && echo "        setting RANLIB to $RANLIB"
    AC_SUBST(RANLIB)
else
    AC_PROG_RANLIB
fi
])

dnl
dnl AC_SHEBANG_PERL - can we she-bang perl?
dnl
AC_DEFUN(AC_SHEBANG_PERL,
[AC_CACHE_CHECK([if your perl works in shell scripts], ac_cv_shebang_perl,
[echo "#!$PerlCmd"'
exit $1;
' > conftest
chmod u+x conftest
(SHELL=/bin/sh; export SHELL; ./conftest 69 > /dev/null)
if test $? -ne 69; then
   ac_cv_shebang_perl=yes
else
   ac_cv_shebang_perl=no
fi
rm -f conftest
])])

dnl
dnl Extra testing of the result AC_PROG_CC, testing the gcc version no.
dnl *Must* be called after AC_PROG_CC
dnl
AC_DEFUN(AC_HAVE_GCC,
[AC_CACHE_CHECK([whether you have an ok gcc], ac_cv_have_gcc,
[if test -z "$GCC"; then
    echo ''
    echo "You would be better off with gcc"
    echo "Perhaps it is already installed, but not in your PATH?"
    ac_cv_have_gcc='no'
else
changequote(, )dnl
    cmd_string="`$CC -v 2>&1 | grep 'version ' | sed -e 's/.*version [^0-9]*\([0-9][0-9]*\)\.\([0-9][0-9]*\).*/expr 20 \\\< \1 \\\* 10 + \2/g' `"
changequote([, ])dnl
    if test `eval $cmd_string 2>/dev/null` != "1"; then
	echo ''
        echo "I'm not sure if your version of gcc will work,"
        echo "but it's worth a shot, eh?"
    fi
    ac_cv_have_gcc='yes'
fi
])
HaveGcc=`echo $ac_cv_have_gcc | sed 'y/yesno/YESNO/'`
AC_SUBST(HaveGcc)
])

dnl
dnl AC_PROG_GNUCPP gathers the path to the cpp that the
dnl gcc driver calls upon.
dnl
dnl Substitutes: GNUCPP and RAWCPP (latter is 'GNUCPP -traditional')
dnl
AC_DEFUN(AC_PROG_GNUCPP,
[AC_CACHE_CHECK([how to invoke GNU cpp directly], ac_cv_gnu_cpp,
[if test "$HaveGcc" = "YES"; then
	echo > conftest.c
	gcc -v -E conftest.c >/dev/null 2>conftest.out
	# \x5c = backslash
	echo 'tr/\x5c/\//; /(\S+\/cpp)/ && print "[$]1";' > conftest.pl
	ac_cv_gnu_cpp="`eval $PerlCmd -n conftest.pl conftest.out`"
	rm -fr conftest*
 else
	# We need to be able to invoke CPP directly, preferably
	# with input from stdin (mkdependHS and hscpp depend on
	# this at the moment).
	# Take a guess at what to use, this probably won't work.
	echo Warning: GNU cpp not found, using $CPP
	ac_cv_gnu_cpp =	$CPP
 fi
])
GNUCPP=$ac_cv_gnu_cpp
RAWCPP="$GNUCPP -traditional"
AC_SUBST(GNUCPP)
AC_SUBST(RAWCPP)
])

dnl Small feature test for perl version. Assumes PerlCmd
dnl contains path to perl binary
dnl
AC_DEFUN(AC_CHECK_PERL_VERSION,
[$PerlCmd -v >conftest.out 2>&1
if grep "version 4" conftest.out >/dev/null 2>&1; then
   if grep "Patch level: 35" conftest.out >/dev/null 2>&1; then
      echo "
************************************************************************
Uh-oh...looks like you have Perl 4.035.

Perl version 4.035 has a bug to do with recursion that will bite if
you run the lit2texi script, when making Info files from
literate files of various sorts.  Either use perl5, the last version of perl4 
(4.036), or an older version (e.g., perl 4.019). Failing that, don't create
any Info files :-)
************************************************************************
"
   fi
else
   if grep "version 5" conftest.out >/dev/null 2>&1; then
      :
   else
     echo "I'm not sure if your version of perl will work,"
     echo "but it's worth a shot, eh?"
   fi
fi
rm -fr conftest*
])

dnl ** figure out the alignment restriction of a type
dnl    (required SIZEOF test but AC_CHECK_SIZEOF doesn't call PROVIDE
dnl     so we can't call REQUIRE)

dnl GHC_CHECK_ALIGNMENT(TYPE)
AC_DEFUN(GHC_CHECK_ALIGNMENT,
[changequote(<<, >>)dnl
dnl The name to #define.
define(<<AC_TYPE_NAME>>, translit(alignment_$1, [a-z *], [A-Z_P]))dnl
dnl The cache variable name.
define(<<AC_CV_NAME>>, translit(ac_cv_alignment_$1, [ *], [_p]))dnl
dnl The name of the corresponding size.
define(<<AC_CV_SIZEOF_NAME>>, translit(ac_cv_sizeof_$1, [ *], [_p]))dnl
changequote([, ])dnl
AC_MSG_CHECKING(alignment of $1)
AC_CACHE_VAL(AC_CV_NAME,
[AC_TRY_RUN([
#include <stdio.h>
#if HAVE_STDDEF_H
#include <stddef.h>
#endif
#ifndef offsetof
#define offsetof(ty,field) ((size_t)((char *)&((ty *)0)->field - (char *)(ty *)0))
#endif
int
main()
{
  FILE *f=fopen("conftestval", "w");
  if (!f) exit(1);
  fprintf(f, "%d\n", offsetof(struct { char c; $1 ty;},ty));
  exit(0);
}],
AC_CV_NAME=`cat conftestval`,
AC_CV_NAME=$AC_CV_SIZEOF_NAME,
AC_CV_NAME=$AC_CV_SIZEOF_NAME)])
AC_MSG_RESULT($AC_CV_NAME)
AC_DEFINE_UNQUOTED(AC_TYPE_NAME, $AC_CV_NAME)
AC_PROVIDE($AC_TYPE_NAME)
undefine([AC_TYPE_NAME])dnl
undefine([AC_CV_NAME])dnl
undefine([AC_CV_SIZEOF_NAME])dnl
])

