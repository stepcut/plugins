#
# Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
# LGPL version 2.1 or later (see http://www.gnu.org/copyleft/lesser.html)
#

# sanity test
AC_INIT(src/plugins/Plugins.hs)

# untested on earlier than 2.52, but it won't work anyway
AC_PREREQ(2.53)

# Find out what type of system we're running on
AC_CANONICAL_BUILD

# This isn't right for cygwin.
PREFIX="$prefix"
if test "$prefix" = "NONE"
then
	PREFIX="$ac_default_prefix"
fi
AC_SUBST(PREFIX)

Platform="$build_cpu-$build_vendor-$build_os"

case $Platform in
powerpc-apple-darwin*)
        MACOSX=yes
	;;
*)
        MACOSX=no
	;;
esac
if test "$MACOSX" = "yes" 
then
  WHOLE_ARCHIVE_FLAG=-all_load
  LEADING_UNDERSCORE=1
else
  WHOLE_ARCHIVE_FLAG=--whole-archive
  LEADING_UNDERSCORE=0
fi

AC_SUBST(WHOLE_ARCHIVE_FLAG)
AC_SUBST(LEADING_UNDERSCORE)

# Find pwd, in a cygwin friendly way. 
# Suggested by: http://www.haskell.org/ghc/docs/latest/html/users_guide/ch11s04.html
TOP=`echo "Directory.getCurrentDirectory >>= putStrLn.init.tail.show " | ghci -ignore-dot-ghci -v0`
AC_SUBST(TOP)

# necessary tools

# allow user supplied haskell compiler
AC_ARG_WITH(ghc,
  AC_HELP_STRING([--with-ghc=<ghc>],[use a specific Haskell compiler]),
  [ GHC="$withval" 
    if test ! -f "$GHC" ; then
       AC_MSG_ERROR([$GHC not found. You need GHC to build this project])
    fi
  ],
  [ AC_CHECK_PROG(GHC,ghc,ghc)
    if test -z "$GHC" ; then
       AC_MSG_ERROR([You need GHC to build this project])
    fi 
  ]
  )
AC_SUBST(GHC)

# find path to GHC libs, for runtime_loader
if test -n "$GHC" ; then
    AC_MSG_CHECKING([for ghc library directory])
    GHC_LIB_PATH=`$GHC --print-libdir`
    AC_MSG_RESULT([$GHC_LIB_PATH])
fi
AC_SUBST(GHC_LIB_PATH)

# check ghc version here
if test -n "$GHC" ; then
    AC_MSG_CHECKING([for ghc version])
    GHC_VERSION=`$GHC --numeric-version`
    AC_MSG_RESULT([$GHC_VERSION])
fi
AC_SUBST(GHC_VERSION)

# Work out value of __GLASGOW_HASKELL__
if test -n "$GHC" ; then
 AC_MSG_CHECKING([for value of __GLASGOW_HASKELL__])
 echo "main = print __GLASGOW_HASKELL__" > t.hs
 GLASGOW_HASKELL=`echo 'main' | "$GHC" --interactive -ignore-dot-ghci -v0 -cpp t.hs`
 rm t.hs
 AC_MSG_RESULT([$GLASGOW_HASKELL])
fi
AC_SUBST(GLASGOW_HASKELL)

dnl ** quote char breaks sed
changequote(, )dnl
MAJOR=`echo "$GHC_VERSION" | sed 's/^\([^\.]*\)\.\([^\.]*\).*/\1/'`
MINOR=`echo "$GHC_VERSION" | sed 's/^\([^\.]*\)\.\([^\.]*\).*/\2/'`
changequote([, ])dnl

if test "$MAJOR" -lt "6"; then
	AC_MSG_ERROR(Found major $MAJOR. You need a ghc version >= 6.2) ;
fi
if test "$MINOR" -lt "2"; then
	AC_MSG_ERROR(You need a ghc version >= 6.2) ;
fi

#Allow plugins to be built with Cabal libraries
AC_ARG_ENABLE(cabal, 
   [  --enable-cabal          Enable use of Cabal packages in pluggable-1-branch
                          of GHC],
   [ CABAL=1 ],
   [ CABAL=0 ])

# used by the Makefile`s to alter dependencies.
if test "$MAJOR" -ge "6" -a "$MINOR" -ge "4"; then
  CABAL=1
fi

AC_SUBST(CABAL)

# Allow a debugging version of hs-plugins to be built
AC_ARG_ENABLE(debug,
   [  --enable-debug          Enable a debug version of hs-plugins to be built],
   [ DEBUG_OPTS=-DDEBUG ],
   [ DEBUG_OPTS= ])

AC_SUBST(DEBUG_OPTS)

# Search for ghc-pkg
AC_MSG_CHECKING([for ghc-pkg])
ghc_pkg_guess=`echo $GHC | sed 's,ghc\(@<:@^/\\@:>@*\)$,ghc-pkg\1,'`
if "$ghc_pkg_guess" -l > /dev/null 2>&1; then
      matching_ghc_pkg=$ghc_pkg_guess
else
      matching_ghc_pkg=no
fi
if test x"$matching_ghc_pkg" = xno; then
    AC_CHECK_PROG([GHCPKG], [ghc-pkg])
    if test -z "$GHCPKG" ; then
       AC_MSG_ERROR([ghc-pkg not found.])
    fi
else
    GHCPKG=$matching_ghc_pkg
fi
AC_MSG_RESULT([$GHCPKG])
AC_SUBST(GHCPKG)

AC_CHECK_PROG(HADDOCK,haddock,haddock)
if test -z "$HADDOCK" ; then
  AC_MSG_WARN(You need Haddock if you want developer documentation)
fi

AC_CHECK_PROG(HAPPY,happy,happy)
if test -z "$HAPPY" ; then
  AC_MSG_WARN(If you change or remove the parser you'll need Happy)
fi

AC_CHECK_PROG(ALEX,alex,alex)
if test -z "$ALEX" ; then
	AC_MSG_WARN(If you change or remove the lexer files you'll need alex)
fi

AC_CHECK_PROG(LD,ld,ld)
if test -z "$LD" ; then
  AC_MSG_WARN(You need ld -export-dynamic)
fi

AC_CHECK_PROG(AR,ar,ar)
if test -z "$AR" ; then
  AC_MSG_WARN(You need ar to build the library)
fi

AC_CHECK_PROG(RANLIB,ranlib,ranlib)
if test -z "$RANLIB" ; then
  AC_MSG_WARN(You need randlib to build the library)
fi

AC_CHECK_PROG(RM,rm,rm)
if test -z "$RM" ; then
  AC_MSG_WARN(You need rm!)
fi

AC_CHECK_PROG(TEX,tex,tex)
if test -z "$TEX" ; then
	AC_MSG_WARN(You'll need tex if you wish to build the documentation)
fi
AC_CHECK_PROG(TEX2PAGE,tex2page,tex2page)
if test -z "$TEX2PAGE" ; then
	AC_MSG_WARN(You'll need tex2page if you wish to build the
		    documentation: http://www.ccs.neu.edu/home/dorai/tex2page/tex2page-doc.html)
fi

AC_CHECK_FUNC(arc4random, [SYMS="$SYMS -DHAVE_ARC4RANDOM"])

AC_SUBST(SYMS)

AC_PROG_INSTALL

AC_CONFIG_FILES(config.mk config.h)

AC_OUTPUT

