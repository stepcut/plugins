#if CABAL == 0 && GLASGOW_HASKELL < 604
Package {
        name            = "eval",
        auto            = False,
        hs_libraries    = [ "HSeval" ],
#ifdef INSTALLING
        import_dirs     = [ "${LIBDIR}/imports" ],
        library_dirs    = [ "${LIBDIR}/" ],
#else
        import_dirs     = [ "${TOP}/src/eval" ],
        library_dirs    = [ "${TOP}/src/eval" ],
#endif
        include_dirs    = [],
        c_includes      = [],
        source_dirs     = [],
        extra_libraries = [],
        package_deps    = [ "plugins"
#if GLASGOW_HASKELL >= 603
                          , "template-haskell" 
#endif
                          ],
        extra_ghc_opts  = [],
        extra_cc_opts   = [],
        extra_ld_opts   = []
}
#else

name:		eval
version:	0.9.8
license:	LGPL
maintainer:	dons@cse.unsw.edu.au
exposed:	True
exposed-modules:
	Eval.Haskell,
	Eval.Meta,
	Eval.Utils,
	Eval

hidden-modules:
#ifdef INSTALLING
import-dirs:          LIBDIR/imports
library-dirs:         LIBDIR
#else
import-dirs:          TOP/src/eval
library-dirs:         TOP/src/eval
#endif
hs-libraries:         HSeval
extra-libraries:
include-dirs:
includes:
depends:	      plugins, template-haskell
hugs-options:
cc-options:
ld-options:
framework-dirs:
frameworks:
haddock-interfaces:
haddock-html:

#endif
