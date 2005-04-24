#if CABAL == 0 && GLASGOW_HASKELL < 604
Package {
        name            = "printf",
        auto            = False,
        hs_libraries    = [ "HSprintf" ],
#ifdef INSTALLING
        import_dirs     = [ "${LIBDIR}/imports" ],
        library_dirs    = [ "${LIBDIR}/" ],
#else
        import_dirs     = [ "${TOP}/src/printf" ],
        library_dirs    = [ "${TOP}/src/printf" ],
#endif
        include_dirs    = [],
        c_includes      = [],
        source_dirs     = [],
        extra_libraries = [],
        package_deps    = [ "eval" ],
        extra_ghc_opts  = [],
        extra_cc_opts   = [],
        extra_ld_opts   = []
}
#else
name:	        printf
version:	0.9.8
license:	LGPL
maintainer:	dons@cse.unsw.edu.au
exposed:	False
exposed-modules:
        Printf.Compile,
        Printf.Lexer,
        Printf.Parser,
        Printf

hidden-modules:
#ifdef INSTALLING
import-dirs:          LIBDIR/imports
library-dirs:         LIBDIR
#else
import-dirs:          TOP/src/printf
library-dirs:         TOP/src/printf
#endif
hs-libraries:         HSprintf
extra-libraries:
include-dirs:
includes:
depends:	      eval
hugs-options:
cc-options:
ld-options:
framework-dirs:
frameworks:
haddock-interfaces:
haddock-html:
#endif
