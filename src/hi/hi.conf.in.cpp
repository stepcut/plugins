#if CABAL == 0 && GLASGOW_HASKELL < 604
Package {
        name            = "hi",
        auto            = False,
        hs_libraries    = [ "HShi" ],
#ifdef INSTALLING
        import_dirs     = [ "${LIBDIR}/imports" ],
        library_dirs    = [ "${LIBDIR}/" ],
#else
        import_dirs     = [ "${TOP}/src/hi" ],
        library_dirs    = [ "${TOP}/src/hi" ],
#endif
        include_dirs    = [],
        c_includes      = [],
        source_dirs     = [],
        extra_libraries = [],
        package_deps    = [ "base", "haskell98" ],
        extra_ghc_opts  = [],
        extra_cc_opts   = [],
        extra_ld_opts   = []
}
#else
name:		hi
version:	0.9.10
license:	BSD3
maintainer:	libraries@haskell.org
exposed:	True
exposed-modules:
	Language.Hi.Parser

hidden-modules:
        Language.Hi.Binary,
        Language.Hi.FastString,
	Language.Hi.Syntax,    
        Language.Hi.FastMutInt,
        Language.Hi.PrimPacked

#ifdef INSTALLING
import-dirs:          LIBDIR/imports
library-dirs:         LIBDIR
#else
import-dirs:          TOP/src/hi
library-dirs:         TOP/src/hi
#endif
hs-libraries:         HShi
extra-libraries:
include-dirs:
includes:	      
depends:	      base, haskell98
hugs-options:
cc-options:
ld-options:
framework-dirs:
frameworks:
haddock-interfaces:
haddock-html:
#endif
