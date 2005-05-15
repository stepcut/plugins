#if CABAL == 0 && GLASGOW_HASKELL < 604
Package {
        name            = "altdata",
        auto            = False,
        hs_libraries    = [ "HSaltdata" ],
#ifdef INSTALLING
        import_dirs     = [ "${LIBDIR}/imports" ],
        library_dirs    = [ "${LIBDIR}" ],
#else
        import_dirs     = [ "${TOP}/src/altdata" ],
        library_dirs    = [ "${TOP}/src/altdata" ],
#endif
        include_dirs    = [],
        c_includes      = [],
        source_dirs     = [],
        extra_libraries = [],
        package_deps    = [ "base" ],
        extra_ghc_opts  = [],
        extra_cc_opts   = [],
        extra_ld_opts   = []
}
#else
name:		altdata
version:	0.9.8
license:	LGPL
maintainer:	dons@cse.unsw.edu.au
exposed:	True
exposed-modules:
	AltData.Dynamic,
	AltData.Typeable

hidden-modules:
#ifdef INSTALLING
import-dirs:          LIBDIR/imports
library-dirs:         LIBDIR
#else
import-dirs:          TOP/src/altdata
library-dirs:         TOP/src/altdata
#endif
hs-libraries:         HSaltdata
extra-libraries:
include-dirs:
includes:	      
depends:	      base
hugs-options:
cc-options:
ld-options:
framework-dirs:
frameworks:
haddock-interfaces:
haddock-html:
#endif
