#if CABAL == 0 && GLASGOW_HASKELL < 604
Package {
        name            = "plugins",
        auto            = False,
#ifdef INSTALLING
        import_dirs     = [ "${LIBDIR}/imports" ],
        library_dirs    = [ "${LIBDIR}/" ],
#else
        import_dirs     = [ "${TOP}/src/plugins" ],
        library_dirs    = [ "${TOP}/src/plugins" ],
#endif
        hs_libraries    = [ "HSplugins" ],
        c_includes      = [ "Linker.h" ],
        include_dirs    = [],
        source_dirs     = [],
        extra_libraries = [],
        package_deps    = [ "altdata", "hi", "unix", "haskell-src", "posix" ],
        extra_ghc_opts  = [],
        extra_cc_opts   = [],
        extra_ld_opts   = []
}

#else

name:		plugins
version:	0.9.8
license:	LGPL
maintainer:	dons@cse.unsw.edu.au
exposed:	True
exposed-modules:
	Plugins.Consts,
	Plugins.Env,
	Plugins.Load,
	Plugins.Make,
	Plugins.MkTemp,
	Plugins.PackageAPI,
	Plugins.ParsePkgConfCabal,
	Plugins.Parser,
	Plugins.Utils,
	Plugins

hidden-modules:
#ifdef INSTALLING
import-dirs:          LIBDIR/imports
library-dirs:         LIBDIR
#else
import-dirs:          TOP/src/plugins
library-dirs:         TOP/src/plugins
#endif
hs-libraries:         HSplugins
extra-libraries:
include-dirs:
includes:	      Linker.h
depends:	      altdata, hi, unix, haskell-src, posix, Cabal
hugs-options:
cc-options:
ld-options:
framework-dirs:
frameworks:
haddock-interfaces:
haddock-html:

#endif
