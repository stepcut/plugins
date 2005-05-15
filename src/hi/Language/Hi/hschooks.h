/* -----------------------------------------------------------------------------
 * $ Id: hschooks.h,v 1.1.1.1 2004/05/24 09:35:39 dons Exp $
 *
 * Hooks into the RTS from the compiler.
 *
 * -------------------------------------------------------------------------- */

#include "HsFFI.h"

// Out-of-line string functions, see PrimPacked.lhs
HsInt plugin_strlen( HsAddr a );
HsInt plugin_memcmp( HsAddr a1, HsAddr a2, HsInt len );
HsInt plugin_memcmp_off( HsAddr a1, HsInt i, HsAddr a2, HsInt len );
