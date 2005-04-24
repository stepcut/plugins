/*
These routines customise the error messages
for various bits of the RTS.  They are linked
in instead of the defaults.
*/

#include <string.h>

/* For GHC 4.08, we are relying on the fact that RtsFlags has
 * compatibile layout with the current version, because we're
 * #including the current version of RtsFlags.h below.  4.08 didn't
 * ship with its own RtsFlags.h, unfortunately.   For later GHC
 * versions, we #include the correct RtsFlags.h.
 */

#include "Rts.h"
#include "RtsFlags.h"

#include "HsFFI.h"

HsInt
plugin_strlen( HsAddr a )
{
    return (strlen((char *)a));
}

HsInt
plugin_memcmp( HsAddr a1, HsAddr a2, HsInt len )
{
    return (memcmp((char *)a1, a2, len));
}

HsInt
plugin_memcmp_off( HsAddr a1, HsInt i, HsAddr a2, HsInt len )
{
    return (memcmp((char *)a1 + i, a2, len));
}

