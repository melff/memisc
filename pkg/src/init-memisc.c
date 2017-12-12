#include "memisc.h"
#include <R_ext/Visibility.h>
#include <Rversion.h>

#if R_VERSION >= R_Version(3,4,0)
void attribute_visible R_init_memisc(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallMethods, NULL, NULL);
		R_useDynamicSymbols(dll, FALSE);
		
}
#endif
