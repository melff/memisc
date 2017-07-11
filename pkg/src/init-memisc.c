#include "memisc.h"
#include <R_ext/Visibility.h>

void attribute_visible R_init_memisc(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallMethods, NULL, NULL);
		R_useDynamicSymbols(dll, FALSE);
		
}
