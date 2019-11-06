#include "memisc.h"
#include <Rversion.h>

#include <R_ext/Visibility.h>
#include <R_ext/Rdynload.h>
void attribute_visible R_init_memisc(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallMethods, NULL, NULL);
		R_useDynamicSymbols(dll, FALSE);
}
