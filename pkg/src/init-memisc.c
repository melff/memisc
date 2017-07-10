#include "memisc.h"

void R_init_memisc(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallMethods, NULL, NULL);
		R_useDynamicSymbols(dll, FALSE);
}
