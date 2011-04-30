#include <R.h>
#include <Rdefines.h>
#include <string.h>
#include <Rmath.h>
#include <stdio.h>
#include <Rinternals.h>

SEXP numeric_if_possible(SEXP x){
    int real_works = 1, int_works = 1, i, nn;
    const char *elem;
    char *end;
    for(i = 0; i < LENGTH(x); i++){
        elem = CHAR(STRING_ELT(x,i));
        nn = strtol(elem,&end,10);
        if(strlen(end)>0) int_works = 0;
        nn = strtod(elem,&end);
        if(strlen(end)>0) {
            real_works=0;
            break;
        }
    }
    if(int_works)
        return AS_INTEGER(x);
    else if(real_works)
        return AS_NUMERIC(x);
    else return duplicate(x);
}



