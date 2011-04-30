#include <R.h>
#include <Rdefines.h>
#include <string.h>
#include <Rmath.h>
#include <stdio.h>
#include <Rinternals.h>
//#include "memisc.h"

// #define DEBUG
SEXP update_data_frame (SEXP data, SEXP with, SEXP s_hh, SEXP s_ii, SEXP s_jj, SEXP append){
    SEXP res, match, matched;
    SEXP x,y = R_NilValue;
    SEXP rownames;
    int i, j, k, l, hh = 0, ii, jj, nn, found;
    if(!inherits(data,"data.frame") || !inherits(with,"data.frame"))
      error("need data.frame arguments");
    PROTECT(res=duplicate(data));
    int protectcounter = 1;
    PROTECT(with=AS_LIST(with));
    protectcounter++;
    int M = LENGTH(VECTOR_ELT(data,0));
    int N = LENGTH(VECTOR_ELT(with,0));
    PROTECT(match = NEW_INTEGER(M));
    protectcounter++;
    PROTECT(matched = NEW_INTEGER(N));
    protectcounter++;
    PROTECT(s_hh = AS_INTEGER(s_hh));
    protectcounter++;
    PROTECT(s_ii = AS_INTEGER(s_ii));
    protectcounter++;
    PROTECT(s_jj = AS_INTEGER(s_jj));
    protectcounter++;
    PROTECT(append = AS_LOGICAL(append));
    protectcounter++;
    nn = LENGTH(s_ii);
    if(LENGTH(s_ii)!=LENGTH(s_jj)) error("ii and jj need to have same length");
    for(i = 0; i < LENGTH(s_hh); i++)
      INTEGER(s_hh)[i]--;
    for(i = 0; i < LENGTH(s_ii); i++)
      INTEGER(s_ii)[i]--;
    for(i = 0; i < LENGTH(s_jj); i++)
      INTEGER(s_jj)[i]--;
#ifdef DEBUG
    Rprintf("\nres at %d\n",res);
    PrintValue(res);
    Rprintf("\nwith at %d\n",with);
    PrintValue(with);
    Rprintf("\ns_hh\n");
    PrintValue(s_hh);
    Rprintf("\ns_ii\n");
    PrintValue(s_ii);
    Rprintf("\ns_jj\n");
    PrintValue(s_jj);
#endif
    for(k = 0; k < nn; k++){
        ii = INTEGER(s_ii)[k];
        jj = INTEGER(s_jj)[k];
        x = VECTOR_ELT(data,ii);
        y = VECTOR_ELT(with,jj);
        PROTECT(y = coerceVector(y,TYPEOF(x)));
        protectcounter++;
        SET_VECTOR_ELT(with,jj,y);
    }
    for(i = 0; i < M; i++) INTEGER(match)[i] = -1;
    for(j = 0; j < N; j++){
      INTEGER(matched)[j] = 0;
      for(i = 0; i < M; i++){
        found = 1;
        for(k = 0; k < nn; k++){
            ii = INTEGER(s_ii)[k];
            jj = INTEGER(s_jj)[k];
#ifdef DEBUG            
            Rprintf("\ni=%d, j=%d, k=%d, ii=%d, jj=%d",i,j,k,ii,jj);
#endif            
            x = VECTOR_ELT(data,ii);
            y = VECTOR_ELT(with,jj);
#ifdef DEBUG            
            Rprintf("\n   TYPEOF(x)=%d, TYPEOF(x)=%d",TYPEOF(x),TYPEOF(y));
#endif
            switch(TYPEOF(x)){
              case LGLSXP:
                found *= (LOGICAL(x)[i]==LOGICAL(y)[j]);
                break;
              case INTSXP:
                found *= (INTEGER(x)[i]==INTEGER(y)[j]);
                break;
              case REALSXP:
                found *= (REAL(x)[i]==REAL(y)[j]);
                break;
              case STRSXP:
                found *= (strcmp(CHAR(STRING_ELT(x,i)),CHAR(STRING_ELT(y,j)))==0);
                break;
              default: error("cannot handle type %d ",TYPEOF(x));
              }
        }
        if(found){
#ifdef DEBUG            
          Rprintf("\nFound matching element for %d in %d",j,i);
#endif
          INTEGER(matched)[j]++;
          INTEGER(match)[i] = j;
        }
      }
    }
#ifdef DEBUG            
    Rprintf("\nmatch\n");
    PrintValue(match);
    Rprintf("\nmatched\n");
    PrintValue(matched);
#endif
    for(k = 0; k < LENGTH(s_hh); k++){
        hh = INTEGER(s_hh)[k];
        x = VECTOR_ELT(res,hh);
        y = VECTOR_ELT(with,k);
#ifdef DEBUG            
        Rprintf("\nx\n");
        PrintValue(x);
        Rprintf("\ny at %d, element %d of 'with' at %d\n",y,k,with);
        PrintValue(y);
#endif
        for(i = 0; i < M; i++){
          j = INTEGER(match)[i];
          if(j >= 0)
            switch(TYPEOF(x)){
                case LGLSXP:
                  LOGICAL(x)[i]=LOGICAL(y)[j];
                  break;
                case INTSXP:
                  INTEGER(x)[i]=INTEGER(y)[j];
                  break;
                case REALSXP:
                  REAL(x)[i]=REAL(y)[j];
                  break;
                case STRSXP:
                  SET_STRING_ELT(x,i,mkChar(CHAR(STRING_ELT(y,j))));
                  break;
                default: error("cannot handle type %d ",TYPEOF(x));
                }
        }
#ifdef DEBUG            
        Rprintf("\nx\n");
        PrintValue(x);
#endif
    }

    if(LOGICAL(append)[0]){
#ifdef DEBUG            
      Rprintf("\nappending unmatched data\n");
#endif
      int n_unmatched = 0;
      SEXP unmatched;
      for(k = 0; k < LENGTH(matched); k++){
          if(INTEGER(matched)[k]==0) n_unmatched++;
      }
      PROTECT(unmatched = NEW_INTEGER(n_unmatched));
      protectcounter++;
      l = 0;
      for(k = 0; k < LENGTH(matched); k++){
          if(INTEGER(matched)[k]==0){
            INTEGER(unmatched)[l] = k;
            l++;
          }
      }
#ifdef DEBUG            
      PrintValue(unmatched);
      Rprintf("\ndoing the appending work now ...filling up NA's\n");
#endif
      for(k = 0; k < LENGTH(res); k++){
        x = VECTOR_ELT(res,k);
        PROTECT(x = lengthgets(x,M+n_unmatched));
        protectcounter++;
        SET_VECTOR_ELT(res,k,x);
#ifdef DEBUG
        Rprintf("\nx\n");            
        PrintValue(x);
#endif
        for(l = M; l < M + n_unmatched; l++){
          switch(TYPEOF(x)){
              case LGLSXP:
                LOGICAL(x)[l]=NA_LOGICAL;
                break;
              case INTSXP:
                INTEGER(x)[l]=NA_INTEGER;
                break;
              case REALSXP:
                REAL(x)[l]=NA_REAL;
                break;
              case STRSXP:
                SET_STRING_ELT(x,i,NA_STRING);
                break;
              default: error("cannot handle type %d ",TYPEOF(x));
              }
        }
#ifdef DEBUG            
        PrintValue(x);
#endif
      }
#ifdef DEBUG            
      Rprintf("\ndoing the appending work now ...filling up data\n");
      PrintValue(s_hh);
      PrintValue(with);
#endif
      for(k = 0; k < LENGTH(s_hh); k++){
        hh = INTEGER(s_hh)[k];
        x = VECTOR_ELT(res,hh);
        y = VECTOR_ELT(with,k);
#ifdef DEBUG            
        Rprintf("\nx\n");
        PrintValue(x);
        Rprintf("\ny at %d, element %d of with at %d\n",y,k,with);
        PrintValue(y);
#endif
        for(l = 0; l < n_unmatched; l++){
          i = l + M;
          j = INTEGER(unmatched)[l];
#ifdef DEBUG            
          Rprintf("i=%d, j=%d\n",i,j);
#endif
          switch(TYPEOF(x)){
              case LGLSXP:
                LOGICAL(x)[i]=LOGICAL(y)[j];
                break;
              case INTSXP:
                INTEGER(x)[i]=INTEGER(y)[j];
                break;
              case REALSXP:
                REAL(x)[i]=REAL(y)[j];
                break;
              case STRSXP:
                SET_STRING_ELT(x,i,mkChar(CHAR(STRING_ELT(y,j))));
                break;
              default: error("cannot handle type %d ",TYPEOF(x));
              }
        }
#ifdef DEBUG            
        Rprintf("\nx\n");
        PrintValue(x);
#endif
      }
      PROTECT(rownames=NEW_INTEGER(M + n_unmatched));
      protectcounter++;
      for(i = 0; i < M + n_unmatched; i++){
        INTEGER(rownames)[i] = i + 1;
      }
      PROTECT(rownames = AS_CHARACTER(rownames));
      protectcounter++;
      setAttrib(res,install("row.names"),rownames);
    } else {
      PROTECT(rownames=NEW_INTEGER(M));
      protectcounter++;
      for(i = 0; i < M; i++){
        INTEGER(rownames)[i] = i + 1;
      }
      PROTECT(rownames = AS_CHARACTER(rownames));
      protectcounter++;
      setAttrib(res,install("row.names"),rownames);
#ifdef DEBUG            
      Rprintf("\nrownames\n");
      PrintValue(rownames);
#endif
    }
    for(i = 0; i < LENGTH(match); i++)
      INTEGER(match)[i]++;
    setAttrib(res,install("match"),match);
    setAttrib(res,install("matched"),matched);
#ifdef DEBUG
    Rprintf("\nmatch\n");
    PrintValue(match);
    Rprintf("\nmatched\n");
    PrintValue(matched);
    for(k = 0; k < LENGTH(res); k++){
      x = VECTOR_ELT(res,k);
      Rprintf("\nres[[%d]]=",k);
      PrintValue(x);
    }
    Rprintf("\nres at %d\n",res);
    PrintValue(res);
#endif
    UNPROTECT(protectcounter);
    return res;
}
