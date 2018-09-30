#include <R.h>
#include <Rdefines.h>
#include <string.h>
#include <Rmath.h>
#include <stdio.h>
#include <ctype.h>
#include <Rinternals.h>
#include "memisc.h"
#include "rofile.h"
#include <stdlib.h>

int _R_atoi(char *text){
  char *end_ptr;
  if(!strlen(text)) return NA_INTEGER; 
  int result = (int)strtol(text,&end_ptr,10);
  if(!isdigit(end_ptr[-1])) return NA_INTEGER;
  else return result;
}

double _R_atof(char *text){
  char *end_ptr;
  if(!strlen(text)) return NA_REAL; 
  double result = strtod(text,&end_ptr);
  if(!isdigit(end_ptr[-1]) && isdigit(end_ptr[-1]) != '.') return NA_REAL;
  else return result;
}

SEXP readfixed(SEXP s_file, SEXP what, SEXP s_nlines, SEXP s_start, SEXP s_stop){
  PROTECT(s_start = coerceVector(s_start,INTSXP));
  PROTECT(s_stop = coerceVector(s_stop,INTSXP));
  FILE *f = rofile_FILE(s_file);
  if(LENGTH(s_start) != LENGTH(s_stop)) error("start and stop must have equal length");
  int n = asInteger(s_nlines);
  int nvar = LENGTH(s_start);
  int *start = INTEGER(s_start);
  int *stop = INTEGER(s_stop);
  size_t max_lenline = 0;
  ssize_t cur_len;
  char *buffer = NULL;
  char *item, *currdata;
  SEXP data;
  PROTECT(data=allocVector(VECSXP,nvar));
  int i,j;
  int *length = (int *) R_alloc(nvar,sizeof(int));
  int maxlen = 0;
  SEXP x,y;
  for(j = 0; j < nvar; j++){
    length[j] = stop[j] - start[j] + 1;
    if(maxlen < length[j]) maxlen = length[j];
    x = VECTOR_ELT(what,j);
    SET_VECTOR_ELT(data,j,lengthgets(x,n));
  }
  item = R_alloc(maxlen+1,1);
#ifdef DEBUG
  Rprintf("Requested number of lines: %d\n",n);
#endif  
  for(i = 0; i < n; i++){
    cur_len = getline(&buffer,&max_lenline,f);
#ifdef DEBUG
    Rprintf("Requested line length: %d\n",max_lenline);
    Rprintf("Actual line length: %d\n",strlen(buffer));
    if(i < 3)
      Rprintf("Buffer: >>%s<<\n",buffer);
#endif    
    currdata = buffer;
    for(j = 0; j < nvar; j++){
      x = VECTOR_ELT(data,j);
      currdata = buffer + start[j]-1;
      memset(item,0,maxlen+1);
      memcpy(item,currdata,length[j]);
      trim(item,length[j]);
#undef DEBUG
      if(TYPEOF(x)==INTSXP)
        INTEGER(x)[i] = _R_atoi(item);
      else if (TYPEOF(x)==REALSXP)
        REAL(x)[i] = _R_atof(item);
      else
        SET_STRING_ELT(x,i,mkChar(item));
    }
  }
  for(j = 0; j < nvar; j++){
    x = VECTOR_ELT(what,j);
    y = VECTOR_ELT(data,j);
    copyMostAttrib(x,y);
  }  
  UNPROTECT(3);
  return data;
}

SEXP countlines(SEXP s_file){
  FILE *f = rofile_FILE(s_file);
  size_t max_lenline = 0;
  ssize_t cur_len;
  char *buffer = NULL;
  int i, n;

  for(i = 0;; i++){
    cur_len = getline(&buffer,&max_lenline,f);
#ifdef DEBUG
    Rprintf("Line: %d\n",i);
#endif    
    if(feof(f)) {
#ifdef DEBUG
      Rprintf("Requested line length: %d\n",max_lenline);
      Rprintf("Actual line length: %d\n",strlen(buffer));
      Rprintf("Buffer: >>%s<<\n",buffer);
#endif    
      n = i;
      break;
    }
  }
  free(buffer);
  return ScalarInteger(n);
}

SEXP readfixedslice(SEXP s_file, SEXP what, SEXP s_vars, SEXP s_cases, SEXP s_start, SEXP s_stop){
  FILE *f = rofile_FILE(s_file);
  PROTECT(s_vars = coerceVector(s_vars,LGLSXP));
  PROTECT(s_cases = coerceVector(s_cases,LGLSXP));
  PROTECT(s_start =  coerceVector(s_start,INTSXP));
  PROTECT(s_stop = coerceVector(s_stop,INTSXP));
  if(LENGTH(s_start) != LENGTH(s_stop)) error("start and stop must have equal length");
  if(LENGTH(s_vars) != LENGTH(s_stop)) error("vars argument has wrong length");
  int m = 0, n = 0;
  int nvar = LENGTH(what);
  int ncases = LENGTH(s_cases);
  int ii,i,j,k;
  for(i = 0; i < LENGTH(s_cases); i++) n += LOGICAL(s_cases)[i];
  for(j = 0; j < LENGTH(s_vars); j++) m += LOGICAL(s_vars)[j];
  int *start = INTEGER(s_start);
  int *stop = INTEGER(s_stop);
  size_t max_lenline = 0;
  ssize_t cur_len;
  char *buffer = NULL;
  char *item, *currdata;

  SEXP data;
  PROTECT(data = allocVector(VECSXP,m));
  SEXP x, y;
  int *length = (int *) R_alloc(nvar,sizeof(int));
  int maxlen = 0;
  k = 0;
  for(j = 0; j < nvar; j++){
    length[j] = stop[j] - start[j] + 1;
    if(LOGICAL(s_vars)[j]){
      if(maxlen < length[j]) maxlen = length[j];
      x = VECTOR_ELT(what,j);
      SET_VECTOR_ELT(data,k,lengthgets(x,n));
      k++;
    }
  }
  item = R_alloc(maxlen+1,1);
  ii = 0;
  for(i = 0; i < ncases; i++){
    cur_len = getline(&buffer,&max_lenline,f);
#ifdef DEBUG
    Rprintf("Requested line length: %d\n",max_lenline);
    Rprintf("Actual line length: %d\n",strlen(buffer));
    Rprintf("Buffer: >>%s<<\n",buffer);
#endif
    if(LOGICAL(s_cases)[i]){
      currdata = buffer;
      k = 0;
      for(j = 0; j < nvar; j++){
        currdata = buffer + start[j]-1;
        if(LOGICAL(s_vars)[j]){
          x = VECTOR_ELT(data,k);
          memset(item,0,maxlen+1);
          memcpy(item,currdata,length[j]);
          trim(item,length[j]);
#ifdef DEBUG
          Rprintf("Item: >>%s<<\n",item);
#endif
#undef DEBUG    
          if(TYPEOF(x)==INTSXP)
            INTEGER(x)[ii] = _R_atoi(item);
          else if (TYPEOF(x)==REALSXP)
            REAL(x)[ii] = _R_atof(item);
          else
            SET_STRING_ELT(x,ii,mkChar(item));
          k++;
          }
        }
        ii++;
      }
    }
  k = 0;
  for(j = 0; j < nvar; j++){
    if(LOGICAL(s_vars)[j]){
      x = VECTOR_ELT(what,j);
      y = VECTOR_ELT(data,k);
      copyMostAttrib(x,y);
      k++;
    }
  }

  UNPROTECT(5);
  return data;
}


SEXP readfixedchunk(SEXP s_file, SEXP what, SEXP s_vars, SEXP s_nlines, SEXP s_start, SEXP s_stop){
  PROTECT(s_vars = coerceVector(s_vars,LGLSXP));
  PROTECT(s_start = coerceVector(s_start,INTSXP));
  PROTECT(s_stop = coerceVector(s_stop,INTSXP));
  FILE *f = rofile_FILE(s_file);
  if(LENGTH(s_start) != LENGTH(s_stop)) error("start and stop must have equal length");
	int m = 0;
  int n = asInteger(s_nlines);
  int nvar = LENGTH(what);
  int i,j,k;
  for(j = 0; j < LENGTH(s_vars); j++) m += LOGICAL(s_vars)[j];
  int *start = INTEGER(s_start);
  int *stop = INTEGER(s_stop);
  size_t max_lenline = 0;
  ssize_t cur_len;
  char *buffer = NULL;
  char *item, *currdata;
  SEXP data;
  PROTECT(data=allocVector(VECSXP,m));
  int *length = (int *) R_alloc(nvar,sizeof(int));
  int maxlen = 0;
  SEXP x,y;
	k = 0;
  for(j = 0; j < nvar; j++){
    length[j] = stop[j] - start[j] + 1;
    if(LOGICAL(s_vars)[j]){
      if(maxlen < length[j]) maxlen = length[j];
      x = VECTOR_ELT(what,j);
      SET_VECTOR_ELT(data,k,lengthgets(x,n));
      k++;
    }
  }
  item = R_alloc(maxlen+1,1);
#undef DEBUG
#ifdef DEBUG
  Rprintf("Requested number of lines: %d\n",n);
#endif  
  for(i = 0; i < n; i++){
    cur_len = getline(&buffer,&max_lenline,f);
#ifdef DEBUG
    Rprintf("Requested line length: %d\n",max_lenline);
    Rprintf("Actual line length: %d\n",strlen(buffer));
    if(i == 0)
      Rprintf("Buffer: >>%s<<\n",buffer);
#endif    
    currdata = buffer;
		k = 0;
    for(j = 0; j < nvar; j++){
        currdata = buffer + start[j]-1;
        if(LOGICAL(s_vars)[j]){
          x = VECTOR_ELT(data,k);
          memset(item,0,maxlen+1);
          memcpy(item,currdata,length[j]);
          trim(item,length[j]);
#ifdef DEBUG
          Rprintf("Item: >>%s<<\n",item);
#endif
#undef DEBUG    
          if(TYPEOF(x)==INTSXP)
            INTEGER(x)[i] = _R_atoi(item);
          else if (TYPEOF(x)==REALSXP)
            REAL(x)[i] = _R_atof(item);
          else
            SET_STRING_ELT(x,i,mkChar(item));
          k++;
          }
		}
  }
  k = 0;
  for(j = 0; j < nvar; j++){
    if(LOGICAL(s_vars)[j]){
      x = VECTOR_ELT(what,j);
      y = VECTOR_ELT(data,k);
      copyMostAttrib(x,y);
      k++;
    }
  }
  UNPROTECT(4);
  return data;
}
