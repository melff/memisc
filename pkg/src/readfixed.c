#include <R.h>
#include <Rdefines.h>
#include <string.h>
#include <Rmath.h>
#include <stdio.h>
#include <ctype.h>
#include <Rinternals.h>
#include "memisc.h"
#include "rofile.h"

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

size_t Rgetline(char **lineptr, size_t *lenptr, FILE *file){

  char *line = *lineptr;
  size_t len = *lenptr;
  
  if (file == NULL) {
        error("File pointer is null.");
  }
  int chunk_size = 128;
  if(len < chunk_size) len = chunk_size;
    
  if(line == NULL){
    line = (char *)R_alloc(len,sizeof(char));
    if(line == NULL)
      error("Could not allocate line pointer");
    /* Rprintf("Initial buffer size: %d\n",len); */
  }
    
  char ch = getc(file);
  size_t n = 0;
    
  while((ch != '\n') && (ch != '\r') && (ch != EOF)){
    /* Rprintf("Current char: %c\n",ch);
     * Rprintf("Current buffer: %s\n",line); */
    if(n == len){
      /* Rprintf("Increasing buffer size\n"); */
      char *tmp = (char *)R_alloc(len+chunk_size,sizeof(char));
      if(tmp == NULL)
        error("Could not extend buffer size");
      memcpy(tmp,line,len);
      line = tmp;
      len += chunk_size;
      /* Rprintf("New buffer size: %d\n",len); */
    }
    line[n] = ch;
    n++;

    ch = getc(file);
  }
  while((ch == '\n') || (ch == '\r')) ch = getc(file);
  if((ch != '\n') && (ch != '\r') && (ch != EOF)) ungetc(ch,file);
  
  line[n] = '\0';
  *lineptr = line;
  *lenptr = len;
  return n;
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
  size_t cur_len;
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
    cur_len = Rgetline(&buffer,&max_lenline,f);
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
  size_t cur_len;
  char *buffer = NULL;
  int i, n;

  for(i = 0;; i++){
    cur_len = Rgetline(&buffer,&max_lenline,f);
#ifdef DEBUG
    Rprintf("Line: %d\n",i);
#endif    
    if(feof(f)) {
#ifdef DEBUG
      Rprintf("Requested line length: %d\n",max_lenline);
      Rprintf("Actual line length: %d\n",strlen(buffer));
      Rprintf("Buffer: >>%s<<\n",buffer);
#endif    
      if(cur_len > 0)
         n = i + 1;
      else 
         n = i;
      break;
    }
  }
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
  size_t cur_len;
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
    cur_len = Rgetline(&buffer,&max_lenline,f);
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
  size_t cur_len;
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
    cur_len = Rgetline(&buffer,&max_lenline,f);
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
