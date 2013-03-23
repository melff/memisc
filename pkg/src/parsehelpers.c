#include <R.h>
#include <Rdefines.h>
#include <string.h>
#include <Rmath.h>
#include <stdio.h>
#include <Rinternals.h>
#include <ctype.h>
#include "memisc.h"

void trim (char *string, int len){
  int i;
  for(i = len-1; i >= 0; i--){
    if(string[i] == 32) string[i]=0;
    else break;
  }
}

SEXP is_value(SEXP s_text){
  PROTECT(s_text = AS_CHARACTER(s_text));
  const char *text = CHAR(STRING_ELT(s_text,0));
  int i, result = 1; 
  for(i = 0; i < strlen(text); i++){
    if(!result) break;
    result = result && isdigit(text[i]);
  }
  UNPROTECT(1);
  return ScalarLogical(result);
}

SEXP is_varname(SEXP s_text){
  PROTECT(s_text = AS_CHARACTER(s_text));
  const char *text = CHAR(STRING_ELT(s_text,0));
  int i, result = isalpha(text[0]); 
  for(i = 1; i < strlen(text); i++){
    if(!result) break;
    result = result && isalnum(text[i]);
  }
  UNPROTECT(1);
  return ScalarLogical(result);
}

SEXP is_slashed_varname(SEXP s_text){
  PROTECT(s_text = AS_CHARACTER(s_text));
  const char *text = CHAR(STRING_ELT(s_text,0));
  if(text[0] != '/'){
      UNPROTECT(1);
      return ScalarLogical(FALSE);
  }
  text++;
  if(strlen(text)<1){
      UNPROTECT(1);
      return ScalarLogical(FALSE);
  }
  int i, result = isalpha(text[0]); 
  for(i = 1; i < strlen(text); i++){
    if(!result) break;
    result = result && isalnum(text[i]);
  }
  UNPROTECT(1);
  return ScalarLogical(result);
}

SEXP firstnum(SEXP s_text){
  PROTECT(s_text = AS_CHARACTER(s_text));
  const char *text = CHAR(STRING_ELT(s_text,0));
  int pos = 0;
  while(isdigit(text[pos])) pos++;
  if(text[pos] == '.') pos++;  
  while(text[pos] == ' ') pos++;
  char *result = R_alloc(pos+1,1);
  memset(result,0,pos+1);
  memcpy(result,text,pos);
  UNPROTECT(1);
  return mkString(result);
}

#undef DEBUG
SEXP is_floatspec(SEXP s_text){
  PROTECT(s_text = AS_CHARACTER(s_text));
  const char *text = CHAR(STRING_ELT(s_text,0));
  size_t pos, sl = strlen(text);
#ifdef DEBUG
  Rprintf("Tested string: %s\n",text);
#endif
  if(strlen(text) < 3 || text[0]!='(' || text[sl-1] != ')'){
    UNPROTECT(1);
    return ScalarLogical(FALSE);
    }
  for(pos=1; pos<sl-1;pos++){
    if(!isdigit(text[pos])){
    UNPROTECT(1);
    return ScalarLogical(FALSE);
    }
  }
  UNPROTECT(1);
  return ScalarLogical(TRUE);
}


SEXP has_digits(SEXP s_text){
  PROTECT(s_text = AS_CHARACTER(s_text));
  const char *text = CHAR(STRING_ELT(s_text,0));
  size_t pos, sl = strlen(text);
#ifdef DEBUG
  Rprintf("Tested string: %s\n",text);
#endif
  for(pos = 0; pos < sl; pos++){
    if(isdigit(text[pos])){
      UNPROTECT(1);
      return ScalarLogical(TRUE);
    }
  }
  UNPROTECT(1);
  return ScalarLogical(FALSE);
}


SEXP get_digits(SEXP s_text){
  PROTECT(s_text = AS_CHARACTER(s_text));
  const char *text = CHAR(STRING_ELT(s_text,0));
  size_t pos, startpos = -1, stoppos = -1, sl = strlen(text);
  for(pos = 0; pos < sl; pos++){
    if(isdigit(text[pos])){
      startpos = pos;
      break;
    }
  }
  if(startpos < 0) {
    UNPROTECT(1);
    return mkString("");
  }
  for(pos = startpos+1; pos < sl; pos++){
    if(!isdigit(text[pos])){
      stoppos = pos;
      break;
    }
  }
  if(stoppos < 0) stoppos = sl;
  size_t reslen = stoppos-startpos;
  char *result = R_alloc(reslen+1,1);
  memset(result,0,reslen+1);
  memcpy(result,text+startpos,reslen);
  UNPROTECT(1);
  return mkString(result);
}

SEXP str_contains(SEXP s_text, SEXP s_target){
  PROTECT(s_text = AS_CHARACTER(s_text));
  PROTECT(s_target = AS_CHARACTER(s_target));
  const char *text = CHAR(STRING_ELT(s_text,0));
  const char *target = CHAR(STRING_ELT(s_target,0));
#ifdef DEBUG
  Rprintf("Tested string: %s, length %d\n",text,strlen(text));
  Rprintf("Target string: %s, length %d\n",target,strlen(target));
#endif
  if(strlen(text) < strlen(target)){
    UNPROTECT(2);
    return ScalarLogical(FALSE);
  }

  size_t tlen = strlen(target), i;
  for(i = 0; i+tlen <= strlen(text); i++) {
    if(memcmp(text + i,target,tlen)== 0){
      UNPROTECT(2);
      return ScalarLogical(TRUE);
    }
  }
  UNPROTECT(2);
  return ScalarLogical(FALSE);
}


