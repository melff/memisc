#include <R.h>
#include <Rdefines.h>
#include <string.h>
#include <Rmath.h>
#include <stdio.h>
#include <Rinternals.h>
#include "memisc.h"

int ftell32 (FILE *f){
  size_t tellval = ftell(f);
  if(tellval > INT_MAX) error("ftell: return value to large");
  return (int)tellval;
}

FILE *rofile_FILE(SEXP s_file);
SEXP rofile_fclose(SEXP s_file)
{
    FILE *f = rofile_FILE(s_file);
    if (f != NULL) {
        fclose(f);
        R_ClearExternalPtr(s_file);
    }
    return R_NilValue;
}

SEXP rofile (SEXP name){
  PROTECT(name = AS_CHARACTER(name));
  FILE* f;
  f = fopen(CHAR(STRING_ELT(name, 0)),"rb");
  if (f == NULL){
      UNPROTECT(1);
      return R_NilValue;
      }
  else {
    SEXP ans = R_MakeExternalPtr(f, install("rofile"), R_NilValue);
    R_RegisterCFinalizer(ans, (R_CFinalizer_t) rofile_fclose);
    setAttrib(ans,install("file.name"),name);
    UNPROTECT(1);
    return ans;
    }
}


FILE *rofile_FILE(SEXP s_file){
  if(TYPEOF(s_file) != EXTPTRSXP || R_ExternalPtrTag(s_file) != install("rofile")) error("not an rofile");
  FILE *f = R_ExternalPtrAddr(s_file);
  if (f == NULL){
      SEXP name = getAttrib(s_file,install("file.name"));
      if(name == R_NilValue || name == NULL){
        error("need filename to reopen file");
        }
      R_SetExternalPtrAddr(s_file,f);
      f = fopen(CHAR(STRING_ELT(name, 0)),"rb");
      if(f == NULL){
        error("cannot reopen file -- does it still exist?");
      }
      Rprintf("File '%s' reopened\n\n",CHAR(STRING_ELT(name, 0)));
  }
  return(f);
}

SEXP roftell (SEXP s_file){
  FILE *f = rofile_FILE(s_file);
  return ScalarInteger(ftell32(f));
}

/*int seek_code[] = { SEEK_SET, SEEK_CUR, SEEK_END };*/ 

SEXP rofseek (SEXP s_file, SEXP s_pos, SEXP s_whence){
  const int seek_code[] = { SEEK_SET, SEEK_CUR, SEEK_END };
  FILE *f = rofile_FILE(s_file);
  PROTECT(s_pos = AS_INTEGER(s_pos));
  PROTECT(s_whence = AS_INTEGER(s_whence));
  long pos = INTEGER(s_pos)[0];
  int whence = INTEGER(s_whence)[0];
  int retcode = fseek(f,pos,seek_code[whence]);
  UNPROTECT(2);
  if (retcode == 0) return ScalarLogical(TRUE);
  else return ScalarLogical(FALSE);
}

#define bufsize 2000
#undef DEBUG

SEXP rofreadline(SEXP s_file){
  FILE *f = rofile_FILE(s_file);
  int found = 0, nlines=1;
  size_t i, sl, offset = 0;
  char *buf = S_alloc(bufsize,1);
  char *tmp;
  while(!found){
    tmp = buf + offset;
    tmp = fgets(tmp,bufsize,f);
#ifdef DEBUG
    Rprintf("Read buffer: %s",tmp);
#endif
    sl = strlen(tmp);
    for(i = sl; i > sl - 5; i--){
        if(tmp[i] == '\n' || tmp[i] == '\r') {
          found = 1;
          tmp[i] = '\0';
        } 
      }
    if(!found){
      buf = S_realloc(buf,(nlines+1)*bufsize,nlines*bufsize,1);
      offset += sl; /*overwrite \0 */
      nlines++;
    }
  }
#ifdef DEBUG
    Rprintf("Buffer: %s",buf);
#endif
  return mkString(buf);
}
