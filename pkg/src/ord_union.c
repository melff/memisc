#include<Rinternals.h>

SEXP ord_union(SEXP x, SEXP y){
  PROTECT(x = coerceVector(x,STRSXP));
  PROTECT(y = coerceVector(y,STRSXP));
  int n = length(x);
  int m = length(y);
  int front_x = 0;
  int front_y = 0;
  int i,j;
  SEXP result;
  int len, incr;
  PROTECT(result = allocVector(STRSXP,0));
  SEXP tmp_x, tmp_y;
  int found;
  while(front_x < n){
    tmp_x = STRING_ELT(x,front_x);
    found = 0;
    for(j = front_y; j < m; j++){
      tmp_y = STRING_ELT(y,j);
      if(tmp_x == tmp_y){
        found = 1;
        break;
      }
    }
    len = length(result);
    if(found){
      if(j > front_y){
        incr = j - front_y + 1;
        PROTECT(result = lengthgets(result,len+incr));
        for(i = 0; i < incr; i++){
          SET_STRING_ELT(result,len+i,STRING_ELT(y,front_y+i));
        }
        front_x++;
        front_y+=incr;
        UNPROTECT(1);
      }
      else {
        PROTECT(result = lengthgets(result,len+1));
        SET_STRING_ELT(result,len,STRING_ELT(x,front_x));
        front_x++;
        front_y++;
        UNPROTECT(1);
      }
    }
    else {
      PROTECT(result = lengthgets(result,len+1));
      SET_STRING_ELT(result,len,STRING_ELT(x,front_x));
      front_x++;
      UNPROTECT(1);
    }
  }
  if(front_y < m){
    len = length(result);
    incr = m - front_y;
    PROTECT(result = lengthgets(result,len+incr));
    for(i = 0; i < incr; i++){
      SET_STRING_ELT(result,len+i,STRING_ELT(y,front_y+i));
    }
    UNPROTECT(1);
  }
  UNPROTECT(3);
  return result;
}
