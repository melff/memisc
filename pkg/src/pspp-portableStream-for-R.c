#include <R.h>
#include <Rdefines.h>
#include <string.h>
#include <Rmath.h>
#include <stdio.h>
#include <Rinternals.h>
#include "memisc.h"


/** Code translation **/ /*From foreign package*/
static const unsigned char latin1_tab[256] = {
    "                                                                "
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz ."
    "<(+|&[]!$*);^-/|,%_>?`:$@'=\"      ~-   0123456789   -() {}\\     "
    "                                                                "
};

//  0,0,0,0,0,  0,0,0,0,0, /* 0-9: control and reserved codes */
//  0,0,0,0,0,  0,0,0,0,0, /* 10-19: control codes */
//  0,0,0,0,0,  0,0,0,0,0, /* 20-29: control codes */
//  0,0,0,0,0,  0,0,0,0,0, /* 30-39: control codes */
//  0,0,0,0,0,  0,0,0,0,0, /* 40-49: control codes */
//  0,0,0,0,0,  0,0,0,0,0, /* 50-59: control codes */
//  0,0,0,0,               /* 60: control code, 61-63: `reserved' */
//  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', /* 64-73: digits */
//  'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', /* 74-99: captial letters A-Z*/
//  'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
//  'U', 'V', 'W', 'X', 'Y', 'Z',
//  'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', /* 100-25: lowercase letters a-z*/
//  'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
//  'u', 'v', 'w', 'x', 'y', 'z',
//  32,                     /* 126: space*/
//  46, 60, 40, 43,      /* 127-130: symbols .<(+     */
//  124, /* 131: solid vertical pipe */
//  38, 91, 93, 33, 36, 42, 41, 59, 94, 45, 47, /* 132-142: symbols &[]!$*);^-/ */
//  166, /* 143: broken vertical pipe */
//  44, 37, 95, 62, 63, 96, 58, /* 144-150: symbols ,%_>?`: */
//  163, /* 151: British pound symbol */
//  64, 39, 61, 34, /* 152-155: @'="*/
//  /** Symbols beyond 155 do usually not make sense in latin1 **/
//  32, /* 156: Less than or equal symbol*/
//  32, /* 157: Empty box */
//  177, /* 158: Plus or minus */
//  32, /* 159: Filled box */
//  176, /* 160: Degree symbol */
//  32, /* 161: Dagger */
//  126, /* 162: Tilde */
//  32, /* 163: En dash */
//  32 ,/* 164: Lower left corner box draw */
//  32, /* 165: Upper left corner box draw */
//  32, /* 166: Greater than or equal symbol */
//  32,32,32,32,32, 32,32,32,32,32, /* 167-176: Superscript 0 through 9 */
//  32, /* 177: Lower right corner box draw */
//  32, /* 178: Upper right corner box draw */
//  32, /* 179: Not equal symbol */
//  32, /* 180: Em dash */
//  32, /* 181: Superscript ( */
//  32, /* 182: Superscript ) */
//  32, /* 183: Horizontal dagger (?) */
//  123,125,92, /* 184-186: Symbols {}\ */
//  162, /* 187: Cents symbol */
//  183, /* 188: Centered dot/bullet */
//  0,
//  0, 0, 0, 0, 0,   0, 0, 0, 0, 0, /* 189-255: Reserved */
//  0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
//  0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
//  0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
//  0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
//  0, 0, 0, 0, 0,   0, 0, 0, 0, 0,
//  0, 0, 0, 0, 0, 0
// };

/** Number translation **/


static const unsigned char Por2int_tab[256] =
  {
    255,255,255,255,255,255,255,255,255,255, /*10*/
    255,255,255,255,255,255,255,255,255,255, /*10*/
    255,255,255,255,255,255,255,255,255,255, /*10*/
    255,255,255,255,255,255,255,255,255,255, /*10*/
    255,255,255,255,255,255,255,255,         /*8*/
    0,1,2,3,4,5,6,7,8,9,                     /*10*/
    255,255,255,255,255,255,255,             /*7*/
    10,11,12,13,14,15,16,17,18,19,20,        /*11*/
    21,22,23,24,25,26,27,28,29,              /*9  sum=85*/
    255,255,255,255,255,                     /*5*/
    255,255,255,255,255,255,255,255,255,255, /*10*/
    255,255,255,255,255,255,255,255,255,255, /*10*/
    255,255,255,255,255,255,255,255,255,255, /*10*/
    255,255,255,255,255,255,255,255,255,255, /*10*/
    255,255,255,255,255,255,255,255,255,255, /*10*/
    255,255,255,255,255,255,255,255,255,255, /*10*/
    255,255,255,255,255,255,255,255,255,255, /*10*/
    255,255,255,255,255,255,255,255,255,255, /*10*/
    255,255,255,255,255,255,255,255,255,255, /*10*/
    255,255,255,255,255,255,255,255,255,255, /*10*/
    255,255,255,255,255,255,255,255,255,255, /*10*/
    255,255,255,255,255,255,255,255,255,255, /*10*/
    255,255,255,255,255,255,255,255,255,255, /*10*/
    255,255,255,255,255,255,255,255,255,255, /*10*/
    255,255,255,255,255,255,255,255,255,255, /*10*/
    255,255,255,255,255,255,255,255,255,255, /*10*/
    255,255,255,255,255,255
  };


int Por2int(int len, char* text){
  int i, curr, target, stop, sign;
  sign = 1;
  curr = 1;
  target = 0;
  stop = 0;
  if(*text=='-'){
    sign = -1;
    stop = 1;
  }
  for(i = len - 1; i >=stop ; i--){
#ifdef DEBUG
    Rprintf("Source: %c\t",text[i]);
    Rprintf("Digit: %d\n",Por2int_tab[(int)text[i]]);
#endif
    target += curr*(int)Por2int_tab[(int)text[i]];
    curr *= 30;
  }
  return sign*target;
}

double Por2mantissa(int len, char* text){
  int i;
  double curr = 1./30., target;
  target = 0;
  for(i = 0; i < len ; i++){
#ifdef DEBUG
    Rprintf("Source: %c\t",text[i]);
    Rprintf("Digit: %d\n",Por2int_tab[(int)text[i]]);
#endif
    target += curr*(double)Por2int_tab[(int)text[i]];
#ifdef DEBUG
    Rprintf("Update: %f\n",curr*(double)Por2int_tab[(int)text[i]]);
    Rprintf("Target: %f\n",target);
#endif
    curr /= 30.;
  }
  return target;
}

double Por2double(int len, char* text){
  int sign=1;
  int exp_sign = 0;
  int exponent=0;
  int l_charact = len;
  char *t_mant = NULL;
  int l_mant = 0;
  char *t_exp = NULL;
  int l_exp = 0;
  char *end = text + len;
  char *tmp = text;
  double result = 0;
#ifdef DEBUG
  Rprintf("\nPor2double ----------------------------");
  Rprintf("\n input = %s",text);
#endif
  if(*text == '*') return NA_REAL;
  if(*text == '+') {
    text++;
    l_charact--;
    }
  if(*text == '-'){
    sign = -1;
    text++;
    l_charact--;
  }
  for(tmp = text; tmp < end; tmp++){
    if(*tmp == '.'){
      l_charact = tmp - text;
      tmp++;
      t_mant = tmp;
      l_mant = end - tmp;
      break;
    }
    if(*tmp == '+' || *tmp == '-'){
      l_charact = tmp - text;
      if(*tmp == '+')
        exp_sign = 1;
      if(*tmp == '-')
        exp_sign = -1;
      tmp++;
      t_exp = tmp;
      l_exp = end - tmp;
      exponent = Por2int(l_exp,t_exp);
      if(exp_sign == -1){ /** "un-normalize" **/
        if(exponent >= l_charact){
          l_mant = l_charact;
          l_charact = 0;
          exponent -= l_mant;
          exponent = -exponent;
          t_mant = text;
        }
        else {
          l_mant = exponent;
          l_charact -= exponent;
          t_mant = text + l_charact;
          exponent = 0;
        }
      }
      break;
    }
  }
  if(l_charact)
    result += (double)Por2int(l_charact,text);
  if(l_mant){
    result += Por2mantissa(l_mant, t_mant);
    }
  if(exponent != 0){
#ifdef DEBUG
      Rprintf("\n ####### Por2double ");
      Rprintf(" input = %s",text);
      Rprintf(" exponent = %d",exponent);
      Rprintf(" result = %f",result*R_pow_di(30,exponent));
#endif
      result *= R_pow_di(30,exponent);
    }

#ifdef DEBUG
  Rprintf("\nresult = %f",result);
#endif

  if(sign == -1)
    return -result;
  else
    return result;
}
/** porStreamBuf **/

#define BUFSIZE 85




typedef struct {
  FILE *f;
  unsigned char buf[BUFSIZE];
  int pos;
  int line;
  unsigned char translate[256];
  Rboolean at_end;
} porStreamBuf;

void initPorStreamBuf(porStreamBuf *b){
#ifdef DEBUG
  Rprintf("\ninitPorStreamBuf");
  Rprintf("\nb = %d",b);
#endif
  int i;
  for(i = 0; i < 256; i++) b->translate[i] = i;
  b->pos = 0;
  b->line = 0;
  b->at_end = FALSE;
}

/*
  in: codes as in the file
*/

void por_make_trans(porStreamBuf *b, const char *in){
  unsigned char i;
  unsigned char j;
  /*for(i = 64; i <= 188; i++){*/
  for(i = 188; i >= 64; i--){ /* Sometimes symbol translations are buggy,
          so the translation of chars and numbers comes later and thus
          maintains priority ... */
    j = (unsigned char)in[i];
    if(i <= 155 || i==126 || (i >= 184 && i<= 186) ){
    b->translate[j] = latin1_tab[i];
#ifdef DEBUG
    Rprintf("\ni=%d, in: '%c' out: '%c' ",i,in[i],latin1_tab[i]);
#endif
    }
  }
}
// #undef DEBUG

// #define DEBUG
int fillPorStreamBuf(porStreamBuf *b) {
#ifdef DEBUG
  Rprintf("\nfillPorStreamBuf");
  Rprintf("\nb = %d",b);
  Rprintf("\nb->buf = %d",b->buf);
#endif
  memset(b->buf,0,BUFSIZE);
#ifdef DEBUG
  Rprintf("\nfile = %d",b->f);
#endif
  if(feof(b->f)){
    b->pos = 0;
    b->at_end = TRUE;
    return 0;
  }
  int fpos = ftell(b->f);
  char *dummy = fgets((char *)b->buf,BUFSIZE,b->f);
  if(!dummy) {
      fseek(b->f,fpos,SEEK_SET);
      int idummy = fread((char *)b->buf,1,BUFSIZE,b->f);

      error("cannot read from file at pos %d (fread result = <%s>)",fpos,b->buf);
  }

#ifdef DEBUG
  Rprintf("\nbuffer = |%s|",b->buf);
#endif
  int i, len = strlen((char *)b->buf), prtlen = 0;
  for(i = 0; i < len; i++) b->buf[i] = b->translate[(int)b->buf[i]];
  /* The following is for buggy portable files with short lines */

  for(i = len; i >= 0; i++){
    if (b->buf[i] >= 32) /* found a printable character*/ {
      prtlen = i+1;
      break;
    }
  }
  if(80 - prtlen > 0){ /* pad short lines with spaces */
    warning("short line encountered");
    memset(b->buf+prtlen-1,' ',80 - prtlen);
    }
  memset(b->buf+80,'\0',BUFSIZE - 80 - 1);
#ifdef DEBUG
  Rprintf("\ncooked buffer = |%s|",b->buf);
#endif
  b->pos = 0;
  b->line++;
  return prtlen;
}
#undef DEBUG

/** Internal functions to read from a porStreamBuf **/


void printPorStreamBuf(porStreamBuf *b){
    Rprintf("\nbuffer = |%s|",b->buf);
}

char readOnePushbackPorStream1(porStreamBuf *b) {
#ifdef DEBUG
  Rprintf("\nreadOnePushbackPorStream1");
  Rprintf("\nb = %d",b);
  Rprintf("\nb->buf = %d",b->buf);
  Rprintf("\nbuffer = |%s|",b->buf);
#endif
  if(b->pos < 80){
#ifdef DEBUG
    Rprintf("\nans=%c",b->buf[b->pos]);
#endif
    return b->buf[b->pos];
    }
  else {
    fillPorStreamBuf(b);
#ifdef DEBUG
    Rprintf("\nans=%c",b->buf[0]);
#endif
    return b->buf[0];
  }
}

char readOnePorStream1(porStreamBuf *b) {
#ifdef DEBUG
  Rprintf("\nreadOnePorStream1");
  Rprintf("\nb = %d",b);
  Rprintf("\nb->buf = %d",b->buf);
  Rprintf("\nbuffer = |%s|",b->buf);
#endif
  char ans;
  if(b->pos < 80)
    ans = b->buf[b->pos];
  else {
    fillPorStreamBuf(b);
    b->pos = 0;
    ans = b->buf[0];
  }
  b->pos++;
#ifdef DEBUG
  Rprintf("\nans=%c",ans);
#endif
  return ans;
}

Rboolean atEndPorStream(porStreamBuf *b){
    /*char test =*/ readOnePushbackPorStream1(b);
    return b->at_end/* || test == 'Z'*/;
}


#define HardMaxRead 5*80

char *readPorStream1(porStreamBuf *b, int n) {
#ifdef DEBUG
  Rprintf("\nreadPorStream1");
  Rprintf("\nb = %d",b);
  Rprintf("\nb->buf = %d",b->buf);
  Rprintf("\nbuffer = |%s|",b->buf);
  Rprintf("\nn = %d",n);
#endif
  if(n > HardMaxRead) n = HardMaxRead;
  if(b->pos == 80) fillPorStreamBuf(b);
  char *ans = S_alloc(n+1,1);
  if(b->pos + n <= 80){
    memcpy(ans, b->buf + b->pos, n);
    b->pos += n;
#ifdef DEBUG
    Rprintf("\nans =  %s",ans);
    Rprintf("\nEND readPorStream1");
#endif
    return ans;
  }
  /* else */
  char *tmp = ans;
  int nread;
  if(80 - b->pos > 0){
    nread = 80 - b->pos;
    memcpy(ans,b->buf + b->pos, nread);
    n -= nread;
    tmp += nread;
    b->pos = 0;
    fillPorStreamBuf(b);
  }
  int i, nlines = n/80, remainder = n%80;
  for(i = 0; i < nlines; i++){
    memcpy(tmp,b->buf,80);
    tmp += 80;
    fillPorStreamBuf(b);
  }
  if(remainder > 0)
    memcpy(tmp,b->buf,remainder);
  b->pos = remainder;
#ifdef DEBUG
  Rprintf("\nans =  %s",ans);
  Rprintf("\nEND readPorStream1");
#endif
  return ans;
}

int readPorStreamTo(porStreamBuf *b, char *target, int n) {
  if(n > HardMaxRead) n = HardMaxRead;
  if(b->pos == 80) fillPorStreamBuf(b);
  if(b->pos + n <= 80){
    memcpy(target, b->buf + b->pos, n);
    b->pos += n;
    return n;
  }
  /* else */
  char *tmp = target;
  int nread;
  if(80 - b->pos > 0){
    nread = 80 - b->pos;
    memcpy(target,b->buf + b->pos, nread);
    n -= nread;
    tmp += nread;
    b->pos = 0;
    fillPorStreamBuf(b);
  }
  int i, nlines = n/80, remainder = n%80;
  for(i = 0; i < nlines; i++){
    memcpy(tmp,b->buf,80);
    tmp += 80;
    fillPorStreamBuf(b);
  }
  if(remainder > 0)
    memcpy(tmp,b->buf,remainder);
  b->pos = remainder;
#ifdef DEBUG
  Rprintf("\ntarget =  %s",target);
  Rprintf("\nEND readPorStreamTo");
#endif
  return n;
}

int seekPorStream1(porStreamBuf *b, int pos) {
  fseek(b->f,0,SEEK_SET);
  b->line = 0;
  int i, nlines = pos/80, remainder = pos%80;
#ifdef DEBUG
  Rprintf("\nseekPorStream1");
  Rprintf("\npos = %d",pos);
  Rprintf("\nb = %d",b);
  Rprintf("\nb->buf = %d",b->buf);
  Rprintf("\nbuffer = |%s|",b->buf);
  Rprintf("\nremainder = %d",remainder);
  Rprintf("\n#################################################");
#endif
  for(i = 0; i < nlines; i++){
    fillPorStreamBuf(b);
#ifdef DEBUG
    Rprintf("\ni=%d, buffer = |%s|",i,b->buf);
#endif
  }
  fillPorStreamBuf(b);
  b->pos = remainder;
#ifdef DEBUG
    Rprintf("\ni=%d, buffer = |%s|",i,b->buf);
    Rprintf("\nbuffer remainder = |%s|",b->buf+remainder);
#endif
  return remainder;
}

int slashpos(unsigned char* str, size_t n){
  int i;
  for(i = 0; i < n; i++){
    if(str[i] == '/' || str[i] == '*')
      return i+1;
  }
  return 0;
}


int readToSlashPorStream1(porStreamBuf *b, char *ans, int n){
#ifdef DEBUG
  Rprintf("\nreadToSlashPorStream1");
  Rprintf("\nbuffer = |%s|",b->buf);
  Rprintf("\nb->pos = %d",b->pos);
  Rprintf("\nn = %d",n);
#endif
  if(n > HardMaxRead) n = HardMaxRead;
  if(b->pos == 80) fillPorStreamBuf(b);
  int len, slashp = slashpos(b->buf+b->pos,80 - b->pos);
  if(slashp){
    memcpy(ans,b->buf+b->pos,slashp);
    b->pos += slashp;
#ifdef DEBUG
    Rprintf("\nb->pos = %d",b->pos);
    Rprintf("\nans = %s",ans);
#endif
    return slashp;
  }
  else {
    len = 80 - b->pos;
    memcpy(ans,b->buf + b->pos,len);
    int i, maxlines = (n + b->pos)/80+1 ;
#ifdef DEBUG
    Rprintf("\nmaxlines = %d",maxlines);
#endif
    for(i = 0; i < maxlines; i++){
#ifdef DEBUG
      Rprintf("\ncurrent = %s",ans);
#endif
      fillPorStreamBuf(b);
      slashp = slashpos(b->buf,80);
#ifdef DEBUG
      Rprintf("\nslashp = %d",slashp);
#endif
      if(slashp){
        memcpy(ans+len,b->buf,slashp);
        b->pos = slashp;
#ifdef DEBUG
        Rprintf("\nans = %s",ans);
#endif
        return slashp;
      } else {
        int remainder = (n - len >= 80 ? 80 : n - len);
        memcpy(ans+len,b->buf,remainder);
        len+=80;
        if(len > n) break;
      }
    }
// #ifdef DEBUG
    Rprintf("\nWARNING: slash not found");
    Rprintf("\nans = %s",ans);
// #endif
    return -1;
  }
}

int readIntPorStream1 (porStreamBuf *b){
#ifdef DEBUG
  Rprintf("\nreadIntPorStream1");
  Rprintf("\nb = %d",b);
  Rprintf("\nb->buf = %d",b->buf);
  Rprintf("\nbuffer = |%s|",b->buf);
#endif
    /* Check for missing value */
    if(readOnePushbackPorStream1(b) == '*'){
        readOnePorStream1(b);
        if(readOnePorStream1(b) != '.') warning("malformed sysmis entry");
        return NA_INTEGER;
    }
    char ans[80];
    memset(ans,0,80);
    readToSlashPorStream1 (b,ans,80);
#ifdef DEBUG
    Rprintf("\nans =  |%s|",ans);
#endif
    ans[strlen(ans)-1] = '\0'; /* chop off '/'*/
#ifdef DEBUG
    Rprintf("\nans =  %s",ans);
#endif
    int len = strlen(ans);
#ifdef DEBUG
    Rprintf("\nresult =  %d",Por2int(len,ans));
#endif

    return Por2int(len,ans);
}


double readDoublePorStream1 (porStreamBuf *b){
#ifdef DEBUG
  Rprintf("\nreadDoublePorStream1");
  Rprintf("\nb = %d",b);
  Rprintf("\nb->buf = %d",b->buf);
  Rprintf("\nbuffer = |%s|",b->buf);
#endif
    /* Check for missing value */
    if(readOnePushbackPorStream1(b) == '*'){
        readOnePorStream1(b);
        if(readOnePorStream1(b) != '.') warning("malformed sysmis entry");
        return NA_REAL;
    }

    char ans[80];
    memset(ans,0,80);
    readToSlashPorStream1 (b,ans,80);
#ifdef DEBUG
    Rprintf("\nans=|%s|",ans);
#endif

    /*if(ans[strlen(ans)-1] == '*') return NA_REAL;*/
    ans[strlen(ans)-1] = '\0'; /* chop off '/'*/
    int len = strlen(ans);

#ifdef DEBUG
    Rprintf("\tresult = |%f|",Por2double(len,ans));
#endif
    return Por2double(len,ans);
}


char* readStringPorStream1(porStreamBuf *b){
#ifdef DEBUG
  Rprintf("\nreadStringPorStream1");
  Rprintf("\nb = %d",b);
  Rprintf("\nb->buf = %d",b->buf);
  Rprintf("\nbuffer = |%s|",b->buf);
#endif
    int len = readIntPorStream1 (b);
#ifdef DEBUG
    Rprintf("\nLength: %d",len);
#endif
    char *ans = readPorStream1(b,len);
#ifdef DEBUG
    Rprintf("\nString: %s",ans);
#endif
    return ans;
}




/** *******************************************************/

/** PorStream objects **/

SEXP closePorStream (SEXP);

SEXP NewPorStream (SEXP name){
#ifdef DEBUG
  Rprintf("\nNewPorStream");
#endif
  PROTECT(name = coerceVector(name,STRSXP));
//   porStreamBuf *b = (porStreamBuf *)S_alloc(1,sizeof(porStreamBuf));
  porStreamBuf *b = Calloc(1,porStreamBuf);
  initPorStreamBuf(b);
  b->f = fopen(CHAR(STRING_ELT(name, 0)),"rb");
#ifdef DEBUG
  Rprintf("\nfile = %d",b->f);
#endif
  if(b->f == NULL){
    UNPROTECT(1);
    return R_NilValue;
  }
  else {
    fillPorStreamBuf(b);
    SEXP ans = R_MakeExternalPtr(b, install("porStreamBuf"), R_NilValue);
    R_RegisterCFinalizer(ans, (R_CFinalizer_t) closePorStream);
    setAttrib(ans,install("file.name"),name);
    UNPROTECT(1);
    return ans;
  }
}

porStreamBuf *get_porStreamBuf(SEXP porStream){
  if(TYPEOF(porStream) != EXTPTRSXP || R_ExternalPtrTag(porStream) != install("porStreamBuf"))
    error("not a porStream");
  porStreamBuf *b = R_ExternalPtrAddr(porStream);
  if (b == NULL){
    b = Calloc(1,porStreamBuf);
    R_SetExternalPtrAddr(porStream,b);
    initPorStreamBuf(b);
    SEXP name = getAttrib(porStream,install("file.name"));
    if(name == R_NilValue || name == NULL){
      R_SetExternalPtrAddr(porStream,NULL);
      Free(b);
      error("need filename to reopen file");
      }
    b->f = fopen(CHAR(STRING_ELT(name, 0)),"rb");
    if(b->f == NULL){
      R_SetExternalPtrAddr(porStream,NULL);
      Free(b);
      error("cannot reopen file -- does it still exist?");
    }
    Rprintf("File '%s' reopened\n",CHAR(STRING_ELT(name, 0)));
  }
  if (b == NULL) error("something strange happened here!?");
  return(b);
}

SEXP closePorStream (SEXP porStream){
  if(TYPEOF(porStream) != EXTPTRSXP || R_ExternalPtrTag(porStream) != install("porStreamBuf"))
    error("not a porStream");
  porStreamBuf *b = R_ExternalPtrAddr(porStream);
  if (b != NULL) {
      fclose(b->f);
      /*Rprintf("\n'portable' file closed\n");*/
      R_ClearExternalPtr(porStream);
  }
  return R_NilValue;
}

/** R interface and routines **/

SEXP tellPorStream (SEXP porStream){
  porStreamBuf *b = get_porStreamBuf(porStream);
  return ScalarInteger(80*(b->line-1)+b->pos);
}

SEXP lineTellPorStream (SEXP porStream){
  porStreamBuf *b = get_porStreamBuf(porStream);
  return ScalarInteger(b->line);
}

SEXP offsetTellPorStream (SEXP porStream){
  porStreamBuf *b = get_porStreamBuf(porStream);
  return ScalarInteger(b->pos);
}


SEXP readOnePushbackPorStream(SEXP porStream){
  porStreamBuf *b = get_porStreamBuf(porStream);
  char ans[2];
  memset(ans,0,2);
  ans[0] = readOnePushbackPorStream1(b);
  return mkString(ans);
}

SEXP readOnePorStream(SEXP porStream){
  porStreamBuf *b = get_porStreamBuf(porStream);
  char ans[2];
  memset(ans,0,2);
  ans[0] = readOnePorStream1(b);
  return mkString(ans);
}

SEXP readPorStream(SEXP porStream, SEXP s_n){
  porStreamBuf *b = get_porStreamBuf(porStream);
  int n = asInteger(s_n);
  return mkString(readPorStream1(b,n));
}

SEXP seekPorStream(SEXP porStream, SEXP s_pos){
  porStreamBuf *b = get_porStreamBuf(porStream);
  int pos = asInteger(s_pos);
  return ScalarInteger(seekPorStream1(b,pos));
}

SEXP readToSlashPorStream(SEXP porStream, SEXP s_n){
  porStreamBuf *b = get_porStreamBuf(porStream);
  int n = asInteger(s_n);
  char *ans = S_alloc(n,1);
  readToSlashPorStream1(b,ans,n);
  return mkString(ans);
}

SEXP readIntPorStream(SEXP porStream){
  porStreamBuf *b = get_porStreamBuf(porStream);
  return ScalarInteger(readIntPorStream1(b));
}

SEXP readDoublePorStream(SEXP porStream){
  porStreamBuf *b = get_porStreamBuf(porStream);
  return ScalarReal(readDoublePorStream1(b));
}

char* readCHARPorStream(porStreamBuf *b,char *buf,int len){
    memset(buf,0,len);
    int n = readIntPorStream1 (b);
    if(atEndPorStream(b)) return buf;
    if(n > len) error("string has length %d but should have maximal length %d",n,len);
    /*SEXP ans;
    PROTECT(ans = allocString(n));
    char *cans = CHAR(ans);*/
    readPorStreamTo(b,buf,n);
    buf[n] = 0;
    /*UNPROTECT(1);*/
    return buf;
}

SEXP readStringPorStream(SEXP porStream){
  porStreamBuf *b = get_porStreamBuf(porStream);
  return ScalarString(mkChar(readStringPorStream1(b)));
}

SEXP setTranslationPorStream(SEXP porStream, SEXP s_in){
  porStreamBuf *b = get_porStreamBuf(porStream);
  SEXP s_out;
  PROTECT(s_out = allocVector(RAWSXP,256));
  PROTECT(s_in = asChar(s_in));
  const char *in = CHAR(s_in);
  por_make_trans(b,in);
  memcpy(RAW(s_out),&(b->translate[0]),256);
  UNPROTECT(2);
  return s_out;
}

SEXP readDataPorStream(SEXP porStream, SEXP what, SEXP s_n, SEXP s_types){
#ifdef DEBUG
  Rprintf("\n############################");
  Rprintf("\n#readDataPorStream");
  Rprintf("\n############################");
#endif
  porStreamBuf *b = get_porStreamBuf(porStream);
  int n = asInteger(s_n);

#ifdef DEBUG
  Rprintf("\nRequired number of cases: %d",n);
  Rprintf("\nBuffer contents: |%s|",b->buf);
  Rprintf("\nLine: %d",b->line);
  Rprintf("\nPosition: %d",b->pos);
  Rprintf("\nBuffer remainder: %s",b->buf + b->pos);
#endif
  PROTECT(s_types = coerceVector(s_types,INTSXP));
  int nvar = length(s_types);
  int *types = INTEGER(s_types);

  SEXP x, y, data;
  char *charbuf;
  int charbuflen = 0;
  PROTECT(data = allocVector(VECSXP,nvar));
  int i,j;
  for(j = 0; j < nvar; j++){
    if(types[j]==0)
      SET_VECTOR_ELT(data,j,allocVector(REALSXP,n));
    else {
      SET_VECTOR_ELT(data,j,allocVector(STRSXP,n));
      if(types[j] > charbuflen) charbuflen = types[j];
      }
  }
  charbuf = R_alloc(charbuflen+1,sizeof(char));

#ifdef DEBUG
//   PrintValue(data);
#endif

  for(i = 0; i < n; i++){
    if(atEndPorStream(b) || (b->pos < 80 && b->buf[b->pos] == 'Z')){
#ifdef DEBUG
      Rprintf("\nReached end of cases at i=%d",i);
#endif
      int new_length = i;
      for(j = 0; j < nvar; j++){
        x = VECTOR_ELT(data,j);
        SET_VECTOR_ELT(data,j,lengthgets(x,new_length));
      }
      n = new_length;
      break;
    }
#ifdef DEBUG
    Rprintf("\nCase number: %d\n",i);
#endif
    for(j = 0; j < nvar; j++){
      if(atEndPorStream(b)) {
          printPorStreamBuf(b);
          warning("\nPremature end of data");
          break;
      }
#ifdef DEBUG
      PrintValue(VECTOR_ELT(data,j));
#endif
      if(types[j]==0) REAL(VECTOR_ELT(data,j))[i] = readDoublePorStream1(b);
      else SET_STRING_ELT(VECTOR_ELT(data,j), i,
                          mkChar(readCHARPorStream(b,charbuf,types[j])));
#ifdef DEBUG
      if(i<3 && types[j]>0)
      PrintValue(STRING_ELT(VECTOR_ELT(data,j),i));
#endif
      }
    }
  for(j = 0; j < nvar; j++){
    x = VECTOR_ELT(what,j);
    y = VECTOR_ELT(data,j);
    copyMostAttrib(x,y);
  }
  UNPROTECT(2);
  return data;
}

SEXP countCasesPorStream(SEXP porStream, SEXP s_types){
#ifdef DEBUG
  Rprintf("\n############################");
  Rprintf("\n#countCasesPorStream");
  Rprintf("\n############################");
#endif
  porStreamBuf *b = get_porStreamBuf(porStream);

#ifdef DEBUG
  Rprintf("\nBuffer contents: |%s|",b->buf);
  Rprintf("\nLine: %d",b->line);
  Rprintf("\nPosition: %d",b->pos);
  Rprintf("\nBuffer remainder: %s",b->buf + b->pos);
#endif
  PROTECT(s_types = AS_INTEGER(s_types));
  int nvar = LENGTH(s_types);
  int *types = INTEGER(s_types);


  int i, j, k;
  char *charbuf;
  int charbuflen = 0;
  for(j = 0; j < nvar; j++){
      if(types[j]!=0 && types[j] > charbuflen) charbuflen = types[j];
      k++;
  }
  charbuf = R_alloc(charbuflen+1,sizeof(char));

  for(i = 0; ; i++){
#ifdef DEBUG
    Rprintf("\n===================");
    Rprintf("\nCase nr. %d",i);
    Rprintf("\nBuffer contents: |%s|",b->buf);
    Rprintf("\nLine: %d",b->line);
    Rprintf("\nPosition: %d",b->pos);
    Rprintf("\nCurrent char: '%c'",b->buf[b->pos]);
    Rprintf("\nBuffer remainder: %s",b->buf + b->pos);
#endif
    if(atEndPorStream(b) || (b->pos < 80 && b->buf[b->pos] == 'Z')){
#ifdef DEBUG
      Rprintf("\nReached end of cases at i=%d",i);
#endif
      break;
    }
#ifdef DEBUG
    Rprintf("\nCase number: %d  nvar = %d\n",i,nvar);
#endif
    for(j = 0; j < nvar; j++){
      if(atEndPorStream(b)) {
          printPorStreamBuf(b);
          warning("\nPremature end of data");
          break;
      }
#ifdef DEBUG1
      Rprintf("\n(j = %d)",j);
      if(types[j]==0) Rprintf(" %f",readDoublePorStream1(b));
      else Rprintf(" '%s'",readCHARPorStream(b,charbuf,types[j]));
#else
      if(types[j]==0) readDoublePorStream1(b);
      else readCHARPorStream(b,charbuf,types[j]);
#endif
      }
#ifdef DEBUG
    Rprintf("\n");
#endif
    }
  UNPROTECT(1);
  return ScalarInteger(i);
}
#undef DEBUG

SEXP readSubsetPorStream(SEXP porStream, SEXP what, SEXP s_vars, SEXP s_cases, SEXP s_types){
  porStreamBuf *b = get_porStreamBuf(porStream);
  PROTECT(s_vars = coerceVector(s_vars,LGLSXP));
  PROTECT(s_cases = coerceVector(s_cases,LGLSXP));
  PROTECT(s_types = coerceVector(s_types,INTSXP));
  int nvar = length(s_types);
  int ncases = length(s_cases);
  int *types = INTEGER(s_types);
  if(LENGTH(s_vars)!=nvar) error("\'s_vars\' argument has wrong length");

  int ii,i,j,k, m=0, n = 0;
  for(j = 0; j < nvar; j++) m+=LOGICAL(s_vars)[j];
  for(i = 0; i < ncases; i++) n+=LOGICAL(s_cases)[i];

  SEXP x, y, data;
  char *charbuf;
  int charbuflen = 0;
  PROTECT(data = allocVector(VECSXP,m));
  k = 0;
  for(j = 0; j < nvar; j++){
    if(types[j] > charbuflen) charbuflen = types[j];
    if(LOGICAL(s_vars)[j]){
      if(types[j]==0)
        SET_VECTOR_ELT(data,k,allocVector(REALSXP,n));
      else {
        SET_VECTOR_ELT(data,k,allocVector(STRSXP,n));
        }
      k++;
    }
  }
  charbuf = R_alloc(charbuflen+1,sizeof(char));
  ii = 0;
  for(i = 0; i < ncases; i++){
    if(atEndPorStream(b) || (b->pos < 80 && b->buf[b->pos] == 'Z')){
      int new_length = ii;
      for(j = 0; j < m; j++){
        x = VECTOR_ELT(data,j);
        SET_VECTOR_ELT(data,j,lengthgets(x,new_length));
      }
      n = new_length;
      break;
    }
    if(LOGICAL(s_cases)[i]){
      k = 0;
      for(j = 0; j < nvar; j++){
        if(atEndPorStream(b)) {
            printPorStreamBuf(b);
            warning("\nPremature end of data");
        }
        if(types[j]==0){
          if(LOGICAL(s_vars)[j]){
            REAL(VECTOR_ELT(data,k))[ii] = readDoublePorStream1(b);
            k++;
          }
          else {
            readDoublePorStream1(b);
          }
        }
        else {
          if(LOGICAL(s_vars)[j]){
            SET_STRING_ELT(VECTOR_ELT(data,k), ii,
                              mkChar(readCHARPorStream(b,charbuf,types[j])));
            k++;
          }
          else {
            readCHARPorStream(b,charbuf,types[j]);
          }
        }
      }
      ii++;
    }
    else {
      for(j = 0; j < nvar; j++){
        if(atEndPorStream(b)) {
            printPorStreamBuf(b);
            error("\nPremature end of data");
        }
        if(types[j]==0) readDoublePorStream1(b);
        else readCHARPorStream(b,charbuf,types[j]);
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
