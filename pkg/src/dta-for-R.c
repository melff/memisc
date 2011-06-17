#include <R.h>
#include <Rdefines.h>
#include <string.h>
#include <Rmath.h>
#include <stdio.h>
#include <Rinternals.h>
#include "memisc.h"
#include "dumbswap.h"

#define asString(x) CHAR(asChar(x))

#define dta_HILO 1
#define dta_LOHI 2
#ifdef WORDS_BIGENDIAN
#define MY_DTA_ENDIAN dta_HILO
#else
#define MY_DTA_ENDIAN dta_LOHI
#endif

#define DTA_MAXSTR    244
#define DTA_BYTE      251
#define DTA_SHORT     252
#define DTA_LONG      253
#define DTA_FLOAT     254
#define DTA_DOUBLE    255

#define DTA_NA_BYTE   0x7f
#define DTA_NA_SHORT  0x7fff
#define DTA_NA_LONG   0x7fffffff
#define DTA_NA_FLOAT  dta_na_float
#define DTA_NA_DOUBLE dta_na_double

typedef struct {
  FILE *f;
  int start_data;
  int l_record;
  int n_records;
  int swap;
} dta_file;

static double dta_na_float;
static double dta_na_double;

char headbuf[109];

SEXP dta_file_finalize(SEXP);
SEXP dta_file_open (SEXP name){
  dta_file *dtaf = Calloc(1,dta_file);
  dtaf->swap = 0;
  dtaf->start_data = 0;
  dtaf->l_record = 0;
  dtaf->n_records = 0;
  dtaf->f = fopen(asString(name),"r+b");
  if (dtaf->f == NULL){
      Free(dtaf);
      error("cannot open file");
      }
  SEXP ans = R_MakeExternalPtr(dtaf, install("dta_file"), R_NilValue);
  R_RegisterCFinalizer(ans, (R_CFinalizer_t) dta_file_finalize);
  setAttrib(ans,install("file.name"),name);
  return ans;
}

SEXP dta_file_finalize(SEXP s_file)
{
    if(TYPEOF(s_file) != EXTPTRSXP || R_ExternalPtrTag(s_file) != install("dta_file")) error("not a Stata file");
    dta_file *dtaf = R_ExternalPtrAddr(s_file);
    Rprintf("closing file %s\n",asString(getAttrib(s_file,install("filename"))));
    if (dtaf->f != NULL)
        fclose(dtaf->f);
    R_ClearExternalPtr(s_file);
    return R_NilValue;
}

dta_file *get_dta_file(SEXP s_file){
  if(TYPEOF(s_file) != EXTPTRSXP || R_ExternalPtrTag(s_file) != install("dta_file")) error("not an Stata file");
  dta_file *dtaf = R_ExternalPtrAddr(s_file);
  if (dtaf == NULL){
    dta_file *dtaf = Calloc(1,dta_file);
    dtaf->swap = 0;
    R_SetExternalPtrAddr(s_file,dtaf);
    SEXP name = getAttrib(s_file,install("filename"));
    if(name == R_NilValue || name == NULL){
      Free(dtaf);
      error("need filename to reopen file");
      }
    dtaf->f = fopen(CHAR(STRING_ELT(name, 0)),"r+b");
    if(dtaf->f == NULL){
      Free(dtaf);
      error("cannot reopen file -- does it still exist?");
    }
    Rprintf("File '%s' reopened\n\n",asString(name));
  }
  return(dtaf);
}


int dta_read_byte(dta_file *dtaf){
  char target;
  int read_len = fread(&target,1,1,dtaf->f);
  if(!read_len) return NA_INTEGER;
  if(target == DTA_NA_BYTE) return NA_INTEGER;
  else return (int)target;
}

int dta_read_short(dta_file *dtaf){
  short target;
  int read_len = fread(&target,2,1,dtaf->f);
  if(!read_len) return NA_INTEGER;
  sswap_if(target,dtaf->swap);
  if(target == DTA_NA_SHORT) return NA_INTEGER;
  return (int)target;
}

int dta_read_int(dta_file *dtaf){
  int target;
  int read_len = fread(&target,4,1,dtaf->f);
  if(!read_len) return NA_INTEGER;
  iswap_if(target,dtaf->swap);
  if(target == DTA_NA_LONG) return NA_INTEGER;
  return (int)target;
}

double dta_read_float(dta_file *dtaf){
  float target;
  int read_len = fread(&target,4,1,dtaf->f);
  if(!read_len) return NA_REAL;
  fswap_if(target,dtaf->swap);
  if(target == DTA_NA_FLOAT) return NA_REAL;
  return (double)target;
}

double dta_read_double(dta_file *dtaf){
  double target;
  int read_len = fread(&target,8,1,dtaf->f);
  if(!read_len) return NA_REAL;
  dswap_if(target,dtaf->swap);
  if(target == DTA_NA_DOUBLE) return NA_REAL;
  return target;
}

int dta_read_string(dta_file *dtaf, char* target, int nchar){
  int read_len = fread(target,1,nchar,dtaf->f);
  return read_len;
}

int dta_skip_record(dta_file *dtaf){
  return fseek(dtaf->f,dtaf->l_record,SEEK_CUR);
}

SEXP dta_skip_records(SEXP s_dta_file, SEXP s_n){
  int i, n = asInteger(s_n);
  dta_file *dtaf = get_dta_file(s_dta_file);
  for(i = 0; i < n; i++)
    dta_skip_record(dtaf);
  return R_NilValue;
}

SEXP dta_ftell (SEXP s_file){
  dta_file *dtaf = get_dta_file(s_file);
  return ScalarInteger(ftell(dtaf->f));
}

SEXP dta_feof (SEXP s_file){
  dta_file *dtaf = get_dta_file(s_file);
  return ScalarLogical(feof(dtaf->f));
}


SEXP dta_fseek (SEXP s_file, SEXP s_pos, SEXP s_whence){
  int seek_code[] = { SEEK_SET, SEEK_CUR, SEEK_END };
  dta_file *dtaf = get_dta_file(s_file);
  PROTECT(s_pos = AS_INTEGER(s_pos));
  PROTECT(s_whence = AS_INTEGER(s_whence));
  long pos = INTEGER(s_pos)[0];
  int whence = INTEGER(s_whence)[0]-1;
  if(whence > 2) return ScalarLogical(FALSE);
  int retcode = fseek(dtaf->f,pos,seek_code[whence]);
  UNPROTECT(2);
  if (retcode == 0) return ScalarLogical(TRUE);
  else return ScalarLogical(FALSE);
}


SEXP dta_read_version(SEXP s_dta_file){
  char ds_format;
  dta_file *dtaf = get_dta_file(s_dta_file);
  rewind(dtaf->f);
  fread(&ds_format,1,1,dtaf->f);
  return ScalarInteger(ds_format);
}

SEXP dta_read_header(SEXP s_dta_file, SEXP s_lablen){
  dta_na_float = pow(2.0,0x7f);
  dta_na_double = pow(2.0,0x3ff);
  char byteorder;
  char filetype;
  int nvar;
  int nobs;
  int lablen = asInteger(s_lablen) + 1;
  char data_label[81];
  char time_stamp[18];
  SEXP ans, names;
  dta_file *dtaf = get_dta_file(s_dta_file);
  fseek(dtaf->f,1,SEEK_SET);
  fread(&byteorder,1,1,dtaf->f);
  if(byteorder != MY_DTA_ENDIAN) dtaf->swap = 1;
  fread(&filetype,1,1,dtaf->f);
  dta_read_byte(dtaf);
  nvar = dta_read_short(dtaf);
  nobs = dta_read_int(dtaf);
  dtaf->n_records = nobs;
  dta_read_string(dtaf,data_label,lablen);
  dta_read_string(dtaf,time_stamp,18);
  PROTECT(ans = allocVector(VECSXP,7));
  PROTECT(names = allocVector(STRSXP,7));
  SET_VECTOR_ELT(ans,0,ScalarInteger(byteorder));
  SET_VECTOR_ELT(ans,1,ScalarInteger(filetype));
  SET_VECTOR_ELT(ans,2,ScalarInteger(nvar));
  SET_VECTOR_ELT(ans,3,ScalarInteger(nobs));
  SET_VECTOR_ELT(ans,4,mkString(data_label));
  SET_VECTOR_ELT(ans,5,mkString(time_stamp));
  SET_VECTOR_ELT(ans,6,ScalarInteger(dtaf->swap));
  SET_STRING_ELT(names,0,mkChar("byteorder"));
  SET_STRING_ELT(names,1,mkChar("filetype"));
  SET_STRING_ELT(names,2,mkChar("nvar"));
  SET_STRING_ELT(names,3,mkChar("nobs"));
  SET_STRING_ELT(names,4,mkChar("data_label"));
  SET_STRING_ELT(names,5,mkChar("time_stamp"));
  SET_STRING_ELT(names,6,mkChar("swapcode"));
  SET_NAMES(ans,names);
  UNPROTECT(2);
  return ans;
}

SEXP dta_read_descriptors(SEXP s_dta_file, SEXP s_nvar, SEXP s_len_varname, SEXP s_len_fmt, SEXP s_len_lblname){
  dta_file *dtaf = get_dta_file(s_dta_file);
  int j, nvar = asInteger(s_nvar);
  int l_varn = asInteger(s_len_varname) + 1;
  int l_fmt = asInteger(s_len_fmt) + 1;
  int l_lbl = asInteger(s_len_lblname) + 1;
  SEXP typelist, varlist, srtlist, fmtlist, lbllist, ans, names;
  char *varname;
  char *fmt;
  char *lbl;
  varname = R_alloc(l_varn,1);
  fmt = R_alloc(l_fmt,1);
  lbl = R_alloc(l_lbl,1);
  PROTECT(typelist = allocVector(RAWSXP,nvar));
  PROTECT(varlist = allocVector(STRSXP,nvar));
  PROTECT(srtlist = allocVector(INTSXP,nvar));
  PROTECT(fmtlist = allocVector(STRSXP,nvar));
  PROTECT(lbllist = allocVector(STRSXP,nvar));
  PROTECT(ans = allocVector(VECSXP,5));
  PROTECT(names = allocVector(STRSXP,5));
  
  dta_read_string(dtaf,(char *)RAW(typelist),nvar);
  for(j = 0; j < nvar; j++){
    dta_read_string(dtaf,varname,l_varn);
    SET_STRING_ELT(varlist,j,mkChar(varname));
  }
  for(j = 0; j < nvar; j++)
    INTEGER(srtlist)[j] = dta_read_short(dtaf);
  dta_read_short(dtaf);
  for(j = 0; j < nvar; j++){
    dta_read_string(dtaf,fmt,l_fmt);
    SET_STRING_ELT(fmtlist,j,mkChar(fmt));
  }
  for(j = 0; j < nvar; j++){
    dta_read_string(dtaf,lbl,l_lbl);
    SET_STRING_ELT(lbllist,j,mkChar(lbl));
  }
  SET_VECTOR_ELT(ans,0,typelist);
  SET_VECTOR_ELT(ans,1,varlist);
  SET_VECTOR_ELT(ans,2,srtlist);
  SET_VECTOR_ELT(ans,3,fmtlist);
  SET_VECTOR_ELT(ans,4,lbllist);
  SET_STRING_ELT(names,0,mkChar("typelist"));
  SET_STRING_ELT(names,1,mkChar("varlist"));
  SET_STRING_ELT(names,2,mkChar("srtlist"));
  SET_STRING_ELT(names,3,mkChar("fmtlist"));
  SET_STRING_ELT(names,4,mkChar("lbllist"));
  SET_NAMES(ans,names);
  UNPROTECT(7);
  return ans;
}

SEXP dta_trans_types(SEXP s_types){
  int i,nvar = LENGTH(s_types);
  SEXP typelist;
  PROTECT(typelist = allocVector(RAWSXP,nvar));
  for(i = 0; i < nvar; i++)
    switch(RAW(s_types)[i]){
      case 'b': RAW(typelist)[i] = DTA_BYTE; break;
      case 'i': RAW(typelist)[i] = DTA_SHORT; break;
      case 'l': RAW(typelist)[i] = DTA_LONG; break;
      case 'f': RAW(typelist)[i] = DTA_FLOAT; break;
      case 'd': RAW(typelist)[i] = DTA_DOUBLE; break;
      default:
        if(RAW(s_types)[i] >= 0x80) RAW(typelist)[i] = RAW(s_types)[i] - 0x7f;
        else RAW(typelist)[i] = 0;
        break;
      }
   UNPROTECT(1);
   return typelist;
}

SEXP dta_calc_obssize(SEXP s_dta_file, SEXP typelist){
  dta_file *dtaf = get_dta_file(s_dta_file);
  unsigned char *types = RAW(typelist);
  int j, obs_size = 0, nvar=LENGTH(typelist);
  for(j = 0; j < nvar; j++){
      if(types[j]<=DTA_MAXSTR){
        obs_size += types[j];
      }
      else switch(types[j]){
        case DTA_BYTE:
          obs_size += 1;
          break;
        case DTA_SHORT:
          obs_size += 2;
          break;
        case DTA_LONG:
          obs_size += 4;
          break;
        case DTA_FLOAT:
          obs_size += 4;
          break;
        case DTA_DOUBLE:
          obs_size += 8;
          break;
        default:
          error("unknown data type %d",types[j]);
          break;
      }
    }
  dtaf->l_record = obs_size;
  return ScalarInteger(obs_size);
}

SEXP dta_read_varlabs(SEXP s_dta_file, SEXP s_nvar, SEXP s_len_varlab){
  dta_file *dtaf = get_dta_file(s_dta_file);
  int j, nvar = asInteger(s_nvar);
  int l_varl = asInteger(s_len_varlab) + 1;
  char *varlab;
  SEXP ans;
  varlab = R_alloc(l_varl,1);
  PROTECT(ans = allocVector(STRSXP,nvar));
  for(j = 0; j < nvar; j++){
    dta_read_string(dtaf,varlab,l_varl);
    SET_STRING_ELT(ans,j,mkChar(varlab));
  }
  UNPROTECT(1);
  return ans;
}

SEXP dta_read_expansion_fields(SEXP s_dta_file, SEXP s_shortext){
  dta_file *dtaf = get_dta_file(s_dta_file);
  int use_short = asLogical(s_shortext);
  int byte;
  int len;
  while(1){
    byte = dta_read_byte(dtaf);
    if(use_short)
      len = dta_read_short(dtaf);
    else
      len = dta_read_int(dtaf);
    /*Rprintf("\n%d",byte);
    Rprintf("\t%d",len);*/ 
    if(byte > 0 && len > 0)
        fseek(dtaf->f,len,SEEK_CUR);
    else break;
  }
  dtaf->start_data = ftell(dtaf->f);
  return R_NilValue;
}

SEXP dta_seek_data(SEXP s_dta_file){
  dta_file *dtaf = get_dta_file(s_dta_file);
  int ret = fseek(dtaf->f,dtaf->start_data,SEEK_SET);
  if(ret == 0) return ScalarInteger(ftell(dtaf->f));
  else return ScalarInteger(NA_INTEGER);
}

char charbuf[256]; 

SEXP dta_make_prototype(SEXP s_types){
  int j, nvar = length(s_types);
  SEXP ans, double_item, integer_item, character_item;
  PROTECT(ans = allocVector(VECSXP,nvar));
  unsigned char *types = RAW(s_types);
  PROTECT(double_item = R_getClassDef("double.item"));
  PROTECT(integer_item = R_getClassDef("integer.item"));
  PROTECT(character_item = R_getClassDef("character.item"));
  for(j = 0; j< nvar; j++){
    if(types[j]<=DTA_MAXSTR)
      SET_VECTOR_ELT(ans,j,R_do_new_object(character_item));
    else switch(types[j]){
      case DTA_BYTE:
      case DTA_SHORT:
      case DTA_LONG:
        SET_VECTOR_ELT(ans,j,R_do_new_object(integer_item));
        break;
      case DTA_FLOAT:
      case DTA_DOUBLE:
        SET_VECTOR_ELT(ans,j,R_do_new_object(double_item));
        break;
      default:
        error("unknown data type %d",types[j]);
        break;
    }
  }
  UNPROTECT(4);
  return ans;
}

SEXP dta_read_data(SEXP s_dta_file, SEXP what, SEXP s_nobs, SEXP s_types){
  dta_file *dtaf = get_dta_file(s_dta_file);
  int nobs = asInteger(s_nobs);
  int nvar = length(s_types);
  int i,j;
  SEXP ans, x, y;
  PROTECT(ans = allocVector(VECSXP,nvar));
  unsigned char *types = RAW(s_types);
  for(j = 0; j< nvar; j++){
    if(types[j]<=DTA_MAXSTR)
      SET_VECTOR_ELT(ans,j,allocVector(STRSXP,nobs));
    else switch(types[j]){
      case DTA_BYTE:
      case DTA_SHORT:
      case DTA_LONG:
        SET_VECTOR_ELT(ans,j,allocVector(INTSXP,nobs));
        break;
      case DTA_FLOAT:
      case DTA_DOUBLE:
        SET_VECTOR_ELT(ans,j,allocVector(REALSXP,nobs));
        break;
      default:
        error("unknown data type %d",types[j]);
        break;
    }
  }
  for(i = 0; i < nobs; i++){
    for(j = 0; j < nvar; j++){
      x = VECTOR_ELT(ans,j);
      if(types[j]<=DTA_MAXSTR){
        dta_read_string(dtaf,charbuf,types[j]);
        charbuf[types[j]] = 0; /* just to be sure ... */
        SET_STRING_ELT(x,i,mkChar(charbuf));
      }
      else switch(types[j]){
        case DTA_BYTE:
          INTEGER(x)[i] = dta_read_byte(dtaf);
          break;
        case DTA_SHORT:
          INTEGER(x)[i] = dta_read_short(dtaf);
          break;
        case DTA_LONG:
          INTEGER(x)[i] = dta_read_int(dtaf);
          break;
        case DTA_FLOAT:
          REAL(x)[i] = dta_read_float(dtaf);
          break;
        case DTA_DOUBLE:
          REAL(x)[i] = dta_read_double(dtaf);
          break;
        default:
          error("I should never arrive here!!");
          break;
      }
    }
  }
  for(j = 0; j < nvar; j++){
    x = VECTOR_ELT(what,j);
    y = VECTOR_ELT(ans,j);
    copyMostAttrib(x,y);
  }
  UNPROTECT(1);
  return ans;
}


SEXP dta_read_labels (SEXP s_dta_file, SEXP s_lbl_len, SEXP s_padding){
  dta_file *dtaf = get_dta_file(s_dta_file);
  int l_lbl = asInteger(s_lbl_len) + 1 + asInteger(s_padding);
  int len = dta_read_int(dtaf);
  if(len == NA_INTEGER) return R_NilValue;
  /*Rprintf("len=%d",len);*/ 
  char *lbl;
  lbl = R_alloc(l_lbl,1);
  dta_read_string(dtaf,lbl,l_lbl);
  /*Rprintf("\n%s",lbl);*/ 
  int i,n = dta_read_int(dtaf);
  /*Rprintf("\nn=%d",n);*/ 
  int txtlen = dta_read_int(dtaf);
  /*Rprintf("\ntxtlen=%d",n);*/ 
  char *txtbuf = R_alloc(txtlen,1);
  SEXP ans, off, val, txt;
  PROTECT(ans = allocVector(VECSXP,1));
  PROTECT(off = allocVector(INTSXP,n));
  PROTECT(val = allocVector(INTSXP,n));
  PROTECT(txt = allocVector(STRSXP,n));
  for(i = 0; i < n; i++)
    INTEGER(off)[i] = dta_read_int(dtaf);
  for(i = 0; i < n; i++)
    INTEGER(val)[i] = dta_read_int(dtaf);
/*  PrintValue(off);
  PrintValue(val);*/
  dta_read_string(dtaf,txtbuf,txtlen);
  /*Rprintf("\n%s",txtbuf);*/ 
  
  char *tmp;
  for(i = 0; i < n; i++){
    tmp = txtbuf + INTEGER(off)[i];
    SET_STRING_ELT(txt,i,mkChar(tmp));
    }
  SET_NAMES(val,txt);
  SET_VECTOR_ELT(ans,0,val);
  SET_NAMES(ans,mkString(lbl));
  UNPROTECT(4);
  return ans;
}

SEXP _dta_read_labels (SEXP s_dta_file, SEXP s_lbl_len, SEXP s_padding){
  dta_file *dtaf = get_dta_file(s_dta_file);
  int l_lbl = asInteger(s_lbl_len) + 1 + asInteger(s_padding);
  int len = dta_read_int(dtaf);
  Rprintf("len=%d\n",len);
  char *lbl;
  lbl = R_alloc(l_lbl,1);
  dta_read_string(dtaf,lbl,l_lbl);
  Rprintf(lbl);
  int i,n = dta_read_int(dtaf);
  Rprintf("\nn=%d",n);
  int txtlen = dta_read_int(dtaf);
  Rprintf("\ntxtlen=%d\n",n);
  char *txtbuf = R_alloc(txtlen,1);
  SEXP ans, off, val, names;
  PROTECT(ans = allocVector(VECSXP,4));
  PROTECT(off = allocVector(INTSXP,n));
  PROTECT(val = allocVector(INTSXP,n));
  PROTECT(names = allocVector(STRSXP,4));
  for(i = 0; i < n; i++)
    INTEGER(off)[i] = dta_read_int(dtaf);
  for(i = 0; i < n; i++)
    INTEGER(val)[i] = dta_read_int(dtaf);
  dta_read_string(dtaf,txtbuf,txtlen);
  char *tmp;
  for(i=0; i < n; i++){
    tmp = txtbuf + INTEGER(off)[i];
    Rprintf("%s\n",tmp);
  }
/*  for(i = 0; i < txtlen;i++)
    if(txtbuf[i] == 0)txtbuf[i] = '\n';*/
//   PrintValue(off);
//   PrintValue(val);
  SET_VECTOR_ELT(ans,0,mkString(lbl));
  SET_VECTOR_ELT(ans,1,off);
  SET_VECTOR_ELT(ans,2,val);
  SET_VECTOR_ELT(ans,3,mkString(txtbuf));
  SET_STRING_ELT(names,0,mkChar("varname"));
  SET_STRING_ELT(names,1,mkChar("offsets"));
  SET_STRING_ELT(names,2,mkChar("values"));
  SET_STRING_ELT(names,3,mkChar("lbltext"));
  SET_NAMES(ans,names);
  UNPROTECT(4);
  return ans;
}


SEXP dta_read_subset(SEXP s_dta_file, SEXP what, SEXP vars, SEXP obs, SEXP s_types){
  dta_file *dtaf = get_dta_file(s_dta_file);
  PROTECT(vars = coerceVector(vars,LGLSXP));
  PROTECT(obs = coerceVector(obs,LGLSXP));
  int nobs = length(obs);
  /*if(nobs > dtaf->n_records) error("obs argument has wrong length");*/
  int nvar = length(vars);
  if(nvar != length(s_types)) error("vars and types arguments differ in length");
  int i,j,k,l;
  SEXP ans, x, y;
  int n=0, m = 0;
  for(j = 0; j < nvar; j++)
    m += LOGICAL(vars)[j];
  for(j = 0; j < nobs; j++)
    n += LOGICAL(obs)[j];
  
  PROTECT(ans = allocVector(VECSXP,m));
  unsigned char *types = RAW(s_types);
  k = 0;
  for(j = 0; j< nvar; j++){
    if(LOGICAL(vars)[j]){
      if(types[j]<=DTA_MAXSTR)
        SET_VECTOR_ELT(ans,k,allocVector(STRSXP,n));
      else switch(types[j]){
        case DTA_BYTE:
        case DTA_SHORT:
        case DTA_LONG:
          SET_VECTOR_ELT(ans,k,allocVector(INTSXP,n));
          break;
        case DTA_FLOAT:
        case DTA_DOUBLE:
          SET_VECTOR_ELT(ans,k,allocVector(REALSXP,n));
          break;
        default:
          error("unknown data type %d",types[j]);
          break;
      }
      k++;
    }
  }
  k = 0;
  int tmp_int;
  double tmp_double;
  for(i = 0; i < nobs; i++){
    if(LOGICAL(obs)[i]){
      l = 0;
      for(j = 0; j < nvar; j++){
          if(types[j]<=DTA_MAXSTR){
            dta_read_string(dtaf,charbuf,types[j]);
            charbuf[types[j]] = 0; /* just to be sure ... */
            if(LOGICAL(vars)[j]){
              x = VECTOR_ELT(ans,l);
              SET_STRING_ELT(x,k,mkChar(charbuf));
              l++;
            }
          }
          else switch(types[j]){
            case DTA_BYTE:
              tmp_int = dta_read_byte(dtaf);
              if(LOGICAL(vars)[j]){
                x = VECTOR_ELT(ans,l);
                INTEGER(x)[k] = tmp_int;
                l++;
              }
              break;
            case DTA_SHORT:
              tmp_int = dta_read_short(dtaf);
              if(LOGICAL(vars)[j]){
                x = VECTOR_ELT(ans,l);
                INTEGER(x)[k] = tmp_int;
                l++;
              }
              break;
            case DTA_LONG:
              tmp_int = dta_read_int(dtaf);
              if(LOGICAL(vars)[j]){
                x = VECTOR_ELT(ans,l);
                INTEGER(x)[k] = tmp_int;
                l++;
              }
              break;
            case DTA_FLOAT:
              tmp_double = dta_read_float(dtaf);
              if(LOGICAL(vars)[j]){
                x = VECTOR_ELT(ans,l);
                REAL(x)[k] = tmp_double;
                l++;
              }
              break;
            case DTA_DOUBLE:
              tmp_double = dta_read_double(dtaf);
              if(LOGICAL(vars)[j]){
                x = VECTOR_ELT(ans,l);
                REAL(x)[k] = tmp_double;
                l++;
              }
              break;
            default:
              error("I should never arrive here!!");
              break;
          }
      }
      k++;
    }
    else {
      dta_skip_record(dtaf);
    }
  }
  
  k = 0;
  for(j = 0; j < nvar; j++){
    if(LOGICAL(vars)[j]){
      x = VECTOR_ELT(what,j);
      y = VECTOR_ELT(ans,k);
      copyMostAttrib(x,y);
      k++;
    }
  }

  UNPROTECT(3);
  return ans;
}







