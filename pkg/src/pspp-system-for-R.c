#include <R.h>
#include <Rdefines.h>
#include <string.h>
#include <Rmath.h>
#include <stdio.h>
#include <Rinternals.h>
#include "memisc.h"
#include "dumbswap.h"

typedef int R_int32;

typedef double R_flt64;

typedef struct {
  FILE *f;
  unsigned char bytes[8];
  int byte_pos;
  int case_size;
  int compressed;
  R_flt64 bias;
  R_flt64 *buf;
  int swap_code;
  int data_pos;
  R_flt64 sysmis;
  R_flt64 highest;
  R_flt64 lowest;
} sys_file;

typedef union {
  R_flt64 as_double;
  char as_char[8];
} sys_word;

static double second_lowest_double_val(void);
#define SYSMIS -DBL_MAX
#define HIGHEST DBL_MAX
#define LOWEST second_lowest_double_val()

#undef DEBUG
/** sysfile tools **/

void init_sys_file(sys_file *s){
#ifdef DEBUG
  Rprintf("\ninit_sys_file");
  Rprintf("\ns = %d",s);
  Rprintf("\ns->f = %d",s->f);
#endif
  memset(s->bytes,0,8);
  s->byte_pos = 0;
  s->case_size = 0;
  s->bias = 100.;
  s->swap_code = 0;
  s->sysmis = SYSMIS;
  s->highest = HIGHEST;
  s->lowest = LOWEST;
  fseek(s->f,0,SEEK_SET);
}

int sys_read_int(R_int32 *target, sys_file *s){
  R_int32 x;
  int read_len = (int)fread(&x,sizeof(R_int32),1,s->f);
  *target = iswap(x,s->swap_code);
  return read_len;
}

int sys_read_real(R_flt64 *target, sys_file *s){
  R_flt64 x;
  int read_len = (int)fread(&x,sizeof(R_flt64),1,s->f);
  *target = dswap(x,s->swap_code);
  return read_len;
}

int sys_read_octet(char *target, sys_file *s){
  int read_len = (int)fread(target,8,1,s->f);
  return read_len;
}

int sys_read_string(char *target, sys_file *s){
  int str_len;
  int read_len = (int)fread(&str_len,1,1,s->f);
  str_len = (str_len/8)*8+7;
  char *string = S_alloc(str_len+1,1);
  read_len = (int)fread(string,str_len,1,s->f);
  return read_len;
}

int sys_read(void *target, int n, sys_file *s){
  return (int)fread(target,1,n,s->f);
}

/*int sys_read_word(sys_word *word, sys_file *s){
  int read_len = fread(word,8,1,s->f);
  return read_len;
}

int sys_read_bytes(sys_file *s){
  int read_len = fread(s->bytes,8,1,s->f);
  return read_len;
}*/

int sys_read_case(sys_file *s){
#ifdef DEBUG
  Rprintf("\nsys_read_case");
  Rprintf("\ncase size: %d",s->case_size);
#endif
  int read_len;
  if(!s->compressed)
    return (int)fread(s->buf,8,s->case_size,s->f);
  else{
    int j,k = s->byte_pos;
    for(j = 0; j < s->case_size; j++){
      for(; k < 8; k++){ /* Ignore "0" bytes */
          if (s->bytes[k]!=0) break;
      }
      if(k >= 8) /* Read new command bytes */{
        read_len = (int)fread(s->bytes,1,8,s->f);
#ifdef DEBUG
        Rprintf("\nread length for bytes: %d",read_len);
        Rprintf("\nnew bytes=(%d,%d,%d,%d,%d,%d,%d,%d)",
            s->bytes[0],
            s->bytes[1],
            s->bytes[2],
            s->bytes[3],
            s->bytes[4],
            s->bytes[5],
            s->bytes[6],
            s->bytes[7]);
#endif
        k = 0;
        if(read_len == 0) return 0;
        if(read_len < 8) return j;
      }
#ifdef DEBUG
      Rprintf("\nbytes[%d]=%d",k,s->bytes[k]);
#endif

      if(s->bytes[k] == 252) /* End of file */ return j;
      else if(s->bytes[k] == 253) /* Uncompressed data */{
        read_len = (int)fread(s->buf+j,8,1,s->f);
        if(!read_len) return j;
      }
      else if(s->bytes[k] == 254) /* 8 blanks */{
        memset(s->buf+j,' ',8);
      }
      else if(s->bytes[k] == 255) /* system missing value */{
        s->buf[j] = dswap(s->sysmis,s->swap_code);
      }
      else /* compressed data */{
        s->buf[j] = (R_flt64)s->bytes[k] - s->bias;
      }
      k++;
#ifdef DEBUG
      Rprintf("\ns->buf[%d]=%g",j,s->buf[j]);
#endif
    }
    s->byte_pos = k;
#ifdef DEBUG
    Rprintf("\ns->f = %d",s->f);
    Rprintf("\ns->byte_pos = %d",s->byte_pos);
    Rprintf("\ns->case_size = %d",s->case_size);
    Rprintf("\nFile pos = %d",ftell(s->f));
#endif
    return j;
  }
  return 0; /* -Wall */
#undef DEBUG
}



/** sysfile objects **/

SEXP closeSysFile (SEXP);

SEXP NewSysFile (SEXP name){
#ifdef DEBUG
  Rprintf("\nNewSysFile");
#endif
  PROTECT(name = coerceVector(name,STRSXP));
//   sys_file *s = (sys_file *)S_alloc(1,sizeof(sys_file));
  sys_file *s = Calloc(1,sys_file);
  s->f = fopen(CHAR(STRING_ELT(name, 0)),"rb");
#ifdef DEBUG
  Rprintf("\nfile = %d",s->f);
#endif
  if(s->f == NULL){
    Free(s);
    UNPROTECT(1);
    return R_NilValue;
  }
  else {
    init_sys_file(s);
    SEXP ans = R_MakeExternalPtr(s, install("sys_file"), R_NilValue);
		PROTECT(ans);
    R_RegisterCFinalizer(ans, (R_CFinalizer_t) closeSysFile);
    setAttrib(ans,install("file.name"),name);
    UNPROTECT(2);
    return ans;
  }
}


SEXP sys_file_restore_from_attrib(SEXP SysFile, sys_file *s,const char* attribname){
  SEXP ans = getAttrib(SysFile,install(attribname));
    if(ans == R_NilValue || ans == NULL) {
      fclose(s->f);
      R_SetExternalPtrAddr(SysFile,NULL);
      Free(s);
      error("cannot restore sysfile data: missing '%s' attribute",attribname);
    }
  return ans;
}

sys_file *get_sys_file(SEXP SysFile){
  if(TYPEOF(SysFile) != EXTPTRSXP || R_ExternalPtrTag(SysFile) != install("sys_file"))
    error("not a SysFile");
  sys_file *s = R_ExternalPtrAddr(SysFile);
  if (s == NULL){
    error("external pointer is NULL, you need to recreate this object");
    /* sys_file *s = Calloc(1,sys_file);
     * R_SetExternalPtrAddr(SysFile,s);
     * SEXP name;
     * PROTECT(name = getAttrib(SysFile,install("file.name")));
     * if(name == R_NilValue || name == NULL) error("need filename to reopen file");
     * s->f = fopen(CHAR(STRING_ELT(name, 0)),"rb");
     * if(s->f == NULL){
     *   R_SetExternalPtrAddr(SysFile,NULL);
     *   Free(s);
     *   error("cannot reopen file -- does it still exist?");
     * }
     * init_sys_file(s);
     * SEXP tmp;
     * tmp = sys_file_restore_from_attrib(SysFile,s,"bias");
     * s->bias = (R_flt64) asReal(tmp);
     * tmp = sys_file_restore_from_attrib(SysFile,s,"swap_code");
     * s->swap_code = asInteger(tmp);
     * tmp = sys_file_restore_from_attrib(SysFile,s,"case_size");
     * s->case_size = asInteger(tmp);
     * tmp = sys_file_restore_from_attrib(SysFile,s,"data_pos");
     * s->data_pos = asInteger(tmp);
     * tmp = sys_file_restore_from_attrib(SysFile,s,"sysmis");
     * s->sysmis = (R_flt64) asReal(tmp);
     * s->buf = Calloc(s->case_size,R_flt64);
     * Rprintf("File '%s' reopened\n\n",CHAR(STRING_ELT(name, 0)));
     * Rprintf("\ns= %llx",s);
     * UNPROTECT(1); */
  }
  if(s== NULL || s->f == NULL)
        error("file pointer is NULL");
  return(s);
}

SEXP closeSysFile (SEXP SysFile){
  if(TYPEOF(SysFile) != EXTPTRSXP || R_ExternalPtrTag(SysFile) != install("sys_file"))
    error("not a SysFile");
  sys_file *s = R_ExternalPtrAddr(SysFile);
  if (s != NULL) {
      fclose(s->f);
      Free(s->buf);
      R_ClearExternalPtr(SysFile);
  }
  return R_NilValue;
}


/** interface functions **/

struct sysfile_header
  {
    char rec_type[4+1];           /* Record-type code, "$FL2". */
    char prod_name[60+1];         /* Product identification. */
    R_int32 layout_code;        /* 2. */
    R_int32 case_size;          /* Number of `value's per case. */
    R_int32 compressed;         /* 1=compressed, 0=not compressed. */
    R_int32 weight_index;       /* 1-based index of weighting var, or zero. */
    R_int32 ncases;             /* Number of cases, -1 if unknown. */
    R_flt64 bias;               /* Compression bias (100.0). */
    char creation_date[9+1];      /* `dd mmm yy' creation date of file. */
    char creation_time[8+1];      /* `hh:mm:ss' 24-hour creation time. */
    char file_label[64+1];        /* File label. */
    char padding[3];
  };


SEXP read_sysfile_header(SEXP SysFile){
#ifdef DEBUG
  Rprintf("\nread_sysfile_header");
#endif

  sys_file *s = get_sys_file(SysFile);

  struct sysfile_header h;

  memset(h.rec_type,0,4+1);
  memset(h.prod_name,0,6+1);
  memset(h.creation_date,0,9+1);
  memset(h.creation_time,0,8+1);
  memset(h.file_label,0,64+1);
  sys_read(h.rec_type,4,s);
  if(strncmp(h.rec_type,"$FL2",4)!=0) error("not a sysfile");
  sys_read(h.prod_name,60,s);
  sys_read_int(&h.layout_code,s);
  sys_read_int(&h.case_size,s);
  sys_read_int(&h.compressed,s);
  sys_read_int(&h.weight_index,s);
  sys_read_int(&h.ncases,s);
  sys_read_real(&h.bias,s);
  sys_read(h.creation_date,9,s);
  sys_read(h.creation_time,8,s);
  sys_read(h.file_label,64,s);
  sys_read(h.padding,3,s);

  SEXP ans;
  PROTECT(ans = allocVector(VECSXP,12));
  int protectcounter = 1;

#ifdef DEBUG
  Rprintf("\nh.rec_type = %s",h.rec_type);
  Rprintf("\nh.case_size = %x",h.case_size);
  Rprintf("\nh.case_size = %d",h.case_size);
  Rprintf("\nh.layout_code = %x",h.layout_code);
  Rprintf("\nh.layout_code = %d",h.layout_code);
#endif
  if(h.layout_code == 2)
    s->swap_code = 0; /*endian equality to system */
  else
    s->swap_code = 1; /*endian difference to system */
  s->compressed = iswap(h.compressed,s->swap_code);
  s->case_size = iswap(h.case_size,s->swap_code);
  s->bias = dswap(h.bias,s->swap_code);
  s->buf = Calloc(s->case_size,R_flt64);

  SET_VECTOR_ELT(ans,0,mkString(h.prod_name));
  SET_VECTOR_ELT(ans,1,ScalarInteger(iswap(h.layout_code,s->swap_code)));
  SET_VECTOR_ELT(ans,2,ScalarInteger(iswap(h.case_size,s->swap_code)));
  SET_VECTOR_ELT(ans,3,ScalarInteger(iswap(h.compressed,s->swap_code)));
  SET_VECTOR_ELT(ans,4,ScalarInteger(iswap(h.weight_index,s->swap_code)));
  SET_VECTOR_ELT(ans,5,ScalarInteger(iswap(h.ncases,s->swap_code)));
  SET_VECTOR_ELT(ans,6,ScalarReal(dswap(h.bias,s->swap_code)));
  SET_VECTOR_ELT(ans,7,mkString(h.creation_date));
  SET_VECTOR_ELT(ans,8,mkString(h.creation_time));
  SET_VECTOR_ELT(ans,9,mkString(h.file_label));
  SET_VECTOR_ELT(ans,10,ScalarInteger(s->swap_code));
  SET_VECTOR_ELT(ans,11,mkString(h.rec_type));


  SEXP names;
  PROTECT(names = allocVector(STRSXP,12));
  protectcounter++;
  SET_STRING_ELT(names,0,mkChar("prod_name"));
  SET_STRING_ELT(names,1,mkChar("layout_code"));
  SET_STRING_ELT(names,2,mkChar("case_size"));
  SET_STRING_ELT(names,3,mkChar("compressed"));
  SET_STRING_ELT(names,4,mkChar("weight_index"));
  SET_STRING_ELT(names,5,mkChar("ncases"));
  SET_STRING_ELT(names,6,mkChar("bias"));
  SET_STRING_ELT(names,7,mkChar("creation_date"));
  SET_STRING_ELT(names,8,mkChar("creation_time"));
  SET_STRING_ELT(names,9,mkChar("file_label"));
  SET_STRING_ELT(names,10,mkChar("swap_code"));
  SET_STRING_ELT(names,11,mkChar("rec_type"));
  SET_NAMES(ans,names);

  UNPROTECT(protectcounter);
#ifdef DEBUG
  PrintValue(ans);
#endif
  return ans;
}



struct sysfile_variable
  {
    R_int32 rec_type;           /* 2. */
    R_int32 type;               /* 0=numeric, 1-255=string width,
                                   -1=continued string. */
    R_int32 has_var_label;      /* 1=has a variable label, 0=doesn't. */
    R_int32 n_missing_values;   /* Missing value code of -3,-2,0,1,2, or 3. */
    R_int32 print;      /* Print format. */
    R_int32 write;      /* Write format. */
    char name[8+1];               /* Variable name. */
    R_int32 label_len;
    char* label;
    R_flt64 missing_values[3];
    /* The rest of the structure varies. */
  };

/* #define DEBUG */
SEXP read_sysfile_var(SEXP SysFile){
#ifdef DEBUG
  Rprintf("\nread_sysfile_var");
#endif
  sys_file *s = get_sys_file(SysFile);
  int protectcounter = 0;

  SEXP var, vnames;
  int i ;
  struct sysfile_variable curr_var;
  memset(curr_var.name,0,8+1);
  sys_read_int(&curr_var.rec_type,s);
#ifdef DEBUG
  Rprintf("\ntag: %d",curr_var.rec_type);
#endif
  if(curr_var.rec_type != 2) {
      #ifdef DEBUG
      Rprintf("%d\n",curr_var.rec_type);
      Rprintf("%s\n",&curr_var.rec_type);
      #endif
      /* error("expecting a variable record"); */
      fseek(s->f,-4,SEEK_CUR);
      return R_NilValue;
  }
  sys_read_int(&curr_var.type,s);
  sys_read_int(&curr_var.has_var_label,s);
  sys_read_int(&curr_var.n_missing_values,s);
  sys_read_int(&curr_var.print,s);
  sys_read_int(&curr_var.write,s);
  sys_read_octet(curr_var.name,s);
  trim(curr_var.name,8);

  long pos;
  if(curr_var.has_var_label){
      sys_read_int(&curr_var.label_len,s);
      curr_var.label = R_alloc(curr_var.label_len+1,1);
      memset(curr_var.label,0,curr_var.label_len+1);
      sys_read(curr_var.label,curr_var.label_len,s);
      trim(curr_var.label,curr_var.label_len);
      pos = ftell(s->f);
      if(pos%4 != 0) fseek(s->f,(pos/4+1)*4,SEEK_SET);
  }
  int nmiss = 0;
  if(curr_var.n_missing_values!=0){
      nmiss = curr_var.n_missing_values;
      for(i = 0; i < abs(nmiss); i++)
        sys_read_real(curr_var.missing_values+i,s);
#ifdef DEBUG
      Rprintf("\n");
      for(i = 0; i < abs(nmiss); i++){
        Rprintf("\nR_PosInf = (as hex)%llx",R_PosInf);
        Rprintf("\n%d missval:",i);
        Rprintf("\nas char = %s",(char *)(curr_var.missing_values)+i);
        Rprintf("\nas hex = %llx",curr_var.missing_values[i]);
        Rprintf("\nas hex = %.9a",curr_var.missing_values[i]);
        Rprintf("\nas real = %.9e",curr_var.missing_values[i]);
        }
#endif

  }


  PROTECT(var = allocVector(VECSXP,8));
  protectcounter++;

  SET_VECTOR_ELT(var,0,mkString(curr_var.name));
  SET_VECTOR_ELT(var,1,ScalarInteger(curr_var.type));
  SET_VECTOR_ELT(var,2,ScalarInteger(curr_var.has_var_label));
  SET_VECTOR_ELT(var,3,ScalarInteger(curr_var.n_missing_values));
  SEXP curr_print, curr_write;
  PROTECT(curr_print = allocVector(INTSXP,4));
  PROTECT(curr_write = allocVector(INTSXP,4));
  protectcounter+=2;
  char *tmp;
  tmp = (char *)&curr_var.print;
  for(i = 0; i < 4; i++)
    INTEGER(curr_print)[i] = (int)tmp[i];
  tmp = (char *)&curr_var.write;
  for(i = 0; i < 4; i++)
    INTEGER(curr_write)[i] = (int)tmp[i];
  SET_VECTOR_ELT(var,4,curr_print);
  SET_VECTOR_ELT(var,5,curr_write);

  if(curr_var.has_var_label){
    SET_VECTOR_ELT(var,6,mkString(curr_var.label));
  }
  else SET_VECTOR_ELT(var,6,R_NilValue);

  SEXP s_missings;
  if(curr_var.n_missing_values!=0){
    PROTECT(s_missings = allocVector(REALSXP,abs(nmiss)));
    protectcounter++;
    for(i = 0; i < abs(nmiss); i++)
      REAL(s_missings)[i] = curr_var.missing_values[i];
    SET_VECTOR_ELT(var,7,s_missings);
  }
  else SET_VECTOR_ELT(var,7,R_NilValue);

  PROTECT(vnames = allocVector(STRSXP,8));
  protectcounter++;
  SET_STRING_ELT(vnames,0,mkChar("name"));
  SET_STRING_ELT(vnames,1,mkChar("type"));
  SET_STRING_ELT(vnames,2,mkChar("has_var_label"));
  SET_STRING_ELT(vnames,3,mkChar("n_missing_values"));
  SET_STRING_ELT(vnames,4,mkChar("print"));
  SET_STRING_ELT(vnames,5,mkChar("write"));
  SET_STRING_ELT(vnames,6,mkChar("label"));
  SET_STRING_ELT(vnames,7,mkChar("missings"));
  SET_NAMES(var,vnames);
#ifdef DEBUG
  Rprintf("\nCurrent variable: %s",curr_var.name);
  PrintValue(var);
  Rprintf("\nPosition: %f",ftell(s->f));
#endif

  UNPROTECT(protectcounter);
  return var;
}
#undef DEBUG

SEXP test_sysfile_int32(SEXP SysFile){
#ifdef DEBUG
  Rprintf("\ntest_sysfile_int32");
#endif
  sys_file *s = get_sys_file(SysFile);
  R_int32 number;
  sys_read_int(&number,s);
  fseek(s->f,-4,SEEK_CUR);
#ifdef DEBUG
    Rprintf("\nnumber = %x",number);
#endif
  return ScalarInteger(number);
}

SEXP read_sysfile_value_labels (SEXP SysFile){
#ifdef DEBUG
  Rprintf("\nread_sysfile_value_labels");
#endif
  sys_file *s = get_sys_file(SysFile);
  int protectcounter = 0;
  R_int32 rec_type;
  R_int32 nlabels;
  sys_read_int(&rec_type,s);
  if(rec_type != 3) error("expecting a value label record");
  sys_read_int(&nlabels,s);
#ifdef DEBUG
  Rprintf("\nnlabels=%d",nlabels);
#endif
  SEXP labels, values;
  PROTECT(labels=allocVector(STRSXP,nlabels));
  PROTECT(values=allocVector(REALSXP,nlabels));
  protectcounter += 2;
  int i;
  double value;

  char *labbuf = S_alloc(256,1);
  
  for(i = 0; i < nlabels; i++){
    sys_read_real(&value,s);
    REAL(values)[i] = value;
    unsigned char lablen, readlen;
    sys_read(&lablen,1,s);
    readlen = (unsigned char)((lablen/8)*8+7);
    sys_read(labbuf,readlen,s);
    labbuf[lablen] = 0;
#ifdef DEBUG
    Rprintf("\nvalues[%d]=%g",i,value);
    Rprintf("\nlabels[%d]=|%s|",i,labbuf);
    Rprintf("\nlength(labels[%d])=%d",i,lablen);
#endif
    trim(labbuf,lablen);
    SET_STRING_ELT(labels,i,mkChar(labbuf));
  }
  SET_NAMES(values,labels);
//   long pos = ftell(f);
//   if(pos%4 != 0) fseek(f,(pos/4+1)*4,SEEK_SET);
  sys_read_int(&rec_type,s);
  if(rec_type != 4) error("expecting a value label variables record, found instead a record tagged %d",rec_type);
  R_int32 nvars;
  sys_read_int(&nvars,s);
  R_int32 var;
  SEXP variables;
  PROTECT(variables = allocVector(INTSXP,nvars));
  protectcounter++;
  for(i = 0; i < nvars; i++){
    sys_read_int(&var,s);
    INTEGER(variables)[i] = var;
  }
  SEXP ans, names;
  PROTECT(ans = allocVector(VECSXP,2));
  PROTECT(names = allocVector(STRSXP,2));
  protectcounter+=2;
  SET_VECTOR_ELT(ans,0,values);
  SET_STRING_ELT(names,0,mkChar("labels"));
  SET_VECTOR_ELT(ans,1,variables);
  SET_STRING_ELT(names,1,mkChar("variables"));
  SET_NAMES(ans,names);
  UNPROTECT(protectcounter);
#ifdef DEBUG
    PrintValue(ans);
#endif
  return ans;
}

#define DEBUG
SEXP num_to_string8(SEXP num_values){
  char tmp_char[9];
  int nlabels = length(num_values);
  R_flt64 tmp_real;
  int i;
  SEXP ans;
  PROTECT(ans = allocVector(STRSXP,nlabels));
  memset(tmp_char,0,9);
  for(i = 0; i < nlabels; i++){
    tmp_real = REAL(num_values)[i];
    memcpy(tmp_char,&tmp_real,8);
    SET_STRING_ELT(ans,i,mkChar(tmp_char));
  }
  UNPROTECT(1);
  return ans;
}
#undef DEBUG

SEXP read_sysfile_document(SEXP SysFile){
#ifdef DEBUG
  Rprintf("\nread_sysfile_document");
#endif
  sys_file *s = get_sys_file(SysFile);
  int protectcounter = 0;
  R_int32 rec_type;
  sys_read_int(&rec_type,s);
  if(rec_type != 6) error("expecting a document record");
  R_int32 nlines;
  sys_read_int(&nlines,s);
  SEXP lines;
  PROTECT(lines = allocVector(STRSXP,nlines));
  protectcounter++;
  char buf[81];
  memset(buf,0,81);
  int i;
  for(i = 0; i < nlines; i++){
    sys_read(buf,80,s);
    SET_STRING_ELT(lines,i,mkChar(buf));
  }
  UNPROTECT(protectcounter);
  return lines;
}

static const int info_int32 = 3;
static const int info_flt64 = 4;
static const int aux_var = 11;
static const int long_var_names = 13;
static const int aux_enc = 20;

SEXP read_sysfile_aux(SEXP SysFile){
#ifdef DEBUG
  Rprintf("\nread_sysfile_aux");
#endif
  sys_file *s = get_sys_file(SysFile);
  int protectcounter = 0;
  R_int32 rec_type, subtype;
  sys_read_int(&rec_type,s);
  if(rec_type != 7) error("expecting a machine info record");
  sys_read_int(&subtype,s);
  SEXP ans, names, data, datanames;
  R_int32 size, count;
  int i, j;
  if(subtype == info_int32){
#ifdef DEBUG
    Rprintf("\nsubtype == info_int32");
#endif
    PROTECT(ans = allocVector(VECSXP,2));
    PROTECT(names = allocVector(STRSXP,2));
    protectcounter+=2;
    SET_VECTOR_ELT(ans,0,mkString("info_int32"));
    SET_STRING_ELT(names,0,mkChar("type"));
    sys_read_int(&size,s);
    sys_read_int(&count,s);
#ifdef DEBUG
    Rprintf("\nsize = %d",size);
    Rprintf("\nsize = %x",size);
    Rprintf("\ncount = %d",count);
#endif
    if(size != 4) error("we're in trouble here: size != 4");
    PROTECT(data = allocVector(INTSXP,8));
    PROTECT(datanames = allocVector(STRSXP,8));
    protectcounter+=2;
    int x;
    for(i = 0; i < 8; i++){
      sys_read_int(&x,s);
      INTEGER(data)[i] = x;
    }
    static const char* int32names[] = {
      "version_major",
      "version_minor",
      "version_revision",
      "machine_code",
      "floating_point_rep",
      "compression_code",
      "endianness",
      "character_code"
    };
    for(i = 0; i < 8; i++){
      SET_STRING_ELT(datanames,i, mkChar(int32names[i]));
    }
    SET_NAMES(data,datanames);
    SET_STRING_ELT(names,1,mkChar("data"));
    SET_VECTOR_ELT(ans,1,data);
    SET_NAMES(ans,names);
    UNPROTECT(protectcounter);
#ifdef DEBUG
    PrintValue(ans);
#endif
    return ans;
  }
  else if (subtype == info_flt64){
#ifdef DEBUG
    Rprintf("\nsubtype == info_flt64");
#endif
    PROTECT(ans = allocVector(VECSXP,2));
    PROTECT(names = allocVector(STRSXP,2));
    protectcounter+=2;
    SET_VECTOR_ELT(ans,0,mkString("info_flt64"));
    SET_STRING_ELT(names,0,mkChar("type"));
    sys_read_int(&size,s);
    sys_read_int(&count,s);
#ifdef DEBUG
    Rprintf("\nsize = %d",size);
    Rprintf("\nsize = %x",size);
    Rprintf("\ncount = %d",count);
#endif
    if(size != 8) error("we're in trouble here: size != 8");
    PROTECT(data = allocVector(REALSXP,3));
    PROTECT(datanames = allocVector(STRSXP,3));
    protectcounter+=2;
    for(i = 0; i < count; i++)
      sys_read_real(REAL(data)+i,s);
    s->sysmis = REAL(data)[0];
    s->highest = REAL(data)[1];
    s->lowest = REAL(data)[2];
    static const char* flt64names[] = {
      "sysmis",
      "highest",
      "lowest"
    };
    for(i = 0; i < 3; i++){
      SET_STRING_ELT(datanames,i,mkChar(flt64names[i]));
    }
    SET_NAMES(data,datanames);
    SET_STRING_ELT(names,1,mkChar("data"));
    SET_VECTOR_ELT(ans,1,data);
    SET_NAMES(ans,names);
    UNPROTECT(protectcounter);
#ifdef DEBUG
    PrintValue(ans);
#endif
    return ans;
  }
  else if (subtype == aux_var){
#ifdef DEBUG
    Rprintf("\nsubtype == aux_var");
#endif
    PROTECT(ans = allocVector(VECSXP,2));
    PROTECT(names = allocVector(STRSXP,2));
    protectcounter+=2;
    SET_VECTOR_ELT(ans,0,mkString("aux_var"));
    SET_STRING_ELT(names,0,mkChar("type"));
    sys_read_int(&size,s);
    sys_read_int(&count,s);
#ifdef DEBUG
    Rprintf("\nsize = %d",size);
    Rprintf("\nsize = %x",size);
    Rprintf("\ncount = %d",count);
#endif
    if(size != 4) error("we're in trouble here: size != 4");
    int aux_nvars = count/3;
    PROTECT(data = allocVector(VECSXP,aux_nvars));
    protectcounter++;
    SEXP auxdata, auxnames;
    for(i = 0; i < aux_nvars; i++){
      char *_auxnames[]={
        "measure",
        "width",
        "alignment"
      };
      PROTECT(auxdata = allocVector(INTSXP,3));
      PROTECT(auxnames = allocVector(STRSXP,3));
      for(j = 0; j < 3; j++){
        sys_read_int(INTEGER(auxdata)+j,s);
        SET_STRING_ELT(auxnames,j,mkChar(_auxnames[j]));
      }
      SET_NAMES(auxdata,auxnames);
      SET_VECTOR_ELT(data,i,auxdata);
      UNPROTECT(2);
    }
    SET_VECTOR_ELT(ans,1,data);
    SET_STRING_ELT(names,1,mkChar("data"));
    SET_NAMES(ans,names);
    UNPROTECT(protectcounter);
#ifdef DEBUG
    PrintValue(ans);
#endif
    return ans;
  }
  else if(subtype == long_var_names){
#ifdef DEBUG
    Rprintf("\nsubtype == long_var_names");
#endif
    PROTECT(ans = NEW_LIST(2));
    PROTECT(names = allocVector(STRSXP,2));
    protectcounter+=2;
    SET_VECTOR_ELT(ans,0,mkString("longVariableNames"));
    SET_STRING_ELT(names,0,mkChar("type"));
    sys_read_int(&size,s);
    sys_read_int(&count,s);
#ifdef DEBUG
    Rprintf("\nsize = %d",size);
    Rprintf("\nsize = %x",size);
    Rprintf("\ncount = %d",count);
#endif

    SEXP longnames;
    PROTECT(longnames = allocVector(STRSXP,1));
    protectcounter++;
    char *longnames_data;
    longnames_data = R_alloc(count+1,1);
    memset(longnames_data,0,count+1);
    sys_read(longnames_data,count,s);
    SET_STRING_ELT(longnames,0,mkChar(longnames_data));
    SET_VECTOR_ELT(ans,1,longnames);
    SET_STRING_ELT(names,1,mkChar("data"));
    SET_NAMES(ans,names);

    UNPROTECT(protectcounter);
#ifdef DEBUG
    PrintValue(ans);
#endif
    return ans;
  }
  else if(subtype == aux_enc) {
#ifdef DEBUG
    Rprintf("\nsubtype == aux_enc");
#endif
    PROTECT(ans = NEW_LIST(2));
    PROTECT(names = allocVector(STRSXP,2));
    protectcounter+=2;
    SET_VECTOR_ELT(ans,0,mkString("aux_enc"));
    SET_STRING_ELT(names,0,mkChar("type"));
    sys_read_int(&size,s);
    sys_read_int(&count,s);
    
    SEXP AuxEnc;
    PROTECT(AuxEnc = allocVector(STRSXP,1));
    protectcounter++;
    char *AuxEnc_data;
    AuxEnc_data = R_alloc(count+1,1);
    memset(AuxEnc_data,0,count+1);
    sys_read(AuxEnc_data,count,s);
    SET_STRING_ELT(AuxEnc,0,mkChar(AuxEnc_data));
    SET_VECTOR_ELT(ans,1,AuxEnc);
    SET_STRING_ELT(names,1,mkChar("data"));
    SET_NAMES(ans,names);
    UNPROTECT(protectcounter);
#ifdef DEBUG
    PrintValue(ans);
#endif
    return ans;
  }
  else {
#ifdef DEBUG
    Rprintf("\nsubtype == misc");
#endif
    PROTECT(ans = NEW_LIST(2));
    PROTECT(names = allocVector(STRSXP,2));
    protectcounter+=2;
    SET_VECTOR_ELT(ans,0,mkString("misc"));
    SET_STRING_ELT(names,0,mkChar("type"));
    sys_read_int(&size,s);
    sys_read_int(&count,s);
    PROTECT(data = allocVector(RAWSXP,size*count));
    protectcounter++;
    sys_read(RAW(data),size*count,s);
    SET_VECTOR_ELT(ans,1,data);
    SET_STRING_ELT(names,1,mkChar("data"));
    SET_NAMES(ans,names);
    UNPROTECT(protectcounter);
#ifdef DEBUG
    PrintValue(ans);
#endif
    return ans;
  }
  error("something's wrong here");
  return R_NilValue;
}

SEXP dflt_info_flt64 (SEXP SysFile){

  sys_file *s = get_sys_file(SysFile);
  s->sysmis  = SYSMIS;
  s->highest = HIGHEST;
  s->lowest  = LOWEST;

  SEXP data, datanames;
  int i;
  
  PROTECT(data = allocVector(REALSXP,3));
  PROTECT(datanames = allocVector(STRSXP,3));
  REAL(data)[0] = s->sysmis;
  REAL(data)[1] = s->highest;
  REAL(data)[2] = s->lowest;
  static const char* flt64names[] = {
    "sysmis",
    "highest",
    "lowest"
  };
  for(i = 0; i < 3; i++){
    SET_STRING_ELT(datanames,i,mkChar(flt64names[i]));
  }
  SET_NAMES(data,datanames);
  UNPROTECT(2);

  return data;
}

SEXP read_sysfile_dict_term (SEXP SysFile){
#ifdef DEBUG
  Rprintf("\nread_sysfile_dict_term");
#endif
  sys_file *s = get_sys_file(SysFile);
  R_int32 rec_type, filler;
  sys_read_int(&rec_type,s);
  if(rec_type != 999) error("expecting a dictionary termination record");
  sys_read_int(&filler,s);
  s->data_pos = ftell32(s->f);
  return ScalarInteger(s->data_pos);
}

SEXP rewind_sysfile(SEXP SysFile){
	int ret;
#ifdef DEBUG
  Rprintf("\nrewind_sysfile");
#endif
  sys_file *s = get_sys_file(SysFile);
#ifdef DEBUG
  Rprintf("\ns address: %x",s);
  Rprintf("\nFile: %x",s->f);
  Rprintf("\nData pos: %d",s->data_pos);
  int a;
  ret = fseek(s->f,0,SEEK_SET);
  a = getc(s->f);
  Rprintf("\nRead num at 0: %d",a);
#endif
  ret = fseek(s->f,s->data_pos,SEEK_SET);
#ifdef DEBUG
  Rprintf("\ns address: %x",s);
  Rprintf("\nFile after rewind: %x",s->f);
  Rprintf("\nFile pos: %d",ftell(s->f));
	a = getc(s->f);
	Rprintf("\nRead num: %d",a);
	a = getc(s->f);
	Rprintf("\nRead num: %d",a);
  ret = fseek(s->f,s->data_pos,SEEK_SET);
#endif
#undef DEBUG
  if(ret) error("error in sys_rewind");
  memset(s->bytes,0,8);
  s->byte_pos = 0;
  return ScalarInteger(s->data_pos);
}


#define STRMAX 256
SEXP read_sysfile_data (SEXP SysFile, SEXP what,
                        SEXP s_ncases, SEXP s_types){
    sys_file *s = get_sys_file(SysFile);
#ifdef DEBUG
    Rprintf("\nread_sysfile_data");
    PrintValue(s_types);
    PrintValue(s_ncases);
#endif
    if(s->case_size == 0) error("case size is zero -- why??");
    PROTECT(s_types = coerceVector(s_types,INTSXP));
    PROTECT(s_ncases = coerceVector(s_ncases,INTSXP));
    int nvar = 0;
    int *types = INTEGER(s_types);
    int ncases = INTEGER(s_ncases)[0];
    int i, j, k, new_ncases;
    SEXP data, x, y;
    for(j = 0; j < s->case_size; j++)
      if(types[j] >= 0) nvar++;

    PROTECT(data = allocVector(VECSXP,nvar));
   if(s->case_size == 0) error("case size is zero after argument decoding -- why??");
   k = 0;
   for(j = 0; j < s->case_size; j++){
      if(types[j] == 0){
        SET_VECTOR_ELT(data,k,allocVector(REALSXP,ncases));
        k++;
        }
      else if(types[j] >= 1) {
        SET_VECTOR_ELT(data,k,allocVector(STRSXP,ncases));
        k++;
        }
    }
    int str_len = 0;
    int str_count = 0;
    int read_len;
    char char_buf[STRMAX];
   if(s->case_size == 0) error("case size is zero after buffer allocation -- why??");
    R_flt64 swapped_sysmis = dswap(s->sysmis,s->swap_code);
    for(i = 0; i < ncases; i++){
        read_len = sys_read_case(s);
#ifdef DEBUG
      if(i == 0){
        Rprintf("\ns address = %d",s);
        Rprintf("\nbuf address = %d",s->buf);
        Rprintf("\ncase_size = %d",s->case_size);
        Rprintf("\nbuf = ");
        int jj;
        for(jj = 0; jj < s->case_size; jj++)
          Rprintf("%g ",s->buf[jj]);
        Rprintf("\nFile = %d",s->f);
        Rprintf("\n file pos = %d",ftell(s->f));
        Rprintf("\n");
      }
#endif
#undef DEBUG
      if(read_len == 0){
        new_ncases = i;
        for(j = 0; j < nvar; j++){
          x = VECTOR_ELT(data,j);
          SET_VECTOR_ELT(data,j,lengthgets(x,new_ncases));
        }
#ifdef DEBUG
        Rprintf("\nReached end of cases");
#endif
        ncases = new_ncases;
        break;
      }
      if(read_len < s->case_size) {
            warning("end of file in unfinished case, i=%d, read length=%d",i,read_len);
            new_ncases = i;
            ncases = new_ncases;
            break;
      }
#ifdef DEBUG
      Rprintf("\nbuf = |%s|",buf);
#endif
      k = 0;
      for(j = 0; j < s->case_size; j++){
#ifdef DEBUG
        Rprintf("\nj=%d var=%d",j,var);
        Rprintf(" name: %s",CHAR(STRING_ELT(names,var)));
        Rprintf(" types[j]=%d ",types[j]);
#endif
        if(types[j] == 0){
            x = VECTOR_ELT(data,k);
            if(s->buf[j] == swapped_sysmis) REAL(x)[i] = NA_REAL;
            else REAL(x)[i] = dswap(s->buf[j],s->swap_code);
            k++;
        }
        else if(types[j] >= 1) {
          memset(char_buf,0,STRMAX);
          memcpy(char_buf,s->buf+j,8);
          if(types[j]<=8){
            trim(char_buf,(int)strlen(char_buf));
#ifdef DEBUG
            Rprintf("\nchar_buf=|%s|",char_buf);
#endif
            x = VECTOR_ELT(data,k);
            SET_STRING_ELT(x,i,mkChar(char_buf));
            k++;
          } else {
            str_len = types[j];
            str_count = 1;
          }
        }
        else if(types[j] == -1){
          if(8*str_count > STRMAX-8) error("str_count out of bounds, 8*str_count = %d, STRMAX = %d",8*str_count,STRMAX);
          memcpy(&char_buf[8*str_count],s->buf+j,8);
#ifdef DEBUG
          Rprintf("\n8*str_count=%d str_len=%d",8*str_count,str_len);
#endif
          str_count++;
          if(8*str_count >= str_len){
            trim(char_buf,(int)strlen(char_buf));
#ifdef DEBUG
            Rprintf("\nchar_buf=|%s|",char_buf);
#endif
            x = VECTOR_ELT(data,k);
            SET_STRING_ELT(x,i,mkChar(char_buf));
            str_count = 0;
            k++;
          }
        }
        else error("invalid type specifier");
      }
    }
#ifdef DEBUG
        Rprintf("\ns address = %d",s);
        Rprintf("\nbuf address = %d",s->buf);
        Rprintf("\ncase_size = %d",s->case_size);
        Rprintf("\nFile = %d",s->f);
        Rprintf("\n");
#endif
//     Free(buf);
    for(j = 0; j < nvar; j++){
      x = VECTOR_ELT(what,j);
      y = VECTOR_ELT(data,j);
      copyMostAttrib(x,y);
    }
    UNPROTECT(3);
    return data;
}
#undef DEBUG

SEXP count_cases_sysfile (SEXP SysFile){
    sys_file *s = get_sys_file(SysFile);
#ifdef DEBUG
    Rprintf("\ncount_cases_sysfile");
#endif
    if(s->case_size == 0) error("case size is zero -- why??");
    int ncases = 0;
    int i;
    int read_len;
    for(i = 0; ; i++){
        read_len = sys_read_case(s);
      if(read_len == 0){
        ncases = i;
#ifdef DEBUG
        Rprintf("\nReached end of cases");
#endif
        break;
        }
      if(read_len < s->case_size) {
            warning("end of file in unfinished case, i=%d, read length=%d",i,read_len);
            ncases = i;
            break;
      }
    }

    return ScalarInteger(ncases);
}
#undef DEBUG

SEXP read_sysfile_slice (SEXP SysFile, SEXP what,
                          SEXP s_vars, SEXP s_cases, SEXP s_types){
  PROTECT(SysFile);
    sys_file *s = get_sys_file(SysFile);
    if(s->case_size == 0) error("case size is zero -- why??");

  PROTECT(s_vars = coerceVector(s_vars,LGLSXP));
  PROTECT(s_cases = coerceVector(s_cases,LGLSXP));
  PROTECT(s_types = coerceVector(s_types,INTSXP));
  int nvar = length(s_vars);
  int new_ncases, ncases = length(s_cases);
  int *types = INTEGER(s_types);
//   if(LENGTH(s_vars)!=nvar) error("\'s_vars\' argument has wrong length");

  int ii,i,j,k,l, m=0, n = 0;
  for(j = 0; j < nvar; j++) m+=LOGICAL(s_vars)[j];
  for(i = 0; i < ncases; i++) n+=LOGICAL(s_cases)[i];
  SEXP x, y, data;
  /*char *charbuf;
  int charbuflen = 0;*/
  PROTECT(data = allocVector(VECSXP,m));
  k = 0;
  l = 0;
   for(j = 0; j < s->case_size; j++){
      if(types[j] == 0){
        if(k >= nvar) error("index k out of bounds, k = %d, nvar = %d",k,m);
        if(LOGICAL(s_vars)[k]){
          if(l >= m) error("index l out of bounds, l = %d, m = %d",l,m);
          SET_VECTOR_ELT(data,l,allocVector(REALSXP,n));
          l++;
          }
        k++;
      }
      else if(types[j] >= 1) {
        if(k >= nvar) error("index k out of bounds, k = %d, nvar = %d",k,m);
        if(LOGICAL(s_vars)[k]){
          if(l >= m) error("index l out of bounds, l = %d, m = %d",l,m);
          SET_VECTOR_ELT(data,l,allocVector(STRSXP,n));
          l++;
          }
        k++;
      }
    }

    int str_len = 0;
    int str_count = 0;
    int read_len;
    char char_buf[STRMAX];
   if(s->case_size == 0) error("case size is zero after buffer allocation -- why??");
   R_flt64 swapped_sysmis = dswap(s->sysmis,s->swap_code);
#ifdef DEBUG
   Rprintf("\nswapped_sysmis = %g",swapped_sysmis);
#endif
   ii = 0;
    for(i = 0; i < ncases; i++){
      read_len = sys_read_case(s);
      if(read_len == 0){
        new_ncases = i;
        for(j = 0; j < m; j++){
          x = VECTOR_ELT(data,j);
          SET_VECTOR_ELT(data,j,lengthgets(x,new_ncases));
        }
        ncases = new_ncases;
        break;
      }
      if(read_len < s->case_size) {
            warning("end of file in unfinished case, i=%d, read length=%d",i,read_len);
            new_ncases = i;
            ncases = new_ncases;
            break;
      }
      if(LOGICAL(s_cases)[i]){
        k = 0;
        l = 0;
        for(j = 0; j < s->case_size; j++){
            /*Rprintf("i=%d, j=%d, k=%d, l=%d, ii=%d\n",i,j,k,l,ii);*/
            if(types[j] == 0){
              if(k >= nvar) error("index k out of bounds, k = %d, nvar = %d",k,m);
              if(LOGICAL(s_vars)[k]){
                if(l >= m) error("index l out of bounds, l = %d, m = %d",l,m);
                x = VECTOR_ELT(data,l);
                if(s->buf[j] == swapped_sysmis) REAL(x)[ii] = NA_REAL;
                else REAL(x)[ii] = dswap(s->buf[j],s->swap_code);
                l++;
              }
              k++;
            }
            else if(types[j] >= 1) {
              memset(char_buf,0,STRMAX);
              memcpy(char_buf,s->buf+j,8);
              if(types[j]<=8){
                if(k >= nvar) error("index k out of bounds, k = %d, nvar = %d",k,m);
                if(LOGICAL(s_vars)[k]){
                  trim(char_buf,(int)strlen(char_buf));
                  if(l >= m) error("index l out of bounds, l = %d, m = %d",l,m);
                  x = VECTOR_ELT(data,l);
                  SET_STRING_ELT(x,ii,mkChar(char_buf));
                  l++;
                }
                k++;
              } else {
                str_len = types[j];
                str_count = 1;
              }
            }
            else if(types[j] == -1){
              if(8*str_count > STRMAX-8) error("str_count out of bounds, 8*str_count = %d, STRMAX = %d",8*str_count,STRMAX);
              memcpy(&char_buf[8*str_count],s->buf+j,8);
              str_count++;
              if(8*str_count >= str_len){
                trim(char_buf,(int)strlen(char_buf));
                if(k >= nvar) error("index k out of bounds, k = %d, nvar = %d",k,m);
                if(LOGICAL(s_vars)[k]){
                  if(l >= m) error("index l out of bounds, l = %d, m = %d",l,m);
                  x = VECTOR_ELT(data,l);
                  SET_STRING_ELT(x,ii,mkChar(char_buf));
                  l++;
                }
                str_count = 0;
                k++;
              }
            }
            else error("invalid type specifier");
          }
        ii++;
        }
      }
    k = 0;
    for(j = 0; j < nvar; j++){
      if(LOGICAL(s_vars)[j]){
        x = VECTOR_ELT(what,j);
        if(k >= nvar) error("index k out of bounds, k = %d, nvar = %d",k,m);
        y = VECTOR_ELT(data,k);
        copyMostAttrib(x,y);
        k++;
      }
    }

    UNPROTECT(5);
    return data;
}


SEXP check_pointer(SEXP ptr){
  if(TYPEOF(ptr) != EXTPTRSXP) return ScalarLogical(0);
  if(!R_ExternalPtrAddr(ptr))  return ScalarLogical(0);
  return ScalarLogical(1);
}

#define FPREP_IEEE754 754
#define FPREP FPREP_IEEE754
/*
 * This comes from package 'foreign' which again had copied the code
 * from an older version of PSPP
*/
static double
second_lowest_double_val(void)
{
  /* PORTME: Set the value for second_lowest_value, which is the
     "second lowest" possible value for a double.  This is the value
     for LOWEST on MISSING VALUES, etc. */
#if FPREP == FPREP_IEEE754
#ifdef WORDS_BIGENDIAN
    union {
        unsigned char c[8];
        double d;
    } second_lowest = {{0xff, 0xef, 0xff, 0xff, 0xff, 0xff, 0xff, 0xfe}};
#else
    union {
        unsigned char c[8];
        double d;
    } second_lowest = {{0xfe, 0xff, 0xff, 0xff, 0xff, 0xff, 0xef, 0xff}};
#endif
    return second_lowest.d;
#else /* FPREP != FPREP_IEEE754 */
#error Unknown floating-point representation.
#endif /* FPREP != FPREP_IEEE754 */
}


SEXP read_sysfile_chunk (SEXP SysFile, SEXP what,
                          SEXP s_vars, SEXP s_ncases, SEXP s_types){
  PROTECT(SysFile);
    sys_file *s = get_sys_file(SysFile);
    if(s->case_size == 0) error("case size is zero -- why??");

  PROTECT(s_vars = coerceVector(s_vars,LGLSXP));
  PROTECT(s_ncases = coerceVector(s_ncases,INTSXP));
  PROTECT(s_types = coerceVector(s_types,INTSXP));
  int nvar = length(s_vars);
  int new_ncases, ncases = INTEGER(s_ncases)[0];
  int *types = INTEGER(s_types);
//   if(LENGTH(s_vars)!=nvar) error("\'s_vars\' argument has wrong length");

  int ii,i,j,k,l, m=0;
  for(j = 0; j < nvar; j++) m+=LOGICAL(s_vars)[j];

  SEXP x, y, data;
  /*char *charbuf;
  int charbuflen = 0;*/
  PROTECT(data = allocVector(VECSXP,m));
  k = 0;
  l = 0;
   for(j = 0; j < s->case_size; j++){
      if(types[j] == 0){
        if(k >= nvar) error("index k out of bounds, k = %d, nvar = %d",k,m);
        if(LOGICAL(s_vars)[k]){
          if(l >= m) error("index l out of bounds, l = %d, m = %d",l,m);
          SET_VECTOR_ELT(data,l,allocVector(REALSXP,ncases));
          l++;
          }
        k++;
      }
      else if(types[j] >= 1) {
        if(k >= nvar) error("index k out of bounds, k = %d, nvar = %d",k,m);
        if(LOGICAL(s_vars)[k]){
          if(l >= m) error("index l out of bounds, l = %d, m = %d",l,m);
          SET_VECTOR_ELT(data,l,allocVector(STRSXP,ncases));
          l++;
          }
        k++;
      }
    }

    int str_len = 0;
    int str_count = 0;
    int read_len;
    char char_buf[STRMAX];
   if(s->case_size == 0) error("case size is zero after buffer allocation -- why??");
   R_flt64 swapped_sysmis = dswap(s->sysmis,s->swap_code);
#ifdef DEBUG
   Rprintf("\nswapped_sysmis = %g",swapped_sysmis);
#endif
   ii = 0;
    for(i = 0; i < ncases; i++){
      read_len = sys_read_case(s);
      if(read_len == 0){
        new_ncases = i;
        for(j = 0; j < m; j++){
          x = VECTOR_ELT(data,j);
          SET_VECTOR_ELT(data,j,lengthgets(x,new_ncases));
        }
        ncases = new_ncases;
        break;
      }
      if(read_len < s->case_size) {
            warning("end of file in unfinished case, i=%d, read length=%d",i,read_len);
            new_ncases = i;
            ncases = new_ncases;
            break;
      }
      k = 0;
      l = 0;
      for(j = 0; j < s->case_size; j++){
	/*Rprintf("i=%d, j=%d, k=%d, l=%d, ii=%d\n",i,j,k,l,ii);*/
	if(types[j] == 0){
	  if(k >= nvar) error("index k out of bounds, k = %d, nvar = %d",k,m);
	  if(LOGICAL(s_vars)[k]){
	    if(l >= m) error("index l out of bounds, l = %d, m = %d",l,m);
	    x = VECTOR_ELT(data,l);
	    if(s->buf[j] == swapped_sysmis) REAL(x)[ii] = NA_REAL;
	    else REAL(x)[ii] = dswap(s->buf[j],s->swap_code);
	    l++;
	  }
	  k++;
	}
	else if(types[j] >= 1) {
	  memset(char_buf,0,STRMAX);
	  memcpy(char_buf,s->buf+j,8);
	  if(types[j]<=8){
	    if(k >= nvar) error("index k out of bounds, k = %d, nvar = %d",k,m);
	    if(LOGICAL(s_vars)[k]){
	      trim(char_buf,(int)strlen(char_buf));
	      if(l >= m) error("index l out of bounds, l = %d, m = %d",l,m);
	      x = VECTOR_ELT(data,l);
	      SET_STRING_ELT(x,ii,mkChar(char_buf));
	      l++;
	    }
	    k++;
	  } else {
	    str_len = types[j];
	    str_count = 1;
	  }
	}
	else if(types[j] == -1){
	  if(8*str_count > STRMAX-8) error("str_count out of bounds, 8*str_count = %d, STRMAX = %d",8*str_count,STRMAX);
	  memcpy(&char_buf[8*str_count],s->buf+j,8);
	  str_count++;
	  if(8*str_count >= str_len){
	    trim(char_buf,(int)strlen(char_buf));
	    if(k >= nvar) error("index k out of bounds, k = %d, nvar = %d",k,m);
	    if(LOGICAL(s_vars)[k]){
	      if(l >= m) error("index l out of bounds, l = %d, m = %d",l,m);
	      x = VECTOR_ELT(data,l);
	      SET_STRING_ELT(x,ii,mkChar(char_buf));
	      l++;
	    }
	    str_count = 0;
	    k++;
	  }
	}
	else error("invalid type specifier");
      }
      ii++;
    }
    k = 0;
    for(j = 0; j < nvar; j++){
      if(LOGICAL(s_vars)[j]){
        x = VECTOR_ELT(what,j);
        if(k >= nvar) error("index k out of bounds, k = %d, nvar = %d",k,m);
        y = VECTOR_ELT(data,k);
        copyMostAttrib(x,y);
        k++;
      }
    }

    UNPROTECT(5);
    return data;
}
