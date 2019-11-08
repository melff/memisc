#include <R.h>
#include <Rdefines.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <Rinternals.h>
#include "memisc.h"
#include "dumbswap.h"
#include "dta-for-R.h"

#define asString(x) CHAR(asChar(x))

#ifdef WORDS_BIGENDIAN
#define MY_DTA_BYTEORDER "MSF"
#else
#define MY_DTA_BYTEORDER "LSF"
#endif

typedef struct {
  FILE *f;
  int release;
  int nvars;
  off_t nobs;
  int l_record;
  off_t startdata;
  off_t enddata;
  struct {
    off_t  hdr ;
    off_t  map ;  
    off_t  types ; 
    off_t  varnames ;
    off_t  srtlist ;
    off_t  fmts ;
    off_t  vlblnames ;
    off_t  varlabs ;
    off_t  chars ;
    off_t  data ;
    off_t  strls ; 
    off_t  vallabs ;
    off_t  tlr ;
    off_t  bot ;
  } map ;
  int swap;
} dta117_file;


#define DTA117_MAXSTR    2045

#define DTA117_STR_L     32768

#define DTA117_DOUBLE    65526
#define DTA117_FLOAT     65527
#define DTA117_LONG      65528
#define DTA117_SHORT     65529
#define DTA117_BYTE      65530


SEXP dta117_file_close(SEXP);
SEXP dta117_file_open (SEXP name){
  dta117_file *dtaf = Calloc(1,dta117_file);
  dtaf->release = 0;
  dtaf->swap = 0;
  dtaf->nvars = 0;
  dtaf->nobs = 0;
  dtaf->l_record = 0;
  dtaf->startdata = 0;
  dtaf->enddata = 0;
  dtaf->f = fopen(asString(name),"r+b");
  if (dtaf->f == NULL){
      Free(dtaf);
      error("cannot open file");
      }
  dtaf->map.hdr       = 0;
  dtaf->map.map       = 0;
  dtaf->map.varnames  = 0;
  dtaf->map.srtlist   = 0;
  dtaf->map.fmts      = 0;
  dtaf->map.vlblnames = 0;
  dtaf->map.varlabs   = 0;
  dtaf->map.chars     = 0;
  dtaf->map.data      = 0;
  dtaf->map.strls     = 0;
  dtaf->map.vallabs   = 0;
  dtaf->map.tlr       = 0;
  dtaf->map.bot       = 0;
  SEXP ans = R_MakeExternalPtr(dtaf, install("dta117_file"), R_NilValue);
	PROTECT(ans);
  R_RegisterCFinalizer(ans, (R_CFinalizer_t) dta117_file_close);
  setAttrib(ans,install("file.name"),name);
	UNPROTECT(1);
  return ans;
}

SEXP dta117_file_close(SEXP s_file)
{
    if(TYPEOF(s_file) != EXTPTRSXP || R_ExternalPtrTag(s_file) != install("dta117_file")) error("not a Stata file");
    dta117_file *dtaf = R_ExternalPtrAddr(s_file);
    if(dtaf != NULL){
      /* Rprintf("closing file %s\n",asString(getAttrib(s_file,install("file.name")))); */
      if(dtaf->f != NULL)
        fclose(dtaf->f);
      R_ClearExternalPtr(s_file);
    }
    return R_NilValue;
}

dta117_file *get_dta117_file(SEXP s_file){
  if(TYPEOF(s_file) != EXTPTRSXP || R_ExternalPtrTag(s_file) != install("dta117_file")) error("not an Stata file");
  dta117_file *dtaf = R_ExternalPtrAddr(s_file);
  if (dtaf == NULL || dtaf->f == NULL){
    SEXP name = getAttrib(s_file,install("file.name"));
    if(dtaf != NULL){
      Free(dtaf);
    }
    error("external pointer is NULL, you need to recreate this object");
  }
  return dtaf;
}



int dta117_read_byte(dta117_file *dtaf){
  char target;
  size_t read_len = fread(&target,1,1,dtaf->f);
  if(!read_len) return NA_INTEGER;
  if(target == DTA_NA_BYTE) return NA_INTEGER;
  else return (int)target;
}

int dta117_read_short(dta117_file *dtaf){
  short target;
  size_t read_len = fread(&target,2,1,dtaf->f);
  if(!read_len) return NA_INTEGER;
  sswap_if(target,dtaf->swap);
  if(target == DTA_NA_SHORT) return NA_INTEGER;
  return (int)target;
}

int dta117_read_ushort(dta117_file *dtaf){
  unsigned short target;
  size_t read_len = fread(&target,2,1,dtaf->f);
  if(!read_len) return NA_INTEGER;
  sswap_if(target,dtaf->swap);
  if(target == DTA_NA_SHORT) return NA_INTEGER;
  return (int)target;
}


int dta117_read_int(dta117_file *dtaf){
  int target;
  size_t read_len = fread(&target,4,1,dtaf->f);
  if(!read_len) return NA_INTEGER;
  iswap_if(target,dtaf->swap);
  if(target == DTA_NA_LONG) return NA_INTEGER;
  return (int)target;
}

int dta117_read_int64(dta117_file *dtaf){
  off_t target;
  size_t read_len = fread(&target,8,1,dtaf->f);
  dswap_if(target,dtaf->swap);
  /* Rprintf("int64: %d\n",target); */
  return target;
}

double dta117_read_float(dta117_file *dtaf){
  float target;
  size_t read_len = fread(&target,4,1,dtaf->f);
  if(!read_len) return NA_REAL;
  fswap_if(target,dtaf->swap);
  if(target == DTA_NA_FLOAT) return NA_REAL;
  return (double)target;
}

double dta117_read_double(dta117_file *dtaf){
  double target;
  size_t read_len = fread(&target,8,1,dtaf->f);
  if(!read_len) return NA_REAL;
  dswap_if(target,dtaf->swap);
  if(target == DTA_NA_DOUBLE) return NA_REAL;
  return target;
}

int dta117_read_string(dta117_file *dtaf, char* target, int nchar){
  int read_len = (int)fread(target,1,nchar,dtaf->f); // can safely assume that retval will not be larger than INT_MAX ?
  return read_len;
}

int dta117_skip_record(dta117_file *dtaf){
  return fseek(dtaf->f,dtaf->l_record,SEEK_CUR);
}

SEXP dta117_skip_records(SEXP s_dta_file, SEXP s_n){
  int i, n = asInteger(s_n);
  dta117_file *dtaf = get_dta117_file(s_dta_file);
  for(i = 0; i < n; i++)
    dta117_skip_record(dtaf);
  return R_NilValue;
}

SEXP dta117_ftell (SEXP s_file){
  dta117_file *dtaf = get_dta117_file(s_file);
  return ScalarInteger(ftell32(dtaf->f));
}

SEXP dta117_feof (SEXP s_file){
  dta117_file *dtaf = get_dta117_file(s_file);
  return ScalarLogical(feof(dtaf->f));
}


SEXP dta117_fseek (SEXP s_file, SEXP s_pos, SEXP s_whence){
  int seek_code[] = { SEEK_SET, SEEK_CUR, SEEK_END };
  dta117_file *dtaf = get_dta117_file(s_file);
  PROTECT(s_pos = AS_INTEGER(s_pos));
  PROTECT(s_whence = AS_INTEGER(s_whence));
  long pos = INTEGER(s_pos)[0];
  int whence = INTEGER(s_whence)[0]-1;
  if(whence > 2){
		UNPROTECT(2);
		return ScalarLogical(FALSE);
	}
  int retcode = fseek(dtaf->f,pos,seek_code[whence]);
  UNPROTECT(2);
  if (retcode == 0) return ScalarLogical(TRUE);
  else return ScalarLogical(FALSE);
}



#define asString(x) CHAR(asChar(x))

int read1toend(char* buf, int n, FILE *f){

  int i;
  for(i=0; i < n - 1; i++){
    buf[i] = buf[i + 1];
  }
  int c = fgetc(f);
  if(c == EOF)
    return -1L;
  else{
    buf[n - 1] = (char)c;
    return strlen(buf);
  }
}

off_t find_in_file(FILE *f, const char* pattern, int before, off_t limit){

  /* Rprintf("Pattern = '%s'\n", pattern); */

  int n = strlen(pattern);
  char *buf = calloc(n + 1, sizeof(char));
  int ret = fread(buf,1,n,f);
  /* Rprintf("Buffer content = '%s'\n",buf); */

  int rc = memcmp(pattern,buf,n);

  while(rc != 0 && ret >= 0 && (ftell(f) < limit || limit < 0)){
    ret = read1toend(buf,n,f);
    /* Rprintf("Buffer content = '%s' ",buf);
     * Rprintf("Return value = '%d'\n",ret); */
    rc = memcmp(pattern,buf,n);
  }
  /* Rprintf("Buffer content = '%s'\n",buf); */

  if(rc != 0)
    return -1L;
  else {
    ret = ftell(f);
    if(before)
      ret -= strlen(pattern);
    return ret;
  }
}

int assert_in_file(FILE *f, const char* pattern){

  /* Rprintf("Pattern = '%s'\n", pattern); */

  int n = strlen(pattern);
  char *buf = calloc(n + 1, sizeof(char));
  int ret = fread(buf,1,n,f);
  /* Rprintf("Buffer content = '%s'\n",buf); */

  int rc = memcmp(pattern,buf,n);

  /* Rprintf("Comparison = %d\n",rc); */

  if(rc != 0)
    fseek(f,-n,SEEK_CUR);

  return rc != 0 ? 0 : 1;
}


SEXP dta117_find_in_file(SEXP s_dta_file, SEXP s_pattern){
  dta117_file *dtaf = get_dta117_file(s_dta_file);
  const char *pattern = asString(s_pattern);
  off_t ret = find_in_file(dtaf->f, pattern,0,-1);
  if(ret>=0)
    return ScalarInteger(ret);
  else
    return R_NilValue;
}

SEXP dta117_check_magic(SEXP s_dta_file){
  dta117_file *dtaf = get_dta117_file(s_dta_file);
  fseek(dtaf->f,0,SEEK_SET);
  int ret = assert_in_file(dtaf->f,"<stata_dta>");
  return ScalarInteger(ret);
}

char *dta117_read_range(FILE *f, size_t start, size_t end){
  /* Rprintf("Read from %d to %d",start,end); */
  size_t len = end - start;
  char *buf = R_alloc(sizeof(char),len+1);
  /* Rprintf(" - length: %d\n",len); */
  memset(buf,0,len+1);
  fseek(f,start,SEEK_SET);
  int ret = fread(buf,1,len,f);
  /* Rprintf("Buffer content = '%s'\n",buf); */
  return buf;
}


SEXP dta117_read_header(SEXP s_dta_file){

  dta117_file *dtaf = get_dta117_file(s_dta_file);

  fseek(dtaf->f,0,SEEK_SET);
  off_t header_start = find_in_file(dtaf->f,"<header>",0,-1);
  off_t release_start = find_in_file(dtaf->f,"<release>",0,-1);
  off_t release_end = find_in_file(dtaf->f,"</release>",1,-1);
  off_t byteorder_start = find_in_file(dtaf->f,"<byteorder>",0,-1);
  off_t byteorder_end = find_in_file(dtaf->f,"</byteorder>",1,-1);
  off_t K_start = find_in_file(dtaf->f,"<K>",0,-1);
  off_t K_end = find_in_file(dtaf->f,"</K>",1,-1);
  off_t N_start = find_in_file(dtaf->f,"<N>",0,-1);
  off_t N_end = find_in_file(dtaf->f,"</N>",1,-1);
  off_t label_start = find_in_file(dtaf->f,"<label>",0,-1);
  off_t label_end = find_in_file(dtaf->f,"</label>",1,-1);
  off_t timestamp_start = find_in_file(dtaf->f,"<timestamp>",0,-1);
  off_t timestamp_end = find_in_file(dtaf->f,"</timestamp>",1,-1);

  off_t header_end = find_in_file(dtaf->f,"</header>",1,-1);
  
  int release = atoi(dta117_read_range(dtaf->f,release_start,release_end));
  dtaf->release = release;

  if(release != 117 && release != 118 && release != 119)
    error("Unknown relase code or unsupported release %d",release);
  
  char *byteorder = dta117_read_range(dtaf->f,byteorder_start,byteorder_end);

  if(strcmp(byteorder,MY_DTA_BYTEORDER)!=0) dtaf->swap = 1;
  
  fseek(dtaf->f,K_start,SEEK_SET);
  int nvars;
  if(release < 119)
    nvars= dta117_read_short(dtaf);
  else
    nvars= dta117_read_int(dtaf);

  
  off_t nobs;
  fseek(dtaf->f,N_start,SEEK_SET);
  if(release == 117)
    nobs = dta117_read_int(dtaf);
  else 
    nobs = dta117_read_int64(dtaf);
    
  char *label = dta117_read_range(dtaf->f,label_start,label_end);
  char *timestamp = dta117_read_range(dtaf->f,timestamp_start,timestamp_end);

  int l_label = label[0];
  int l_timestamp = timestamp[0];

  dtaf->nvars = nvars;
  dtaf->nobs  = nobs;
  
  SEXP ans, names;
  PROTECT(ans = allocVector(VECSXP,7));
  PROTECT(names = allocVector(STRSXP,7));
  SET_VECTOR_ELT(ans,0,ScalarInteger(release));
  SET_VECTOR_ELT(ans,1,mkString(byteorder));
  SET_VECTOR_ELT(ans,2,ScalarInteger(nvars));
  SET_VECTOR_ELT(ans,3,ScalarInteger(nobs));
  SET_VECTOR_ELT(ans,4,mkString(label+1));
  SET_VECTOR_ELT(ans,5,mkString(timestamp+1));
  SET_VECTOR_ELT(ans,6,ScalarInteger(dtaf->swap));
  SET_STRING_ELT(names,0,mkChar("release"));
  SET_STRING_ELT(names,1,mkChar("byteorder"));
  SET_STRING_ELT(names,2,mkChar("nvars"));
  SET_STRING_ELT(names,3,mkChar("nobs"));
  SET_STRING_ELT(names,4,mkChar("label"));
  SET_STRING_ELT(names,5,mkChar("timestamp"));
  SET_STRING_ELT(names,6,mkChar("swapcode"));
  SET_NAMES(ans,names);
  UNPROTECT(2);

  return ans;
  
}

SEXP dta117_read_map(SEXP s_dta_file){

  dta117_file *dtaf = get_dta117_file(s_dta_file);

  fseek(dtaf->f,0,SEEK_SET);
  off_t map_start = find_in_file(dtaf->f,"<map>",0,-1);
  off_t map_end = find_in_file(dtaf->f,"</map>",1,-1);

  fseek(dtaf->f,map_start,SEEK_SET);
  dtaf->map.hdr       = dta117_read_int64(dtaf);
  dtaf->map.map       = dta117_read_int64(dtaf);
  dtaf->map.types     = dta117_read_int64(dtaf);
  dtaf->map.varnames  = dta117_read_int64(dtaf);
  dtaf->map.srtlist   = dta117_read_int64(dtaf);
  dtaf->map.fmts      = dta117_read_int64(dtaf);
  dtaf->map.vlblnames = dta117_read_int64(dtaf);
  dtaf->map.varlabs   = dta117_read_int64(dtaf);
  dtaf->map.chars     = dta117_read_int64(dtaf);
  dtaf->map.data      = dta117_read_int64(dtaf);
  dtaf->map.strls     = dta117_read_int64(dtaf);
  dtaf->map.vallabs   = dta117_read_int64(dtaf);
  dtaf->map.tlr       = dta117_read_int64(dtaf);
  dtaf->map.bot       = dta117_read_int64(dtaf);
  
  return R_NilValue;
}

SEXP dta117_read_vtypes(SEXP s_dta_file){

  dta117_file *dtaf = get_dta117_file(s_dta_file);

  fseek(dtaf->f,0,SEEK_SET);
  off_t vtypes_start = find_in_file(dtaf->f,"<variable_types>",0,-1);
  off_t vtypes_end = find_in_file(dtaf->f,"</variable_types>",1,-1);
  fseek(dtaf->f,vtypes_start,SEEK_SET);
  int i, n = dtaf->nvars;
  int code;
  SEXP s_types;

  PROTECT(s_types = allocVector(INTSXP,n));
  dtaf->l_record = 0;
  for(i = 0; i < n; i++){
    code = dta117_read_ushort(dtaf);
    INTEGER(s_types)[i] = code;
    if(code <= DTA117_MAXSTR)
      dtaf->l_record+=code;
    else switch(code){
      case DTA117_BYTE:
        dtaf->l_record++;
        break;
      case DTA117_SHORT:
        dtaf->l_record+=2;
        break;
      case DTA117_LONG:
        dtaf->l_record+=4;
        break;
      case DTA117_FLOAT:
        dtaf->l_record+=4;
        break;
      case DTA117_DOUBLE:
        dtaf->l_record+=8;
        break;
      case DTA117_STR_L:
        dtaf->l_record+=8;
        break;
    }
  }
  UNPROTECT(1);
  return s_types;
}

SEXP dta117_make_prototype(SEXP s_types){
  int j, nvar = length(s_types);
  SEXP ans, double_item, integer_item, character_item;
  PROTECT(ans = allocVector(VECSXP,nvar));
  int *types = INTEGER(s_types);
  PROTECT(double_item = R_getClassDef("double.item"));
  PROTECT(integer_item = R_getClassDef("integer.item"));
  PROTECT(character_item = R_getClassDef("character.item"));
  for(j = 0; j< nvar; j++){
    if(types[j]<=DTA117_MAXSTR || types[j] == DTA117_STR_L)
      SET_VECTOR_ELT(ans,j,R_do_new_object(character_item));
    else switch(types[j]){
      case DTA117_BYTE:
      case DTA117_SHORT:
      case DTA117_LONG:
        SET_VECTOR_ELT(ans,j,R_do_new_object(integer_item));
        break;
      case DTA117_FLOAT:
      case DTA117_DOUBLE:
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



SEXP dta117_read_vnames(SEXP s_dta_file){

  dta117_file *dtaf = get_dta117_file(s_dta_file);

  fseek(dtaf->f,0,SEEK_SET);
  off_t vnames_start = find_in_file(dtaf->f,"<varnames>",0,-1);
  off_t vnames_end = find_in_file(dtaf->f,"</varnames>",1,-1);
  fseek(dtaf->f,vnames_start,SEEK_SET);
  int ret, i, n = dtaf->nvars;

  int l_varnames;
  if(dtaf->release == 117)
    l_varnames = 32;
  else 
    l_varnames = 128;
  char *buf = R_alloc(1,l_varnames + 1);

  SEXP ans;
  PROTECT(ans = allocVector(STRSXP,n));
  for(i = 0; i < n; i++){
    ret = dta117_read_string(dtaf,buf,l_varnames + 1);
    SET_STRING_ELT(ans,i,mkChar(buf));
  }
  UNPROTECT(1);
  return ans;
}



SEXP dta117_read_sortlist(SEXP s_dta_file){

  dta117_file *dtaf = get_dta117_file(s_dta_file);

  fseek(dtaf->f,0,SEEK_SET);
  off_t slist_start = find_in_file(dtaf->f,"<sortlist>",0,-1);
  off_t slist_end = find_in_file(dtaf->f,"</sortlist>",1,-1);
  fseek(dtaf->f,slist_start,SEEK_SET);
  int i, n = dtaf->nvars;
  unsigned int buf;
  SEXP ans;
  PROTECT(ans = allocVector(INTSXP,n+1));
  for(i = 0; i < n+1; i++){
    if(dtaf->release < 119)
      buf = dta117_read_short(dtaf);
    else
      buf = dta117_read_int(dtaf);
    INTEGER(ans)[i] = buf;
  }
  UNPROTECT(1);
  return ans;
}


SEXP dta117_read_formats(SEXP s_dta_file){

  dta117_file *dtaf = get_dta117_file(s_dta_file);

  fseek(dtaf->f,0,SEEK_SET);
  off_t formats_start = find_in_file(dtaf->f,"<formats>",0,-1);
  off_t formats_end = find_in_file(dtaf->f,"</formats>",1,-1);
  fseek(dtaf->f,formats_start,SEEK_SET);
  int ret, i, n = dtaf->nvars;
  int l_format;
  if (dtaf->release == 117)
    l_format = 48;
  else 
    l_format = 56;
  
  char *buf = R_alloc(1,l_format+1);
  SEXP ans;
  PROTECT(ans = allocVector(STRSXP,n));
  for(i = 0; i < n; i++){
    ret = dta117_read_string(dtaf,buf,l_format + 1);
    SET_STRING_ELT(ans,i,mkChar(buf));
  }
  UNPROTECT(1);
  return ans;
}

SEXP dta117_read_vlab_names(SEXP s_dta_file){

  dta117_file *dtaf = get_dta117_file(s_dta_file);

  fseek(dtaf->f,0,SEEK_SET);
  off_t vlab_names_start = find_in_file(dtaf->f,"<value_label_names>",0,-1);
  off_t vlab_names_end = find_in_file(dtaf->f,"</value_label_names>",1,-1);
  fseek(dtaf->f,vlab_names_start,SEEK_SET);
  int ret, i, n = dtaf->nvars;

  int l_vallabnames;
  if(dtaf->release == 117)
    l_vallabnames = 32;
  else 
    l_vallabnames = 128;
  char *buf = R_alloc(1,l_vallabnames + 1);

  SEXP ans;
  PROTECT(ans = allocVector(STRSXP,n));
  for(i = 0; i < n; i++){
    ret = dta117_read_string(dtaf,buf,l_vallabnames + 1);
    SET_STRING_ELT(ans,i,mkChar(buf));
  }
  UNPROTECT(1);
  return ans;
}

SEXP dta117_read_varlabs(SEXP s_dta_file){

  dta117_file *dtaf = get_dta117_file(s_dta_file);

  fseek(dtaf->f,0,SEEK_SET);
  off_t varlabs_start = find_in_file(dtaf->f,"<variable_labels>",0,-1);
  off_t varlabs_end = find_in_file(dtaf->f,"</variable_labels>",1,-1);
  fseek(dtaf->f,varlabs_start,SEEK_SET);
  int ret, i, n = dtaf->nvars;

  int l_varlabs;
  if(dtaf->release == 117)
    l_varlabs = 80;
  else 
    l_varlabs = 320;
  char *buf = R_alloc(1,l_varlabs + 1);
  
  SEXP ans;
  PROTECT(ans = allocVector(STRSXP,n));
  for(i = 0; i < n; i++){
    ret = dta117_read_string(dtaf,buf,l_varlabs + 1);
    SET_STRING_ELT(ans,i,mkChar(buf));
  }
  UNPROTECT(1);
  return ans;
}

SEXP dta117_read_1vallab(dta117_file *dtaf, int tablen);
SEXP dta117_read_vallabs(SEXP s_dta_file){

  dta117_file *dtaf = get_dta117_file(s_dta_file);

  fseek(dtaf->f,dtaf->map.vallabs,SEEK_SET);
  off_t ret;
  off_t vallabs_start = find_in_file(dtaf->f,"<value_labels>",0,-1);
  off_t vallabs_end = find_in_file(dtaf->f,"</value_labels>",1,-1);
  fseek(dtaf->f,vallabs_start,SEEK_SET);
  int i, nlabs = 0, tablen;
  while(find_in_file(dtaf->f,"<lbl>",0,vallabs_end) > 0) nlabs++;
  fseek(dtaf->f,vallabs_start,SEEK_SET);
  SEXP ans, names;

  int l_vallabnames;
  if(dtaf->release == 117)
    l_vallabnames = 32;
  else 
    l_vallabnames = 128;
  char *labname = R_alloc(1,l_vallabnames + 1);

  PROTECT(ans = allocVector(VECSXP,nlabs));
  PROTECT(names = allocVector(STRSXP,nlabs));
  for(i = 0; i < nlabs; i++){
    ret = find_in_file(dtaf->f,"<lbl>",0,vallabs_end);
    tablen = dta117_read_int(dtaf);
    ret = dta117_read_string(dtaf,labname,l_vallabnames + 1);
    SET_STRING_ELT(names,i,mkChar(labname));
    SET_VECTOR_ELT(ans,i,dta117_read_1vallab(dtaf,tablen));
  }
  SET_NAMES(ans,names);
  UNPROTECT(2);
  return ans;
}

SEXP dta117_read_1vallab(dta117_file *dtaf, int tablen){
  char padding[3];
  dta117_read_string(dtaf,padding,3);
  int i,n = dta117_read_int(dtaf);
  int txtlen = dta117_read_int(dtaf);
  char *txtbuf = R_alloc(txtlen,1);
  SEXP off, val, lab;
  PROTECT(off = allocVector(INTSXP,n));
  PROTECT(val = allocVector(INTSXP,n));
  PROTECT(lab = allocVector(STRSXP,n));
  for(i = 0; i < n; i++)
    INTEGER(off)[i] = dta117_read_int(dtaf);
  for(i = 0; i < n; i++)
    INTEGER(val)[i] = dta117_read_int(dtaf);
  dta117_read_string(dtaf,txtbuf,txtlen);
  char *tmp;
  for(i = 0; i < n; i++){
    tmp = txtbuf + INTEGER(off)[i];
    SET_STRING_ELT(lab,i,mkChar(tmp));
    }
  SET_NAMES(val,lab);
  UNPROTECT(3);
  return val;
}

SEXP dta117_dim(SEXP s_dta_file){

  dta117_file *dtaf = get_dta117_file(s_dta_file);
  SEXP dim;
  PROTECT(dim = allocVector(INTSXP,2));
  INTEGER(dim)[0] = dtaf->nobs;
  INTEGER(dim)[1] = dtaf->nvars;
  UNPROTECT(1);
  return dim;
}

SEXP dta117_seek_data(SEXP s_dta_file){
  dta117_file *dtaf = get_dta117_file(s_dta_file);
  /* Rprintf("Start data: %d\n",dtaf->startdata); */
  if(dtaf->startdata == 0) {
    int checkvalue;
    /* Rprintf("Map data value: %d\n",dtaf->map.data); */
    fseek(dtaf->f,dtaf->map.data,SEEK_SET);
    checkvalue = assert_in_file(dtaf->f,"<data>");
    /* Rprintf("Check value: %d\n",checkvalue); */
    if(checkvalue==0)
      error("Missing <data> tag");
    dtaf->startdata = ftell(dtaf->f);
    fseek(dtaf->f, dtaf->map.strls - strlen("</data>"),SEEK_SET);
    checkvalue = assert_in_file(dtaf->f,"</data>");
    /* Rprintf("Check value: %d\n",checkvalue); */
    if(checkvalue==0)
      error("Missing </data> tag");
    dtaf->enddata = dtaf->map.strls - strlen("</data>");
  }
  /* Rprintf("Start data: %d\n",dtaf->startdata); */
  /* Rprintf("End data: %d\n",dtaf->enddata); */
  fseek(dtaf->f,dtaf->startdata,SEEK_SET);
  return ScalarInteger(dtaf->startdata);
}


char charbuf[DTA_MAXSTR + 1];

SEXP dta117_read_data(SEXP s_dta_file, SEXP what, SEXP s_nobs, SEXP s_types){
  dta117_file *dtaf = get_dta117_file(s_dta_file);
  int nobs = asInteger(s_nobs);
  int nvar = length(s_types);
  int i,j;
  SEXP ans, x, y;
  PROTECT(ans = allocVector(VECSXP,nvar));
  double v_o;
  int *types = INTEGER(s_types);
  for(j = 0; j< nvar; j++){
    if(types[j]<=DTA117_MAXSTR || types[j] == DTA117_STR_L)
      SET_VECTOR_ELT(ans,j,allocVector(STRSXP,nobs));
    else switch(types[j]){
      case DTA117_BYTE:
      case DTA117_SHORT:
      case DTA117_LONG:
        SET_VECTOR_ELT(ans,j,allocVector(INTSXP,nobs));
        break;
      case DTA117_FLOAT:
      case DTA117_DOUBLE:
        SET_VECTOR_ELT(ans,j,allocVector(REALSXP,nobs));
        break;
      default:
        error("unknown data type %d",types[j]);
        break;
    }
  }
  for(i = 0; i < nobs; i++){
    if(ftell(dtaf->f) >= dtaf->enddata) {
      int new_length = i;
      for(j = 0; j < nvar; j++){
        x = VECTOR_ELT(ans,j);
        SET_VECTOR_ELT(ans,j,lengthgets(x,new_length));
      }
      break;
    };
    for(j = 0; j < nvar; j++){
      x = VECTOR_ELT(ans,j);
      if(types[j]<=DTA_MAXSTR){
        dta117_read_string(dtaf,charbuf,types[j]);
        charbuf[types[j]] = 0; /* just to be sure ... */
        SET_STRING_ELT(x,i,mkChar(charbuf));
      }
      else switch(types[j]){
        case DTA117_BYTE:
          INTEGER(x)[i] = dta117_read_byte(dtaf);
          break;
        case DTA117_SHORT:
          INTEGER(x)[i] = dta117_read_short(dtaf);
          break;
        case DTA117_LONG:
          INTEGER(x)[i] = dta117_read_int(dtaf);
          break;
        case DTA117_FLOAT:
          REAL(x)[i] = dta117_read_float(dtaf);
          break;
        case DTA117_DOUBLE:
          REAL(x)[i] = dta117_read_double(dtaf);
          break;
        case DTA117_STR_L:
          v_o = dta117_read_double(dtaf);
          SET_STRING_ELT(x,i,NA_STRING);
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



SEXP dta117_read_slice(SEXP s_dta_file, SEXP what, SEXP vars, SEXP obs, SEXP s_types){
  dta117_file *dtaf = get_dta117_file(s_dta_file);
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
  int *types = INTEGER(s_types);
  k = 0;
  for(j = 0; j< nvar; j++){
    if(LOGICAL(vars)[j]){
    if(types[j]<=DTA117_MAXSTR || types[j] == DTA117_STR_L)
        SET_VECTOR_ELT(ans,k,allocVector(STRSXP,n));
      else switch(types[j]){
        case DTA117_BYTE:
        case DTA117_SHORT:
        case DTA117_LONG:
          SET_VECTOR_ELT(ans,k,allocVector(INTSXP,n));
          break;
        case DTA117_FLOAT:
        case DTA117_DOUBLE:
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
  double v_o;
  int tmp_int;
  double tmp_double;
  for(i = 0; i < nobs; i++){
    if(ftell(dtaf->f) >= dtaf->enddata) {
      l = 0;
      int new_length = k;
      for(j = 0; j < nvar; j++){
        if(LOGICAL(vars)[j]) {
          x = VECTOR_ELT(ans,l);
          SET_VECTOR_ELT(ans,l,lengthgets(x,new_length));
          l++;
        }
      }
      break;
    };
    if(LOGICAL(obs)[i]){
      l = 0;
      for(j = 0; j < nvar; j++){
          if(types[j]<=DTA117_MAXSTR){
            dta117_read_string(dtaf,charbuf,types[j]);
            charbuf[types[j]] = 0; /* just to be sure ... */
            if(LOGICAL(vars)[j]){
              x = VECTOR_ELT(ans,l);
              SET_STRING_ELT(x,k,mkChar(charbuf));
              l++;
            }
          }
          else switch(types[j]){
            case DTA117_BYTE:
              tmp_int = dta117_read_byte(dtaf);
              if(LOGICAL(vars)[j]){
                x = VECTOR_ELT(ans,l);
                INTEGER(x)[k] = tmp_int;
                l++;
              }
              break;
            case DTA117_SHORT:
              tmp_int = dta117_read_short(dtaf);
              if(LOGICAL(vars)[j]){
                x = VECTOR_ELT(ans,l);
                INTEGER(x)[k] = tmp_int;
                l++;
              }
              break;
            case DTA117_LONG:
              tmp_int = dta117_read_int(dtaf);
              if(LOGICAL(vars)[j]){
                x = VECTOR_ELT(ans,l);
                INTEGER(x)[k] = tmp_int;
                l++;
              }
              break;
            case DTA117_FLOAT:
              tmp_double = dta117_read_float(dtaf);
              if(LOGICAL(vars)[j]){
                x = VECTOR_ELT(ans,l);
                REAL(x)[k] = tmp_double;
                l++;
              }
              break;
            case DTA117_DOUBLE:
              tmp_double = dta117_read_double(dtaf);
              if(LOGICAL(vars)[j]){
                x = VECTOR_ELT(ans,l);
                REAL(x)[k] = tmp_double;
                l++;
              }
              break;
            case DTA117_STR_L:
              v_o = dta117_read_double(dtaf);
              if(LOGICAL(vars)[j]){
                x = VECTOR_ELT(ans,l);
                SET_STRING_ELT(x,k,NA_STRING);
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
      dta117_skip_record(dtaf);
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

SEXP dta117_read_chunk(SEXP s_dta_file, SEXP what, SEXP vars, SEXP s_nobs, SEXP s_types){
  dta117_file *dtaf = get_dta117_file(s_dta_file);
  PROTECT(vars = coerceVector(vars,LGLSXP));
  PROTECT(s_nobs = coerceVector(s_nobs,INTSXP));
  int nobs = INTEGER(s_nobs)[0];
  /*if(nobs > dtaf->n_records) error("obs argument has wrong length");*/
  int nvar = length(vars);
  if(nvar != length(s_types)) error("vars and types arguments differ in length");
  int i,j,k,l;
  SEXP ans, x, y;
  int m = 0;
  for(j = 0; j < nvar; j++)
    m += LOGICAL(vars)[j];
  
  PROTECT(ans = allocVector(VECSXP,m));
  int *types = INTEGER(s_types);
  k = 0;
  for(j = 0; j< nvar; j++){
    if(LOGICAL(vars)[j]){
      if(types[j]<=DTA117_MAXSTR || types[j] == DTA117_STR_L)
        SET_VECTOR_ELT(ans,k,allocVector(STRSXP,nobs));
      else switch(types[j]){
        case DTA117_BYTE:
        case DTA117_SHORT:
        case DTA117_LONG:
          SET_VECTOR_ELT(ans,k,allocVector(INTSXP,nobs));
          break;
        case DTA117_FLOAT:
        case DTA117_DOUBLE:
          SET_VECTOR_ELT(ans,k,allocVector(REALSXP,nobs));
          break;
        default:
          error("unknown data type %d",types[j]);
          break;
      }
      k++;
    }
  }
  double v_o;
  int tmp_int;
  double tmp_double;
  for(i = 0; i < nobs; i++){
    if(ftell(dtaf->f) >= dtaf->enddata) {
      l = 0;
      int new_length = i;
      for(j = 0; j < nvar; j++){
        if(LOGICAL(vars)[j]) {
          x = VECTOR_ELT(ans,l);
          SET_VECTOR_ELT(ans,l,lengthgets(x,new_length));
          l++;
        }
      }
      break;
    };
    l = 0;
    for(j = 0; j < nvar; j++){
      if(types[j]<=DTA117_MAXSTR){
        dta117_read_string(dtaf,charbuf,types[j]);
        charbuf[types[j]] = 0; /* just to be sure ... */
        if(LOGICAL(vars)[j]){
          x = VECTOR_ELT(ans,l);
          SET_STRING_ELT(x,i,mkChar(charbuf));
          l++;
        }
      }
      else switch(types[j]){
        case DTA117_BYTE:
          tmp_int = dta117_read_byte(dtaf);
          if(LOGICAL(vars)[j]){
            x = VECTOR_ELT(ans,l);
            INTEGER(x)[i] = tmp_int;
            l++;
          }
          break;
        case DTA117_SHORT:
          tmp_int = dta117_read_short(dtaf);
          if(LOGICAL(vars)[j]){
            x = VECTOR_ELT(ans,l);
            INTEGER(x)[i] = tmp_int;
            l++;
          }
          break;
        case DTA117_LONG:
          tmp_int = dta117_read_int(dtaf);
          if(LOGICAL(vars)[j]){
            x = VECTOR_ELT(ans,l);
            INTEGER(x)[i] = tmp_int;
            l++;
          }
          break;
        case DTA117_FLOAT:
          tmp_double = dta117_read_float(dtaf);
          if(LOGICAL(vars)[j]){
            x = VECTOR_ELT(ans,l);
            REAL(x)[i] = tmp_double;
            l++;
          }
          break;
        case DTA117_DOUBLE:
          tmp_double = dta117_read_double(dtaf);
          if(LOGICAL(vars)[j]){
            x = VECTOR_ELT(ans,l);
            REAL(x)[i] = tmp_double;
            l++;
          }
          break;
        case DTA117_STR_L:
          v_o = dta117_read_double(dtaf);
          if(LOGICAL(vars)[j]){
            x = VECTOR_ELT(ans,l);
            SET_STRING_ELT(x,k,NA_STRING);
            l++;
          }
          break;
        default:
          error("I should never arrive here!!");
          break;
        }
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

