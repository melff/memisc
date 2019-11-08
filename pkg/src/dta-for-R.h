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

static double dta_na_float;
static double dta_na_double;

typedef struct {
  FILE *f;
  int start_data;
  int l_record;
  int n_records;
  int swap;
  char version;
} dta_file;

dta_file *get_dta_file(SEXP);
int dta_read_byte(dta_file*);
int dta_read_short(dta_file*);
int dta_read_int(dta_file*);
double dta_read_float(dta_file*);
double dta_read_double(dta_file*);
int dta_read_string(dta_file*, char*, int);
int dta_skip_record(dta_file*);

