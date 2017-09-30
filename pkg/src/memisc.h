#include <R.h>
#include <Rdefines.h>
#include <string.h>
#include <Rmath.h>
#include <stdio.h>
#include <Rinternals.h>


FILE *get_FILE(SEXP);
void trim (char *, int);
int ftell32 (FILE *f);

SEXP NewSysFile (SEXP name);
SEXP rewind_sysfile(SEXP SysFile);
SEXP count_cases_sysfile (SEXP SysFile);
SEXP read_sysfile_data (SEXP SysFile, SEXP what,
                        SEXP s_ncases, SEXP s_types);
SEXP read_sysfile_chunk (SEXP SysFile, SEXP what,
												 SEXP s_vars, SEXP s_ncases, SEXP s_types);
SEXP read_sysfile_slice(SEXP SysFile, SEXP what,
                          SEXP s_vars, SEXP s_cases, SEXP s_types);
SEXP check_pointer(SEXP ptr);
SEXP restore_sysfile(SEXP SysFile);
SEXP read_sysfile_header(SEXP SysFile);
SEXP read_sysfile_var(SEXP SysFile);
SEXP test_sysfile_int32(SEXP SysFile);
SEXP read_sysfile_value_labels (SEXP SysFile);
SEXP read_sysfile_document(SEXP SysFile);
SEXP read_sysfile_aux(SEXP SysFile);
SEXP read_sysfile_dict_term (SEXP SysFile);
SEXP dflt_info_flt64 (SEXP SysFile);

SEXP NewPorStream (SEXP name);
SEXP closePorStream (SEXP porStream);
SEXP readOnePushbackPorStream(SEXP porStream);
SEXP readOnePorStream(SEXP porStream);
SEXP seekPorStream(SEXP porStream, SEXP s_pos);
SEXP tellPorStream (SEXP porStream);
SEXP lineTellPorStream (SEXP porStream);
SEXP offsetTellPorStream (SEXP porStream);
SEXP readToSlashPorStream(SEXP porStream, SEXP s_n);
SEXP readIntPorStream(SEXP porStream);
SEXP readDoublePorStream(SEXP porStream);
SEXP readStringPorStream(SEXP porStream);
SEXP setTranslationPorStream(SEXP porStream, SEXP s_in);
SEXP countCasesPorStream(SEXP porStream, SEXP s_types);
SEXP readDataPorStream(SEXP porStream, SEXP what,
											 SEXP s_n, SEXP s_types);
SEXP readSlicePorStream(SEXP porStream, SEXP what,
												SEXP s_vars, SEXP s_cases, SEXP s_types);
SEXP readChunkPorStream(SEXP porStream, SEXP what,
												 SEXP s_vars, SEXP s_ncases, SEXP s_types);
SEXP readPorStream(SEXP porStream, SEXP s_n);
SEXP rofile (SEXP name);
SEXP readfixed(SEXP s_file, SEXP what,
							 SEXP s_nlines, SEXP s_start, SEXP s_stop);
SEXP readfixedchunk(SEXP s_file, SEXP what,
										 SEXP s_vars, SEXP s_cases,
										 SEXP s_start, SEXP s_stop);
SEXP rofreadline(SEXP s_file);
SEXP roftell (SEXP s_file);
SEXP rofseek (SEXP s_file, SEXP s_pos,
							SEXP s_whence);
SEXP countlines(SEXP s_file, SEXP s_maxlenline);
SEXP numeric_if_possible(SEXP x);

SEXP dta_file_open (SEXP name);
SEXP dta_read_version(SEXP s_dta_file);
SEXP dta_make_prototype(SEXP s_types);
SEXP dta_seek_data(SEXP s_dta_file);
SEXP dta_read_data(SEXP s_dta_file, SEXP what,
									 SEXP s_nobs, SEXP s_types);
SEXP dta_read_slice(SEXP s_dta_file, SEXP what,
										SEXP vars, SEXP obs, SEXP s_types);
SEXP dta_read_chunk(SEXP s_dta_file, SEXP what,
										 SEXP vars, SEXP s_nobs, SEXP s_types);
SEXP dta_read_header(SEXP s_dta_file, SEXP s_lablen);
SEXP dta_read_descriptors(SEXP s_dta_file, SEXP s_nvar,
													SEXP s_len_varname, SEXP s_len_fmt,
													SEXP s_len_lblname);
SEXP dta_read_varlabs(SEXP s_dta_file, SEXP s_nvar,
											SEXP s_len_varlab);
SEXP dta_read_expansion_fields(SEXP s_dta_file,
															 SEXP s_shortext);
SEXP dta_read_labels (SEXP s_dta_file, SEXP s_lbl_len,
											SEXP s_padding);
SEXP dta_trans_types(SEXP s_types);
SEXP dta_calc_obssize(SEXP s_dta_file, SEXP typelist);
SEXP dta_ftell (SEXP s_file);
SEXP dta_fseek (SEXP s_file, SEXP s_pos, SEXP s_whence);
SEXP dta_feof (SEXP s_file);
SEXP dta_skip_records(SEXP s_dta_file, SEXP s_n);
SEXP ord_union(SEXP x, SEXP y);



static const R_CallMethodDef CallMethods[]  = {
  {"NewSysFile", (DL_FUNC) &NewSysFile, 1},
  {"rewind_sysfile", (DL_FUNC) &rewind_sysfile, 1},
  {"count_cases_sysfile", (DL_FUNC) &count_cases_sysfile, 1},
  {"read_sysfile_data", (DL_FUNC) &read_sysfile_data, 4},
  {"read_sysfile_chunk", (DL_FUNC) &read_sysfile_chunk, 5},
  {"read_sysfile_slice", (DL_FUNC) &read_sysfile_slice, 5},
  {"check_pointer", (DL_FUNC) &check_pointer, 1},
  {"restore_sysfile", (DL_FUNC) &restore_sysfile, 1},
	{"read_sysfile_header", (DL_FUNC) &read_sysfile_header, 1},
	{"read_sysfile_var", (DL_FUNC) &read_sysfile_var, 1},
	{"test_sysfile_int32", (DL_FUNC) &test_sysfile_int32, 1},
	{"read_sysfile_value_labels", (DL_FUNC) &read_sysfile_value_labels, 1},
	{"read_sysfile_document", (DL_FUNC) &read_sysfile_document, 1},
	{"read_sysfile_aux", (DL_FUNC) &read_sysfile_aux, 1},
	{"read_sysfile_dict_term", (DL_FUNC) &read_sysfile_dict_term, 1},
	{"dflt_info_flt64", (DL_FUNC) &dflt_info_flt64, 1},
	{"NewPorStream", (DL_FUNC) &NewPorStream, 1},
	{"closePorStream", (DL_FUNC) &closePorStream, 1},
	{"readOnePushbackPorStream", (DL_FUNC) &readOnePushbackPorStream, 1},
	{"readOnePorStream", (DL_FUNC) &readOnePorStream, 1},
	{"seekPorStream", (DL_FUNC) &seekPorStream, 2},
	{"tellPorStream", (DL_FUNC) &tellPorStream, 1},
	{"lineTellPorStream", (DL_FUNC) &lineTellPorStream, 1},
	{"offsetTellPorStream", (DL_FUNC) &offsetTellPorStream, 1},
	{"readToSlashPorStream", (DL_FUNC) &readToSlashPorStream, 2},
	{"readIntPorStream", (DL_FUNC) &readIntPorStream, 1},
	{"readDoublePorStream", (DL_FUNC) &readDoublePorStream, 1},
	{"readStringPorStream", (DL_FUNC) &readStringPorStream, 1},
	{"setTranslationPorStream", (DL_FUNC) &setTranslationPorStream, 2},
	{"countCasesPorStream", (DL_FUNC) &countCasesPorStream, 2},
	{"readDataPorStream", (DL_FUNC) &readDataPorStream, 4},
	{"readSlicePorStream", (DL_FUNC) &readSlicePorStream, 5},
	{"readChunkPorStream", (DL_FUNC) &readChunkPorStream, 5},
	{"readPorStream", (DL_FUNC) &readPorStream, 2},
	{"rofile", (DL_FUNC) &rofile, 1},
	{"readfixed", (DL_FUNC) &readfixed, 5},
	{"readfixedchunk", (DL_FUNC) &readfixedchunk, 6},
	{"rofreadline", (DL_FUNC) &rofreadline, 1},
	{"roftell", (DL_FUNC) &roftell, 1},
	{"rofseek", (DL_FUNC) &rofseek, 3},
	{"countlines", (DL_FUNC) &countlines, 2},
	{"numeric_if_possible", (DL_FUNC) &numeric_if_possible, 1},
	{"dta_file_open", (DL_FUNC) &dta_file_open, 2},
	{"dta_read_version", (DL_FUNC) &dta_read_version, 1},
	{"dta_make_prototype", (DL_FUNC) &dta_make_prototype, 1},
	{"dta_seek_data", (DL_FUNC) &dta_seek_data, 1},
	{"dta_read_data", (DL_FUNC) &dta_read_data, 4},
	{"dta_read_slice", (DL_FUNC) &dta_read_slice, 5},
	{"dta_read_chunk", (DL_FUNC) &dta_read_chunk, 5},
	{"dta_read_header", (DL_FUNC) &dta_read_header, 2},
	{"dta_read_descriptors", (DL_FUNC) &dta_read_descriptors, 5},
	{"dta_read_varlabs", (DL_FUNC) &dta_read_varlabs, 3},
	{"dta_read_expansion_fields", (DL_FUNC) &dta_read_expansion_fields, 2},
	{"dta_read_labels", (DL_FUNC) &dta_read_labels, 3},
	{"dta_trans_types", (DL_FUNC) &dta_trans_types, 1},
	{"dta_calc_obssize", (DL_FUNC) &dta_calc_obssize, 2},
	{"dta_ftell", (DL_FUNC) &dta_ftell, 1},
	{"dta_fseek", (DL_FUNC) &dta_fseek, 3},
	{"dta_feof", (DL_FUNC) &dta_feof, 1},
	{"dta_skip_records", (DL_FUNC) &dta_skip_records, 2},
	{"ord_union", (DL_FUNC) &ord_union, 2},
  {NULL, NULL, 0}
};
