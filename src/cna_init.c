#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP cna_C_conCov(SEXP, SEXP, SEXP);
extern SEXP cna_C_conj_conCov(SEXP, SEXP, SEXP, SEXP);
extern SEXP cna_C_conjScore(SEXP, SEXP);
extern SEXP cna_C_countUniques(SEXP);
extern SEXP cna_C_disj_conCov(SEXP, SEXP, SEXP, SEXP);
extern SEXP cna_C_duplicatedMat(SEXP);
extern SEXP cna_C_find_asf(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP cna_C_hasSubsetInM(SEXP, SEXP);
extern SEXP cna_C_hasSupersetIn(SEXP, SEXP);
extern SEXP cna_C_increment(SEXP, SEXP, SEXP, SEXP);
extern SEXP cna_C_init_ii(SEXP, SEXP);
extern SEXP cna_C_intList_minimal(SEXP, SEXP);
extern SEXP cna_C_isSubsetOf(SEXP, SEXP);
extern SEXP cna_C_isSubsetOf1(SEXP, SEXP);
extern SEXP cna_C_minimal(SEXP, SEXP);
extern SEXP cna_C_paste_intList(SEXP, SEXP);
extern SEXP cna_C_selectCols(SEXP, SEXP);
extern SEXP cna_C_set_lim(SEXP, SEXP);
extern SEXP cna_C_subsetMin(SEXP, SEXP);
extern SEXP cna_C_uniqueCombs(SEXP, SEXP);
extern SEXP cna_C_uniqueMat(SEXP);
extern SEXP cna_max_which(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"cna_C_conCov",          (DL_FUNC) &cna_C_conCov,          3},
  {"cna_C_conj_conCov",     (DL_FUNC) &cna_C_conj_conCov,     4},
  {"cna_C_conjScore",       (DL_FUNC) &cna_C_conjScore,       2},
  {"cna_C_countUniques",    (DL_FUNC) &cna_C_countUniques,    1},
  {"cna_C_disj_conCov",     (DL_FUNC) &cna_C_disj_conCov,     4},
  {"cna_C_duplicatedMat",   (DL_FUNC) &cna_C_duplicatedMat,   1},
  {"cna_C_find_asf",        (DL_FUNC) &cna_C_find_asf,        7},
  {"cna_C_hasSubsetInM",    (DL_FUNC) &cna_C_hasSubsetInM,    2},
  {"cna_C_hasSupersetIn",   (DL_FUNC) &cna_C_hasSupersetIn,   2},
  {"cna_C_increment",       (DL_FUNC) &cna_C_increment,       4},
  {"cna_C_init_ii",         (DL_FUNC) &cna_C_init_ii,         2},
  {"cna_C_intList_minimal", (DL_FUNC) &cna_C_intList_minimal, 2},
  {"cna_C_isSubsetOf",      (DL_FUNC) &cna_C_isSubsetOf,      2},
  {"cna_C_isSubsetOf1",     (DL_FUNC) &cna_C_isSubsetOf1,     2},
  {"cna_C_minimal",         (DL_FUNC) &cna_C_minimal,         2},
  {"cna_C_paste_intList",   (DL_FUNC) &cna_C_paste_intList,   2},
  {"cna_C_selectCols",      (DL_FUNC) &cna_C_selectCols,      2},
  {"cna_C_set_lim",         (DL_FUNC) &cna_C_set_lim,         2},
  {"cna_C_subsetMin",       (DL_FUNC) &cna_C_subsetMin,       2},
  {"cna_C_uniqueCombs",     (DL_FUNC) &cna_C_uniqueCombs,     2},
  {"cna_C_uniqueMat",       (DL_FUNC) &cna_C_uniqueMat,       1},
  {"cna_max_which",         (DL_FUNC) &cna_max_which,         1},
  {NULL, NULL, 0}
};

void R_init_cna(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
