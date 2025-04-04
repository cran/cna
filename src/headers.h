
#include <Rcpp.h>
using namespace Rcpp;

#include "typedefs.h"

// utils.cpp
bool C_allTRUE(const LogicalVector x);
bool C_allFALSE(const LogicalVector x);
bool C_isSubsetOf(const IntegerVector x, const IntegerVector y);
bool intList_equal(const intList x, const intList y);
LogicalVector C_hasSupersetIn(const intList x, const intList y, const bool ignore_equals);
LogicalVector C_hasSubsetInM(const IntegerMatrix y, const IntegerMatrix x);
intList C_append_intList(const intList x, const intList y);

// C_character.cpp
std::string concat2(const std::string x, const std::string y, const std::string sep);
std::string C_concat(const CharacterVector x, const std::string sep);
CharacterVector C_mconcat(const charList x, const std::string sep, const bool sorted);
std::string C_charList2string(const charList x, const std::string disj, 
                              const std::string conj, const bool sorted);
CharacterVector C_recCharList2char(const recCharList x, const std::string disj,
                                   const std::string conj, const bool sorted);
  
// C_relist.cpp
List C_relist_Int(const IntegerVector x, const IntegerVector l);
List C_relist_Num(const NumericVector x, const IntegerVector l);
List C_relist_Log(const LogicalVector x, const IntegerVector l);
List C_relist_Char(const CharacterVector x, const IntegerVector l);
List C_relist_List(const List x, const IntegerVector l);
  
// minimal_submodel.cpp
LogicalMatrix C_disj_contained(const intList x, const intList y, const bool shortcut);
LogicalVector C_is_submodel(const recIntList x, const intList ref, const bool strict);
LogicalVector C_minimal(const recIntList x, const recIntList ref, const bool strict);
bool C_intList_minimal_old(const intList x, const recIntList ref, const bool ignore_equals);
LogicalVector C_minimal_old(const recIntList x, const recIntList ref, const bool ignore_equals);
IntegerVector initComb(const int k);
void nextComb(IntegerVector ii, const int k, const int n);
bool checkLastComb(const IntegerVector ii, const int k, const int n);
LogicalVector C_rowSubsetColAnys(const LogicalMatrix x, const IntegerVector rows);
bool C_checkHall_k(const LogicalMatrix x, const int k);
bool C_checkHallsCondition(const LogicalMatrix x);
  
// asf_search.cpp
NumericVector C_conCov(const NumericVector x, const NumericVector y, const IntegerVector f);
double C_subsetMin(const NumericVector x, const IntegerVector sub);
NumericMatrix C_conjScore(const NumericMatrix x, const IntegerMatrix m);
IntegerVector C_init_ii(const IntegerVector nn, const LogicalVector st);
IntegerVector C_set_lim(const IntegerVector nn, const LogicalVector st);
int C_max_which(const LogicalVector x);
IntegerVector C_increment(IntegerVector ii, const IntegerVector nn, 
                          const LogicalVector st, const IntegerVector lim);
IntegerMatrix C_find_asf(const IntegerVector conjlen, const numMatList x, 
                         const NumericVector y, const IntegerVector f,
                         const double con, const double cov, 
                         const int maxSol, const IntegerVector def);

// C_redundant.cpp
int C_find_first_false(const LogicalVector x);
LogicalVector C_redund(const LogicalMatrix x);
List C_mredund(const LogicalMatrix x, const IntegerVector l);

// conCov.cpp
double C_con(const NumericVector x, const NumericVector y, const IntegerVector f);
double C_cov(const NumericVector x, const NumericVector y, const IntegerVector f);
double C_ccon(const NumericVector x, const NumericVector y, const IntegerVector f);
double C_ccov(const NumericVector x, const NumericVector y, const IntegerVector f);
double C_wcon(const NumericVector x, const NumericVector y, const IntegerVector f);
double C_wcov(const NumericVector x, const NumericVector y, const IntegerVector f);
double C_wccov(const NumericVector x, const NumericVector y, const IntegerVector f);
double C_wccon(const NumericVector x, const NumericVector y, const IntegerVector f);

ccFun pickCCFn(const int ccDef);
NumericVector C_conCov(const NumericVector x, const NumericVector y, const IntegerVector f, 
                       const ccFun conFn, const ccFun covFn);
NumericVector C_conj_conCov(const IntegerVector cols, const NumericMatrix x,
                            const NumericVector y, const IntegerVector f,
                            const ccFun conFn, const ccFun covFn);
NumericMatrix C_mconj_conCov(const IntegerMatrix cols, const NumericMatrix x,
                             const NumericVector y, const IntegerVector f,
                             const IntegerVector def);
NumericMatrix C_conCovFromArray(
    const NumericVector x, const IntegerVector dim, const IntegerVector f, 
    const IntegerVector def);

// conCovDet.cpp
NumericVector C_con_det(const NumericVector x, const NumericVector y, const IntegerVector f);
NumericVector C_cov_det(const NumericVector x, const NumericVector y, const IntegerVector f);
NumericVector C_ccon_det(const NumericVector x, const NumericVector y, const IntegerVector f);
NumericVector C_ccov_det(const NumericVector x, const NumericVector y, const IntegerVector f);
NumericVector C_wcon_det(const NumericVector x, const NumericVector y, const IntegerVector f);
NumericVector C_wcov_det(const NumericVector x, const NumericVector y, const IntegerVector f);
NumericVector C_wccov_det(const NumericVector x, const NumericVector y, const IntegerVector f);
NumericVector C_wccon_det(const NumericVector x, const NumericVector y, const IntegerVector f);

ccFunDet pickCCFnDet(const int ccDef);
NumericVector conCovDetailed(
  const NumericVector x, const NumericVector y, const IntegerVector f, 
  const ccFunDet conFn, const ccFunDet covFn);
NumericMatrix C_conCovFromArrayDetailed(
    const NumericVector x, const IntegerVector dim, const IntegerVector f, 
    const IntegerVector def);

// uniqueCombs.cpp
IntegerVector C_countUniques(const IntegerMatrix x);
LogicalVector C_duplicatedMat(const IntegerMatrix x);
IntegerMatrix C_uniqueMat(const IntegerMatrix x);
IntegerMatrix C_selectCols(const IntegerMatrix x, const IntegerVector idx);
IntegerMatrix C_uniqueCombs(const IntegerMatrix x, const IntegerVector idx);

