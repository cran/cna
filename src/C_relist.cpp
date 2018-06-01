#include <Rcpp.h>
using namespace Rcpp;

typedef ListOf<CharacterVector> charList;

// relist a character vector x
// l specifies the lengths of the CharacterVectors in the resulting list
template <typename T>
List C_relist(T& x, IntegerVector l){
  if (x.size() != sum(l)){
    stop("length(x) and sum(l) must be equal.");
  }
  int start=0L, n=l.size(), end=l(0)-1L;
  List out(n);
  for (int i=0L; i<n; i++){
    // Rcpp::Rcout << start << " " << end << std::endl;
    if (start<=end){
      out[i] = x[Range(start, end)];
    } else {
      out[i]=T(0);
    }
    if (i==n-1L) break;
    start += l(i);
    end += l(i+1L);
  }  
  return out;
}

// Exported "methods"
// [[Rcpp::export]]
List C_relist_Int(IntegerVector x, IntegerVector l){
  return C_relist(x, l);
}
// [[Rcpp::export]]
List C_relist_Num(NumericVector x, IntegerVector l){
  return C_relist(x, l);
}
// [[Rcpp::export]]
List C_relist_Log(LogicalVector x, IntegerVector l){
  return C_relist(x, l);
}
// [[Rcpp::export]]
List C_relist_Char(CharacterVector x, IntegerVector l){
  return C_relist(x, l);
}
// [[Rcpp::export]]
List C_relist_List(List x, IntegerVector l){
  return C_relist(x, l);
}
