
#include <Rcpp.h>
using namespace Rcpp;

typedef ListOf<IntegerVector> intList;
typedef ListOf<intList> recIntList; // recursive intList = list of intList
typedef ListOf<NumericMatrix> numMatList;

typedef ListOf<CharacterVector> charList;
typedef ListOf<charList> recCharList;

typedef double (*ccFun)(const NumericVector a, const NumericVector b, 
                const IntegerVector f);
typedef NumericVector (*ccFunDet)(const NumericVector a, const NumericVector b, 
                       const IntegerVector f);

