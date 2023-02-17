
#include <Rcpp.h>
// #include <R_ext/Utils.h>
// #include "headers.h"
using namespace Rcpp;

// round values in a matrix to 0 or 1
// (auxiliary function used in C_Varies)
NumericVector C_round(NumericVector x){
  NumericVector out(x.size());
  for (int i=0; i<out.size(); i++){
    if (x[i] == 0.5){
      out[i] = 0;
    } else {
      out[i] = round(x[i]);
    }
  }
  return out;
}

// check if a numeric vector has more than 1 unique value
// [[Rcpp::export]]
bool C_varies(NumericVector x, NumericVector y, std::string asfSelection = "none"){
  if (asfSelection == "none") return(true);

  NumericVector xx = clone(x);
  NumericVector yy = clone(y);
  if (asfSelection == "cs"){
    xx = C_round(x);
    yy = C_round(y);
  }
  const int l=x.size();
  bool out=false;
  double val0;
  int i;
  for (i=0; i<l; ++i){
    if (xx(i) == yy(i)){
      val0 = yy(i);
      break;
    }
  }
  if (i<l){
    for (i=i+1; i<l; ++i){
      if ((xx(i) == yy(i)) && (val0 != yy(i))){
        out=true;
        break;
      }
    }
  } 
  return out;
}
