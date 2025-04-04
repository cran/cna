
#include <Rcpp.h>
// #include <R_ext/Utils.h>
// #include "headers.h"
using namespace Rcpp;

// round values in a vector to 0 or 1
// (auxiliary function used in C_Varies)
NumericVector C_round(NumericVector x, NumericVector cutoff = 0.5, 
                      std::string border = "up"){
  NumericVector out(x.size());
  double borderVal;
  if (border == "up"){
    borderVal=1.0;
  } else {
    borderVal=0.0;
  }
  for (int i=0; i<out.size(); i++){
    if (x[i] > cutoff(0)){
      out[i] = 1.0;
    } else if (x[i] < cutoff(0)){
      out[i] = 0.0;
    } else {
      out[i] = borderVal;
    }
  }
  return out;
}

// check if x varies within the cases where x==y ('concordant cases')
// [[Rcpp::export]]
bool C_varies(NumericVector x, NumericVector y, 
              NumericVector cutoff = 0.5, std::string border = "up",
              std::string asfSelection = "none"){
  if (asfSelection == "none") return(true);
  NumericVector xx = clone(x);
  NumericVector yy = clone(y);
  if (asfSelection == "cs"){
    xx = C_round(x, cutoff, border);
    yy = C_round(y, cutoff, border);
  }
  const int l=x.size();
  bool out=false;
  double val0=0.0;
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
