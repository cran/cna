
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
bool C_isSubsetOf1(IntegerVector x, IntegerVector y) {
  int lx=x.size(), ly=y.size();
  bool out;
  out=TRUE;
  bool found;
  for (int i=0; i < lx; i++) {
    found=FALSE;
    for (int j=0; j < ly; j++) {
      if (x[i]==y[j]){
        found=TRUE;
        break;
      }
    }
    if (!found){
      out=FALSE;
      break;
    }
  }
  return(out);
}

// [[Rcpp::export]]
LogicalVector C_hasSubsetInM(IntegerMatrix y, IntegerMatrix x) { 
  // "M" indicates application to matrices
  int nry=y.nrow(), nrx=x.nrow();
  LogicalVector out(nry);
  for (int iy=0; iy < nry; iy++) {
    IntegerVector y1 = y(iy, _);
    bool foundSubset=FALSE;
    for (int ix=0; ix < nrx; ix++) {
      IntegerVector x1 = x(ix, _);
      if (C_isSubsetOf1(x1, y1)){
        foundSubset=TRUE;
        break;
      }
    }    
    out[iy]=foundSubset;
  }  
  return(out);
}


/* R
C_isSubsetOf1(1:2, 1:3)
C_isSubsetOf1(1:2, 2:4)

cbind(1:2, 3:4)
C_hasSubsetInM(rbind(1:3, 2:4, 3:5), cbind(1:2, 3:4))
*/

