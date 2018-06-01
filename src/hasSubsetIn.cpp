
#include <Rcpp.h>
#include "headers.h"

using namespace Rcpp;

// [[Rcpp::export]]
LogicalVector C_hasSubsetInM(IntegerMatrix y, IntegerMatrix x) { 
  // "M" indicates application to matrices
  int nry=y.nrow(), nrx=x.nrow();
  LogicalVector out(nry);
  for (int iy=0; iy < nry; iy++) {
    IntegerVector y1 = y(iy, _);
    bool foundSubset=false;
    for (int ix=0; ix < nrx; ix++) {
      IntegerVector x1 = x(ix, _);
      if (C_isSubsetOf(x1, y1)){
        foundSubset=true;
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

