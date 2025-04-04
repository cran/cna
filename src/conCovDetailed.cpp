
#include <Rcpp.h>
#include "typedefs.h"

using namespace Rcpp;

/* con/cov measures with "detailed" output: 
 * NumericVector with ratio, num & denom 
   ---------------------------------------- */


// Calculate con and cov
//   x   membership scores of antecendens
//   y   membership scores of consequens
//   f   frequencies
NumericVector C_xyratio_det(const NumericVector x, const NumericVector y, const IntegerVector f){
  int n=x.size();
  double sX=0.0, sXY=0.0;
  for (int i=0; i<n; ++i){
    sX  += x[i]*f[i];
    sXY += std::min(x[i], y[i])*f[i];
  };
  return(NumericVector::create(sXY / sX, sXY, sX));  // (ratio, num, denom)
}

NumericVector C_xyratio_compl_det(const NumericVector x, const NumericVector y, const IntegerVector f){
  int n=x.size();
  double sX=0.0, sXY=0.0;
  for (int i=0; i<n; ++i){
    sX  += (1.0 - x[i])*f[i];
    sXY += (1.0 - std::max(x[i], y[i]))*f[i];
  };
  return(NumericVector::create(sXY / sX, sXY, sX));
}

// [[Rcpp::export]]
NumericVector C_con_det(const NumericVector x, const NumericVector y, const IntegerVector f){
  return(C_xyratio_det(x, y, f));
}

// [[Rcpp::export]]
NumericVector C_cov_det(const NumericVector x, const NumericVector y, const IntegerVector f){
  return(C_xyratio_det(y, x, f));
}

// [[Rcpp::export]]
NumericVector C_ccon_det(const NumericVector x, const NumericVector y, const IntegerVector f){
  return(C_xyratio_compl_det(y, x, f));
}

// [[Rcpp::export]]
NumericVector C_ccov_det(const NumericVector x, const NumericVector y, const IntegerVector f){
  return(C_xyratio_compl_det(x, y, f));
}

/* ========================================================================== */

// [[Rcpp::export]]
NumericVector C_wcon_det(const NumericVector x, const NumericVector y, const IntegerVector f){
  int n=x.size();
  double sY=0.0, sXY=0.0, sXy=0.0, sXxYy=0.0, sf=0.0;
  for (int i=0; i<n; ++i){
    sY  += y[i]*f[i];
    sXY += std::min(x[i], y[i])*f[i];
    sXy += std::min(x[i], 1.0-y[i])*f[i];
    sXxYy += std::min(std::min(x[i], y[i]), 
                      std::min(1.0-x[i], 1.0-y[i]))*f[i];
    sf += f[i];
  };
  double w = sY / (sf - sY), denom = sXY + w * (sXy - sXxYy);
  return(NumericVector::create(sXY / denom, sXY, denom));
}

// [[Rcpp::export]]
NumericVector C_wcov_det(const NumericVector x, const NumericVector y, const IntegerVector f){
  return(C_wcon_det(y, x, f));
}

// [[Rcpp::export]]
NumericVector C_wccov_det(const NumericVector x, const NumericVector y, const IntegerVector f){
  int n=x.size();
  double sy=0.0, sxy=0.0, sxY=0.0, sXxYy=0.0, sf=0.0;
  NumericVector xc(n), yc(n);
  for (int i=0; i<n; ++i){
    xc[i] = 1.0 - x[i];
    yc[i] = 1.0 - y[i];
    sy  += yc[i]*f[i];
    sxy += std::min(xc[i], yc[i])*f[i];
    sxY += std::min(xc[i], y[i])*f[i];
    sXxYy += std::min(std::min(x[i], y[i]), std::min(xc[i], yc[i]))*f[i];
    sf += f[i];
  };
  double w = sy / (sf - sy), denom = sxy + w * (sxY - sXxYy);
  return(NumericVector::create(sxy / denom, sxy, denom));
}

// [[Rcpp::export]]
NumericVector C_wccon_det(const NumericVector x, const NumericVector y, const IntegerVector f){
  return(C_wccov_det(y, x, f));
}


/* ========================================================================== */

ccFunDet pickCCFnDet(const int ccDef){
  ccFunDet out;
  switch(ccDef){
    case 1: default: { out=C_con_det; break; };
    case 2: { out=C_cov_det; break; };
    case 3: { out=C_ccon_det; break; };
    case 4: { out=C_ccov_det; break; };
    case 5: { out=C_wcon_det; break; };
    case 6: { out=C_wcov_det; break; };
    case 7: { out=C_wccon_det; break; };
    case 8: out=C_wccov_det;
  }
  return out;
}

NumericVector conCovDetailed(
  const NumericVector x, const NumericVector y, const IntegerVector f, 
  const ccFunDet conFn, const ccFunDet covFn){
  NumericVector conDet = covFn(x, y, f);  
  NumericVector out(6);
  out[Range(0, 2)] = conFn(x, y, f);
  out[Range(3, 5)] = covFn(x, y, f);
  return out;
}

// -----------------------------------------------------------------------------

// Calculate con/cov for a 3-dim array [n, 2, ncond]
// used in cna:::cond2condTbl() and cna:::getInfo_asf
// [[Rcpp::export]]
NumericMatrix C_conCovFromArrayDetailed(
    const NumericVector x, const IntegerVector dim, const IntegerVector f, 
    const IntegerVector def){
  int n = dim(0), nslice = dim(2), startInd=0;
  NumericMatrix out(6, nslice);
  ccFunDet conFn = pickCCFnDet(def(0)), 
           covFn = pickCCFnDet(def(1));
  for (int i=0; i<nslice; ++i){
      NumericVector xvect = x[Range(startInd, startInd + n-1)];
      startInd += n;
      NumericVector yvect = x[Range(startInd, startInd + n-1)];
      startInd += n;
      out(_, i) = conCovDetailed(xvect, yvect, f, conFn, covFn);
    }
  return out;
}

