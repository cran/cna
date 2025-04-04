
#include <Rcpp.h>
#include "typedefs.h"
#include "headers.h"

using namespace Rcpp;

/* con/cov measures with compact output: 
 * only ratio (as double)
   ------------------------------------- */

// Calculate con and cov
//   x   membership scores of antecendens
//   y   membership scores of consequens
//   f   frequencies
double C_xyratio(const NumericVector x, const NumericVector y, const IntegerVector f){
  int n=x.size();
  double sX=0.0, sXY=0.0;
  for (int i=0; i<n; ++i){
    sX  += x[i]*f[i];
    sXY += std::min(x[i], y[i])*f[i];
  };
  return(sXY / sX);
}

double C_xyratio_compl(const NumericVector x, const NumericVector y, const IntegerVector f){
  int n=x.size();
  double sX=0.0, sXY=0.0;
  for (int i=0; i<n; ++i){
    sX  += (1.0 - x[i])*f[i];
    sXY += (1.0 - std::max(x[i], y[i]))*f[i];
  };
  return(sXY / sX);
}

// [[Rcpp::export]]
double C_con(const NumericVector x, const NumericVector y, const IntegerVector f){
  return(C_xyratio(x, y, f));
}

// [[Rcpp::export]]
double C_cov(const NumericVector x, const NumericVector y, const IntegerVector f){
  return(C_xyratio(y, x, f));
}

// [[Rcpp::export]]
double C_ccon(const NumericVector x, const NumericVector y, const IntegerVector f){
  return(C_xyratio_compl(y, x, f));
}

// [[Rcpp::export]]
double C_ccov(const NumericVector x, const NumericVector y, const IntegerVector f){
  return(C_xyratio_compl(x, y, f));
}

/* ========================================================================== */

// [[Rcpp::export]]
double C_wcon(const NumericVector x, const NumericVector y, const IntegerVector f){
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
  double w = sY / (sf - sY);
  return(sXY / (sXY + w * (sXy - sXxYy)));
}

// [[Rcpp::export]]
double C_wcov(const NumericVector x, const NumericVector y, const IntegerVector f){
  return(C_wcon(y, x, f));
}

// [[Rcpp::export]]
double C_wccov(const NumericVector x, const NumericVector y, const IntegerVector f){
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
  double w = sy / (sf - sy);
  return(sxy / (sxy + w * (sxY - sXxYy)));
}

// [[Rcpp::export]]
double C_wccon(const NumericVector x, const NumericVector y, const IntegerVector f){
  return(C_wccov(y, x, f));
}


/* ========================================================================== */

ccFun pickCCFn(const int ccDef){
  ccFun out;
  switch(ccDef){
    case 1: default:{ out=C_con; break; };
    case 2: { out=C_cov; break; };
    case 3: { out=C_ccon; break; };
    case 4: { out=C_ccov; break; };
    case 5: { out=C_wcon; break; };
    case 6: { out=C_wcov; break; };
    case 7: { out=C_wccon; break; };
    case 8: out=C_wccov;
  }
  return out;
}

NumericVector C_conCov(const NumericVector x, const NumericVector y, const IntegerVector f, 
                       const ccFun conFn, const ccFun covFn){
  NumericVector conCov(2);
  conCov(0) = conFn(x, y, f);
  conCov(1) = covFn(x, y, f);
  return conCov;
}

/* -------------------------------------------------------------------------- */

// Calculate con and cov of conjunctions
NumericVector C_conj_conCov(const IntegerVector cols, const NumericMatrix x,
                            const NumericVector y, const IntegerVector f,
                            const ccFun conFn, const ccFun covFn){
  int n=x.nrow(), p=cols.size();
  NumericVector lhs_scores(n), conCov(2);
  for (int i=0; i<n; ++i){
    double minX=1;
    for (int j=0; j<p; ++j){
      double Xval=x(i, cols[j]-1);
      if (Xval<minX) minX=Xval;
    };
    lhs_scores(i) = minX;
  };
  return C_conCov(lhs_scores, y, f, conFn, covFn);
}

// [[Rcpp::export]]
NumericMatrix C_mconj_conCov(const IntegerMatrix cols, const NumericMatrix x,
                             const NumericVector y, const IntegerVector f,
                             const IntegerVector def){
  ccFun conFn = pickCCFn(def(0)), covFn = pickCCFn(def(1));
  int ncond = cols.nrow();
  NumericMatrix out(2, ncond);
  for (int i=0; i<ncond; ++i){
    out(_, i) = C_conj_conCov(cols(i, _), x, y, f, conFn, covFn);
  };
  return out;
}

/* -------------------------------------------------------------------------- */

// Calculate con/cov for a 3-dim array [n, 2, ncond]
// used in cna:::cond2condTbl() and cna:::getInfo_asf
// [[Rcpp::export]]
NumericMatrix C_conCovFromArray(
    const NumericVector x, const IntegerVector dim, const IntegerVector f, 
    const IntegerVector def){
  int n = dim(0), nslice = dim(2), startInd=0;
  NumericMatrix out(2, nslice);
  ccFun conFn = pickCCFn(def(0)), covFn = pickCCFn(def(1));
  for (int i=0; i<nslice; ++i){
    NumericVector xvect = x[Range(startInd, startInd + n-1)];
    startInd += n;
    NumericVector yvect = x[Range(startInd, startInd + n-1)];
    startInd += n;
    out(_, i) = C_conCov(xvect, yvect, f, conFn, covFn);
  }
  return out;
}

/* -------------------------------------------------------------------------- */


// Used in detailMeasures() - more precisely: where?
// [[Rcpp::export]]
NumericMatrix C_severalMeasures(
    const NumericVector x, const IntegerVector dim, const IntegerVector f, 
    const IntegerVector def){
  int n = dim(0), nmeasures = def.size(), nslice = dim(2), startInd=0;
  NumericMatrix out(nslice, nmeasures);
  for (int i=0; i<nslice; ++i){
    NumericVector xvect = x[Range(startInd, startInd + n-1)];
    startInd += n;
    NumericVector yvect = x[Range(startInd, startInd + n-1)];
    startInd += n;
    for (int j=0; j<nmeasures; ++j){
      ccFun Fn = pickCCFn(def(j));
      out(i, j) = Fn(xvect, yvect, f);
    }
  }
  return out;
}
