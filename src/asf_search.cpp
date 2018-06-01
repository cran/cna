
#include <Rcpp.h>
#include "typedefs.h"

using namespace Rcpp;

//=============================================================================

// Functions to manipulate intList's
// ---------------------------------

// Auxiliary fun used in isSubsetOf 
IntegerVector rmFirst(IntegerVector x) {
  int n = x.size();
  IntegerVector out(n-1);
  if (n>1) out = x[Range(1, n-1)];
  return out;
}

// [[Rcpp::export]]
bool C_isSubsetOf(IntegerVector x, IntegerVector y) {
  int nx = x.size();
  int ny = y.size();
  int i=0, j=0;
  bool out;
  while (true) {
    if (nx == i){
      out = true;
      break;
    }
    if (ny == j){
      out = false;
      break;
    }
    if (nx-i > ny-j){
      out = false;
      break;
    }
    int x1=x[i], y1=y[j];
    if (x1 < y1){
      out = false;
      break;
    }
    if (x1 == y1){
      ++i;
    }
    ++j;
  }
  return(out);
}


// [[Rcpp::export]]
LogicalVector C_hasSupersetIn(intList x, intList y){
  int ylen = y.size(), xlen = x.size();
  //  Rcpp::Rcout << "The length of the 1st list is: " << ylen << std::endl;
  //  Rcpp::Rcout << "The length of the 2nd list is: " << xlen << std::endl;
  LogicalVector out(xlen);
  for (int ix=0; ix<xlen; ix++) {
    bool foundSuperset=false;
    for (int iy=0; iy<ylen; iy++) {
      if (C_isSubsetOf(x[ix], y[iy])){
        foundSuperset=true;
        break;
      }
    }    
    out[ix]=foundSuperset;
  }  
  return out;
}

//// [[Rcpp::export]]
bool C_allTRUE(LogicalVector x){
  int len = x.size();
  bool out = true;
  for (int i=0; i<len; i++){
    if (!x[i]){
      out = false;
      break;
    }
  }
  return out;
}
//// [[Rcpp::export]]
bool C_allFALSE(LogicalVector x){
  int len = x.size();
  bool out = true;
  for (int i=0; i<len; i++){
    if (x[i]){
      out = false;
      break;
    }
  }
  return out;
}

// Determine if an intList x is minimal with respect to a reference intList
// (ref contains existing solutions)
// [[Rcpp::export]]
bool C_intList_minimal(intList x, recIntList ref, bool ignore_equals=false){
  int lenr = ref.size();
  bool is_minimal = true;
  for (int j=0; j<lenr; j++){
    //Rcpp::Rcout << "j: " << j << std::endl;
    LogicalVector lv = C_hasSupersetIn(as<intList>(ref[j]), x);
    //Rcpp::Rcout << "lv: " << lv << std::endl;
    if (C_allTRUE(lv)){ 
      is_minimal = false;
      if (ignore_equals){
        bool eq = C_allTRUE(C_hasSupersetIn(x, as<intList>(ref[j])));
        if (eq){ 
          is_minimal = true;
        }
      }
      //Rcpp::Rcout << "is_minimal: " << is_minimal << std::endl;
      if (!is_minimal) break;
    }
  }
  return(is_minimal);
}


// Determine the intLists in x that are minimal with respect to existing solutions ref
// [[Rcpp::export]]
LogicalVector C_minimal(recIntList x, recIntList ref, bool ignore_equals=false){
  int lenx = x.size();
  LogicalVector out(lenx);
  for (int i=0; i<lenx; i++){
    out[i] = C_intList_minimal(as<intList>(x[i]), ref, ignore_equals);  
  }
  return(out);
}

/*/ Print an intlist to the console
// [[Rcpp::export]]
void C_show_intList(intList x){
  int lenx = x.size();
  for (int i=0; i<lenx; i++){
    IntegerVector xi = x[i];
    Rcpp::Rcout << xi << " ";
    if (i<lenx-1) Rcpp::Rcout << "/ ";
  }
  Rcpp::Rcout << std::endl;
  return;
}*/


// paste two intList's
// [[Rcpp::export]]
intList C_paste_intList(intList x, intList y){
  int lenx = x.size(), leny = y.size();
  List out(lenx + leny);
  for (int i=0; i<lenx; i++){
    out[i] = x[i];
  }
  for (int i=0; i<leny; i++){
    out[lenx + i] = y[i];
  }
  //  Rcpp::Rcout << as<intList>(out) << std::endl;
  return(as<intList>(out));
}


//=============================================================================

// Calculate con and cov of disjunctions
//
//   x   membership scores of antecendens
//   y   membership scores of consequens
// [[Rcpp::export]]
NumericVector C_conCov(NumericVector x, NumericVector y, IntegerVector f){
  int n=x.size();
  NumericVector Sums(3), conCov(2);
  for (int i=0; i<n; i++){
    Sums(0)+=x[i]*f(i);
    Sums(1)+=y[i]*f(i);
    Sums(2)+=std::min(x[i], y[i])*f(i);
  };
  conCov(0) = Sums(2)/Sums(0);
  conCov(1) = Sums(2)/Sums(1);
  return(conCov);
}

//=============================================================================

// C_subsetMin: minimum of a subset of the elements of an integer vector
//   x    vector
//   sub  subset of elements to consider
// [[Rcpp::export]]
double C_subsetMin(NumericVector x, IntegerVector sub){
  int len = sub.size();
  double out = x(sub(0)-1);
  if (len == 1) return(out);
  for (int i=1; i<sub.size(); i++){
    out = std::min(x(sub(i)-1), out);
  }
  return(out);
}


// membership scores of a series of conjunctions
//   m   IntegerMatrix, the rows representing the components of the conjunctions
//   x  'scores'-matrix
// [[Rcpp::export]]
NumericMatrix C_conjScore(NumericMatrix x, IntegerMatrix m){
  int n=x.nrow(), p=m.nrow();
  NumericMatrix out(n,p);
  for (int i=0; i<n; i++){
    for (int j=0; j<p; j++){
      // Rcpp::Rcout << i << " " << j << " " << C_subsetMin(x(i, _), m(j, _)) << std::endl;
      out(i,j) = C_subsetMin(x(i, _), m(j, _));
    }
  }
  return(out);
}

//==============================================================================

// initialize ii
// [[Rcpp::export]]
IntegerVector C_init_ii(IntegerVector nn, LogicalVector st){
  int l=nn.size();
  IntegerVector ini(l);
  ini.fill(0);
  for (int j=0; j<l-1; j++){
    if (st[j]){
      ini[j+1] = ini[j] + 1;
    }
  }
  return(ini);
}

// upper limit of ii
// [[Rcpp::export]]
IntegerVector C_set_lim(IntegerVector nn, LogicalVector st){
  int l=nn.size();
  IntegerVector lim(l);
  lim = nn - 1; 
  if (l<=1) return(lim);
  for (int j=l-2; j>=0; j--){
    if (st[j]){
      lim[j] = lim[j+1] - 1;
    }
  }
  return(lim);
}

// C_max_which
// [[Rcpp::export]]
int C_max_which(LogicalVector x) {
  int n = x.size();
  int i;
  for (i=n-1; i >= 0; i--) {
    if (x[i]) break;
  }
  return(i+1);
}


// increment ii
// [[Rcpp::export]]
IntegerVector C_increment(IntegerVector ii, IntegerVector nn, LogicalVector st, IntegerVector lim){
  int l = ii.size();
  if (ii[l-1] < lim[l-1]){    // end of loop not yet reached in last position
    ii[l-1] += 1;
    return(ii);
  } 
  // p = first position with increment in index
  int p=C_max_which(ii < lim);
  if (p == 0){
    ii.fill(0);
    return(ii);
  }
  // increment index positions < p
  if (p==1){
    ii[0] += 1; 
  } else {
    ii[Range(0, p-1)] = C_increment(ii[Range(0, p-1)], nn[Range(0, p-1)], st[Range(0, p-2)], lim[Range(0, p-1)]);
  }
  // reset index positions >= p
  for (int j=p; j<l; j++){
    if (st[j-1]){
      ii[j] = ii[j-1] + 1;
    } else {
      ii[j] = 0;
    } 
  }
  return(ii);
}


/*
// [[Rcpp::export]]
int test_increment(IntegerVector nn, LogicalVector st){
  IntegerVector ii=C_init_ii(nn, st);
  IntegerVector lim=C_set_lim(nn, st);
  int count=0;
  do {
    C_increment(ii, nn, st, lim);
    count++;
  } while (as<bool>(any(ii>0)));
  return(count);
} */


/* R
(nn <- rep(2:4, c(2, 1, 3)))
st <- diff(nn) == 0
C_set_lim(nn, st)
C_init_ii(nn, st)

test_increment(c(4L, 4L), T)
test_increment(c(4L, 4L), F)
test_increment(c(3L, 5L, 7L), c(F, F))

get_n <- function(nn){
  choose_args <- unname(unclass(rle(nn)[2:1]))
  prod(do.call(mapply, c(list(choose), choose_args)))
}
mytest <- function(nn){ 
  n <- get_n(nn)
  st <- diff(nn) == 0L
  cat(n, "\n")
  stopifnot(test_increment(nn, st) == n)
}
mytest(c(3L, 5L, 7L))
mytest(c(1L, 2L, 3L, 3L, 4L, 5L, 5L, 5L, 6L, 7L))
mytest(sort(c(1L, 2L, 3L, 3L, 4L, 5L, 5L, 5L, 6L, 7L)))
mytest(sample(c(1L, 2L, 3L, 3L, 4L, 5L, 5L, 5L, 6L, 7L)))

*/

//==============================================================================


// Find asf's of a given structure (=lengths of the conjunctions in the disjunctions)
// [[Rcpp::export]]
IntegerMatrix C_find_asf(IntegerVector conjlen, numMatList x, NumericVector y, IntegerVector f,
                         double con = 1.0, double cov = 1.0, int maxSol = 1e6){
  int n_conj = conjlen.size();

  // st = indicator, =true if conj hast same length as precedent conj
  LogicalVector st(n_conj - 1);
  st = (Rcpp::diff(conjlen) == 0);
  
  // nn = number of conjunctions
  IntegerVector nn(n_conj);
  for (int i=0; i<n_conj; i++){
    nn(i)=as<NumericMatrix>(x[i]).ncol();
  }

  // intialize ii, count, out; define lim
  IntegerVector ii=C_init_ii(nn, st);
  int count=0;
  IntegerMatrix out(maxSol, n_conj);
  IntegerVector lim=C_set_lim(nn, st);
  
  // Main loop over combinations
  do {
    // Calculate membership Scores for disjunction
    //Rcpp::Rcout << ii << " count=" << count << std::endl;
    NumericVector ms=as<NumericMatrix>(x[0])(_, ii[0]);
    //Rcpp::Rcout << /*"min: " << minim << ", */ "max: " << ms << std::endl;
    for (int i=1; i<n_conj; i++){
      ms=pmax(ms, as<NumericMatrix>(x[i])(_, ii[i]));
      //Rcpp::Rcout << /*"min: " << minim << ", */ "max: " << ms << std::endl;
    }
    // calculate con und cov
    NumericVector coco=C_conCov(ms, y, f);
    //Rcpp::Rcout << "coco: " << coco << std::endl;
    
    // Conditionally insert in next row of the matrix
    if (coco(0) >= con && coco(1) >= cov){
      for (int j=0; j<n_conj; j++){
        out(count, j) = ii[j];
      }
      count++;
    }

    // increment:
    C_increment(ii, nn, st, lim);
    
    if (count>=maxSol){
      Rcpp::Rcout << "Not all candidate solutions are recorded (maxSol="<< maxSol << ")" 
                  << std::endl;
      break;
    }
  } while (as<bool>(any(ii>0)));
  
  // if (count > 0){
  //   Rcpp::Rcout << "count="<< count << std::endl;
  // }
  
  // Return matrix with 0 rows if count=0:
  if (count == 0){ 
    IntegerMatrix a(0, n_conj);
    return a;
  }
  
  // Return result matrix
  return out(Range(0, count-1), _);
}

