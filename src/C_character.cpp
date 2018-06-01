#include <Rcpp.h>
#include <string>
#include "typedefs.h"

using namespace Rcpp;

std::string concat2(std::string x, std::string y, std::string sep){
  return x + sep + y;
}
// paste(x, collapse = sep)
// [[Rcpp::export]]
std::string C_concat(CharacterVector x, std::string sep){
  std::string out = ""; 
  std::string sep0 = "";
  for (int i=0; i<x.length(); i++){
    if (i==1){
      sep0 = sep;
    }
    out = concat2(out, Rcpp::as<std::string>(x[i]), sep0);
  }
  return out;
}

// applies C_concat to each element of x separately 
// optionally sorts the elements (disjuncts)
// [[Rcpp::export]]
CharacterVector C_mconcat(charList x, std::string sep, bool sorted = false){
  int n = x.size();
  CharacterVector out(n);
  for (int i=0; i<n; i++){
    CharacterVector xi = x[i];
    CharacterVector xis = Rcpp::clone(xi);
    if (sorted){
      xi = xis.sort();
    }
    out(i) = C_concat(xis, sep);
  }
  return out;
}

// convert a charlist to a character string
// [[Rcpp::export]]
std::string C_charList2string(charList x, std::string disj = "+", std::string conj = "*",
                              bool sorted = false){
  CharacterVector conjs = C_mconcat(x, conj, sorted);
  if (sorted){
    conjs = conjs.sort();
  }
  return C_concat(conjs, disj);
}

// convert a recursive charlist to a character vector
// [[Rcpp::export]]
CharacterVector C_recCharList2char(recCharList x, std::string disj = "+", std::string conj = "*",
                                  bool sorted = false){
  int n = x.size();
  CharacterVector out(n);
  for (int i=0; i<n; i++){
    charList xi = as<charList>(x[i]);
    out[i] = C_charList2string(xi, disj, conj, sorted);
  }
  return out;
}

