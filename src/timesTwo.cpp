#include <Rcpp.h>
using namespace Rcpp;

//' @title timesTwo
//' @description
//' Times two input
//' @name timesTwo
//' @param x NumericVector
//'
//' @export

// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
timesTwo(42)
*/
