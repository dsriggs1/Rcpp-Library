#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector rollingsum(NumericVector x) {
  
  int n = x.size();
  NumericVector out(n);
  
  // Initialize the first value as NA
  out[0] = NA_REAL;
  
  // Compute the rolling sum
  double sum = 0;
  
  for (int i = 1; i < n; i++) {
    
    sum += x[i-1];
    out[i] = sum;
  }
  
  return out;
}
