#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector rollingsum(NumericVector x, int lag) {
  
  int n = x.size();
  NumericVector out(n);
  
  // Initialize the first j values as NA
  for (int i = 0; i < lag; i++) {
    out[i] = NA_REAL;
  }
  
  // Compute the rolling sum
  double sum = 0;
  
  for (int i = lag; i < n; i++) {
    
    sum += x[i-lag];
    out[i] = sum;
  }
  
  return out;
}
