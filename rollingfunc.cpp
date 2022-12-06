#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

// The superclass "Rolling"
template <typename T>
class Rolling {
protected:
  T data;
  int lag;
public:
  Rolling(T d, int l) : data(d), lag(l) {}
  virtual NumericVector compute() = 0;
  
  // This method initializes the first "lag" values as NA
  NumericVector initialize_output() {
    int n = this->data.size();
    NumericVector out(n);
    
    // Initialize the first j values as NA
    for (int i = 0; i < this->lag; i++) {
      out[i] = NA_REAL;
    }
    
    return out;
  }
};

// The subclass "RollingSum"
template <typename T>
class RollingSum : public Rolling<T> {
public:
  RollingSum(T d, int l) : Rolling<T>(d, l) {}
  NumericVector compute() override {
    // Use the "initialize_output" method from the superclass
    NumericVector out = this->initialize_output();
    
    // Declare and initialize the n variable inside the compute method
    int n = this->data.size();
    
    // Compute the rolling sum
    double sum = 0;
    
    for (int i = this->lag; i < n; i++) {
      
      sum += this->data[i-this->lag];
      out[i] = sum;
    }
    
    return out;
  }
};

// The subclass "RollingAverage"
template <typename T>
class RollingAverage : public Rolling<T> {
public:
  RollingAverage(T d, int l) : Rolling<T>(d, l) {}
  NumericVector compute() override {
    // Use the "initialize_output" method from the superclass
    NumericVector out = this->initialize_output();
    
    // Declare and initialize the n variable inside the compute method
    int n = this->data.size();
    
    // Compute the rolling average
    double sum = 0;
    
    for (int i = this->lag; i < n; i++) {
      
      sum += this->data[i-this->lag];
      out[i] = sum / ((i+1)-this->lag);
    }
    
    return out;
  }
};

// [[Rcpp::export]]
NumericVector rollingsum(NumericVector x, int lag) {
  RollingSum<NumericVector> s(x, lag);
  return s.compute();
}

// [[Rcpp::export]]
NumericVector rollingaverage(NumericVector x, int lag) {
  RollingAverage<NumericVector> s(x, lag);
  return s.compute();
}
