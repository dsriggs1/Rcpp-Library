#include <Rcpp.h>
using namespace Rcpp;
#include <algorithm> // for std::max_element

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

// The subclass "RollingMax"
template <typename T>
class RollingMax : public Rolling<T> {
public:
  RollingMax(T d, int l) : Rolling<T>(d, l) {}
  NumericVector compute() override {
    // Use the "initialize_output" method from the superclass
    NumericVector out = this->initialize_output();
    
    // Declare and initialize the n variable inside the compute method
    int n = this->data.size();
    
    // Compute the rolling max
    // Initialize the rolling max to the first value in x
    double max = this->data[0];
    
    // Compute the rolling max
    for (int i = this->lag; i < n; i++) {
      
      max = std::max(max, this->data[i-this->lag]);
      out[i] = max;
    }
    
    return out;
  }
};

// The subclass "RollingMin"
template <typename T>
class RollingMin : public Rolling<T> {
public:
  RollingMin(T d, int l) : Rolling<T>(d, l) {}
  NumericVector compute() override {
    // Use the "initialize_output" method from the superclass
    NumericVector out = this->initialize_output();
    
    // Declare and initialize the n variable inside the compute method
    int n = this->data.size();
    
    // Compute the rolling min
    // Initialize the rolling min to the first value in x
    double min = this->data[0];
    
    // Compute the rolling min
    for (int i = this->lag; i < n; i++) {
      
      min = std::min(min, this->data[i-this->lag]);
      out[i] = min;
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

// [[Rcpp::export]]
NumericVector rollingmax(NumericVector x, int lag) {
  RollingMax<NumericVector> s(x, lag);
  return s.compute();
}

// [[Rcpp::export]]
NumericVector rollingmin(NumericVector x, int lag) {
  RollingMin<NumericVector> s(x, lag);
  return s.compute();
}
