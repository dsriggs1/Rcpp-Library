#include <Rcpp.h>
using namespace Rcpp;
#include <algorithm> // for std::max_element
#include <vector>

// [[Rcpp::plugins(cpp11)]]
template <typename T>
class RollingM {
protected:
  T data;
  int lag;
public:
  RollingM(T d, int l) : data(d), lag(l) {}
  virtual NumericMatrix compute() = 0;

  // This method initializes the first "lag" values as NA
  NumericMatrix initialize_output() {
    int nrow = data.nrow(), ncol = data.ncol();
    NumericMatrix out(nrow, ncol);
    // Initialize the first i row values as NA for each j column
     for (int j = 0; j < ncol; j++){
      for (int i = 0; i < this->lag; i++) {
        out(i, j) = NA_REAL;
      }
    }

    return out;
  }
};

// The subclass "RollingSum"
template <typename T>
class RollingSumM : public RollingM<T> {
public:
  RollingSumM(T data, int window) : RollingM<T>(data, window) {}

  NumericMatrix compute() {
    // Get the data and window size from the superclass
    T x = this->data;
    //int window = this->lag;

    // Initialize the output matrix with the appropriate number of rows and columns
    NumericMatrix out = this->initialize_output();
    int nrow = this->data.nrow(), ncol = this->data.ncol();

    // Implement the rollingsum algorithm
    for (int j = 0; j < ncol; j++) {
      double sum = 0;
      for (int i = this->lag; i < nrow; i++) {
        sum += this->data(i-this->lag, j);
        out(i, j) = sum;
      }
    }

    return out;
  }
};

// [[Rcpp::export]]
NumericMatrix rollingsum(NumericMatrix x, int window) {
  RollingSumM<NumericMatrix> rolling_sum(x, window);
  return rolling_sum.compute();
}
NumericMatrix rollingSumM(NumericMatrix data, int window) {
  RollingSumM<NumericMatrix> rollingsum(data, window);
  return rollingsum.compute();
}