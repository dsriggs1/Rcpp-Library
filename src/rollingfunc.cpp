#include <Rcpp.h>
using namespace Rcpp;
#include <algorithm> // for std::max_element
#include <vector>

// [[Rcpp::plugins(cpp11)]]
template <typename T>
class Rolling {
protected:
  T data;
  int lag;
public:
  /**
   * Constructor for the Rolling class
   *
   * @param d a numeric vector or matrix
   * @param l a positive integer representing the lag
   */
  Rolling(T d, int l) : data(d), lag(l) {}
  /**
   * Pure virtual function to compute the rolling statistic. Must be overridden in derived classes.
   */
  virtual Rcpp::RObject compute() = 0;
  /**
   * Overloaded function to initialize the output vector or matrix.
   * Initializes the first i values (or rows) as NA, where i is the lag.
   *
   * @param data a numeric vector
   * @return a numeric vector with the same size as data, initialized with NAs for the first i values
   */
  NumericVector initialize_output(Rcpp::NumericVector data) {
    int n = data.size();
    NumericVector out(n);

    // Initialize the first i values as NA
    for (int i = 0; i < this->lag; i++) {
      out[i] = NA_REAL;
    }

    return out;
  }
  /**
   * Overloaded function to initialize the output vector or matrix.
   * Initializes the first i rows as NA for each j column, where i is the lag.
   *
   * @param data a numeric matrix
   * @return a numeric matrix with the same size as data, initialized with NAs for the first i rows for each j column
   */
  NumericMatrix initialize_output(Rcpp::NumericMatrix data) {
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
class RollingSum : public Rolling<T> {
public:
  /**
   * Constructor for the RollingSum class
   *
   * @param d a numeric vector or matrix
   * @param l a positive integer representing the lag
   */
  RollingSum(T d, int l) : Rolling<T>(d, l) {}
  /**
   * Computes the rolling sum of the input data
   */
  Rcpp::RObject compute() override {
    // Use the "initialize_output" method from the superclass
    NumericVector out = this->initialize_output(this->data);

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

template <>
class RollingSum<NumericMatrix> : public Rolling<NumericMatrix> {
public:
  /**
   * Constructor for the RollingSum class for a numeric matrix input
   *
   * @param d a numeric matrix
   * @param l a positive integer representing the lag
   */
  RollingSum(NumericMatrix d, int l) : Rolling<NumericMatrix>(d, l) {}
  /**
   * Computes the rolling sum of the input data, treating each column separately
   */
  Rcpp::RObject compute() override {
    // Use the "initialize_output" method from the superclass
    NumericMatrix out = this->initialize_output(this->data);

    int nrow = this->data.nrow(), ncol = this->data.ncol();

    for (int j = 0; j < ncol; j++){
    double sum = 0;
      for (int i = this->lag; i < nrow; i++){
        sum += this->data(i-this->lag, j);
        out(i, j) = sum;
      }
    }
    return out;
  }
};

// The subclass "RollingAverage"
/**
 * @brief A class template for computing rolling averages of a given data type.
 *
 * @tparam T The type of data for which to compute rolling averages. Can be
 *           a numeric vector or matrix.
 *
 * @see Rolling
 */
template <typename T>
class RollingAverage : public Rolling<T> {
public:
  /**
   * @brief Constructs a RollingAverage object with the given data and lag.
   *
   * @param d The data for which to compute rolling averages.
   * @param l The lag (number of periods) over which to compute rolling averages.
   */
  RollingAverage(T d, int l) : Rolling<T>(d, l) {}

  /**
   * @brief Computes the rolling average of the data stored in this object.
   *
   * @return An Rcpp::RObject containing the rolling average of the data.
   *         The type of the returned object will depend on the type of the
   *         data (e.g., a NumericVector for a numeric vector input, or a
   *         NumericMatrix for a numeric matrix input).
   */
  Rcpp::RObject compute() override {
    // Use the "initialize_output" method from the superclass
    NumericVector out = this->initialize_output(this->data);

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

/**
 * @brief Specialization of the RollingAverage class template for computing
 *        rolling averages of a numeric matrix.
 *
 * @see RollingAverage
 * @see Rolling
 */
template <> class RollingAverage<NumericMatrix> : public Rolling<NumericMatrix> {
public:
  /**
   * @brief Constructs a RollingAverage object with the given data and lag.
   *
   * @param d The data for which to compute rolling averages.
   * @param l The lag (number of periods) over which to compute rolling averages.
   */
  RollingAverage(NumericMatrix d, int l) : Rolling<NumericMatrix>(d, l) {}

  /**
   * @brief Computes the rolling average of the data stored in this object.
   *
   * @return An Rcpp::RObject containing the rolling average of the data.
   *         The returned object will be a NumericMatrix.
   */
  Rcpp::RObject compute() override {
    // Use the "initialize_output" method from the superclass
    NumericMatrix out = this->initialize_output(this->data);

    int n = this->data.size();

    // Compute the rolling average
    double sum = 0;

    for (int j=0; j < n; j++){
      for (int i = this->lag; i < n; i++) {
        sum += this->data(i-this->lag, j);
        out(i, j) = sum / ((i+1)-this->lag);
      }
    }
      return out;
    }
};


// The subclass "RollingMax"
/**
 * @brief A class template for computing rolling maxima of a given data type.
 *
 * @tparam T The type of data for which to compute rolling maxima. Can be
 *           a numeric vector.
 *
 * @see Rolling
 */
template <typename T>
class RollingMax : public Rolling<T> {
public:
  /**
   * @brief Constructs a RollingMax object with the given data and lag.
   *
   * @param d The data for which to compute rolling maxima.
   * @param l The lag (number of periods) over which to compute rolling maxima.
   */
  RollingMax(T d, int l) : Rolling<T>(d, l) {}

  /**
   * @brief Computes the rolling maxima of the data stored in this object.
   *
   * @return An Rcpp::RObject containing the rolling maxima of the data.
   *         The type of the returned object will depend on the type of the
   *         data (e.g., a NumericVector for a numeric vector input).
   */
  Rcpp::RObject compute() override {
      // Use the "initialize_output" method from the superclass
     NumericVector out = this->initialize_output(this->data);

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

/**
 * @brief Specialization of the RollingMax class template for computing
 *        rolling maxima of a numeric matrix.
 *
 * @see RollingMax
 * @see Rolling
 */
template <> class RollingMax<NumericMatrix> : public Rolling<NumericMatrix> {
public:
  /**
   * @brief Constructs a RollingMax object with the given data and lag.
   *
   * @param d The data for which to compute rolling maxima.
   * @param l The lag (number of periods) over which to compute rolling maxima.
   */
  RollingMax(NumericMatrix d, int l) : Rolling<NumericMatrix>(d, l) {}

  /**
   * @brief Computes the rolling maxima of the data stored in this object.
   *
   * @return An Rcpp::RObject containing the rolling maxima of the data.
   *         The returned object will be a NumericMatrix.
   */
  Rcpp::RObject compute() override {
    // Use the "initialize_output" method from the superclass
    NumericMatrix out = this->initialize_output(this->data);

    int nrow = this->data.nrow(), ncol = this->data.ncol();

    double max = this->data(0, 0);

    for (int j = 0; j < ncol; j++){
      for (int i = this->lag; i < nrow; i++) {
        max = std::max(max, this->data(i-this->lag, j));
        out(i, j) = max;
      }
    }
    return out;
  }
};


// The subclass "RollingMin"
/**
 * @brief A class template for computing rolling minima of a given data type.
 *
 * @tparam T The type of data for which to compute rolling minima. Can be
 *           a numeric vector.
 *
 * @see Rolling
 */
template <typename T>
class RollingMin : public Rolling<T> {
public:
  /**
   * @brief Constructs a RollingMin object with the given data and lag.
   *
   * @param d The data for which to compute rolling minima.
   * @param l The lag (number of periods) over which to compute rolling minima.
   */
  RollingMin(T d, int l) : Rolling<T>(d, l) {}

  /**
   * @brief Computes the rolling minima of the data stored in this object.
   *
   * @return An Rcpp::RObject containing the rolling minima of the data.
   *         The type of the returned object will depend on the type of the
   *         data (e.g., a NumericVector for a numeric vector input).
   */
  Rcpp::RObject compute() override {
      // Use the "initialize_output" method from the superclass
     NumericVector out = this->initialize_output(this->data);

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

/**
 * @brief Specialization of the RollingMin class template for computing
 *        rolling minima of a numeric matrix.
 *
 * @see RollingMin
 * @see Rolling
 */
template <> class RollingMin<NumericMatrix> : public Rolling<NumericMatrix> {
public:
  /**
   * @brief Constructs a RollingMin object with the given data and lag.
   *
   * @param d The data for which to compute rolling minima.
   * @param l The lag (number of periods) over which to compute rolling minima.
   */
  RollingMin(NumericMatrix d, int l) : Rolling<NumericMatrix>(d, l) {}

  /**
   * @brief Computes the rolling minima of the data stored in this object.
   *
   * @return An Rcpp::RObject containing the rolling minima of the data.
   *         The returned object will be a NumericMatrix.
   */
  Rcpp::RObject compute() override {
    // Use the "initialize_output" method from the superclass
    NumericMatrix out = this->initialize_output(this->data);

    int nrow = this->data.nrow(), ncol = this->data.ncol();

    double min = this->data(0, 0);

    for (int j = 0; j < ncol; j++){
      for (int i = this->lag; i < nrow; i++) {
        min = std::min(min, this->data(i-this->lag, j));
        out(i, j) = min;
      }
    }
    return out;
  }
};

// [[Rcpp::export]]
// Compute a rolling sum of a NumericVector object.
//
// @param x A NumericVector object representing the input data for which the rolling sum should be computed.
// @param lag An integer representing the lag or window size for the rolling sum computation.
//
// @return An RObject representing the rolling sum of the input data.
Rcpp::RObject rollingsum(NumericVector x, int lag) {
RollingSum<NumericVector> s(x, lag);
return s.compute();
}

// [[Rcpp::export]]
// Compute a rolling sum of a NumericMatrix object.
//
// @param x A NumericMatrix object representing the input data for which the rolling sum should be computed.
// @param lag An integer representing the lag or window size for the rolling sum computation.
//
// @return An RObject representing the rolling sum of the input data.
Rcpp::RObject rollingsumM(NumericMatrix x, int lag) {
RollingSum<NumericMatrix> s(x, lag);
return s.compute();
}

// [[Rcpp::export]]
// Compute a rolling average of a NumericVector object.
//
// @param x A NumericVector object representing the input data for which the rolling average should be computed.
// @param lag An integer representing the lag or window size for the rolling average computation.
//
// @return An RObject representing the rolling average of the input data.
Rcpp::RObject rollingaverage(NumericVector x, int lag) {
  RollingAverage<NumericVector> s(x, lag);
  return s.compute();
}

// [[Rcpp::export]]
// Compute a rolling average of a NumericMatrix object.
//
// @param x A NumericMatrix object representing the input data for which the rolling average should be computed.
// @param lag An integer representing the lag or window size for the rolling average computation.
//
// @return An RObject representing the rolling average of the input data.
Rcpp::RObject rollingaverageM(NumericMatrix x, int lag) {
  RollingAverage<NumericMatrix> s(x, lag);
  return s.compute();
}

// [[Rcpp::export]]
// Compute a rolling max of a NumericVector object.
//
// @param x A NumericVector object representing the input data for which the rolling max should be computed.
// @param lag An integer representing the lag or window size for the rolling max computation.
//
// @return An RObject representing the rolling max of the input data.
Rcpp::RObject rollingmax(NumericVector x, int lag) {
  RollingMax<NumericVector> s(x, lag);
  return s.compute();
}

// [[Rcpp::export]]
// Compute a rolling max of a NumericMatrix object.
//
// @param x A NumericMatrix object representing the input data for which the rolling max should be computed.
// @param lag An integer representing the lag or window size for the rolling max computation.
//
// @return An RObject representing the rolling max of the input data.
Rcpp::RObject rollingmaxM(NumericMatrix x, int lag) {
  RollingMax<NumericMatrix> s(x, lag);
  return s.compute();
}

// [[Rcpp::export]]
// Compute a rolling min of a NumericVector object.
//
// @param x A NumericVector object representing the input data for which the rolling min should be computed.
// @param lag An integer representing the lag or window size for the rolling min computation.
//
// @return An RObject representing the rolling min of the input data.
Rcpp::RObject rollingmin(NumericVector x, int lag) {
  RollingMin<NumericVector> s(x, lag);
  return s.compute();
}

// [[Rcpp::export]]
// Compute a rolling min of a NumericMatrix object.
//
// @param x A NumericMatrix object representing the input data for which the rolling min should be computed.
// @param lag An integer representing the lag or window size for the rolling min computation.
//
// @return An RObject representing the rolling min of the input data.
Rcpp::RObject rollingminM(NumericMatrix x, int lag) {
  RollingMin<NumericMatrix> s(x, lag);
  return s.compute();
}
