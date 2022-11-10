#include <Rcpp.h>
#include "utilities.h"
using namespace Rcpp;

//' Identifies the smallest indices in a vector.
//'
//' @details Identifies the indices of the N smallest values in a double vector x,
//' breaking ties uniformly at random. Uses a full sort.
//'
//' @param Vector of doubles.
//' @param Integer for the number of indices to identify.
//'
//' @export
//' @keywords internal
//[[Rcpp::export]]
Rcpp::IntegerVector smallest_indices1(Rcpp::DoubleVector x, int N){
  // breaks ties according to the values of y, generated uniformly at random
  int xN = x.size();
  int rN = (N > xN ? xN : N);
  Rcpp::DoubleVector y = Rcpp::runif(xN, 0, 1); // break ties uniformly at random

  Rcpp::IntegerVector indices(xN);
  std::iota(indices.begin(), indices.end(), 0); // fill with 0,1,2,...

  // then sort that vector by the values of y and z
  std::sort(indices.begin(), indices.end(), [&](int i, int j){
    if (x[i] == x[j] ) {
      return y[i] < y[j];
    }
    return x[i] < x[j];
  });

  return Rcpp::IntegerVector(indices.begin(), indices.begin() + rN);
}

//' Identifies the smallest indices in a vector.
//'
//' @details Identifies the indices of the N smallest values in a double vector x,
//' breaking ties uniformly at random. Uses a partial sort.
//'
//' @param Vector of doubles.
//' @param Integer for the number of indices to identify.
//'
//' @export
//' @keywords internal
//[[Rcpp::export]]
Rcpp::IntegerVector smallest_indices2(Rcpp::DoubleVector x, int N){
  // breaks ties according to the values of y, generated uniformly at random
  int xN = x.size();
  int rN = (N > xN ? xN : N);
  Rcpp::DoubleVector y = Rcpp::runif(xN, 0, 1); // break ties uniformly at random

  Rcpp::IntegerVector indices(xN);
  std::iota(indices.begin(), indices.end(), 0); // fill with 0,1,2,...

  std::partial_sort(indices.begin(), indices.begin() + rN, indices.end(), [&](int i,int j) {
    if (x[i] == x[j]) {
      return y[i] < y[j];
    }
    return x[i] < x[j];
  });

  return Rcpp::IntegerVector(indices.begin(), indices.begin() + rN);
}
