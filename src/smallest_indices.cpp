#include <Rcpp.h>
#include "utilities.h"
using namespace Rcpp;

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

//[[Rcpp::export]]
Rcpp::IntegerVector smallest_indices3(Rcpp::DoubleVector x, int N){
  // does not break ties, simply returns the N smallest values of x and any values
  // tied for N
  if(N <= 0) {
    throw std::range_error("N must be a positive integer.");
  }

  int xN = x.size();
  int rN = (N > xN ? xN : N);
  Rcpp::DoubleVector kS = Rcpp::DoubleVector(rN);
  Rcpp::IntegerVector out;

  Rcpp::Environment DescTools = Environment::namespace_env("DescTools");
  Rcpp::Function Small = DescTools["Small"];

  kS = Small(x, Rcpp::Named("k") = N);
  out = which_true(x - kS[rN-1] <= 0.00000001);

  return out;
}

//[[Rcpp::export]]
Rcpp::IntegerVector smallest_indices4(Rcpp::DoubleVector x, int N){
  // does not break ties, simply returns the N smallest values of x. If number of
  // tied values makes the return exceed N, then the ties are omitted.
  if(N <= 0) {
    throw std::range_error("N must be a positive integer.");
  }

  int xN = x.size();
  int rN = (N > xN ? xN : N);
  Rcpp::DoubleVector kS = Rcpp::DoubleVector(rN);
  Rcpp::IntegerVector out;

  Rcpp::Environment DescTools = Environment::namespace_env("DescTools");
  Rcpp::Function Small = DescTools["Small"];

  kS = Small(x, Rcpp::Named("k") = N);
  out = which_true(x - kS[rN-1] <= 0.00000001);

  if(out.size() > rN) {
    Rcpp::IntegerVector elub = which_true(x == kS[rN-1]);
    out = Rcpp::setdiff(out, elub);
  }

  return out;
}
