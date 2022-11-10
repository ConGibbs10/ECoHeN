#ifndef UTILITIES_H
#define UTILITIES_H

#include <Rcpp.h>
Rcpp::IntegerVector index_Cpp2R(Rcpp::IntegerVector x);
Rcpp::IntegerVector index_R2Cpp(Rcpp::IntegerVector x);
Rcpp::IntegerVector c_integer(Rcpp::IntegerVector x, Rcpp::IntegerVector y);
Rcpp::LogicalVector vector_equals_string(Rcpp::CharacterVector x, Rcpp::String s);
Rcpp::IntegerVector which_true(Rcpp::LogicalVector x);
Rcpp::IntegerMatrix subset_matrix(Rcpp::IntegerMatrix M, Rcpp::IntegerVector rows, Rcpp::IntegerVector cols);

#endif
