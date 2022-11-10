#ifndef COUNTSTUBS_H
#define COUNTSTUBS_H

#include <Rcpp.h>
Rcpp::IntegerMatrix count_stubs_for_B0minusv_cpp(Rcpp::IntegerVector B0, Rcpp::List G_stats);
Rcpp::IntegerMatrix count_stubs_for_B0_cpp(Rcpp::IntegerVector B0, Rcpp::List G_stats);

#endif
