#ifndef EXTRACTION_H
#define EXTRACTION_H

#include <Rcpp.h>
double lrd(int epoch, double learning_rate, double decay_rate);
bool is_stable(Rcpp::IntegerVector x, Rcpp::IntegerVector y, Rcpp::IntegerVector z);
bool is_cycle(Rcpp::IntegerVector x, Rcpp::IntegerVector y, Rcpp::IntegerVector z);
bool is_empty(Rcpp::IntegerVector x);
Rcpp::List main_search_decay_cpp(double alpha, Rcpp::IntegerVector B0, Rcpp::List G_stats, double learning_rate, double decay_rate, int max_iter, Rcpp::String adj_method);

#endif
