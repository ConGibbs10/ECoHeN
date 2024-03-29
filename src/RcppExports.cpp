// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// count_stubs_for_B0minusv_cpp
Rcpp::IntegerMatrix count_stubs_for_B0minusv_cpp(Rcpp::IntegerVector B0, Rcpp::List G_stats);
RcppExport SEXP _ECoHeN_count_stubs_for_B0minusv_cpp(SEXP B0SEXP, SEXP G_statsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type B0(B0SEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type G_stats(G_statsSEXP);
    rcpp_result_gen = Rcpp::wrap(count_stubs_for_B0minusv_cpp(B0, G_stats));
    return rcpp_result_gen;
END_RCPP
}
// count_stubs_for_B0_cpp
Rcpp::IntegerMatrix count_stubs_for_B0_cpp(Rcpp::IntegerVector B0, Rcpp::List G_stats);
RcppExport SEXP _ECoHeN_count_stubs_for_B0_cpp(SEXP B0SEXP, SEXP G_statsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type B0(B0SEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type G_stats(G_statsSEXP);
    rcpp_result_gen = Rcpp::wrap(count_stubs_for_B0_cpp(B0, G_stats));
    return rcpp_result_gen;
END_RCPP
}
// lrd
double lrd(int epoch, double learning_rate, double decay_rate);
RcppExport SEXP _ECoHeN_lrd(SEXP epochSEXP, SEXP learning_rateSEXP, SEXP decay_rateSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type epoch(epochSEXP);
    Rcpp::traits::input_parameter< double >::type learning_rate(learning_rateSEXP);
    Rcpp::traits::input_parameter< double >::type decay_rate(decay_rateSEXP);
    rcpp_result_gen = Rcpp::wrap(lrd(epoch, learning_rate, decay_rate));
    return rcpp_result_gen;
END_RCPP
}
// is_stable
bool is_stable(Rcpp::IntegerVector x, Rcpp::IntegerVector y, Rcpp::IntegerVector z);
RcppExport SEXP _ECoHeN_is_stable(SEXP xSEXP, SEXP ySEXP, SEXP zSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type z(zSEXP);
    rcpp_result_gen = Rcpp::wrap(is_stable(x, y, z));
    return rcpp_result_gen;
END_RCPP
}
// is_cycle
bool is_cycle(Rcpp::IntegerVector x, Rcpp::IntegerVector y, Rcpp::IntegerVector z);
RcppExport SEXP _ECoHeN_is_cycle(SEXP xSEXP, SEXP ySEXP, SEXP zSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type z(zSEXP);
    rcpp_result_gen = Rcpp::wrap(is_cycle(x, y, z));
    return rcpp_result_gen;
END_RCPP
}
// is_empty
bool is_empty(Rcpp::IntegerVector x);
RcppExport SEXP _ECoHeN_is_empty(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(is_empty(x));
    return rcpp_result_gen;
END_RCPP
}
// main_search_decay_cpp
Rcpp::List main_search_decay_cpp(double alpha, Rcpp::IntegerVector B0, Rcpp::List G_stats, double learning_rate, double decay_rate, Rcpp::String adj_method, int max_iter);
RcppExport SEXP _ECoHeN_main_search_decay_cpp(SEXP alphaSEXP, SEXP B0SEXP, SEXP G_statsSEXP, SEXP learning_rateSEXP, SEXP decay_rateSEXP, SEXP adj_methodSEXP, SEXP max_iterSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type B0(B0SEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type G_stats(G_statsSEXP);
    Rcpp::traits::input_parameter< double >::type learning_rate(learning_rateSEXP);
    Rcpp::traits::input_parameter< double >::type decay_rate(decay_rateSEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type adj_method(adj_methodSEXP);
    Rcpp::traits::input_parameter< int >::type max_iter(max_iterSEXP);
    rcpp_result_gen = Rcpp::wrap(main_search_decay_cpp(alpha, B0, G_stats, learning_rate, decay_rate, adj_method, max_iter));
    return rcpp_result_gen;
END_RCPP
}
// smallest_indices1
Rcpp::IntegerVector smallest_indices1(Rcpp::DoubleVector x, int N);
RcppExport SEXP _ECoHeN_smallest_indices1(SEXP xSEXP, SEXP NSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DoubleVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    rcpp_result_gen = Rcpp::wrap(smallest_indices1(x, N));
    return rcpp_result_gen;
END_RCPP
}
// smallest_indices2
Rcpp::IntegerVector smallest_indices2(Rcpp::DoubleVector x, int N);
RcppExport SEXP _ECoHeN_smallest_indices2(SEXP xSEXP, SEXP NSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::DoubleVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type N(NSEXP);
    rcpp_result_gen = Rcpp::wrap(smallest_indices2(x, N));
    return rcpp_result_gen;
END_RCPP
}
// index_Cpp2R
Rcpp::IntegerVector index_Cpp2R(Rcpp::IntegerVector x);
RcppExport SEXP _ECoHeN_index_Cpp2R(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(index_Cpp2R(x));
    return rcpp_result_gen;
END_RCPP
}
// index_R2Cpp
Rcpp::IntegerVector index_R2Cpp(Rcpp::IntegerVector x);
RcppExport SEXP _ECoHeN_index_R2Cpp(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(index_R2Cpp(x));
    return rcpp_result_gen;
END_RCPP
}
// c_integer
Rcpp::IntegerVector c_integer(Rcpp::IntegerVector x, Rcpp::IntegerVector y);
RcppExport SEXP _ECoHeN_c_integer(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(c_integer(x, y));
    return rcpp_result_gen;
END_RCPP
}
// vector_equals_string
Rcpp::LogicalVector vector_equals_string(Rcpp::CharacterVector x, Rcpp::String s);
RcppExport SEXP _ECoHeN_vector_equals_string(SEXP xSEXP, SEXP sSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::String >::type s(sSEXP);
    rcpp_result_gen = Rcpp::wrap(vector_equals_string(x, s));
    return rcpp_result_gen;
END_RCPP
}
// which_true
Rcpp::IntegerVector which_true(Rcpp::LogicalVector x);
RcppExport SEXP _ECoHeN_which_true(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::LogicalVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(which_true(x));
    return rcpp_result_gen;
END_RCPP
}
// subset_matrix
Rcpp::IntegerMatrix subset_matrix(Rcpp::IntegerMatrix M, Rcpp::IntegerVector rows, Rcpp::IntegerVector cols);
RcppExport SEXP _ECoHeN_subset_matrix(SEXP MSEXP, SEXP rowsSEXP, SEXP colsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::IntegerMatrix >::type M(MSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type rows(rowsSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type cols(colsSEXP);
    rcpp_result_gen = Rcpp::wrap(subset_matrix(M, rows, cols));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_ECoHeN_count_stubs_for_B0minusv_cpp", (DL_FUNC) &_ECoHeN_count_stubs_for_B0minusv_cpp, 2},
    {"_ECoHeN_count_stubs_for_B0_cpp", (DL_FUNC) &_ECoHeN_count_stubs_for_B0_cpp, 2},
    {"_ECoHeN_lrd", (DL_FUNC) &_ECoHeN_lrd, 3},
    {"_ECoHeN_is_stable", (DL_FUNC) &_ECoHeN_is_stable, 3},
    {"_ECoHeN_is_cycle", (DL_FUNC) &_ECoHeN_is_cycle, 3},
    {"_ECoHeN_is_empty", (DL_FUNC) &_ECoHeN_is_empty, 1},
    {"_ECoHeN_main_search_decay_cpp", (DL_FUNC) &_ECoHeN_main_search_decay_cpp, 7},
    {"_ECoHeN_smallest_indices1", (DL_FUNC) &_ECoHeN_smallest_indices1, 2},
    {"_ECoHeN_smallest_indices2", (DL_FUNC) &_ECoHeN_smallest_indices2, 2},
    {"_ECoHeN_index_Cpp2R", (DL_FUNC) &_ECoHeN_index_Cpp2R, 1},
    {"_ECoHeN_index_R2Cpp", (DL_FUNC) &_ECoHeN_index_R2Cpp, 1},
    {"_ECoHeN_c_integer", (DL_FUNC) &_ECoHeN_c_integer, 2},
    {"_ECoHeN_vector_equals_string", (DL_FUNC) &_ECoHeN_vector_equals_string, 2},
    {"_ECoHeN_which_true", (DL_FUNC) &_ECoHeN_which_true, 1},
    {"_ECoHeN_subset_matrix", (DL_FUNC) &_ECoHeN_subset_matrix, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_ECoHeN(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
