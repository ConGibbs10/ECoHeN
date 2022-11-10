#include <Rcpp.h>
using namespace Rcpp;

//' Converts C++ indices to R indices by adding one.
//'
//' @param x Integer vector.
//'
//' @export
//' @keywords internal
//[[Rcpp::export]]
Rcpp::IntegerVector index_Cpp2R(Rcpp::IntegerVector x) {
  int n = x.size();
  Rcpp::IntegerVector out(n);
  out = x + 1;
  return out;
}

//' Converts R indices to C++ by subtracting one.
//'
//' @param x Integer vector.
//'
//' @export
//' @keywords internal
//[[Rcpp::export]]
Rcpp::IntegerVector index_R2Cpp(Rcpp::IntegerVector x) {
  int n = x.size();
  Rcpp::IntegerVector out(n);
  out = x - 1;
  return out;
}

//' Concatenates two integer vectors.
//'
//' @param x Integer vector.
//' @param y Integer vector.
//'
//' @export
//' @keywords internal
//[[Rcpp::export]]
Rcpp::IntegerVector c_integer(Rcpp::IntegerVector x, Rcpp::IntegerVector y) {
  int xN = x.size();
  int zN = xN + y.size();
  Rcpp::IntegerVector z(zN);

  for(int i = 0; i < xN; ++i) {
    z[i] = x[i];
  }
  for(int j = xN; j < zN; ++j) {
    z[j] = y[j-xN];
  }

  return z;
}

//' Construct logical vector by comparing a character vector to a string.
//'
//' @param x Character vector.
//' @param s String.
//'
//' @export
//' @keywords internal
//[[Rcpp::export]]
Rcpp::LogicalVector vector_equals_string(Rcpp::CharacterVector x, Rcpp::String s) {
  int n = x.size();
  Rcpp::LogicalVector lx(n);

  for(int i = 0; i < n; i++) {
    lx[i] = (x[i] == s ? true : false);
  }

  return lx;
}

//' Indices of TRUE values in a logical vector.
//'
//' @param x Logical vector.
//'
//' @export
//' @keywords internal
//[[Rcpp::export]]
Rcpp::IntegerVector which_true(Rcpp::LogicalVector x) {
  int n = x.size();
  Rcpp::IntegerVector out = Rcpp::seq_len(n) - 1;
  return out[x];
}

//' Subset a matrix.
//'
//' @param M Integer matrix.
//' @param rows Integer vector of rows in C++ indices.
//' @param cols Integer vector of columns in C++ indices.
//'
//' @export
//' @keywords internal
//[[Rcpp::export]]
Rcpp::IntegerMatrix subset_matrix(Rcpp::IntegerMatrix M, Rcpp::IntegerVector rows, Rcpp::IntegerVector cols){
  // source: https://stackoverflow.com/questions/59284212/efficient-matrix-subsetting-with-rcpp

  int rl = rows.length();
  int cl = cols.length();
  Rcpp::IntegerMatrix out(rl, cl);

  for (int i = 0; i < cl; i++){
    Rcpp::IntegerMatrix::Column org_c = M(_, cols[i]);
    Rcpp::IntegerMatrix::Column new_c = out(_, i);
    for (int j = 0; j < rl; j++){
      new_c[j] = org_c[rows[j]];
    }
  }

  return out;
}
