#include <Rcpp.h>
#include "utilities.h"
#include "count_stubs.h"
#include "smallest_indices.h"
using namespace Rcpp;

//[[Rcpp::export]]
double lrd(int epoch, double learning_rate, double decay_rate) {
  return learning_rate*pow(decay_rate, (double)epoch);
}

//[[Rcpp::export]]
bool is_stable(Rcpp::IntegerVector x, Rcpp::IntegerVector y, Rcpp::IntegerVector z) {
  return x.size() == y.size() && y.size() == z.size() && Rcpp::setequal(x.sort(), y.sort()) && Rcpp::setequal(y.sort(), z.sort());
}

//[[Rcpp::export]]
bool is_cycle(Rcpp::IntegerVector x, Rcpp::IntegerVector y, Rcpp::IntegerVector z) {
  return x.size() == z.size() && y.size() - x.size() == 1 && Rcpp::setequal(x.sort(), z.sort());
}

//[[Rcpp::export]]
bool is_empty(Rcpp::IntegerVector x) {
  return x.size() == 0;
}

//[[Rcpp::export]]
Rcpp::List main_search_decay_cpp(double alpha, Rcpp::IntegerVector B0, Rcpp::List G_stats, double learning_rate, double decay_rate, int max_iter, Rcpp::String adj_method) {
  // extract variables from input
  int n = as<int>(G_stats["n"]);
  int K = as<int>(G_stats["K"]);
  Rcpp::DataFrame Nodes = as<Rcpp::DataFrame>(G_stats["nodes"]);
  Rcpp::IntegerVector nodes = as<Rcpp::IntegerVector>(Nodes["node"]);

  // in-house and R functions needed
  Rcpp::Function compute_pvals("compute_pvals_adjbinom");
  Rcpp::Function adjust_pvals("p.adjust");

  // variables needed for updates
  double r;
  int max_change;
  Rcpp::IntegerVector B01;
  Rcpp::IntegerVector B02;
  Rcpp::IntegerVector B03;

  Rcpp::IntegerVector exterior;
  Rcpp::DoubleVector exterior_pvals;
  Rcpp::DoubleVector interior_pvals;
  Rcpp::DoubleVector exterior_adj_pvals;
  Rcpp::DoubleVector interior_adj_pvals;

  Rcpp::IntegerVector sIDs;
  Rcpp::DoubleVector sIDs_adj_pvals;
  Rcpp::IntegerVector lIDs;
  Rcpp::DoubleVector lIDs_adj_pvals;

  Rcpp::IntegerVector additions;
  Rcpp::IntegerVector subtractions;

  Rcpp::DoubleVector pvals(n);
  Rcpp::DoubleVector adj_pvals(n);

  // output
  Rcpp::IntegerVector community;
  Rcpp::List Btrace;

  // variables needed for control flow
  int ctr = -1;
  int max_ctr = max_iter - 1;
  bool unstable = true;
  bool noncyclic = true;
  bool nonempty = !is_empty(B0);
  bool conv = true;

  // start updates
  B01 = B0;
  while(ctr < max_ctr && unstable && noncyclic && nonempty) {
    ctr = ++ctr;
    r = lrd(ctr, learning_rate, decay_rate);
    max_change = std::max(1, (int)std::floor(r*(double)n));

    // DEBUGGING
    // Rcpp::Rcout << "Iteration : " << ctr << "\n";
    // Rcpp::Rcout << "Max change : " << max_change << "\n";

    // ADDITIONS
    // compute adjusted and unadjusted pvalues
    pvals = compute_pvals(B01, G_stats);
    adj_pvals = adjust_pvals(pvals, Rcpp::Named("method") = adj_method);
    // get exterior
    exterior = Rcpp::setdiff(nodes, B01);
    exterior_pvals = pvals[index_R2Cpp(exterior)];
    exterior_adj_pvals = adj_pvals[index_R2Cpp(exterior)];
    // find the max_change number of nodes with the smallest pvalues to add to B01
    sIDs = smallest_indices2(exterior_pvals, max_change);
    sIDs_adj_pvals = exterior_adj_pvals[sIDs];
    additions = exterior[Rcpp::as<Rcpp::IntegerVector>(sIDs[sIDs_adj_pvals <= alpha])];
    B02 = c_integer(B01, additions);

    // SUBTRACTIONS
    pvals = compute_pvals(B02, G_stats);
    adj_pvals = adjust_pvals(pvals, Rcpp::Named("method") = adj_method);
    // interior is just B02
    interior_pvals = pvals[index_R2Cpp(B02)];
    interior_adj_pvals = adj_pvals[index_R2Cpp(B02)];
    // find the max_change number of nodes with the largest pvalues to remove from B02
    lIDs = smallest_indices2(-interior_pvals, max_change);
    lIDs_adj_pvals = interior_adj_pvals[lIDs];
    subtractions = B02[Rcpp::as<Rcpp::IntegerVector>(lIDs[lIDs_adj_pvals > alpha])];
    B03 = Rcpp::setdiff(B02, subtractions);

    // save results
    Btrace.push_back(Rcpp::List::create(B02, B03));

    // checks for stability, cycles, and emptiness
    unstable = !is_stable(B01, B02, B03);
    noncyclic = !is_cycle(B01, B02, B03);
    nonempty = !is_empty(B03);

    // set B03 to B01 for next round of iterations;
    B01 = B03;
  }

  // create list of trace items
  Rcpp::List trace = Rcpp::List::create(Rcpp::Named("seed") = B0, Rcpp::Named("B") = Btrace, Rcpp::Named("cycle") = !noncyclic, Rcpp::Named("convergence") = ctr < max_ctr, Rcpp::Named("iterations") = ctr + 1);

  // check conditions
  if (!noncyclic || ctr == max_ctr || !nonempty) {
    community = Rcpp::IntegerVector::create();
    adj_pvals = Rcpp::rep(1, n);
  } else {
    community = B03;
    pvals = compute_pvals(B03, G_stats);
    adj_pvals = adjust_pvals(pvals, Rcpp::Named("method") = adj_method);
  }

  return Rcpp::List::create(Rcpp::Named("community") = community, Rcpp::Named("pvalues") = adj_pvals, Rcpp::Named("trace") = trace);

  // DEBUGGING
  // return Rcpp::List::create(Named("exterior") = exterior, Named("exterior_adj_pvals") = exterior_adj_pvals, Named("sIDs") = sIDs, Named("sIDs_adj_pvals") = sIDs_adj_pvals, Named("additions") = additions, Named("interior_adj_pvals") = interior_adj_pvals, Named("lIDs") = lIDs, Named("lIDs_adj_pvals") = lIDs_adj_pvals, Named("subtractions") = subtractions, Named("B01") = B01, Named("B02") = B02, Named("B03") = B03);
}
