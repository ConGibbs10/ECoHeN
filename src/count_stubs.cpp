#include <Rcpp.h>
#include "utilities.h"
using namespace Rcpp;

//' Count stubs for p-value computation using corollary.
//'
//' @details Counts the number of colored stubs for each color emanating from each
//' node which connects to a provided set of nodes in the observed graph, excluding
//' any stub which could result in a self-loop.
//'
//' @param B0 Vector of vertices representing the candidate community.
//' @param G_stats List of graph statistics returned from `eval_G`.
//'
//' @export
//' @keywords internal
//[[Rcpp::export]]
Rcpp::IntegerMatrix count_stubs_for_B0minusv_cpp(Rcpp::IntegerVector B0, Rcpp::List G_stats) {
  // Unwind input
  int n = as<int>(G_stats["n"]);
  int K = as<int>(G_stats["K"]);
  Rcpp::DataFrame Nodes = as<Rcpp::DataFrame>(G_stats["nodes"]);
  Rcpp::IntegerVector nodes = as<Rcpp::IntegerVector>(Nodes["node"]);
  Rcpp::CharacterVector types = as<Rcpp::CharacterVector>(Nodes["type"]);
  Rcpp::DataFrame Typemap = as<Rcpp::DataFrame>(G_stats["type_map"]);
  Rcpp::CharacterVector fromTypes = as<Rcpp::CharacterVector>(Typemap["from_type"]);
  Rcpp::CharacterVector toTypes = as<Rcpp::CharacterVector>(Typemap["to_type"]);
  Rcpp::IntegerVector totalStubs = as<Rcpp::IntegerVector>(Typemap["stubs"]);
  Rcpp::IntegerMatrix stubs = G_stats["stubs"];
  Rcpp::List AdjNodes = G_stats["adj_nodes"];

  // Initialize output
  Rcpp::IntegerMatrix P(n*K, 5);
  Rcpp::colnames(P) = Rcpp::CharacterVector::create("v", "k", "q", "m", "n");

  // Helpers
  Rcpp::LogicalVector inB0 = Rcpp::in(nodes, B0);
  int nsg = K+(K*(K-1));
  Rcpp::IntegerVector uniM(nsg);
  Rcpp::IntegerVector uniN(nsg);
  Rcpp::IntegerMatrix subStubs;
  // for each subgraph, find what m would be in v is not in B
  for(int sg = 0; sg < nsg; sg++) {
    Rcpp::IntegerMatrix subStubs = subset_matrix(stubs, index_R2Cpp(B0)[vector_equals_string(types[index_R2Cpp(B0)], toTypes[sg])], which_true(vector_equals_string(toTypes[vector_equals_string(fromTypes, toTypes[sg])], fromTypes[sg])));
    uniM[sg] = sum(as<Rcpp::IntegerVector>(internal::convert_using_rfunction(subStubs, "as.vector")));
    uniN[sg] = totalStubs[sg] - uniM[sg];
  }

  //(r<0 ? 0 : 1)
  int r = -1;
  Rcpp::String vtype;
  Rcpp::IntegerVector vtIDs(K);
  Rcpp::CharacterVector vToTypes(K);
  Rcpp::IntegerVector vadj;
  for(int v = 0; v < n; v++) {
    vtype = types[v];
    vtIDs = which_true(vector_equals_string(fromTypes, vtype));
    vToTypes = toTypes[vtIDs];
    vadj = as<Rcpp::IntegerVector>(AdjNodes[v]);
    for(int k = 0; k < K; k++) {
      r += 1;
      // save v
      P(r, 0) = v + 1;
      // save k
      P(r, 1) = stubs(v, k);
      // save q
      P(r, 2) = Rcpp::intersect(as<IntegerVector>(B0[vector_equals_string(types[index_R2Cpp(B0)], vToTypes[k])]), vadj).size();
      // save m and n
      if(inB0[v] == 1) {
        // save m
        P(r, 3) = uniM[vtIDs[k]] - (k == 0 ? stubs(v,0) : 0);
        // save n
        P(r, 4) = uniN[vtIDs[k]];
      } else {
        // save m
        P(r, 3) = uniM[vtIDs[k]];
        // save n
        P(r, 4) = uniN[vtIDs[k]] - (k == 0 ? stubs(v,0) : 0);
      }
    }
  }

  return P;
}

//' Count stubs for p-value computation using theorem.
//'
//' @details Counts the number of colored stubs for each color emanating from each
//' node which connects to a provided set of nodes in the observed graph.
//'
//' @param B0 Vector of vertices representing the candidate community.
//' @param G_stats List of graph statistics returned from `eval_G`.
//'
//' @export
//' @keywords internal
//[[Rcpp::export]]
Rcpp::IntegerMatrix count_stubs_for_B0_cpp(Rcpp::IntegerVector B0, Rcpp::List G_stats) {
  // Unwind input
  int n = as<int>(G_stats["n"]);
  int K = as<int>(G_stats["K"]);
  Rcpp::DataFrame Nodes = as<Rcpp::DataFrame>(G_stats["nodes"]);
  Rcpp::IntegerVector nodes = as<Rcpp::IntegerVector>(Nodes["node"]);
  Rcpp::CharacterVector types = as<Rcpp::CharacterVector>(Nodes["type"]);
  Rcpp::DataFrame Typemap = as<Rcpp::DataFrame>(G_stats["type_map"]);
  Rcpp::CharacterVector fromTypes = as<Rcpp::CharacterVector>(Typemap["from_type"]);
  Rcpp::CharacterVector toTypes = as<Rcpp::CharacterVector>(Typemap["to_type"]);
  Rcpp::IntegerVector totalStubs = as<Rcpp::IntegerVector>(Typemap["stubs"]);
  Rcpp::IntegerMatrix stubs = G_stats["stubs"];
  Rcpp::List AdjNodes = G_stats["adj_nodes"];

  // Initialize output
  Rcpp::IntegerMatrix P(n*K, 5);
  Rcpp::colnames(P) = Rcpp::CharacterVector::create("v", "k", "q", "m", "n");

  // Helpers
  Rcpp::LogicalVector inB0 = Rcpp::in(nodes, B0);
  int nsg = K+(K*(K-1));
  Rcpp::IntegerVector uniM(nsg);
  Rcpp::IntegerVector uniN(nsg);
  Rcpp::IntegerMatrix subStubs;
  // for each subgraph, find what m would be in v is not in B
  for(int sg = 0; sg < nsg; sg++) {
    Rcpp::IntegerMatrix subStubs = subset_matrix(stubs, index_R2Cpp(B0)[vector_equals_string(types[index_R2Cpp(B0)], toTypes[sg])], which_true(vector_equals_string(toTypes[vector_equals_string(fromTypes, toTypes[sg])], fromTypes[sg])));
    uniM[sg] = sum(as<Rcpp::IntegerVector>(internal::convert_using_rfunction(subStubs, "as.vector")));
    uniN[sg] = totalStubs[sg] - uniM[sg];
  }

  //(r<0 ? 0 : 1)
  int r = -1;
  Rcpp::String vtype;
  Rcpp::IntegerVector vtIDs(K);
  Rcpp::CharacterVector vToTypes(K);
  Rcpp::IntegerVector vadj;
  for(int v = 0; v < n; v++) {
    vtype = types[v];
    vtIDs = which_true(vector_equals_string(fromTypes, vtype));
    vToTypes = toTypes[vtIDs];
    vadj = as<Rcpp::IntegerVector>(AdjNodes[v]);
    for(int k = 0; k < K; k++) {
      r += 1;
      // save v
      P(r, 0) = v + 1;
      // save k
      P(r, 1) = stubs(v, k);
      // save q
      P(r, 2) = Rcpp::intersect(as<IntegerVector>(B0[vector_equals_string(types[index_R2Cpp(B0)], vToTypes[k])]), vadj).size();
      // save m
      P(r, 3) = uniM[vtIDs[k]];
      // save n
      P(r, 4) = uniN[vtIDs[k]];
    }
  }

  return P;
}
