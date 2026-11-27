#include <Rcpp.h>
#include <memory>
#include "csisearch.h"

//' Process `csisearch` Calls from R
//'
//' Sets of vertices are given as integer vectors of vertex indices.
//'
//' @param dir_lhs A vector of vertices with outgoing directed edges
//' @param dir_rhs A vector of vertices with incoming directed edges
//' @param lab A vector of labels for vertices
//' @param p_list A list of known distributions, each a list of four sets
//' @param q_list The target distribution as a list of four sets
//' @param label_map A list of edge labels
//' @param local_csi Local CSI statements
//' @param con_vars A set of variables that have assignments in labels
//'   (non-interventional)
//' @param intv_vars A set of of interventional variables
//' @param n Number of vertices
//' @param time_limit Time limit for the search (in hours)
//' @param rules Overrides the set of default rules
//' @param benchmark Record the search time
//' @param benchmark_rules Record time taken by each individual rule
//' @param draw_derivation Form a string representing the derivation steps (as dot)
//' @param draw_all Draw every distribution that was derived
//'   (vs only those that were used to derive the effect)
//' @param formula Output formula as string
//' @param improve Enable search enhancements
//' @param heuristic Use search heuristic
//' @param cache Use caching for separation criteria
//' @param verbose Print diagnostics during search
//' @noRd
// [[Rcpp::export]]
Rcpp::List initialize_csisearch(
  const std::vector<int>& dir_lhs,
  const std::vector<int>& dir_rhs,
  const Rcpp::StringVector& lab,
  const Rcpp::List& p_list,
  const Rcpp::List& q_list,
  const Rcpp::List& label_map,
  const Rcpp::List& local_csi,
  const std::vector<int>& con_vars,
  const std::vector<int>& intv_vars,
  const int& n,
  const double& time_limit,
  const std::vector<int>& rules,
  const bool& benchmark,
  const bool& benchmark_rules,
  const bool& draw_derivation,
  const bool& draw_all,
  const bool& formula,
  const bool& improve,
  const bool& heuristic,
  const bool& cache,
  const bool& verbose)
{
  // Objects are released automatically also when the search is interrupted
  std::unique_ptr<ldag> g;
  if (cache) g.reset(new ldag_cache(n));
  else g.reset(new ldag(n));

  varset con = varset::from_vector(con_vars);
  varset intv = varset::from_vector(intv_vars);

  // Add directed edges
  for (unsigned int i = 0; i < dir_rhs.size(); i++) {
    g->add_edge(dir_lhs[i], dir_rhs[i]);
  }
  g->set_contexts(con, intv);

  derivation d;

  std::unique_ptr<csisearch> s;
  if (heuristic) s.reset(new csisearch_heuristic(n, time_limit, benchmark, benchmark_rules, draw_derivation, draw_all, formula, improve, verbose));
  else s.reset(new csisearch(n, time_limit, benchmark, benchmark_rules, draw_derivation, draw_all, formula, improve, verbose));

  if (draw_derivation) s->set_derivation(&d);

  s->set_labels(lab);
  s->set_graph(g.get());
  s->set_options(rules);
  // The set of disabled response indicators is not used for LDAGs
  s->set_target(
    varset::from_vector(Rcpp::as<std::vector<int>>(q_list[0])),
    varset::from_vector(Rcpp::as<std::vector<int>>(q_list[1])),
    varset::from_vector(Rcpp::as<std::vector<int>>(q_list[2])),
    varset::from_vector(Rcpp::as<std::vector<int>>(q_list[3])),
    varset()
  );
  s->set_contexts(con);
  s->set_interventions(intv);

  // Add known distributions
  for (int i = 0; i < p_list.size(); i++) {
    Rcpp::List p = Rcpp::as<Rcpp::List>(p_list[i]);
    s->add_known(
      varset::from_vector(Rcpp::as<std::vector<int>>(p[0])),
      varset::from_vector(Rcpp::as<std::vector<int>>(p[1])),
      varset::from_vector(Rcpp::as<std::vector<int>>(p[2])),
      varset::from_vector(Rcpp::as<std::vector<int>>(p[3])),
      varset()
    );
  }

  // Map labels to edges for ldag
  for (int i = 0; i < label_map.size(); i++) {
    Rcpp::List label = Rcpp::as<Rcpp::List>(label_map[i]);
    Rcpp::List contexts = Rcpp::as<Rcpp::List>(label["contexts"]);
    varset context_set = varset::from_vector(Rcpp::as<std::vector<int>>(label["vars"]));
    g->add_context_set(context_set);
    for (int j = 0; j < contexts.size(); j++) {
      Rcpp::List context = Rcpp::as<Rcpp::List>(contexts[j]);
      varset zero = varset::from_vector(Rcpp::as<std::vector<int>>(context["zero"]));
      varset one = varset::from_vector(Rcpp::as<std::vector<int>>(context["one"]));
      int equiv = Rcpp::as<int>(context["equivalence"]);
      std::vector<int> from = Rcpp::as<std::vector<int>>(context["from"]);
      std::vector<int> to = Rcpp::as<std::vector<int>>(context["to"]);
      g->add_context(zero, one, equiv, from, to);
    }
  }

  // Add local CSIs
  for (int i = 0; i < local_csi.size(); i++) {
    Rcpp::List loc = Rcpp::as<Rcpp::List>(local_csi[i]);
    varset x = varset::from_vector(Rcpp::as<std::vector<int>>(loc["x"]));
    varset y = varset::from_vector(Rcpp::as<std::vector<int>>(loc["y"]));
    varset z = varset::from_vector(Rcpp::as<std::vector<int>>(loc["z"]));
    varset zero = varset::from_vector(Rcpp::as<std::vector<int>>(loc["zero"]));
    varset one = varset::from_vector(Rcpp::as<std::vector<int>>(loc["one"]));
    g->add_local_csi(x, y, z, zero, one);
  }

  if (verbose) Rcpp::Rcout << "Initializing search" << std::endl;

  return s->initialize();
}
