#include <Rcpp.h>
#include "dosearch.h"

//' Process `dosearch` Calls from R
//'
//' @param dir_lhs A vector of vertices with outgoing directed edges
//' @param dir_rhs A vector of vertices with incoming directed edges
//' @param bi_lhs A vector of vertices with incoming bidirected edges
//' @param bi_rhs A vector of vertices with incoming bidirected edges
//' @param lab A vector of labels for vertices
//' @param p_list A list of known distributions
//' @param q_vec A vector of vertices representing the causal query
//' @param n The number of vertices
//' @param tr A set representing transportability nodes
//' @param sb A set representing selection bias nodes
//' @param md_s A set representing missing data switches
//' @param md_p A set representing missing data proxies
//' @param time_limit Time limit for the search (in hours)
//' @param rules Overrides the set of default rules
//' @param benchmark Record the search time
//' @param benchmark_rules Record time taken by each individual rule
//' @param draw_derivation Form a string representing the
//'   derivation steps (as dot)
//' @param draw_all Draw every distribution that was derived
//'   (vs only those that were used to derive the effect)
//' @param formula Output formula as string
//' @param improve Enable search enhancements
//' @param heuristic Use search heuristic
//' @param md_sym Symbol used to represent active missing data mechanisms
//' @param verbose Print diagnostics during search
//' @noRd
// [[Rcpp::export]]
Rcpp::List initialize_dosearch(
  const std::vector<int>& dir_lhs,
  const std::vector<int>& dir_rhs,
  const std::vector<int>& bi_lhs,
  const std::vector<int>& bi_rhs,
  const Rcpp::StringVector& lab,
  const Rcpp::List& p_list,
  const std::vector<int>& q_vec,
  const int& n,
  const int& tr,
  const int& sb,
  const int& md_s,
  const int& md_p,
  const double& time_limit,
  const std::vector<int>& rules,
  const bool& benchmark,
  const bool& benchmark_rules,
  const bool& draw_derivation,
  const bool& draw_all,
  const bool& formula,
  const bool& improve,
  const bool& heuristic,
  const char& md_sym,
  const bool& verbose)
{
  dcongraph* g = new dcongraph(n);
  g->add_ivars();
  g->initialize_datanodes();

  // Add directed edges
  for (unsigned i = 0; i < dir_rhs.size(); i++) {
    g->add_edge(dir_lhs[i], dir_rhs[i]);
  }
  // Add bidirected edges
  for (unsigned i = 0; i < bi_rhs.size(); i++) {
    g->add_conf(bi_lhs[i], bi_rhs[i]);
  }
  // Add special vertices
  if (tr > 0) g->set_trnodes(tr);
  if (sb > 0) g->set_sbnodes(sb);
  if (md_s > 0) g->set_md_switches(md_s);
  if (md_p > 0) g->set_md_proxies(md_p);

  derivation* d = new derivation();

  dosearch *s;
  if (heuristic) s = new dosearch_heuristic(n, time_limit, benchmark, benchmark_rules, draw_derivation, draw_all, formula, improve, verbose);
  else s = new dosearch(n, time_limit, benchmark, benchmark_rules, draw_derivation, draw_all, formula, improve, verbose);

  if (draw_derivation) s->set_derivation(d);

  s->set_labels(lab);
  s->set_graph(g);
  s->set_options(rules);
  s->set_target(q_vec[0], q_vec[1], q_vec[2], q_vec[3]);
  s->set_md_symbol(md_sym);

  // Add known distributions
  for (int i = 0; i < p_list.size(); i++) {
    std::vector<int> p = p_list[i];
    s->add_known(p[0], p[1], p[2], p[3]);
  }

  if (verbose) Rcpp::Rcout << "Initializing search" << std::endl;

  Rcpp::List result = s->initialize();

  delete g;
  delete d;
  delete s;

  return result;
}
