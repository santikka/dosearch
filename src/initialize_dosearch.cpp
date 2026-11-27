#include <Rcpp.h>
#include <memory>
#include "dosearch.h"

//' Process `dosearch` Calls from R
//'
//' Sets of vertices are given as integer vectors of vertex indices.
//'
//' @param dir_lhs A vector of vertices with outgoing directed edges
//' @param dir_rhs A vector of vertices with incoming directed edges
//' @param bi_lhs A vector of vertices with incoming bidirected edges
//' @param bi_rhs A vector of vertices with incoming bidirected edges
//' @param lab A vector of labels for vertices
//' @param p_list A list of known distributions, each a list of five sets
//' @param q_list The target distribution as a list of five sets
//' @param n The number of vertices
//' @param tr A set representing transportability nodes
//' @param sb A set representing selection bias nodes
//' @param md_s A set representing missing data switches
//' @param md_p A set representing missing data proxies
//' @param time_limit Time limit for the search (in hours)
//' @param rules Overrides the set of default rules
//' @param case_reasoning Add the rules for case-by-general, case-by-case
//'   and general-by-case reasoning for missing data mechanisms
//' @param benchmark Record the search time
//' @param benchmark_rules Record time taken by each individual rule
//' @param draw_derivation Form a string representing the
//'   derivation steps (as dot)
//' @param draw_all Draw every distribution that was derived
//'   (vs only those that were used to derive the effect)
//' @param formula Output formula as string
//' @param improve Enable search enhancements
//' @param heuristic Use search heuristic
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
  const Rcpp::List& q_list,
  const int& n,
  const std::vector<int>& tr,
  const std::vector<int>& sb,
  const std::vector<int>& md_s,
  const std::vector<int>& md_p,
  const double& time_limit,
  const std::vector<int>& rules,
  const bool& case_reasoning,
  const bool& benchmark,
  const bool& benchmark_rules,
  const bool& draw_derivation,
  const bool& draw_all,
  const bool& formula,
  const bool& improve,
  const bool& heuristic,
  const bool& verbose)
{
  // Objects are released automatically also when the search is interrupted
  dag g(n);

  // Add directed edges
  for (unsigned i = 0; i < dir_rhs.size(); i++) {
    g.add_edge(dir_lhs[i], dir_rhs[i]);
  }
  // Add bidirected edges
  for (unsigned i = 0; i < bi_rhs.size(); i++) {
    g.add_conf(bi_lhs[i], bi_rhs[i]);
  }
  // Add special vertices
  g.set_trnodes(varset::from_vector(tr));
  g.set_sbnodes(varset::from_vector(sb));
  g.set_md_switches(varset::from_vector(md_s));
  g.set_md_proxies(varset::from_vector(md_p));

  derivation d;

  std::unique_ptr<dosearch> s;
  if (heuristic) s.reset(new dosearch_heuristic(n, time_limit, benchmark, benchmark_rules, draw_derivation, draw_all, formula, improve, verbose));
  else s.reset(new dosearch(n, time_limit, benchmark, benchmark_rules, draw_derivation, draw_all, formula, improve, verbose));

  if (draw_derivation) s->set_derivation(&d);

  s->set_labels(lab);
  s->set_graph(&g);
  s->set_case_reasoning(case_reasoning);
  s->set_options(rules);
  s->set_target(
    varset::from_vector(Rcpp::as<std::vector<int>>(q_list[0])),
    varset::from_vector(Rcpp::as<std::vector<int>>(q_list[1])),
    varset::from_vector(Rcpp::as<std::vector<int>>(q_list[2])),
    varset::from_vector(Rcpp::as<std::vector<int>>(q_list[3])),
    varset::from_vector(Rcpp::as<std::vector<int>>(q_list[4]))
  );

  // Add known distributions
  for (int i = 0; i < p_list.size(); i++) {
    Rcpp::List p = Rcpp::as<Rcpp::List>(p_list[i]);
    s->add_known(
      varset::from_vector(Rcpp::as<std::vector<int>>(p[0])),
      varset::from_vector(Rcpp::as<std::vector<int>>(p[1])),
      varset::from_vector(Rcpp::as<std::vector<int>>(p[2])),
      varset::from_vector(Rcpp::as<std::vector<int>>(p[3])),
      varset::from_vector(Rcpp::as<std::vector<int>>(p[4]))
    );
  }

  if (verbose) Rcpp::Rcout << "Initializing search" << std::endl;

  return s->initialize();
}
