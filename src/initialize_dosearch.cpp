#include <Rcpp.h>
#include "dosearch.h"

// [[Rcpp::plugins(cpp11)]]

/* 
  Function to handle R requests

  dir_lhs           : vector of vertices with outgoing directed edges
  dir_rhs           : vector of vertices with incoming directed edges
  bi_lhs            : vector of vertices with incoming bidirected edges
  bi_rhs            : vector of vertices with incoming bidirected edges
  lab               : vector of labels for vertices
  p_list            : a list of known distributions
  q_vec             : vector of vertices representing the causal query
  n                 : number of vertices
  tr                : a set representing transportability nodes
  sb                : a set representing selection bias nodes
  md_s              : a set representing missing data switches
  md_p              : a set representing missing data proxies
  md_map            : a matrix connecting proxies, switches and true variables
  time_limit        : time limit for the search (in hours)
  rules             : overrides the set of default rules
  benchmark         : record the search time
  benchmark_rules   : record time taken by each individual rule
  draw_derivation   : form a string representing the derivation steps (as dot)
  draw_all          : draw every distribution that was derived (vs only those that were used to derive the effect)
  formula           : output formula as string
  improve           : enable search enhancements
  heuristic         : use search heuristic
  md_sym            : symbol used to represent active missing data mechanisms
  verbose           : print diagnostics during search
*/

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
    const int& md_t,
    const int& md_s,
    const int& md_p,
    const Rcpp::NumericMatrix& md_map,
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

    // Add directed edges
    for ( unsigned i = 0; i < dir_rhs.size(); i++ ) {
        g->add_edge(dir_lhs[i], dir_rhs[i]);
    }
    // Add bidirected edges
    for ( unsigned i = 0; i < bi_rhs.size(); i++ ) {
        g->add_conf(bi_lhs[i], bi_rhs[i]);
    }

    derivation* d = new derivation();

    dosearch *s;
    if ( heuristic ) s = new dosearch_heuristic(n, time_limit, benchmark, benchmark_rules, draw_derivation, draw_all, formula, improve, verbose);
    else s = new dosearch(n, time_limit, benchmark, benchmark_rules, draw_derivation, draw_all, formula, improve, verbose);

    if ( draw_derivation ) s->set_derivation(d);

    s->set_labels(lab); // Can't print anything before setting labels
    s->set_sb(sb); // Assign selection bias nodes
    s->set_tr(tr); // Assign transportability nodes
    s->set_md(md_t, md_s, md_p); // Set missing data nodes
    s->set_graph(g); // Assing graph object
    s->set_options(rules); // Set global parameters & compute necessary sets
    s->set_target(q_vec[0], q_vec[1], q_vec[2], q_vec[3]); // Set the target distribution of the search
    s->set_md_symbol(md_sym);

    if ( md_s > 0 ) {
        for ( int i = 0; i < md_map.nrow(); i++ ) {
            s->add_md_map_row(md_map(i, 0), md_map(i, 1), md_map(i, 2));
        }
    }

    // Add known distributions
    for ( int i = 0; i < p_list.size(); i++ ) {
        std::vector<int> p = p_list[i];
        s->add_known(p[0], p[1], p[2], p[3]);
    }

    Rcpp::List result = s->initialize();

    delete g;
    delete d;
    delete s;

    return result;
}