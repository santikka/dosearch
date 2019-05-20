#include <Rcpp.h>
#include "csisearch.h"

// [[Rcpp::plugins(cpp11)]]

/* 
  Function to handle R requests

  dir_lhs           : vector of vertices with outgoing directed edges
  dir_rhs           : vector of vertices with incoming directed edges
  lab               : vector of labels for vertices
  p_list            : a list of known distributions
  q_vec             : vector of vertices representing the causal query
  n                 : number of vertices
  label_map         : a list of edge labels
  con_vars          : a set of variables that have assignments in labels (non-interventional)
  intv_vars         : a set of of interventional variables
  rules             : overrides the set of default rules
  draw_derivation   : form a string representing the derivation steps (as dot)
  draw_all          : draw every distribution that was derived (vs only those that were used to derive the effect)
  formula           : output formula as string
  improve           : remove redundant sets from do-calculus operations
  heuristic         : use search heuristic
  cache             : use caching for separation criteria
  verbose           : print diagnostics during search
*/

// [[Rcpp::export]]
Rcpp::List initialize_csisearch(
    const std::vector<int>& dir_lhs,
    const std::vector<int>& dir_rhs,
    const Rcpp::StringVector& lab,
    const Rcpp::List& p_list,
    const std::vector<int>& q_vec,
    const Rcpp::List& label_map,
    const Rcpp::List& local_csi,
    const int& con_vars,
    const int& intv_vars,
    const int& n,
    const std::vector<int>& rules,
    const bool& draw_derivation,
    const bool& draw_all,
    const bool& formula,
    const bool& heuristic,
    const bool& cache,
    const bool& verbose)
{
    ldag *g;
    if ( cache ) g = new ldag_cache(n);
    else g = new ldag(n);

    // Add directed edges
    for ( unsigned int i = 0; i < dir_rhs.size(); i++ ) {
        g->add_edge(dir_lhs[i], dir_rhs[i]);
    }
    g->set_contexts(con_vars, intv_vars);

    derivation* d;

    csisearch *s;
    if ( heuristic ) s = new csisearch_heuristic(n, draw_derivation, draw_all, formula, verbose);
    else s = new csisearch(n, draw_derivation, draw_all, formula, verbose);

    if ( draw_derivation ) s->set_derivation(d);

    s->set_labels(lab); // Can't print anything before setting labels
    s->set_graph(g); // Assing graph object
    s->set_options(rules); // Apply settings
    s->set_target(q_vec[0], q_vec[1], q_vec[2], q_vec[3]); // Set the target distribution of the search
    s->set_contexts(con_vars);
    s->set_interventions(intv_vars);

    // Add known distributions
    for ( int i = 0; i < p_list.size(); i++ ) {
        std::vector<int> p = p_list[i];
        s->add_known(p[0], p[1], p[2], p[3]);
    }

    // Map labels to edges for ldag
    for ( int i = 0; i < label_map.size(); i++ ) {
        Rcpp::List label = Rcpp::as<Rcpp::List>(label_map[i]);
        Rcpp::List contexts = Rcpp::as<Rcpp::List>(label["contexts"]);
        int context_set = Rcpp::as<int>(label["vars"]);
        g->add_context_set(context_set);
        for ( int j = 0; j < contexts.size(); j++ ) {
            Rcpp::List context = Rcpp::as<Rcpp::List>(contexts[j]);
            int zero = Rcpp::as<int>(context["zero"]);
            int one = Rcpp::as<int>(context["one"]);
            int equiv = Rcpp::as<int>(context["equivalence"]);
            std::vector<int> from = Rcpp::as<std::vector<int>>(context["from"]);
            std::vector<int> to = Rcpp::as<std::vector<int>>(context["to"]);
            g->add_context(zero, one, equiv, from, to);
        }
    }

    // Add local CSIs
    for ( int i = 0; i < local_csi.size(); i++ ) {
        Rcpp::List loc = Rcpp::as<Rcpp::List>(local_csi[i]);
        int x = Rcpp::as<int>(loc["x"]);
        int y = Rcpp::as<int>(loc["y"]);
        int z = Rcpp::as<int>(loc["z"]);
        int zero = Rcpp::as<int>(loc["zero"]);
        int one = Rcpp::as<int>(loc["one"]);
        g->add_local_csi(x, y, z, zero, one);
    }

    if ( verbose ) Rcpp::Rcout << "Initializing search" << endl;

    Rcpp::List result = s->initialize();

    delete g;
    delete d;
    delete s;

    return result;
}