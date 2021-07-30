// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// initialize_csisearch
Rcpp::List initialize_csisearch(const std::vector<int>& dir_lhs, const std::vector<int>& dir_rhs, const Rcpp::StringVector& lab, const Rcpp::List& p_list, const std::vector<int>& q_vec, const Rcpp::List& label_map, const Rcpp::List& local_csi, const int& con_vars, const int& intv_vars, const int& n, const double& time_limit, const std::vector<int>& rules, const bool& benchmark, const bool& draw_derivation, const bool& draw_all, const bool& formula, const bool& heuristic, const bool& cache, const bool& verbose);
RcppExport SEXP _dosearch_initialize_csisearch(SEXP dir_lhsSEXP, SEXP dir_rhsSEXP, SEXP labSEXP, SEXP p_listSEXP, SEXP q_vecSEXP, SEXP label_mapSEXP, SEXP local_csiSEXP, SEXP con_varsSEXP, SEXP intv_varsSEXP, SEXP nSEXP, SEXP time_limitSEXP, SEXP rulesSEXP, SEXP benchmarkSEXP, SEXP draw_derivationSEXP, SEXP draw_allSEXP, SEXP formulaSEXP, SEXP heuristicSEXP, SEXP cacheSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<int>& >::type dir_lhs(dir_lhsSEXP);
    Rcpp::traits::input_parameter< const std::vector<int>& >::type dir_rhs(dir_rhsSEXP);
    Rcpp::traits::input_parameter< const Rcpp::StringVector& >::type lab(labSEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type p_list(p_listSEXP);
    Rcpp::traits::input_parameter< const std::vector<int>& >::type q_vec(q_vecSEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type label_map(label_mapSEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type local_csi(local_csiSEXP);
    Rcpp::traits::input_parameter< const int& >::type con_vars(con_varsSEXP);
    Rcpp::traits::input_parameter< const int& >::type intv_vars(intv_varsSEXP);
    Rcpp::traits::input_parameter< const int& >::type n(nSEXP);
    Rcpp::traits::input_parameter< const double& >::type time_limit(time_limitSEXP);
    Rcpp::traits::input_parameter< const std::vector<int>& >::type rules(rulesSEXP);
    Rcpp::traits::input_parameter< const bool& >::type benchmark(benchmarkSEXP);
    Rcpp::traits::input_parameter< const bool& >::type draw_derivation(draw_derivationSEXP);
    Rcpp::traits::input_parameter< const bool& >::type draw_all(draw_allSEXP);
    Rcpp::traits::input_parameter< const bool& >::type formula(formulaSEXP);
    Rcpp::traits::input_parameter< const bool& >::type heuristic(heuristicSEXP);
    Rcpp::traits::input_parameter< const bool& >::type cache(cacheSEXP);
    Rcpp::traits::input_parameter< const bool& >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(initialize_csisearch(dir_lhs, dir_rhs, lab, p_list, q_vec, label_map, local_csi, con_vars, intv_vars, n, time_limit, rules, benchmark, draw_derivation, draw_all, formula, heuristic, cache, verbose));
    return rcpp_result_gen;
END_RCPP
}
// initialize_dosearch
Rcpp::List initialize_dosearch(const std::vector<int>& dir_lhs, const std::vector<int>& dir_rhs, const std::vector<int>& bi_lhs, const std::vector<int>& bi_rhs, const Rcpp::StringVector& lab, const Rcpp::List& p_list, const std::vector<int>& q_vec, const int& n, const int& tr, const int& sb, const int& md_s, const int& md_p, const double& time_limit, const std::vector<int>& rules, const bool& benchmark, const bool& draw_derivation, const bool& draw_all, const bool& formula, const bool& heuristic, const char& md_sym, const bool& verbose);
RcppExport SEXP _dosearch_initialize_dosearch(SEXP dir_lhsSEXP, SEXP dir_rhsSEXP, SEXP bi_lhsSEXP, SEXP bi_rhsSEXP, SEXP labSEXP, SEXP p_listSEXP, SEXP q_vecSEXP, SEXP nSEXP, SEXP trSEXP, SEXP sbSEXP, SEXP md_sSEXP, SEXP md_pSEXP, SEXP time_limitSEXP, SEXP rulesSEXP, SEXP benchmarkSEXP, SEXP draw_derivationSEXP, SEXP draw_allSEXP, SEXP formulaSEXP, SEXP heuristicSEXP, SEXP md_symSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<int>& >::type dir_lhs(dir_lhsSEXP);
    Rcpp::traits::input_parameter< const std::vector<int>& >::type dir_rhs(dir_rhsSEXP);
    Rcpp::traits::input_parameter< const std::vector<int>& >::type bi_lhs(bi_lhsSEXP);
    Rcpp::traits::input_parameter< const std::vector<int>& >::type bi_rhs(bi_rhsSEXP);
    Rcpp::traits::input_parameter< const Rcpp::StringVector& >::type lab(labSEXP);
    Rcpp::traits::input_parameter< const Rcpp::List& >::type p_list(p_listSEXP);
    Rcpp::traits::input_parameter< const std::vector<int>& >::type q_vec(q_vecSEXP);
    Rcpp::traits::input_parameter< const int& >::type n(nSEXP);
    Rcpp::traits::input_parameter< const int& >::type tr(trSEXP);
    Rcpp::traits::input_parameter< const int& >::type sb(sbSEXP);
    Rcpp::traits::input_parameter< const int& >::type md_s(md_sSEXP);
    Rcpp::traits::input_parameter< const int& >::type md_p(md_pSEXP);
    Rcpp::traits::input_parameter< const double& >::type time_limit(time_limitSEXP);
    Rcpp::traits::input_parameter< const std::vector<int>& >::type rules(rulesSEXP);
    Rcpp::traits::input_parameter< const bool& >::type benchmark(benchmarkSEXP);
    Rcpp::traits::input_parameter< const bool& >::type draw_derivation(draw_derivationSEXP);
    Rcpp::traits::input_parameter< const bool& >::type draw_all(draw_allSEXP);
    Rcpp::traits::input_parameter< const bool& >::type formula(formulaSEXP);
    Rcpp::traits::input_parameter< const bool& >::type heuristic(heuristicSEXP);
    Rcpp::traits::input_parameter< const char& >::type md_sym(md_symSEXP);
    Rcpp::traits::input_parameter< const bool& >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(initialize_dosearch(dir_lhs, dir_rhs, bi_lhs, bi_rhs, lab, p_list, q_vec, n, tr, sb, md_s, md_p, time_limit, rules, benchmark, draw_derivation, draw_all, formula, heuristic, md_sym, verbose));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_dosearch_initialize_csisearch", (DL_FUNC) &_dosearch_initialize_csisearch, 19},
    {"_dosearch_initialize_dosearch", (DL_FUNC) &_dosearch_initialize_dosearch, 21},
    {NULL, NULL, 0}
};

RcppExport void R_init_dosearch(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
