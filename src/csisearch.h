#ifndef CSISEARCH_H
#define CSISEARCH_H

#include "search.h"
#include "ldag.h"
#include "derivation.h"

class csisearch : public search {
public:
  csisearch(const int& n_, const double& tl, const bool& bm, const bool& br, const bool& dd, const bool& da, const bool& fa, const bool& im, const bool& verb);
  varset con_vars, intv_vars;
  // Values assigned to the context variables in the part of a formula that
  // is being derived (see assigned_formula)
  p pending;
  ldag* g;
  virtual void add_distribution(distr& nquery);
  virtual void add_known(const varset& a, const varset& b, const varset& c, const varset& d, const varset& e);
  virtual distr& next_distribution(const int& i);
  void assign_candidate(distr& required);
  bool check_trivial();
  bool separation_criterion();
  int rule_limit(const int& ruleid) const;
  varset rule_mask(const int& ruleid, const p& pp) const;
  void set_target(const varset& a, const varset& b, const varset& c, const varset& d, const varset& e);
  void set_graph(ldag* g_);
  void set_options(const std::vector<int>& rule_vec);
  void set_labels(const Rcpp::StringVector& lab);
  void set_contexts(const varset& con);
  void set_interventions(const varset& intv);
  std::string derive_formula(distr& dist, bindings& open, bool& difference);
  std::string assigned_formula(distr& dist, const varset& zero, const varset& one, bindings& open, bool& difference);
  int assigned_case(const distr& dist, const p& values) const;
  p parent_values(const distr& dist, p values) const;
  bool single_input(const distr& dist, p values) const;
  p compact_atom(const distr& dist, varset drop, varset move, p values) const;
  std::string compact_formula(distr& dist, varset drop, varset move);
  std::string sum_formula(distr& dist, varset z, bindings& open, bool& difference);
  std::string written_sum(distr& dist, const varset& w);
  varset case_variables(const distr& dist) const;
  varset assigned(const p& pp) const;
  void set_pending(const varset& zero, const varset& one);
  bool is_difference(const distr& dist) const;
  std::size_t sort_length(const distr& dist);
  std::size_t term_length(const distr& dist);
  varset free_vars(const distr& dist);
  std::string dec_to_text(const varset& dec, const varset& zero, const varset& one, const std::vector<int>& scope) const;
  std::string to_string(const p& pp) const;
  std::string to_string(const p& pp, const std::vector<int>& scope) const;
  bool valid_rule(const int& ruleid, const p& pp) const;
  void apply_rule(const int& ruleid, const p& pp, const varset& z);
  void derive_distribution(const distr& iquery, const distr& required, const int& ruleid, int& remaining, bool& found);
  void get_ruleinfo(const int& ruleid, const p& pp, const varset& z);
  void enumerate_candidates();
  virtual ~csisearch();
};

class csisearch_heuristic: public csisearch {
public:
  struct comp_distr {
    bool operator()(distr const * d1, distr const * d2) {
      return d1->score < d2->score;
    }
  };
  csisearch_heuristic(const int& n_, const double& tl, const bool& bm, const bool& br, const bool& dd, const bool& da, const bool& fa, const bool& im, const bool& verb);
  void add_distribution(distr& nquery);
  void add_known(const varset& a, const varset& b, const varset& c, const varset& d, const varset& e);
  distr& next_distribution(const int& i);
  virtual ~csisearch_heuristic();
private:
  int compute_score(const p& pp) const;
  std::priority_queue<distr*, std::vector<distr*>, comp_distr> Q;
};

#endif
