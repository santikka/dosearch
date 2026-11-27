#ifndef DOSEARCH_H
#define DOSEARCH_H

#include <functional>
#include "search.h"
#include "dag.h"
#include "derivation.h"

class dosearch: public search {
public:
  dosearch(const int& n_, const double& tl, const bool& bm, const bool& br, const bool& dd, const bool& da, const bool& fa, const bool& im, const bool& verb);
  varset md_s, md_p, md_t, tr, sb, trsb;
  bool md, case_reasoning, proxy_deletion;
  // Divisions whose formula is a single distribution (see division_type):
  // 0 if not known yet, 1 for a conditional distribution, 2 for a marginal
  // distribution and 3 if the formula is not a single distribution
  mutable std::vector<char> divisions;
  // Values assigned to the response indicators in the part of a formula
  // that is being derived (see assigned_formula)
  p pending;
  // Removed proxy variables whose scope has not ended (see removed_formula)
  // and those of them that appear in the part of the formula that is being
  // derived (see input_formula)
  varset removed, shown;
  dag* g;
  virtual void add_distribution(distr& nquery);
  virtual void add_known(const varset& a, const varset& b, const varset& c, const varset& d, const varset& e);
  virtual distr& next_distribution(const int& i);
  void assign_candidate(distr& required);
  bool check_trivial();
  bool unmentioned_variable() const;
  bool anchored_outcome() const;
  bool separation_criterion();
  int rule_limit(const int& ruleid) const;
  varset rule_mask(const int& ruleid, const p& pp) const;
  void set_target(const varset& a, const varset& b, const varset& c, const varset& d, const varset& e);
  void set_graph(dag* g_);
  void set_options(const std::vector<int>& rule_vec);
  void set_labels(const Rcpp::StringVector& lab);
  void set_case_reasoning(const bool& cr);
  std::string derive_formula(distr& dist, bindings& open, bool& difference);
  std::string assigned_formula(distr& dist, const varset& d, const varset& e, bindings& open, bool& difference);
  bool is_difference(const distr& dist) const;
  std::size_t sort_length(const distr& dist);
  std::size_t term_length(const distr& dist);
  varset free_vars(const distr& dist);
  varset exchanged_proxies(const distr& dist) const;
  varset removed_proxies(const distr& dist) const;
  int assigned_case(const distr& dist, const p& values) const;
  p parent_values(const distr& dist, p values) const;
  bool single_input(const distr& dist, p values) const;
  bool compact_division(const distr& dist) const;
  bool compact_division(const distr& dist, const p& values) const;
  int division_type(const distr& dist, const p& values) const;
  void division_sets(const distr& dist, varset& drop, varset& move) const;
  void division_sets(const distr& dist, const p& values, varset& drop, varset& move) const;
  int input_of(const distr& dist, p values) const;
  p compact_atom(const distr& dist, varset drop, varset move, p values) const;
  std::string compact_formula(distr& dist, varset drop, varset move, bindings& open);
  std::string removed_formula(const varset& px, bindings& open, const std::function<std::string()>& derive);
  std::string input_formula(p pp);
  std::string sum_formula(distr& dist, varset z, bindings& open, bool& difference);
  std::string written_sum(distr& dist, const varset& w);
  varset case_variables(const distr& dist) const;
  varset assigned(const p& pp) const;
  void set_pending(const varset& zero, const varset& one);
  std::string division_part(distr& part, const distr& dist);
  std::string dec_to_text(const varset& dec, const varset& enabled, const varset& disabled, const std::vector<int>& scope) const;
  std::string to_string(const p& pp) const;
  std::string to_string(const p& pp, const std::vector<int>& scope) const;
  bool valid_rule(const int& ruleid, const p& pp) const;
  void apply_rule(const int& ruleid, const p& pp, const varset& z);
  bool removes_mechanism(const int& ruleid, const p& pp) const;
  void derive_distribution(const distr& iquery, const distr& required, const int& ruleid, int& remaining, bool& found);
  void get_ruleinfo(const int& ruleid, const p& pp, const varset& z);
  void enumerate_candidates();
  virtual ~dosearch();
};

class dosearch_heuristic: public dosearch {
public:
  struct comp_distr {
    bool operator()(distr const * d1, distr const * d2) {
      return d1->score < d2->score;
    }
  };
  dosearch_heuristic(const int& n_, const double& tl, const bool& bm, const bool& br, const bool& dd, const bool& da, const bool& fa, const bool& im, const bool& verb);
  void add_distribution(distr& nquery);
  void add_known(const varset& a, const varset& b, const varset& c, const varset& d, const varset& e);
  distr& next_distribution(const int& j);
  virtual ~dosearch_heuristic();
private:
  int compute_score(const p& pp) const;
  int compute_score_md(const p& pp) const;
  std::priority_queue<distr*, std::vector<distr*>, comp_distr> Q;
};

#endif
