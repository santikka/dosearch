#ifndef SEARCH_H
#define SEARCH_H

#include <unordered_map>
#include <vector>
#include <deque>
#include <string>
#include <queue>
#include <chrono>
#include <Rcpp.h>
#include "set.h"
#include "derivation.h"

// A distribution with the variables a on the left-hand side and b on the
// right-hand side. For DAGs, c is the subset of b that is intervened on,
// and d and e are the response indicators that are enabled (= 1) and
// disabled (= 0), respectively. For LDAGs, c and d are the variables that
// are assigned to 0 and 1, respectively, and e is always empty.
struct p {
  varset a, b, c, d, e;
  bool operator==(const p& pp) const {
    return a == pp.a && b == pp.b && c == pp.c && d == pp.d && e == pp.e;
  }
};

struct p_hash {
  std::size_t operator()(const p& pp) const {
    std::size_t h = pp.a.hash();
    hash_combine(h, pp.b.hash());
    hash_combine(h, pp.c.hash());
    hash_combine(h, pp.d.hash());
    hash_combine(h, pp.e.hash());
    return h;
  }
};

struct rindep {
  varset x, y, z, u, v;
  bool intv;
};

struct output {
  p to, from, rp;
  rindep ri;
  bool valid, enumerate;
};

struct distr {
  int rule_num, index, score, pa1, pa2;
  p pp;
};

// Variables bound by the summations of a formula whose scope has not ended,
// and the number of primes that their names had before (see search::bind)
typedef std::vector<std::pair<int, int>> bindings;

class search {
public:
  search(const int& n_, const double& tl, const bool& bm, const bool& br, const bool& dd, const bool& da, const bool& fa, const bool& im, const bool& verb);
  Rcpp::List initialize();
  Rcpp::NumericVector named_rule_times() const;
  void find();
  void set_derivation(derivation* d_);
  void get_candidate(distr& required, const int& req);
  int lookup(const p& pp) const;
  bool equal_p(const p& p1, const p& p2) const;
  void draw(const distr& dist, const bool& recursive, derivation& d);
  void draw_recursive(const distr& dist, derivation& d, std::vector<char>& drawn);
  void enumerate_distribution(const int& ruleid, const p& pp, const varset& z, bool& found, distr& iquery, distr& required, int& remaining);
  void add_to_list(const distr& dist);
  std::string label(const int& v, const std::vector<int>& scope) const;
  void init_scope(const varset& vars);
  std::vector<int> bind(const varset& s, bindings& open);
  void unbind(const varset& s, const std::vector<int>& prev);
  void close(bindings& open);
  std::string closed_formula(distr& dist, const bool& paren);
  std::string term_formula(distr& dist, bindings& open);
  virtual void add_distribution(distr& nquery) = 0;
  virtual void add_known(const varset& a, const varset& b, const varset& c, const varset& d, const varset& e) = 0;
  virtual distr& next_distribution(const int& i) = 0;
  virtual void assign_candidate(distr& required) = 0;
  virtual bool check_trivial() = 0;
  virtual bool separation_criterion() = 0;
  virtual int rule_limit(const int& ruleid) const = 0;
  virtual varset rule_mask(const int& ruleid, const p& pp) const = 0;
  virtual void set_target(const varset& a, const varset& b, const varset& c, const varset& d, const varset& e) = 0;
  virtual void set_options(const std::vector<int>& rule_vec) = 0;
  virtual void set_labels(const Rcpp::StringVector& lab) = 0;
  virtual std::string derive_formula(distr& dist, bindings& open, bool& difference) = 0;
  virtual varset case_variables(const distr& dist) const = 0;
  virtual varset assigned(const p& pp) const = 0;
  virtual void set_pending(const varset& zero, const varset& one) = 0;
  virtual std::size_t sort_length(const distr& dist) = 0;
  virtual varset free_vars(const distr& dist) = 0;
  virtual std::string to_string(const p& pp) const = 0;
  virtual bool valid_rule(const int& ruleid, const p& pp) const = 0;
  virtual void apply_rule(const int& ruleid, const p& pp, const varset& z) = 0;
  virtual void derive_distribution(const distr& iquery, const distr& required, const int& ruleid, int& remaining, bool& found) = 0;
  virtual void get_ruleinfo(const int& ruleid, const p& pp, const varset& z) = 0;
  virtual void enumerate_candidates() = 0;
  virtual ~search();
  const int n;
  const double time_limit;
  const bool benchmark;
  const bool benchmark_rules;
  const bool draw_derivation;
  const bool draw_all;
  const bool formula;
  const bool improve;
  const bool verbose;
  p target;
  varset lhs, full;
  int index;
  derivation *deriv;
  std::vector<distr> target_dist;
  std::vector<std::string> labels;
  std::vector<int> rules;
  std::vector<double> rule_times;
  bool trivial_id;
  // Distributions indexed from 1. A deque keeps references to existing
  // elements valid when new distributions are added during the search.
  std::deque<distr> L;
  std::unordered_map<p, int, p_hash> ps;
  std::unordered_map<int, std::string> rule_names;
  std::vector<int> candidates;
  output info;
  // The scopes of the variables in formulas (see search::bind), the
  // variables whose names share their primes (a true variable and its proxy
  // variable) and the lengths of the formulas that order the terms of
  // products (see sort_length)
  std::vector<int> refs, tops, links;
  std::vector<std::size_t> lengths;
  // The binary variables of case-by-case reasoning in the derivation of the
  // target, and whether the summations over them are written out as sums of
  // their terms (see search::initialize)
  varset cases;
  bool expand;
};

#endif
