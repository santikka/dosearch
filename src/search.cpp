#include "search.h"

search::search(const int& n_, const double& tl, const bool& bm, const bool& br, const bool& dd, const bool& da, const bool& fa, const bool& im, const bool& verb):
  n(n_), time_limit(tl), benchmark(bm), benchmark_rules(br), draw_derivation(dd), draw_all(da), formula(fa), improve(im), verbose(verb) {
  expand = false;
  index = 0;
  deriv = nullptr;
  trivial_id = false;
  full = varset::full(n);
  links = std::vector<int>(n + 1, 0);
  // Distributions are indexed from 1, index 0 is a placeholder
  distr placeholder;
  placeholder.rule_num = 0;
  placeholder.index = 0;
  placeholder.score = 0;
  placeholder.pa1 = 0;
  placeholder.pa2 = 0;
  L.push_back(placeholder);
}

search::~search() {
}

Rcpp::List search::initialize() {
  info.ri.intv = false;
  info.valid = false;
  info.enumerate = false;
  std::string formula_str = "";
  std::string derivation = "";
  Rcpp::CharacterVector variants(0);
  // The criteria for non-identifiability are only evaluated when the search
  // enhancements are enabled
  if (improve && check_trivial()) {
    return Rcpp::List::create(
      Rcpp::Named("identifiable") = false,
      Rcpp::Named("formula") = formula_str,
      Rcpp::Named("variants") = variants,
      Rcpp::Named("derivation") = derivation,
      Rcpp::Named("time") = 0,
      Rcpp::Named("rule_times") = named_rule_times()
    );
  }
  std::chrono::duration<double, std::milli> total_time;
  auto t1 = std::chrono::steady_clock::now();
  auto t2 = std::chrono::steady_clock::now();
  if (!trivial_id) {
    t1 = std::chrono::steady_clock::now();
    find();
    t2 = std::chrono::steady_clock::now();
  }
  total_time = t2 - t1;
  bool identifiable = target_dist.size() > 0;
  if (identifiable && formula) {
    // The variables of the target and the variables that are free in the
    // formula are in scope in the whole formula
    lengths = std::vector<std::size_t>(L.size(), 0);
    cases = case_variables(target_dist[0]);
    varset fv = target.a | target.b | free_vars(target_dist[0]);
    init_scope(fv);
    formula_str = closed_formula(target_dist[0], false);
    // If case-by-case reasoning is used, the variants of the formula are
    // given for each combination of values of the binary variables of
    // case-by-case reasoning that are free in the formula. The values are
    // assigned in the formula, which reduces case-by-case reasoning over the
    // variables to the cases of the values, and the summations over the other
    // variables of case-by-case reasoning are written out as sums of the
    // terms for both values.
    if (!cases.empty()) {
      std::vector<int> free = ((fv & cases) - assigned(target)).elements();
      int m = free.size();
      if (m > 10) {
        Rcpp::warning("The variants of the formula are not given for more than 10 variables of case-by-case reasoning.");
      } else {
        uint64_t total = static_cast<uint64_t>(1) << m;
        std::vector<std::string> forms;
        expand = true;
        for (uint64_t i = 0; i < total; i++) {
          varset zero, one;
          // The values of the variables in the order of the variables, so
          // the value of the last variable changes first
          for (int k = 0; k < m; k++) {
            if ((i >> (m - 1 - k)) & 1) one.insert(free[k]);
            else zero.insert(free[k]);
          }
          init_scope(fv);
          set_pending(zero, one);
          forms.push_back(closed_formula(target_dist[0], false));
        }
        set_pending(varset(), varset());
        expand = false;
        // A variable that is in scope only where it is assigned a value does
        // not change the formula, so it does not distinguish the variants
        uint64_t relevant = 0;
        for (int k = 0; k < m; k++) {
          uint64_t bit = static_cast<uint64_t>(1) << (m - 1 - k);
          for (uint64_t i = 0; i < total; i++) {
            if (!(i & bit) && forms[i] != forms[i | bit]) {
              relevant |= bit;
              break;
            }
          }
        }
        std::vector<int> none;
        Rcpp::CharacterVector names;
        for (uint64_t i = 0; i < total; i++) {
          if (i & ~relevant) continue;
          std::string name = "";
          for (int k = 0; k < m; k++) {
            if (!((relevant >> (m - 1 - k)) & 1)) continue;
            if (!name.empty()) name += ", ";
            name += label(free[k], none) + (((i >> (m - 1 - k)) & 1) ? " = 1" : " = 0");
          }
          variants.push_back(forms[i]);
          names.push_back(name);
        }
        variants.names() = names;
      }
    }
  }
  if (draw_derivation) {
    deriv->init();
    if (draw_all) {
      for (unsigned int i = 1; i < L.size(); i++) {
        draw(L[i], FALSE, *deriv);
      }
      if (identifiable) {
        draw(target_dist[0], FALSE, *deriv);
      }
    } else {
      if (identifiable) {
        draw(target_dist[0], TRUE, *deriv);
      }
    }
    deriv->finish();
    derivation = deriv->get();
  }
  return Rcpp::List::create(
    Rcpp::Named("identifiable") = identifiable,
    Rcpp::Named("formula") = formula_str,
    Rcpp::Named("variants") = variants,
    Rcpp::Named("derivation") = derivation,
    Rcpp::Named("time") = total_time.count(),
    Rcpp::Named("rule_times") = named_rule_times()
  );
}

Rcpp::NumericVector search::named_rule_times() const {
  Rcpp::NumericVector times(rule_times.begin(), rule_times.end());
  Rcpp::CharacterVector ids(rules.size());
  for (unsigned int r = 0; r < rules.size(); r++) {
    ids[r] = std::to_string(rules[r]);
  }
  times.names() = ids;
  return times;
}

// Expand distributions until the target is found or no new distributions
// can be derived. For each rule, the sets z are enumerated in the order of
// their cardinality (and lexicographically within the same cardinality) but
// only among the subsets of the variables that the rule can apply to.
void search::find() {
  distr required;
  bool found = false;
  unsigned int i = 1;
  int remaining = L.size() - 1;
  bool enable_limit = time_limit > 0;
  auto start = std::chrono::steady_clock::now();
  std::chrono::steady_clock::time_point t1, t2;
  std::chrono::duration<double, std::milli> ms;
  std::chrono::duration<double, std::ratio<3600>> total;
  unsigned long expanded = 0;
  std::vector<int> elems, idx;
  varset z;
  while (remaining > 0 && !found) {
    if (enable_limit) {
      total = std::chrono::steady_clock::now() - start;
      if (total.count() > time_limit) return;
    }
    if (++expanded % 128 == 0) Rcpp::checkUserInterrupt();
    distr& iquery = next_distribution(i);
    remaining--;
    const p& pp = iquery.pp;
    for (unsigned int r = 0; r < rules.size(); r++) {
      if (benchmark_rules) t1 = std::chrono::steady_clock::now();
      int ruleid = rules[r];
      if (!valid_rule(ruleid, pp)) continue;
      rule_mask(ruleid, pp).elements(elems);
      int m = elems.size();
      int k_max = rule_limit(ruleid);
      if (k_max > m) k_max = m;
      for (int k = 1; k <= k_max && !found; k++) {
        idx.resize(k);
        for (int t = 0; t < k; t++) idx[t] = t;
        while (true) {
          z = varset();
          for (int t = 0; t < k; t++) z.insert(elems[idx[t]]);
          enumerate_distribution(ruleid, pp, z, found, iquery, required, remaining);
          if (found) break;
          // next combination of size k in lexicographic order
          int t = k - 1;
          while (t >= 0 && idx[t] == m - k + t) t--;
          if (t < 0) break;
          idx[t]++;
          for (int u = t + 1; u < k; u++) idx[u] = idx[u - 1] + 1;
        }
      }
      if (benchmark_rules) {
        t2 = std::chrono::steady_clock::now();
        ms = t2 - t1;
        rule_times[r] += ms.count();
      }
      if (found) return;
    }
    i++;
  }
}

void search::enumerate_distribution(const int& ruleid, const p& pp, const varset& z, bool& found, distr& iquery, distr& required, int& remaining) {
  apply_rule(ruleid, pp, z);
  if (!info.valid) return;
  if (info.enumerate) {
    candidates.clear();
    enumerate_candidates();
    while (!candidates.empty() && !found) {
      int req = candidates.back();
      candidates.pop_back();
      get_candidate(required, req);
      assign_candidate(required);
      if (lookup(info.to) == 0) derive_distribution(iquery, required, ruleid, remaining, found);
    }
  } else {
    if (lookup(info.to) > 0) return;
    if (!info.ri.x.empty() && !separation_criterion()) return;
    if (!info.rp.a.empty()) {
      int req = lookup(info.rp);
      if (req > 0) {
        get_candidate(required, req);
        derive_distribution(iquery, required, ruleid, remaining, found);
      }
    } else derive_distribution(iquery, required, ruleid, remaining, found);
  }
}

void search::add_to_list(const distr& dist) {
  if (dist.index >= static_cast<int>(L.size())) L.resize(dist.index + 1);
  L[dist.index] = dist;
}

void search::set_derivation(derivation* d_) {
  deriv = d_;
}

// In formulas, the scope records the number of primes of each variable,
// or -1 if the variable is not in scope. Variables are distinguished from
// other variables of the same name by primes, or by a superscript when more
// than three primes would be needed. A proxy variable x* is given as
// x^{\ast} in formulas, where its primes are a part of the superscript.
// Without a scope, labels are given as such, e.g., in derivations.
std::string search::label(const int& v, const std::vector<int>& scope) const {
  int k = 0;
  if (v < static_cast<int>(scope.size())) k = scope[v];
  const std::string& name = labels[v-1];
  if (!scope.empty() && name.size() > 1 && name.back() == '*') {
    std::string sup = "\\ast";
    if (k > 3) sup += "(" + std::to_string(k) + ")";
    for (int i = 0; i < k && k <= 3; i++) sup += "\\prime";
    return name.substr(0, name.size() - 1) + "^{" + sup + "}";
  }
  if (k <= 0) return name;
  if (k <= 3) return name + std::string(k, '\'');
  return name + "^{(" + std::to_string(k) + ")}";
}

// The scope of a formula, where the variables vars are in scope
void search::init_scope(const varset& vars) {
  refs = std::vector<int>(n + 1, -1);
  tops = std::vector<int>(n + 1, -1);
  for (int i = vars.first(); i > 0; i = vars.next(i)) {
    refs[i] = 0;
    tops[i] = 0;
  }
}

// A summation applies to every term that follows it in the same product,
// because formulas do not enclose products in parentheses. The value of the
// formula does not depend on this, because the terms that follow the
// summation do not depend on the variables that it binds. The names of the
// variables are still kept unambiguous: refs records the number of primes of
// each variable that is in scope, which is used for the variables in the
// terms, and tops records the largest number of primes of each variable
// whose summation applies at the current position. A bound variable is
// primed once more than any variable of the same name that is in scope or
// whose summation applies, and once more than any such variable that shares
// its primes, i.e., a true variable and its proxy variable. The variables of
// a summation remain in scope until the end of the summation (see unbind),
// and the summation applies until the end of the product (see close).
// Returns the previous values of refs.
std::vector<int> search::bind(const varset& s, bindings& open) {
  std::vector<int> prev;
  for (int i = s.first(); i > 0; i = s.next(i)) {
    int k = tops[i];
    int j = links[i];
    if (j > 0 && tops[j] > k) k = tops[j];
    prev.push_back(refs[i]);
    open.push_back(std::make_pair(i, tops[i]));
    refs[i] = k + 1;
    tops[i] = k + 1;
  }
  return prev;
}

// End the scope of the variables of s bound by bind
void search::unbind(const varset& s, const std::vector<int>& prev) {
  int j = 0;
  for (int i = s.first(); i > 0; i = s.next(i)) refs[i] = prev[j++];
}

// End the summations of a product
void search::close(bindings& open) {
  for (int k = open.size() - 1; k >= 0; k--) {
    tops[open[k].first] = open[k].second;
  }
  open.clear();
}

// The formula of a distribution as an expression of its own, i.e., the
// whole formula, a numerator, a denominator, the expression in brackets or
// a term of a difference or a sum. A difference or a sum is enclosed in
// parentheses if paren is true, i.e., when it is subtracted or when it is a
// case of case-by-case reasoning.
std::string search::closed_formula(distr& dist, const bool& paren) {
  bindings open;
  bool difference = false;
  std::string f = derive_formula(dist, open, difference);
  close(open);
  if (paren && difference) return "\\left(" + f + "\\right)";
  return f;
}

// The formula of a distribution as a term of a product or of a summation.
// The summations of the term apply to the terms that follow it, and they
// are recorded in open. A difference or a sum is enclosed in parentheses.
std::string search::term_formula(distr& dist, bindings& open) {
  bool difference = false;
  std::string f = derive_formula(dist, open, difference);
  if (difference) return "\\left(" + f + "\\right)";
  return f;
}

void search::get_candidate(distr& required, const int& req) {
  const distr& reqd = L[req];
  required.pp = reqd.pp;
  required.pa1 = reqd.pa1;
  required.pa2 = reqd.pa2;
  required.index = req;
  required.rule_num = reqd.rule_num;
  required.score = reqd.score;
}

int search::lookup(const p& pp) const {
  auto it = ps.find(pp);
  if (it == ps.end()) return 0;
  return it->second;
}

bool search::equal_p(const p& pp1, const p& pp2) const {
  return pp1 == pp2;
}

void search::draw(const distr& dist, const bool& recursive, derivation& d) {
  if (recursive) {
    std::vector<char> drawn(L.size(), 0);
    draw_recursive(dist, d, drawn);
    return;
  }
  if (dist.pa1 > 0) {
    const distr& pa1 = L[dist.pa1];
    d.add_edge(to_string(pa1.pp), to_string(dist.pp), rule_names[dist.rule_num]);
    if (dist.pa2 > 0) {
      const distr& pa2 = L[dist.pa2];
      d.add_edge(to_string(pa2.pp), to_string(dist.pp), rule_names[dist.rule_num]);
    }
  }
}

// Draws each distribution of the derivation once even if it is used
// in several steps of the derivation
void search::draw_recursive(const distr& dist, derivation& d, std::vector<char>& drawn) {
  if (dist.index > 0 && dist.index < static_cast<int>(drawn.size())) {
    if (drawn[dist.index]) return;
    drawn[dist.index] = 1;
  }
  if (dist.pa1 > 0) {
    const distr& pa1 = L[dist.pa1];
    d.add_edge(to_string(pa1.pp), to_string(dist.pp), rule_names[dist.rule_num]);
    draw_recursive(pa1, d, drawn);
    if (dist.pa2 > 0) {
      const distr& pa2 = L[dist.pa2];
      d.add_edge(to_string(pa2.pp), to_string(dist.pp), rule_names[dist.rule_num]);
      draw_recursive(pa2, d, drawn);
    }
  }
}
