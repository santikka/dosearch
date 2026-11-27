#include "csisearch.h"

csisearch::csisearch(const int& n_, const double& tl, const bool& bm, const bool& br, const bool& dd, const bool& da, const bool& fa, const bool& im, const bool& verb):search(n_, tl, bm, br, dd, da, fa, im, verb) {
}

csisearch::~csisearch() {
}

void csisearch::set_target(const varset& a, const varset& b, const varset& c, const varset& d, const varset& e) {
  target.a = a; target.b = b; target.c = c; target.d = d; target.e = e;
  if (verbose) Rcpp::Rcout << "Setting target: " << to_string(target) << std::endl;
}

void csisearch::set_graph(ldag* g_) {
  g = g_;
}

void csisearch::set_options(const std::vector<int>& rule_vec) {
  trivial_id = false;
  index = 0;
  lhs = varset();
  if (rule_vec.size() > 0) rules = rule_vec;
  else rules = {0, 1, -3, 3, -4, 4, -5, 5, 6, -7, 7, -8, 8, -2, 2};
  rule_times = std::vector<double>(rules.size(), 0.0);
  rule_names[0] = "M";
  rule_names[1] = "C";
  rule_names[2] = "P";
  rule_names[-2] = "P";
  rule_names[3] = "I+";
  rule_names[-3] = "I-";
  rule_names[4] = "I+0";
  rule_names[-4] = "I+1";
  rule_names[5] = "CbC";
  rule_names[-5] = "CbC";
  rule_names[6] = "GbC";
  rule_names[-6] = "GbC";
  rule_names[7] = "GbC";
  rule_names[-7] = "GbC";
  rule_names[8] = "CbG";
  rule_names[-8] = "CbG";
}

void csisearch::set_labels(const Rcpp::StringVector& lab) {
  labels = std::vector<std::string>(n);
  for (int i = 0; i < n; i++) {
    labels[i] = lab(i);
  }
}

void csisearch::set_contexts(const varset& con) {
  con_vars = con;
}

void csisearch::set_interventions(const varset& intv) {
  intv_vars = intv;
}

void csisearch::add_known(const varset& a, const varset& b, const varset& c, const varset& d, const varset& e) {
  index++;
  distr iquery;
  iquery.rule_num = 0;
  iquery.pp.a = a; iquery.pp.b = b; iquery.pp.c = c; iquery.pp.d = d; iquery.pp.e = e;
  iquery.pa1 = 0;
  iquery.pa2 = 0;
  iquery.index = index;
  iquery.score = 0;
  add_to_list(iquery);
  ps[iquery.pp] = index;
  if (equal_p(iquery.pp, target)) {
    trivial_id = true;
    target_dist.push_back(iquery);
  }
  lhs = lhs | a;
  if (verbose) Rcpp::Rcout << "Adding known distribution: " << to_string(iquery.pp) << std::endl;
}

bool csisearch::check_trivial() {
  varset missing = target.a - lhs;
  if (missing.empty()) return false;
  if (verbose) Rcpp::Rcout << "Target is not identifiable: " << labels[missing.first() - 1] << " does not appear on the left-hand side of any input distribution" << std::endl;
  return true;
}

void csisearch::assign_candidate(distr& required) {
  info.to.c = required.pp.c;
  info.to.d = required.pp.d;
}

distr& csisearch::next_distribution(const int& i) {
  return L[i];
}

void csisearch::derive_distribution(const distr& iquery, const distr& required, const int& ruleid, int& remaining, bool& found) {
  distr nquery;
  nquery.index = 0;
  nquery.score = 0;
  nquery.pp = info.to;
  nquery.pa1 = iquery.index;
  nquery.pa2 = 0;
  nquery.rule_num = ruleid;
  if (!info.rp.a.empty()) nquery.pa2 = required.index;
  if (equal_p(info.to, target)) {
    if (verbose) {
      if (!info.rp.a.empty()) Rcpp::Rcout << "Derived: " << to_string(info.to) << " from " << to_string(info.from) << " and " << to_string(info.rp) << " using rule: " << std::to_string(ruleid) << std::endl;
      else Rcpp::Rcout << "Derived: " << to_string(info.to) << " from " << to_string(info.from) << " using rule: " << std::to_string(ruleid) << std::endl;
      Rcpp::Rcout << "Target found" << std::endl;
      Rcpp::Rcout << "Index = " << index << std::endl;
    }
    target_dist.push_back(nquery);
    found = true;
  } else {
    if (verbose) {
      if (!info.rp.a.empty()) Rcpp::Rcout << "Derived: " << to_string(info.to) << " from " << to_string(info.from) << " and " << to_string(info.rp) << " using rule: " << std::to_string(ruleid) << std::endl;
      else Rcpp::Rcout << "Derived: " << to_string(info.to) << " from " << to_string(info.from) << " using rule: " << std::to_string(ruleid) << std::endl;
    }
    index++;
    remaining++;
    nquery.index = index;
    add_distribution(nquery);
  }
}

void csisearch::add_distribution(distr& nquery) {
  add_to_list(nquery);
  ps[nquery.pp] = index;
}

// Candidates for the required distribution differ only by the values
// assigned to the context variables of the new variables. The candidates
// are enumerated in the order of the bit representations of the variables
// assigned to zero (i) and to one (j).
void csisearch::enumerate_candidates() {
  varset acon = info.rp.a & con_vars;
  if (acon.empty()) {
    int exist = lookup(info.rp);
    if (exist > 0) candidates.push_back(exist);
    return;
  }
  std::vector<int> elems = acon.elements();
  int e = elems.size();
  if (e > 62) Rcpp::stop("Too many context variables to enumerate.");
  p rq = info.rp;
  uint64_t f = (static_cast<uint64_t>(1) << e) - 1;
  for (uint64_t i = 0; i <= f; i++) { // loop assignments to zero
    uint64_t comp = f & ~i;
    uint64_t j = 0;
    while (true) { // loop assignments to one that do not intersect i
      rq.c = info.rp.c;
      rq.d = info.rp.d;
      for (int k = 0; k < e; k++) {
        if ((i >> k) & 1) rq.c.insert(elems[k]);
        else if ((j >> k) & 1) rq.d.insert(elems[k]);
      }
      int exist = lookup(rq);
      if (exist > 0) candidates.push_back(exist);
      if (j == comp) break;
      j = (j - comp) & comp;
    }
  }
}

bool csisearch::separation_criterion() {
  varset u = info.ri.u & con_vars;
  varset v = info.ri.v & con_vars;
  varset iv = info.ri.v & intv_vars;
  return g->csi_criterion(info.ri.x, info.ri.y, info.ri.z, u, v, iv, u | v);
}

// Rules other than marginalization and conditioning are only applied to
// single variables at a time
int csisearch::rule_limit(const int& ruleid) const {
  if (ruleid * ruleid > 3) return 1;
  else return n;
}

// Variables that the set z of each rule can consist of
varset csisearch::rule_mask(const int& ruleid, const p& pp) const {
  const varset& a = pp.a;
  const varset& b = pp.b;
  const varset& c = pp.c;
  const varset& d = pp.d;
  switch (ruleid) {
    case 0 : return a - (c | d);
    case 1 : return a;
    case 2 : return b & lhs;
    case -2 : return lhs - (a | b);
    case 3 : return full - (a | b | intv_vars);
    case -3 : return b - a;
    case 4 : return con_vars - (a | b);
    case -4 : return (con_vars | intv_vars) - (a | b);
    case 5 : return a & c & lhs;
    case -5 : return a & d & lhs;
    case 6 : return a & (c | d) & lhs;
    case 7 :
    case -7 : return (con_vars & lhs) - (a | b);
    case 8 :
    case -8 : return ((a | b) & con_vars) - (c | d);
    default : return varset();
  }
}

// The formula of a distribution, which shows how the distribution is derived
// from the input distributions. Summations apply to every term that follows
// them in the same product (see search::bind), and the variables that they
// bind are recorded in open until the end of the product. difference is set
// to true if the formula is a difference, which is enclosed in parentheses
// only when it is a term of a product or of a summation, when it is
// subtracted or when it is a case of case-by-case reasoning (see
// search::closed_formula and search::term_formula).
std::string csisearch::derive_formula(distr& dist, bindings& open, bool& difference) {
  std::string formula = "";
  varset empty;
  if (dist.pa1 > 0) {
    int r = dist.rule_num;
    int rsq = r * r;
    distr& pa1 = L[dist.pa1];
    if (dist.pa2 > 0) {
      distr& pa2 = L[dist.pa2];
      if (rsq == 4) {
        // The shorter term is given first
        bool first = term_length(pa1) < term_length(pa2);
        formula = term_formula(first ? pa1 : pa2, open);
        formula += term_formula(first ? pa2 : pa1, open);
      } else if (rsq == 25) {
        // A case that is a difference is enclosed in parentheses. If a value
        // is assigned to the variable of case-by-case reasoning, only the case
        // of that value is given.
        distr& zero = (r > 0) ? pa1 : pa2;
        distr& one = (r > 0) ? pa2 : pa1;
        varset z = zero.pp.c - dist.pp.c;
        if (pending.c.intersects(z)) formula = derive_formula(zero, open, difference);
        else if (pending.d.intersects(z)) formula = derive_formula(one, open, difference);
        else formula = "\\left[" + closed_formula(pa1, true) + " \\vee " + closed_formula(pa2, true) + "\\right]";
      } else if (rsq == 36) {
        formula = closed_formula(pa2, false) + " - " + closed_formula(pa1, true);
        difference = true;
      } else if (rsq == 49) {
        formula = closed_formula(pa1, false) + " - " + closed_formula(pa2, true);
        difference = true;
      }
    } else {
      if (rsq == 9 || rsq == 16) {
        formula = derive_formula(pa1, open, difference);
      } else if (rsq == 0 && single_input(pa1, parent_values(dist, pending))) {
        varset z = pa1.pp.a - dist.pp.a;
        p prev_pending = pending;
        pending.c -= z;
        pending.d -= z;
        formula = compact_formula(pa1, z, empty);
        pending = prev_pending;
      } else if (rsq == 1 && single_input(pa1, parent_values(dist, pending))) {
        formula = compact_formula(pa1, empty, pa1.pp.a - dist.pp.a);
      } else if (rsq == 0) {
        formula = sum_formula(pa1, pa1.pp.a - dist.pp.a, open, difference);
      } else if (rsq == 1) {
        std::string num = closed_formula(pa1, false);
        bindings den_open;
        bool sum = false;
        std::string den = sum_formula(pa1, dist.pp.a, den_open, sum);
        close(den_open);
        formula = "\\frac{" + num + "}{" + den + "}";
      } else if (rsq == 64) {
        formula = assigned_formula(pa1, dist.pp.c - pa1.pp.c, dist.pp.d - pa1.pp.d, open, difference);
      } else if (rsq == 36) {
        // General-by-case reasoning (rule 6) when the variable is the
        // only variable on the left-hand side, so the general
        // distribution is not required
        formula = "1 - " + closed_formula(pa1, true);
        difference = true;
      }
    }
  } else {
    // An input distribution, evaluated at the values that are assigned to
    // its variables in the distributions derived from it
    p pp = dist.pp;
    varset unassigned = (pp.a | pp.b) - (pp.c | pp.d);
    pp.c |= pending.c & unassigned;
    pp.d |= pending.d & unassigned;
    formula = to_string(pp, refs);
  }
  return formula;
}

// Is the distribution derived from an input distribution by rules that have
// a single parent distribution, i.e., the insertion and deletion of
// observations, marginalization, conditioning and case-by-general reasoning?
// The insertion and deletion of observations equate distributions without
// changing their formulas, so the formula of such a distribution is a
// marginal or conditional distribution of the input distribution (see
// compact_atom), and so are the formulas of its marginal and conditional
// distributions. They are given as such in formulas instead of explicit
// summations and quotients (see compact_formula). The formula of the
// distribution is derived at the values assigned to the variables in values,
// so case-by-case reasoning over a variable that is assigned a value reduces
// to the case of that value (see assigned_case).
bool csisearch::single_input(const distr& dist, p values) const {
  const distr* d = &dist;
  while (d->pa1 > 0) {
    if (d->pa2 > 0) {
      int c = assigned_case(*d, values);
      if (c == 0) return false;
      d = &L[c];
      continue;
    }
    int rsq = d->rule_num * d->rule_num;
    if (rsq != 0 && rsq != 1 && rsq != 9 && rsq != 16 && rsq != 64) return false;
    values = parent_values(*d, values);
    d = &L[d->pa1];
  }
  return true;
}

// The parent of a distribution derived by case-by-case reasoning (rules 5
// and -5) for the value that is assigned to its variable in values, or
// 0 if no value is assigned to it (see derive_formula)
int csisearch::assigned_case(const distr& dist, const p& values) const {
  if (dist.pa2 == 0 || dist.rule_num * dist.rule_num != 25) return 0;
  int zero = (dist.rule_num > 0) ? dist.pa1 : dist.pa2;
  int one = (dist.rule_num > 0) ? dist.pa2 : dist.pa1;
  varset z = L[zero].pp.c - dist.pp.c;
  if (values.c.intersects(z)) return zero;
  if (values.d.intersects(z)) return one;
  return 0;
}

// The values assigned to the variables in the formula of the parent of a
// distribution derived by a rule with a single parent, where values are the
// values assigned in the formula of the distribution. As in derive_formula,
// the values do not apply under the summation of marginalization (rule 0)
// and of the denominator of conditioning (rule 1), and case-by-general
// reasoning (rule 8) assigns the values of the step.
p csisearch::parent_values(const distr& dist, p values) const {
  int rsq = dist.rule_num * dist.rule_num;
  const distr& pa1 = L[dist.pa1];
  varset z;
  if (rsq == 0) z = pa1.pp.a - dist.pp.a;
  else if (rsq == 1) z = dist.pp.a;
  values.c -= z;
  values.d -= z;
  if (rsq == 64) {
    varset zero = dist.pp.c - pa1.pp.c;
    varset one = dist.pp.d - pa1.pp.d;
    values.c |= zero;
    values.d |= one;
    values.c -= one;
    values.d -= zero;
  }
  return values;
}

// The input distribution of a distribution derived by rules with a single
// parent (see single_input) after the marginalization over the variables
// drop and the conditioning on the variables move of the left-hand side of
// the distribution and over the variables of the marginalization and
// conditioning steps of the derivation, where the formula is derived at the
// values assigned to the variables in values (see single_input)
p csisearch::compact_atom(const distr& dist, varset drop, varset move, p values) const {
  const distr* d = &dist;
  while (d->pa1 > 0) {
    int c = assigned_case(*d, values);
    if (c > 0) {
      d = &L[c];
      continue;
    }
    const distr& pa1 = L[d->pa1];
    int rsq = d->rule_num * d->rule_num;
    if (rsq == 0) drop |= pa1.pp.a - d->pp.a;
    else if (rsq == 1) move |= pa1.pp.a - d->pp.a;
    values = parent_values(*d, values);
    d = &pa1;
  }
  p pp = d->pp;
  pp.a -= drop | move;
  pp.b |= move;
  return pp;
}

// The formula of the distribution of compact_atom, where the values assigned
// to the variables are given as in derive_formula, and case-by-case
// reasoning over a variable that is assigned a value reduces to the case of
// that value (see single_input)
std::string csisearch::compact_formula(distr& dist, varset drop, varset move) {
  if (dist.pa1 == 0) {
    p pp = dist.pp;
    varset unassigned = (pp.a | pp.b) - (pp.c | pp.d);
    pp.c |= pending.c & unassigned;
    pp.d |= pending.d & unassigned;
    pp.a -= drop | move;
    pp.b |= move;
    return to_string(pp, refs);
  }
  int c = assigned_case(dist, pending);
  if (c > 0) return compact_formula(L[c], drop, move);
  distr& pa1 = L[dist.pa1];
  int rsq = dist.rule_num * dist.rule_num;
  if (rsq == 0) {
    varset z = pa1.pp.a - dist.pp.a;
    p prev = pending;
    pending.c -= z;
    pending.d -= z;
    std::string formula = compact_formula(pa1, drop | z, move);
    pending = prev;
    return formula;
  }
  if (rsq == 1) return compact_formula(pa1, drop, move | (pa1.pp.a - dist.pp.a));
  if (rsq != 64) return compact_formula(pa1, drop, move);
  p prev = pending;
  varset zero = dist.pp.c - pa1.pp.c;
  varset one = dist.pp.d - pa1.pp.d;
  pending.c |= zero;
  pending.d |= one;
  pending.c -= one;
  pending.d -= zero;
  std::string formula = compact_formula(pa1, drop, move);
  pending = prev;
  return formula;
}

// The summation of the formula of a distribution over the variables z. The
// summation applies to the terms that follow it in the same product, and
// its variables are recorded in open (see search::bind). The values assigned
// to the variables do not apply under the summation over them. When the
// variants of the formula are derived (see search::initialize), the
// summation over the context variables of case-by-case reasoning is written
// out as a sum of the terms for their values (see written_sum), and
// difference is set to true if no summation over other variables remains.
std::string csisearch::sum_formula(distr& dist, varset z, bindings& open, bool& difference) {
  varset empty;
  varset w = expand ? z & cases : empty;
  z -= w;
  p prev_pending = pending;
  pending.c -= z;
  pending.d -= z;
  std::vector<int> prev = bind(z, open);
  std::string formula;
  if (w.empty()) {
    formula = "\\sum_{" + dec_to_text(z, empty, empty, refs) + "}" + term_formula(dist, open);
  } else if (z.empty()) {
    formula = written_sum(dist, w);
    difference = true;
  } else {
    formula = "\\sum_{" + dec_to_text(z, empty, empty, refs) + "}\\left(" + written_sum(dist, w) + "\\right)";
  }
  unbind(z, prev);
  pending = prev_pending;
  return formula;
}

// The formula of a distribution as a sum of its formulas for each
// combination of values of the context variables w, from the value 0 to the
// value 1, where the value of the last variable changes first. The values
// are assigned in the formula, which reduces case-by-case reasoning over the
// variables to the cases of the values.
std::string csisearch::written_sum(distr& dist, const varset& w) {
  std::vector<int> elems = w.elements();
  int m = elems.size();
  p prev = pending;
  std::string formula = "";
  for (uint64_t i = 0; i < (static_cast<uint64_t>(1) << m); i++) {
    varset zero, one;
    for (int k = 0; k < m; k++) {
      if ((i >> (m - 1 - k)) & 1) one.insert(elems[k]);
      else zero.insert(elems[k]);
    }
    pending = prev;
    pending.c |= zero;
    pending.d |= one;
    pending.c -= one;
    pending.d -= zero;
    if (i > 0) formula += " + ";
    formula += closed_formula(dist, false);
  }
  pending = prev;
  return formula;
}

// The context variables of case-by-case reasoning (rules 5 and -5) in the
// derivation of a distribution
varset csisearch::case_variables(const distr& dist) const {
  varset vars;
  std::vector<char> seen(L.size(), 0);
  std::vector<const distr*> stack(1, &dist);
  while (!stack.empty()) {
    const distr* d = stack.back();
    stack.pop_back();
    if (d->pa1 == 0) continue;
    if (d->pa2 > 0 && d->rule_num * d->rule_num == 25) {
      const distr& zero = L[(d->rule_num > 0) ? d->pa1 : d->pa2];
      vars |= zero.pp.c - d->pp.c;
    }
    int parents[2] = {d->pa1, d->pa2};
    for (int pa : parents) {
      if (pa > 0 && !seen[pa]) {
        seen[pa] = 1;
        stack.push_back(&L[pa]);
      }
    }
  }
  return vars;
}

// The variables that are assigned a value in a distribution
varset csisearch::assigned(const p& pp) const {
  return pp.c | pp.d;
}

// Assign the values 0 and 1 to the variables zero and one in the formula
// that is derived next
void csisearch::set_pending(const varset& zero, const varset& one) {
  pending = p();
  pending.c = zero;
  pending.d = one;
}

// The formula of a distribution evaluated at the values assigned to the
// variables zero (= 0) and one (= 1) by case-by-general reasoning.
// Evaluation commutes with the summations over other variables, products,
// quotients and differences, so the values are assigned in the input
// distributions of the formula, except under the summations over the
// variables themselves. Case-by-case reasoning over a variable that is
// assigned a value reduces to the case of that value.
std::string csisearch::assigned_formula(distr& dist, const varset& zero, const varset& one, bindings& open, bool& difference) {
  p prev = pending;
  pending.c |= zero;
  pending.d |= one;
  pending.c -= one;
  pending.d -= zero;
  std::string formula = derive_formula(dist, open, difference);
  pending = prev;
  return formula;
}

// Is the formula of a distribution a difference?
bool csisearch::is_difference(const distr& dist) const {
  if (dist.pa1 == 0) return false;
  int rsq = dist.rule_num * dist.rule_num;
  if (dist.pa2 > 0) return rsq == 36 || rsq == 49;
  if (rsq == 36) return true;
  if (rsq == 9 || rsq == 16 || rsq == 64) return is_difference(L[dist.pa1]);
  return false;
}

// The length of the formula of a distribution without primes as an
// expression of its own (see search::closed_formula), not counting the values
// assigned by case-by-general reasoning. The terms of products are ordered
// by these lengths (see term_length).
std::size_t csisearch::sort_length(const distr& dist) {
  bool memo = dist.index > 0 && dist.index < static_cast<int>(lengths.size());
  if (memo && lengths[dist.index] > 0) return lengths[dist.index];
  std::size_t len = 0;
  std::vector<int> none;
  varset empty;
  if (dist.pa1 > 0) {
    int rsq = dist.rule_num * dist.rule_num;
    const distr& pa1 = L[dist.pa1];
    if (dist.pa2 > 0) {
      const distr& pa2 = L[dist.pa2];
      if (rsq == 4) len = term_length(pa1) + term_length(pa2);
      else if (rsq == 25) len = term_length(pa1) + term_length(pa2) + 19;
      else if (rsq == 36) len = sort_length(pa2) + term_length(pa1) + 3;
      else if (rsq == 49) len = sort_length(pa1) + term_length(pa2) + 3;
    } else if (rsq == 9 || rsq == 16 || rsq == 64) {
      len = sort_length(pa1);
    } else if (rsq == 0 && single_input(pa1, parent_values(dist, pending))) {
      len = to_string(compact_atom(pa1, pa1.pp.a - dist.pp.a, empty, parent_values(dist, pending))).length();
    } else if (rsq == 1 && single_input(pa1, parent_values(dist, pending))) {
      len = to_string(compact_atom(pa1, empty, pa1.pp.a - dist.pp.a, parent_values(dist, pending))).length();
    } else if (rsq == 0) {
      len = term_length(pa1) + 7 + dec_to_text(pa1.pp.a - dist.pp.a, empty, empty, none).length();
    } else if (rsq == 1) {
      len = sort_length(pa1) + term_length(pa1) + 16 + dec_to_text(dist.pp.a, empty, empty, none).length();
    } else if (rsq == 36) {
      len = term_length(pa1) + 4;
    }
  } else len = to_string(dist.pp).length();
  if (memo) lengths[dist.index] = len;
  return len;
}

// The length of the formula of a distribution without primes as a term of a
// product or a case of case-by-case reasoning, where a difference is enclosed
// in parentheses
std::size_t csisearch::term_length(const distr& dist) {
  if (is_difference(dist)) return sort_length(dist) + 13;
  return sort_length(dist);
}

// Variables that are free in the formula of a distribution,
// i.e., variables that appear in the formula outside the sums that bind them
varset csisearch::free_vars(const distr& dist) {
  varset vars = dist.pp.a | dist.pp.b;
  if (dist.pa1 == 0) return vars;
  int rsq = dist.rule_num * dist.rule_num;
  const distr& pa1 = L[dist.pa1];
  if (dist.pa2 > 0) return free_vars(pa1) | free_vars(L[dist.pa2]);
  if (rsq == 9 || rsq == 16) return free_vars(pa1);
  if (rsq == 0) return free_vars(pa1) - (pa1.pp.a - dist.pp.a);
  if (rsq == 64) return free_vars(pa1) | (dist.pp.c - pa1.pp.c) | (dist.pp.d - pa1.pp.d);
  return free_vars(pa1);
}

std::string csisearch::dec_to_text(const varset& dec, const varset& zero, const varset& one, const std::vector<int>& scope) const {
  std::string s = "";
  for (int i = dec.first(); i > 0; i = dec.next(i)) {
    if (!s.empty()) s += ",";
    if (zero.contains(i)) s += label(i, scope) + " = 0";
    else if (one.contains(i)) s += label(i, scope) + " = 1";
    else s += label(i, scope);
  }
  return s;
}

std::string csisearch::to_string(const p& pp) const {
  return to_string(pp, std::vector<int>());
}

std::string csisearch::to_string(const p& pp, const std::vector<int>& scope) const {
  std::string s = "";
  s += "p(" + dec_to_text(pp.a, pp.a & pp.c, pp.a & pp.d, scope);
  if (!pp.b.empty()) {
    s += "|" + dec_to_text(pp.b, pp.b & pp.c, pp.b & pp.d, scope);
  }
  s += ")";
  return s;
}

bool csisearch::valid_rule(const int& ruleid, const p& pp) const {
  const varset& a = pp.a;
  const varset& b = pp.b;
  const varset& c = pp.c;
  const varset& d = pp.d;
  switch (ruleid) {
    // Marginalization
    case 0 : {
      // there must be other variables
      return a.size() != 1;
    }
    // Conditioning
    case 1 : {
      // there must be other variables
      return a.size() != 1;
    }
    // Product rule
    case 2 : {
      // there must be conditioning variables
      return !b.empty();
    }
    case -2 : {
      // there must be other variables available
      return !(lhs - (a | b)).empty();
    }
    // Deletion of observations
    case -3 : {
      // there must be observations to delete
      return !b.empty();
    }
    // Case-by-case reasoning
    case 5 : {
      // There must be a variable z = 0
      return !c.empty();
    }
    // Case-by-case reasoning
    case -5 : {
      // There must be a variable z = 1
      return !d.empty();
    }
    default : {
      return true;
    }
  }
  return true;
}

void csisearch::apply_rule(const int& ruleid, const p& pp, const varset& z) {
  const varset& a = pp.a;
  const varset& b = pp.b;
  const varset& c = pp.c;
  const varset& d = pp.d;
  info.valid = false;
  switch (ruleid) {
    // Marginalisation
    case 0 : {
      if (!z.is_subset(a)) return; // z has to be in a
      if (z == a) return; // there has to be something else in a
      if (z.intersects(c | d)) return; // cannot marginalize over z = 0 or z = 1
      break;
    }
    // Conditioning
    case 1 : {
      if (!z.is_subset(a)) return; // z has to be in a
      if (z == a) return; // there has to be something else in a
      if ((a - z).intersects(c | d)) return; // cannot marginalize over y = 0 or y = 1
      break;
    }
    // Product rule
    case 2 : {
      if (!z.is_subset(b)) return; // z has to be in b
      if (!z.is_subset(lhs)) return; // all variables in z must be observed
      break;
    }
    // Product rule
    case -2 : {
      // z cannot be in any of the sets
      if (z.intersects(a)) return; // z intersection a = 0
      if (z.intersects(b)) return; // z intersection b = 0
      if (!z.is_subset(lhs)) return; // all variables in z must be observed
      break;
    }
    // Insertion of observations (Unquantified)
    case 3 : {
      if (z.intersects(a)) return;
      if (z.intersects(b)) return;
      if (z.intersects(intv_vars)) return;
      break;
    }
    // Deletion of observations
    case -3 : {
      if (z.intersects(a)) return;
      if (!z.is_subset(b)) return;
      break;
    }
    // Insertion of observations (Z = 0)
    case 4 : {
      // z cannot be in any of the sets
      if (z.intersects(a)) return;
      if (z.intersects(b)) return;
      if (!z.is_subset(con_vars)) return; // z must be a context variable
      break;
    }
    // Insertion of observations (Z = 1)
    case -4 : {
      // z cannot be in any of the sets
      if (z.intersects(a)) return;
      if (z.intersects(b)) return;
      if (!z.is_subset(con_vars | intv_vars)) return; // z must be a context variable or an intervention variable
      break;
    }
    // Case-by-case reasoning
    case 5 : {
      if (!z.is_subset(a)) return; // z has to be in a
      if (!z.is_subset(c)) return; // z has to be in c
      if (!z.is_subset(lhs)) return; // all variables in z must be observed
      break;
    }
    // Case-by-case reasoning
    case -5 : {
      if (!z.is_subset(a)) return; // z has to be in a
      if (!z.is_subset(d)) return; // z has to be in d
      if (!z.is_subset(lhs)) return; // all variables in z must be observed
      break;
    }
    // General-by-case reasoning (RHS)
    case 6 : {
      if (!z.is_subset(a)) return; // z has to be in a
      if (!z.is_subset(c) && !z.is_subset(d)) return; // z = 0 or z = 1 has to hold
      if (!z.is_subset(lhs)) return; // all variables in z must be observed
      break;
    }
    // General-by-case reasoning (LHS, Z = 0)
    case 7 : {
      if (z.intersects(a)) return; // z intersection a = 0
      if (z.intersects(b)) return; // z intersection b = 0
      if (!z.is_subset(con_vars)) return; // z must be a context variable
      if (!z.is_subset(lhs)) return; // all variables in z must be observed
      break;
    }
    // General-by-case reasoning (LHS, Z = 1)
    case -7 : {
      if (z.intersects(a)) return; // z intersection a = 0
      if (z.intersects(b)) return; // z intersection b = 0
      if (!z.is_subset(con_vars)) return; // z must be a context variable
      if (!z.is_subset(lhs)) return; // all variables in z must be observed
      break;
    }
    // Case-by-general resoning (Z = 0)
    case 8 : {
      if (!z.is_subset(a) && !z.is_subset(b)) return; // z either in a or b
      if (z.intersects(c)) return; // z must be unqualified
      if (z.intersects(d)) return; // z must be unqualified
      if (!z.is_subset(con_vars)) return; // z must be a context variable
      break;
    }
    // Case-by-general resoning (Z = 1)
    case -8 : {
      if (!z.is_subset(a) && !z.is_subset(b)) return; // z either in a or b
      if (z.intersects(c)) return; // z must be unqualified
      if (z.intersects(d)) return; // z must be unqualified
      if (!z.is_subset(con_vars)) return; // z must be a context variable
      break;
    }
    // Unknown rule
    default : {
      return;
    }
  }
  info.valid = true;
  get_ruleinfo(ruleid, pp, z);
}

void csisearch::get_ruleinfo(const int& ruleid, const p& pp, const varset& z) {
  const varset& y = pp.a;
  const varset& x = pp.b;
  const varset& u = pp.c;
  const varset& v = pp.d;
  info.from = pp;
  info.rp.a = varset();
  info.ri.x = varset();
  info.enumerate = false;
  switch (ruleid) {
    // Marginalisation
    case 0 : {
      info.to.a = y - z; info.to.b = x; info.to.c = u; info.to.d = v;
      return;
    }
    // Conditioning
    case 1 : {
      info.to.a = y - z; info.to.b = x | z; info.to.c = u; info.to.d = v;
      return;
    }
    // Product rule
    case 2 : {
      varset xmz = x - z;
      info.to.a = y | z; info.to.b = xmz; info.to.c = u;     info.to.d = v;
      info.rp.a = z;     info.rp.b = xmz; info.rp.c = u & x; info.rp.d = v & x;
      return;
    }
    // Product rule
    case -2 : {
      info.to.a = y | z; info.to.b = x;     info.to.c = u; info.to.d = v;
      info.rp.a = z;     info.rp.b = x | y; info.rp.c = u; info.rp.d = v;
      info.enumerate = true;
      return;
    }
    // Insertion of observations (Unquantified)
    case 3 : {
      info.to.a = y; info.to.b = x | z; info.to.c = u; info.to.d = v;
      info.ri.y = y; info.ri.x = z; info.ri.z = x;
      info.ri.u = u & x; info.ri.v = v & x;
      return;
    }
    // Deletion of observations
    case -3 : {
      varset xmz = x - z;
      varset umz = u - z;
      varset vmz = v - z;
      info.to.a = y; info.to.b = xmz; info.to.c = umz; info.to.d = vmz;
      info.ri.y = y; info.ri.x = z; info.ri.z = xmz;
      info.ri.u = umz & x; info.ri.v = vmz & x;
      return;
    }
    // Insertion of observations (Z = 0)
    case 4 : {
      info.to.a = y; info.to.b = x | z; info.to.c = u | z; info.to.d = v;
      info.ri.y = y; info.ri.x = z; info.ri.z = x;
      info.ri.u = u & x; info.ri.v = v & x;
      return;
    }
    // Insertion of observations (Z = 1)
    case -4 : {
      info.to.a = y; info.to.b = x | z; info.to.c = u; info.to.d = v | z;
      info.ri.y = y; info.ri.x = z; info.ri.z = x;
      info.ri.u = u & x; info.ri.v = v & x;
      return;
    }
    // Case-by-case reasoning
    case 5 : {
      varset umz = u - z;
      info.to.a = y; info.to.b = x; info.to.c = umz; info.to.d = v;
      info.rp.a = y; info.rp.b = x; info.rp.c = umz; info.rp.d = v | z;
      return;
    }
    // Case-by-case reasoning
    case -5 : {
      varset vmz = v - z;
      info.to.a = y; info.to.b = x; info.to.c = u;     info.to.d = vmz;
      info.rp.a = y; info.rp.b = x; info.rp.c = u | z; info.rp.d = vmz;
      return;
    }
    // General-by-case reasoning (RHS)
    case 6 : {
      varset umz = u - z;
      varset vmz = v - z;
      info.to.a = y;     info.to.b = x; info.to.c = umz | (v & z); info.to.d = vmz | (u & z);
      info.rp.a = y - z; info.rp.b = x; info.rp.c = umz;           info.rp.d = vmz;
      return;
    }
    // General-by-case reasoning (LHS, Z = 0)
    case 7 : {
      info.to.a = y | z; info.to.b = x; info.to.c = u;     info.to.d = v | z;
      info.rp.a = y | z; info.rp.b = x; info.rp.c = u | z; info.rp.d = v;
      return;
    }
    // General-by-case reasoning (LHS, Z = 1)
    case -7 : {
      info.to.a = y | z; info.to.b = x; info.to.c = u | z; info.to.d = v;
      info.rp.a = y | z; info.rp.b = x; info.rp.c = u;     info.rp.d = v | z;
      return;
    }
    // Case-by-general resoning (Z = 0)
    case 8 : {
      info.to.a = y; info.to.b = x; info.to.c = u | z; info.to.d = v;
      return;
    }
    // Case-by-general resoning (Z = 1)
    case -8 : {
      info.to.a = y; info.to.b = x; info.to.c = u; info.to.d = v | z;
      return;
    }
  }
}

// csisearch_heuristic

csisearch_heuristic::csisearch_heuristic(const int& n_, const double& tl, const bool& bm, const bool& br, const bool& dd, const bool& da, const bool& fa, const bool& im, const bool& verb):csisearch(n_, tl, bm, br, dd, da, fa, im, verb) {
}

csisearch_heuristic::~csisearch_heuristic() {

}

distr& csisearch_heuristic::next_distribution(const int& i) {
  distr& top = *Q.top();
  Q.pop();
  return top;
}

void csisearch_heuristic::add_distribution(distr& nquery) {
  nquery.score = compute_score(nquery.pp);
  add_to_list(nquery);
  ps[nquery.pp] = index;
  Q.push(&L[index]);
}

void csisearch_heuristic::add_known(const varset& a, const varset& b, const varset& c, const varset& d, const varset& e) {
  index++;
  distr iquery;
  iquery.rule_num = 0;
  iquery.pp.a = a; iquery.pp.b = b; iquery.pp.c = c; iquery.pp.d = d; iquery.pp.e = e;
  iquery.pa1 = 0;
  iquery.pa2 = 0;
  iquery.index = index;
  iquery.score = compute_score(iquery.pp);
  add_to_list(iquery);
  ps[iquery.pp] = index;
  if (equal_p(iquery.pp, target)) {
    trivial_id = true;
    target_dist.push_back(iquery);
  }
  Q.push(&L[index]);
  lhs = lhs | a;
  if (verbose) Rcpp::Rcout << "Adding known distribution: " << to_string(iquery.pp) << std::endl;
}

// Heuristic for search order
int csisearch_heuristic::compute_score(const p& pp) const {
  int score = 0;
  varset common_a = pp.a & target.a;
  varset common_b = pp.b & target.b;
  varset common_c = pp.c & target.c;
  varset common_d = pp.d & target.d;
  score += 10 * common_a.size();
  score += 5 * common_b.size();
  score += 3 * common_c.size();
  score += 3 * common_d.size();
  score -= 2 * (target.a - common_a).size();
  score -= 2 * (pp.a - common_a).size();
  score -= 2 * (pp.b - common_b).size();
  score -= 2 * (target.b - common_b).size();
  score -= 1 * (target.c - common_c).size();
  score -= 1 * (pp.c - common_c).size();
  score -= 1 * (target.d - common_d).size();
  score -= 1 * (pp.d - common_d).size();
  return(score);
}
