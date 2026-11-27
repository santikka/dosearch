#include <algorithm>
#include "dosearch.h"

dosearch::dosearch(const int& n_, const double& tl, const bool& bm, const bool& br, const bool& dd, const bool& da, const bool& fa, const bool& im, const bool& verb):search(n_, tl, bm, br, dd, da, fa, im, verb) {
  case_reasoning = false;
  proxy_deletion = false;
}

dosearch::~dosearch() {
}

void dosearch::set_target(const varset& a, const varset& b, const varset& c, const varset& d, const varset& e) {
  target.a = a; target.b = b; target.c = c; target.d = d; target.e = e;
  if (verbose) Rcpp::Rcout << "Setting target: " << to_string(target) << std::endl;
}

void dosearch::set_graph(dag* g_) {
  g = g_;
}

void dosearch::set_options(const std::vector<int>& rule_vec) {
  trivial_id = false;
  index = 0;
  lhs = varset();
  md_s = g->get_md_switches();
  md_p = g->get_md_proxies();
  md_t = md_s.shift_down(1);
  md = !md_s.empty();
  // A true variable and its proxy variable share their primes in formulas
  for (int i = md_t.first(); i > 0; i = md_t.next(i)) {
    links[i] = i + 2;
    links[i + 2] = i;
  }
  tr = g->get_trnodes();
  sb = g->get_sbnodes();
  trsb = tr | sb;
  proxy_deletion = false;
  if (rule_vec.size() > 0) rules = rule_vec;
  else {
    // Additional rules for missing data problems. The deletion of
    // observations (rule -1) is implied by the exchange of observations and
    // actions and the deletion of actions (rules 2 and -3), which cannot be
    // applied to proxy variables, so the observations of proxy variables can
    // still be deleted when the other rules are improved.
    if (md) {
      if (improve) {
        rules = {4, 5, 9, 10, -1, -2, -3, 2, 3, 6, -6, 7, -7, 8, -8};
        proxy_deletion = true;
      }
      else rules = {4, 5, 9, 10, -1, -2, -3, 1, 2, 3, 6, -6, 7, -7, 8, -8};
    } else {
      if (improve) rules = {4, 5, -2, -3, 2, 3, 6, -6};
      else rules = {4, 5, -1, -2, -3, 1, 2, 3, 6, -6};
    }
  }
  // Case-by-general, case-by-case and general-by-case reasoning for
  // missing data mechanisms are only used when requested, in addition to
  // the default rules or the rules that were given
  if (md && case_reasoning) {
    std::vector<int> case_rules = {-9, 11, -11, 12, 13, -13};
    for (unsigned int r = 0; r < case_rules.size(); r++) {
      if (std::find(rules.begin(), rules.end(), case_rules[r]) == rules.end()) {
        rules.push_back(case_rules[r]);
      }
    }
  }
  rule_times = std::vector<double>(rules.size(), 0.0);
  rule_names[1] = "R1";
  rule_names[-1] = "R1";
  rule_names[2] = "R2";
  rule_names[-2] = "R2";
  rule_names[3] = "R3";
  rule_names[-3] = "R3";
  rule_names[4] = "M";
  rule_names[5] = "C";
  rule_names[6] = "P";
  rule_names[-6] = "P";
  rule_names[7] = "D";
  rule_names[-7] = "D";
  rule_names[8] = "D";
  rule_names[-8] = "D";
  rule_names[9] = "A";
  rule_names[-9] = "A";
  rule_names[10] = "EX";
  rule_names[11] = "CbC";
  rule_names[-11] = "CbC";
  rule_names[12] = "GbC";
  rule_names[13] = "GbC";
  rule_names[-13] = "GbC";
}

void dosearch::set_labels(const Rcpp::StringVector& lab) {
  labels = std::vector<std::string>(n);
  for (int i = 0; i < n; i++) {
    labels[i] = lab(i);
  }
}

void dosearch::set_case_reasoning(const bool& cr) {
  case_reasoning = cr;
}

void dosearch::add_known(const varset& a, const varset& b, const varset& c, const varset& d, const varset& e) {
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
  if (md) lhs = lhs | a | (a & md_p).shift_down(2);
  else lhs = lhs | a;
  if (verbose) Rcpp::Rcout << "Adding known distribution: " << to_string(iquery.pp) << std::endl;
}

// The target is not identifiable if a variable on its left-hand side does
// not appear on the left-hand side of any input distribution (Theorem 1 of
// Tikka et al., 2021) or if either of the criteria below holds. The criteria
// below are derived for the rules of do-calculus and probability calculus,
// so they are not applied to missing data problems.
bool dosearch::check_trivial() {
  varset missing = target.a - lhs;
  if (!missing.empty()) {
    if (verbose) Rcpp::Rcout << "Target is not identifiable: " << labels[missing.first() - 1] << " does not appear on the left-hand side of any input distribution" << std::endl;
    return true;
  }
  if (md) return false;
  return unmentioned_variable() || anchored_outcome();
}

// A variable that does not appear in any input distribution never appears
// in a formula derived from the inputs, so the target is constant in the
// value of each such variable on its right-hand side if it is identifiable.
// The target is not identifiable if such a variable is d-connected to the
// left-hand side of the target given the rest of its right-hand side in the
// graph where the incoming edges of the intervened variables are removed,
// because the target then depends on the variable in some model.
bool dosearch::unmentioned_variable() const {
  varset mentioned;
  for (unsigned int i = 1; i < L.size(); i++) {
    mentioned |= L[i].pp.a | L[i].pp.b;
  }
  varset unmentioned = target.b - mentioned;
  for (int u = unmentioned.first(); u > 0; u = unmentioned.next(u)) {
    varset uu = varset::unit(u);
    if (!g->dsep(target.a, uu, target.b - uu, target.c, false)) {
      if (verbose) Rcpp::Rcout << "Target is not identifiable: " << labels[u - 1] << " does not appear in any input distribution, but it is d-connected to the left-hand side of the target" << std::endl;
      return true;
    }
  }
  return false;
}

// The anchors of y are the parents of y and the children and siblings of y
// that do not appear on the left-hand side of any input distribution.
// A distribution with y on the left-hand side is anchored at y if it
// contains a parent anchor on the right-hand side or a child or sibling
// anchor on the right-hand side that is not intervened on. An anchor is
// adjacent to y, so the separation criteria of do-calculus that would remove
// it never hold, and it cannot be moved to the left-hand side by the chain
// rule. Thus if every input distribution with y on the left-hand side is
// anchored at y by the anchors that the target does not contain, so is every
// derived distribution with y on the left-hand side, and the target cannot
// be derived.
bool dosearch::anchored_outcome() const {
  varset target_w = target.b - target.c;
  for (int y = target.a.first(); y > 0; y = target.a.next(y)) {
    varset pa_y = g->get_parents(y) - lhs - target.b;
    varset ch_y = (g->get_children(y) | g->get_siblings(y)) - lhs - target_w;
    if (pa_y.empty() && ch_y.empty()) continue;
    bool anchored = true;
    for (unsigned int i = 1; i < L.size(); i++) {
      const p& pp = L[i].pp;
      if (!pp.a.contains(y)) continue;
      if (pp.b.intersects(pa_y) || (pp.b - pp.c).intersects(ch_y)) continue;
      anchored = false;
      break;
    }
    if (anchored) {
      if (verbose) Rcpp::Rcout << "Target is not identifiable: every input distribution with " << labels[y - 1] << " on the left-hand side is anchored at " << labels[y - 1] << ", but the target is not" << std::endl;
      return true;
    }
  }
  return false;
}

void dosearch::assign_candidate(distr& required) {
  info.to.d = required.pp.d & (info.to.a | info.to.b | info.to.c);
  info.to.e = required.pp.e & (info.to.a | info.to.b | info.to.c);
}

distr& dosearch::next_distribution(const int& i) {
  return L[i];
}

void dosearch::derive_distribution(const distr& iquery, const distr& required, const int& ruleid, int& remaining, bool& found) {
  index++;
  distr nquery;
  nquery.index = index;
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
    remaining++;
    add_distribution(nquery);
  }
}

void dosearch::add_distribution(distr& nquery) {
  add_to_list(nquery);
  ps[nquery.pp] = index;
}

// Candidates for the required distribution differ only by the values of the
// response indicators of the new variables that have not been assigned a
// value. Each such indicator is either unassigned, enabled or disabled.
// The candidates are enumerated in the order of the bit representations of
// the disabled (i) and the enabled (j) indicators. The candidates without
// disabled indicators are enumerated last, so they are tried first.
void dosearch::enumerate_candidates() {
  varset asw = ((info.rp.a - info.from.a) & md_s) - (info.rp.d | info.rp.e);
  if (asw.empty()) {
    int exist = lookup(info.rp);
    if (exist > 0) candidates.push_back(exist);
    return;
  }
  std::vector<int> elems = asw.elements();
  int m = elems.size();
  if (m > 62) Rcpp::stop("Too many missing data mechanisms to enumerate.");
  p rq = info.rp;
  uint64_t f = (static_cast<uint64_t>(1) << m) - 1;
  uint64_t i = f;
  while (true) { // loop assignments to zero
    uint64_t comp = f & ~i;
    uint64_t j = 0;
    while (true) { // loop assignments to one that do not intersect i
      rq.d = info.rp.d;
      rq.e = info.rp.e;
      for (int k = 0; k < m; k++) {
        if ((i >> k) & 1) rq.e.insert(elems[k]);
        else if ((j >> k) & 1) rq.d.insert(elems[k]);
      }
      int exist = lookup(rq);
      if (exist > 0) candidates.push_back(exist);
      if (j == comp) break;
      j = (j - comp) & comp;
    }
    if (i == 0) break;
    i--;
  }
}

bool dosearch::separation_criterion() {
  return g->dsep(info.ri.x, info.ri.y, info.ri.u, info.ri.v, info.ri.intv);
}

// Case-by-case and general-by-case reasoning are only applied to
// single response indicators at a time
int dosearch::rule_limit(const int& ruleid) const {
  int rsq = ruleid * ruleid;
  if (rsq == 121 || rsq == 144 || rsq == 169) return 1;
  return n;
}

// Variables that the set z of each rule can consist of
varset dosearch::rule_mask(const int& ruleid, const p& pp) const {
  const varset& a = pp.a;
  const varset& b = pp.b;
  const varset& c = pp.c;
  const varset& d = pp.d;
  const varset& e = pp.e;
  switch (ruleid) {
    case 1 : return full - (a | b);
    case 3 : return full - (a | b | md_p);
    case -1 : return proxy_deletion ? (b - c) & md_p : b - c;
    case 2 : return b - (c | md_p);
    case -2 :
    case -3 : return c - md_p;
    case 4 :
    case 5 : return a - trsb;
    case 6 :
    case -8 : return b - (c | trsb);
    case -6 :
    case 8 : return full - (a | b | trsb);
    case 7 :
    case -7 : return (a & (d | e)) - trsb;
    case 9 :
    case -9 : return ((a | (b - c)) & md_s) - (d | e);
    case 10 : return (a | b) & md_p;
    case 11 : return a & d;
    case -11 : return a & e;
    case 12 : return a & (d | e);
    case 13 :
    case -13 : return (md_s & lhs) - (a | b);
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
std::string dosearch::derive_formula(distr& dist, bindings& open, bool& difference) {
  std::string formula = "";
  varset empty;
  if (dist.pa1 > 0) {
    int r = dist.rule_num;
    int rsq = r * r;
    distr& pa1 = L[dist.pa1];
    if (dist.pa2 > 0) {
      distr& pa2 = L[dist.pa2];
      if (rsq == 36) {
        // The shorter term is given first
        bool first = term_length(pa1) < term_length(pa2);
        formula = term_formula(first ? pa1 : pa2, open);
        formula += term_formula(first ? pa2 : pa1, open);
      } else if (rsq == 49 || rsq == 64) {
        distr& num = (rsq == 49) ? pa1 : pa2;
        distr& den = (rsq == 49) ? pa2 : pa1;
        if (compact_division(dist)) {
          varset drop, move;
          division_sets(dist, drop, move);
          formula = compact_formula(num, drop, move, open);
        } else {
          // Variables that are divided out (rule -8) are treated like
          // deleted proxy variables (see removed_proxies)
          formula = removed_formula(removed_proxies(dist), open, [&]() {
            return "\\frac{" + division_part(num, dist) + "}{" + division_part(den, dist) + "}";
          });
        }
      } else if (rsq == 121) {
        // The case of the disabled response indicator is given first, and a
        // case that is a difference is enclosed in parentheses. If a value is
        // assigned to the response indicator, only that case is given.
        distr& enabled = (r > 0) ? pa1 : pa2;
        distr& disabled = (r > 0) ? pa2 : pa1;
        varset z = enabled.pp.d - dist.pp.d;
        if (pending.d.intersects(z)) formula = derive_formula(enabled, open, difference);
        else if (pending.e.intersects(z)) formula = derive_formula(disabled, open, difference);
        else formula = "\\left[" + closed_formula(disabled, true) + " \\vee " + closed_formula(enabled, true) + "\\right]";
      } else if (rsq == 144) {
        formula = closed_formula(pa2, false) + " - " + closed_formula(pa1, true);
        difference = true;
      } else if (rsq == 169) {
        formula = closed_formula(pa1, false) + " - " + closed_formula(pa2, true);
        difference = true;
      }
    } else {
      if (rsq < 16) {
        // Response indicators that are exchanged for actions (rule 2)
        // are enabled, so the formula is evaluated at the enabled value.
        // Proxy variables that are deleted (rule -1) are variables of their
        // own (see removed_proxies).
        varset ddiff = dist.pp.d - pa1.pp.d;
        formula = removed_formula(removed_proxies(dist), open, [&]() {
          if (r == 2 && !ddiff.empty()) return assigned_formula(pa1, ddiff, empty, open, difference);
          return derive_formula(pa1, open, difference);
        });
      } else if (rsq == 25 && single_input(pa1, parent_values(dist, pending))) {
        formula = compact_formula(pa1, empty, pa1.pp.a - dist.pp.a, open);
      } else if (rsq == 25) {
        std::string num = closed_formula(pa1, false);
        bindings den_open;
        bool sum = false;
        std::string den = sum_formula(pa1, dist.pp.a, den_open, sum);
        close(den_open);
        formula = "\\frac{" + num + "}{" + den + "}";
      } else if (rsq == 16 && single_input(pa1, parent_values(dist, pending))) {
        varset z = pa1.pp.a - dist.pp.a;
        p prev_pending = pending;
        pending.d -= z;
        pending.e -= z;
        formula = compact_formula(pa1, z, empty, open);
        pending = prev_pending;
      } else if (rsq == 16) {
        formula = sum_formula(pa1, pa1.pp.a - dist.pp.a, open, difference);
      } else if (rsq == 100) {
        // The exchanged proxy variables take the values of the true
        // variables, so they share the primes of the true variables
        varset px = exchanged_proxies(dist);
        std::vector<int> prev;
        for (int i = px.first(); i > 0; i = px.next(i)) {
          prev.push_back(refs[i]);
          refs[i] = refs[i - 2];
        }
        varset prev_removed = removed;
        removed -= px;
        formula = derive_formula(pa1, open, difference);
        removed = prev_removed;
        int j = 0;
        for (int i = px.first(); i > 0; i = px.next(i)) refs[i] = prev[j++];
      } else if (rsq == 81) {
        // The formula is evaluated at the values assigned to the
        // response indicators
        formula = assigned_formula(pa1, dist.pp.d - pa1.pp.d, dist.pp.e - pa1.pp.e, open, difference);
      } else if (rsq == 144) {
        // The response indicator is the only variable on the left-hand
        // side, so the general distribution is not required
        formula = "1 - " + closed_formula(pa1, true);
        difference = true;
      }
    }
  } else {
    // An input distribution, evaluated at the values that are assigned to
    // its response indicators in the distributions derived from it
    p pp = dist.pp;
    varset unassigned = (pp.a | pp.b) - (pp.d | pp.e);
    pp.d |= pending.d & unassigned;
    pp.e |= pending.e & unassigned;
    formula = input_formula(pp);
  }
  return formula;
}

// The summation of the formula of a distribution over the variables z. The
// summation applies to the terms that follow it in the same product, and
// its variables are recorded in open (see search::bind). The values assigned
// to the response indicators do not apply under the summation over them.
// When the variants of the formula are derived (see search::initialize), the
// summation over the binary variables of case-by-case reasoning is written
// out as a sum of the terms for their values (see written_sum), and
// difference is set to true if no summation over other variables remains.
std::string dosearch::sum_formula(distr& dist, varset z, bindings& open, bool& difference) {
  varset empty;
  varset w = expand ? z & cases : empty;
  z -= w;
  p prev_pending = pending;
  pending.d -= z;
  pending.e -= z;
  std::vector<int> prev = bind(z, open);
  varset prev_removed = removed;
  removed -= z;
  std::string formula;
  if (w.empty()) {
    formula = "\\sum_{" + dec_to_text(z, empty, empty, refs) + "}" + term_formula(dist, open);
  } else if (z.empty()) {
    formula = written_sum(dist, w);
    difference = true;
  } else {
    formula = "\\sum_{" + dec_to_text(z, empty, empty, refs) + "}\\left(" + written_sum(dist, w) + "\\right)";
  }
  removed = prev_removed;
  unbind(z, prev);
  pending = prev_pending;
  return formula;
}

// The formula of a distribution as a sum of its formulas for each
// combination of values of the binary variables w, from the value 0 to the
// value 1, where the value of the last variable changes first. The values
// are assigned in the formula, which reduces case-by-case reasoning over the
// variables to the cases of the values.
std::string dosearch::written_sum(distr& dist, const varset& w) {
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
    pending.d |= one;
    pending.e |= zero;
    pending.d -= zero;
    pending.e -= one;
    if (i > 0) formula += " + ";
    formula += closed_formula(dist, false);
  }
  pending = prev;
  return formula;
}

// The response indicators of case-by-case reasoning (rules 11 and -11) in
// the derivation of a distribution
varset dosearch::case_variables(const distr& dist) const {
  varset vars;
  std::vector<char> seen(L.size(), 0);
  std::vector<const distr*> stack(1, &dist);
  while (!stack.empty()) {
    const distr* d = stack.back();
    stack.pop_back();
    if (d->pa1 == 0) continue;
    if (d->pa2 > 0 && d->rule_num * d->rule_num == 121) {
      const distr& enabled = L[(d->rule_num > 0) ? d->pa1 : d->pa2];
      vars |= enabled.pp.d - d->pp.d;
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
varset dosearch::assigned(const p& pp) const {
  return pp.d | pp.e;
}

// Assign the values 0 and 1 to the response indicators zero and one in the
// formula that is derived next
void dosearch::set_pending(const varset& zero, const varset& one) {
  pending = p();
  pending.d = one;
  pending.e = zero;
}

// The formula given by derive, where the proxy variables px that are removed
// from the distribution (see removed_proxies) are bound. A removed proxy
// variable that is omitted from every input distribution of the formula
// (see input_formula) is not bound, so that it does not prime other
// variables. The formula is then derived again without binding it.
std::string dosearch::removed_formula(const varset& px, bindings& open, const std::function<std::string()>& derive) {
  if (px.empty()) return derive();
  varset prev_removed = removed;
  varset prev_shown = shown;
  varset bound = px;
  std::string formula;
  while (true) {
    std::size_t mark = open.size();
    std::vector<int> prev = bind(bound, open);
    removed |= px;
    shown = prev_shown - px;
    formula = derive();
    varset unused = bound - shown;
    unbind(bound, prev);
    removed = prev_removed;
    if (unused.empty()) break;
    // End the summations that the formula added to the product
    for (std::size_t k = open.size(); k > mark; k--) {
      tops[open[k - 1].first] = open[k - 1].second;
    }
    open.resize(mark);
    bound -= unused;
  }
  shown = prev_shown | (shown - px);
  return formula;
}

// The formula of an input distribution pp, including the values assigned to
// its response indicators. A proxy variable that is removed from a
// distribution derived from the input (see removed_proxies) is missing when
// its response indicator has the value 0, and the formula holds only at this
// value. The proxy variable is then determined by the response indicator, so
// it is omitted from the distribution: conditioning on it does not change
// the distribution, and its probability on the left-hand side is 1. It is
// kept when the response indicator is on the left-hand side and the proxy
// variable is not, because the probability of the response indicator given
// the missing proxy variable is 1, and when it is the only variable on the
// left-hand side.
std::string dosearch::input_formula(p pp) {
  varset px = removed & (pp.a | pp.b) & md_p;
  varset omit;
  for (int v = px.first(); v > 0; v = px.next(v)) {
    if (!pp.e.contains(v - 1)) continue;
    if (pp.a.contains(v - 1) && !pp.a.contains(v)) continue;
    omit.insert(v);
  }
  if ((pp.a - omit).empty()) omit -= pp.a;
  shown |= px - omit;
  pp.a -= omit;
  pp.b -= omit;
  return to_string(pp, refs);
}

// Is the distribution derived from an input distribution by rules that have
// a single parent distribution, i.e., the rules of do-calculus,
// marginalization, conditioning, the assignment of values to response
// indicators and the exchange of proxy variables for true variables, or by
// divisions that are conditioning or marginalization of a single input
// distribution (see compact_division)? The rules of do-calculus equate
// distributions without changing their formulas, so the formula of such a
// distribution is a marginal or conditional distribution of the input
// distribution (see compact_atom), and so are the formulas of its marginal
// and conditional distributions. They are given as such in formulas instead
// of explicit summations and quotients (see compact_formula). The formula of
// the distribution is derived at the values assigned to the response
// indicators in values, so case-by-case reasoning over a response indicator
// that is assigned a value reduces to the case of that value (see
// assigned_case).
bool dosearch::single_input(const distr& dist, p values) const {
  const distr* d = &dist;
  while (d->pa1 > 0) {
    int rsq = d->rule_num * d->rule_num;
    if (d->pa2 > 0) {
      int c = assigned_case(*d, values);
      if (c > 0) {
        d = &L[c];
        continue;
      }
      if (!compact_division(*d, values)) return false;
      d = &L[(rsq == 49) ? d->pa1 : d->pa2];
      continue;
    }
    if (rsq > 16 && rsq != 25 && rsq != 81 && rsq != 100) return false;
    values = parent_values(*d, values);
    d = &L[d->pa1];
  }
  return true;
}

// The parent of a distribution derived by case-by-case reasoning (rules 11
// and -11) for the value that is assigned to its response indicator in
// values, or 0 if no value is assigned to it (see derive_formula)
int dosearch::assigned_case(const distr& dist, const p& values) const {
  if (dist.pa2 == 0 || dist.rule_num * dist.rule_num != 121) return 0;
  int enabled = (dist.rule_num > 0) ? dist.pa1 : dist.pa2;
  int disabled = (dist.rule_num > 0) ? dist.pa2 : dist.pa1;
  varset z = L[enabled].pp.d - dist.pp.d;
  if (values.d.intersects(z)) return enabled;
  if (values.e.intersects(z)) return disabled;
  return 0;
}

// The values assigned to the response indicators in the formula of the parent
// of a distribution derived by a rule with a single parent, where values are
// the values assigned in the formula of the distribution. As in
// derive_formula, the values do not apply under the summation of
// marginalization (rule 4) and of the denominator of conditioning (rule 5),
// and the assignment of values (rules 9 and -9) and the exchange of
// observations for actions (rule 2) assign the values of the step.
p dosearch::parent_values(const distr& dist, p values) const {
  int r = dist.rule_num;
  int rsq = r * r;
  const distr& pa1 = L[dist.pa1];
  varset z;
  if (rsq == 16) z = pa1.pp.a - dist.pp.a;
  else if (rsq == 25) z = dist.pp.a;
  values.d -= z;
  values.e -= z;
  if (rsq == 81 || r == 2) {
    varset d = dist.pp.d - pa1.pp.d;
    varset e = dist.pp.e - pa1.pp.e;
    values.d |= d;
    values.e |= e;
    values.d -= e;
    values.e -= d;
  }
  return values;
}

// Is the division of the distribution (rules 7, -7, 8 and -8) conditioning
// or marginalization of a single input distribution (see division_type)?
// The quotient is then given as such, like the conditional distributions of
// rule 5.
bool dosearch::compact_division(const distr& dist) const {
  return division_type(dist, pending) > 0;
}

bool dosearch::compact_division(const distr& dist, const p& values) const {
  return division_type(dist, values) > 0;
}

// The formula of a division (rules 7, -7, 8 and -8) is a single distribution
// when the divided distribution and the divisor are derived from the same
// input distribution by rules with a single parent (see single_input). The
// rules of do-calculus do not change the formulas, so they may be applied as
// well. The formulas of the divided distribution and the divisor are then
// distributions of the input distribution (see compact_atom). If the formula
// of the divisor is a marginal distribution of the formula of the divided
// distribution, the quotient is a conditional distribution (1), and if it is
// the conditional distribution given the other variables of the left-hand
// side, the quotient is a marginal distribution (2). Otherwise, the quotient
// is given explicitly (0). This depends only on the formulas, so the steps of
// do-calculus may relate the distributions differently than the rule of the
// division, e.g., when the divisor of rule -8 is derived from a marginal
// distribution of the input distribution by inserting observations. The
// formulas are derived at the values assigned to the response indicators in
// values (see single_input), and the type is recorded only when no value is
// assigned to the variables of case-by-case reasoning, which is when it does
// not depend on the values.
int dosearch::division_type(const distr& dist, const p& values) const {
  int rsq = dist.rule_num * dist.rule_num;
  if (dist.pa1 == 0 || dist.pa2 == 0 || (rsq != 49 && rsq != 64)) return 0;
  bool memo = dist.index > 0 && dist.index < static_cast<int>(L.size()) &&
    !(values.d | values.e).intersects(cases);
  if (memo) {
    if (divisions.size() != L.size()) divisions.assign(L.size(), 0);
    if (divisions[dist.index] > 0) return divisions[dist.index] % 3;
  }
  const distr& num = L[(rsq == 49) ? dist.pa1 : dist.pa2];
  const distr& den = L[(rsq == 49) ? dist.pa2 : dist.pa1];
  int type = 0;
  if (single_input(num, values) && single_input(den, values) && input_of(num, values) == input_of(den, values)) {
    varset empty;
    p n = compact_atom(num, empty, empty, values);
    p d = compact_atom(den, empty, empty, values);
    if (d.c == n.c && d.a.is_subset(n.a) && !(d.a == n.a)) {
      if (d.b == n.b) type = 1;
      else if (d.b == (n.b | (n.a - d.a))) type = 2;
    }
  }
  if (memo) divisions[dist.index] = (type > 0) ? type : 3;
  return type;
}

// The variables of the divided distribution of a division whose formula is a
// single distribution (see division_type) that are moved to the right-hand
// side or dropped from the left-hand side. These are the variables of the
// left-hand side of the divisor, which are missing data mechanisms for every
// division rule, so they are the same in the formulas.
void dosearch::division_sets(const distr& dist, varset& drop, varset& move) const {
  division_sets(dist, pending, drop, move);
}

void dosearch::division_sets(const distr& dist, const p& values, varset& drop, varset& move) const {
  int rsq = dist.rule_num * dist.rule_num;
  const distr& den = L[(rsq == 49) ? dist.pa2 : dist.pa1];
  if (division_type(dist, values) == 1) move |= den.pp.a;
  else drop |= den.pp.a;
}

// The index of the input distribution of a distribution derived from a
// single input distribution at the values assigned to the response
// indicators in values (see single_input)
int dosearch::input_of(const distr& dist, p values) const {
  const distr* d = &dist;
  while (d->pa1 > 0) {
    int rsq = d->rule_num * d->rule_num;
    int c = assigned_case(*d, values);
    if (c > 0) d = &L[c];
    else if (d->pa2 > 0 && rsq == 64) d = &L[d->pa2];
    else if (d->pa2 > 0) d = &L[d->pa1];
    else {
      values = parent_values(*d, values);
      d = &L[d->pa1];
    }
  }
  return d->index;
}

// The input distribution of a distribution derived by rules with a single
// parent (see single_input) after the marginalization over the variables
// drop and the conditioning on the variables move of the left-hand side of
// the distribution and over the variables of the marginalization and
// conditioning steps of the derivation. Proxy variables that are exchanged
// for true variables are referred to by the true variables in drop and move.
// The formula is derived at the values assigned to the response indicators
// in values (see single_input).
p dosearch::compact_atom(const distr& dist, varset drop, varset move, p values) const {
  const distr* d = &dist;
  while (d->pa1 > 0) {
    int rsq = d->rule_num * d->rule_num;
    if (d->pa2 > 0) {
      int c = assigned_case(*d, values);
      if (c > 0) {
        d = &L[c];
        continue;
      }
      // A division (see division_type)
      division_sets(*d, values, drop, move);
      d = &L[(rsq == 49) ? d->pa1 : d->pa2];
      continue;
    }
    const distr& pa1 = L[d->pa1];
    if (rsq == 16) drop |= pa1.pp.a - d->pp.a;
    else if (rsq == 25) move |= pa1.pp.a - d->pp.a;
    else if (rsq == 100) {
      varset pt = exchanged_proxies(*d).shift_down(2);
      drop = (drop - pt) | (drop & pt).shift_up(2);
      move = (move - pt) | (move & pt).shift_up(2);
    }
    values = parent_values(*d, values);
    d = &pa1;
  }
  p pp = d->pp;
  pp.a -= drop | move;
  pp.b |= move;
  return pp;
}

// The formula of the distribution of compact_atom, where the values assigned
// to the response indicators, the primes of the exchanged proxy variables and
// the deleted proxy variables are given as in derive_formula. Case-by-case
// reasoning over a response indicator that is assigned a value reduces to the
// case of that value (see single_input).
std::string dosearch::compact_formula(distr& dist, varset drop, varset move, bindings& open) {
  if (dist.pa1 == 0) {
    p pp = dist.pp;
    varset unassigned = (pp.a | pp.b) - (pp.d | pp.e);
    pp.d |= pending.d & unassigned;
    pp.e |= pending.e & unassigned;
    pp.a -= drop | move;
    pp.b |= move;
    return input_formula(pp);
  }
  int r = dist.rule_num;
  int rsq = r * r;
  if (dist.pa2 > 0) {
    int c = assigned_case(dist, pending);
    if (c > 0) return compact_formula(L[c], drop, move, open);
    // A division (see division_type)
    division_sets(dist, drop, move);
    return compact_formula(L[(rsq == 49) ? dist.pa1 : dist.pa2], drop, move, open);
  }
  distr& pa1 = L[dist.pa1];
  std::string formula;
  if (rsq == 16) {
    varset z = pa1.pp.a - dist.pp.a;
    p prev_pending = pending;
    pending.d -= z;
    pending.e -= z;
    formula = compact_formula(pa1, drop | z, move, open);
    pending = prev_pending;
  } else if (rsq == 25) {
    formula = compact_formula(pa1, drop, move | (pa1.pp.a - dist.pp.a), open);
  } else if (rsq == 100) {
    varset px = exchanged_proxies(dist);
    varset pt = px.shift_down(2);
    std::vector<int> prev;
    for (int i = px.first(); i > 0; i = px.next(i)) {
      prev.push_back(refs[i]);
      refs[i] = refs[i - 2];
    }
    varset prev_removed = removed;
    removed -= px;
    formula = compact_formula(pa1, (drop - pt) | (drop & pt).shift_up(2), (move - pt) | (move & pt).shift_up(2), open);
    removed = prev_removed;
    int j = 0;
    for (int i = px.first(); i > 0; i = px.next(i)) refs[i] = prev[j++];
  } else {
    // Assignment of values (rule 9) or a rule of do-calculus
    p prev_pending = pending;
    varset d = dist.pp.d - pa1.pp.d;
    varset e = dist.pp.e - pa1.pp.e;
    if (rsq != 81 && r != 2) {
      d = varset();
      e = varset();
    }
    pending.d |= d;
    pending.e |= e;
    pending.d -= e;
    pending.e -= d;
    formula = removed_formula(removed_proxies(dist), open, [&]() {
      return compact_formula(pa1, drop, move, open);
    });
    pending = prev_pending;
  }
  return formula;
}

// The formula of the divided distribution or the divisor of a division as
// an expression of its own, evaluated at the values of the response
// indicators that are assigned in the quotient but not in the distribution.
// The candidates for the divided distribution of rule -8 can assign values to
// the response indicators of the right-hand side of the divisor (see
// enumerate_candidates), so the divisor is evaluated at these values.
std::string dosearch::division_part(distr& part, const distr& dist) {
  varset vars = part.pp.a | part.pp.b;
  varset unassigned = vars - (part.pp.d | part.pp.e);
  p prev = pending;
  pending.d |= dist.pp.d & unassigned;
  pending.e |= dist.pp.e & unassigned;
  pending.d -= dist.pp.e & unassigned;
  pending.e -= dist.pp.d & unassigned;
  std::string formula = closed_formula(part, false);
  pending = prev;
  return formula;
}

// The formula of a distribution evaluated at the values assigned to the
// response indicators d (enabled) and e (disabled). Evaluation commutes with
// the summations over other variables, products, quotients and differences,
// so the values are assigned in the input distributions of the formula,
// except under the summations over the response indicators themselves.
// Case-by-case reasoning over a response indicator that is assigned a value
// reduces to the case of that value.
std::string dosearch::assigned_formula(distr& dist, const varset& d, const varset& e, bindings& open, bool& difference) {
  p prev = pending;
  pending.d |= d;
  pending.e |= e;
  pending.d -= e;
  pending.e -= d;
  std::string formula = derive_formula(dist, open, difference);
  pending = prev;
  return formula;
}

// Is the formula of a distribution a difference?
bool dosearch::is_difference(const distr& dist) const {
  if (dist.pa1 == 0) return false;
  int rsq = dist.rule_num * dist.rule_num;
  if (dist.pa2 > 0) return rsq == 144 || rsq == 169;
  if (rsq == 144) return true;
  if (rsq < 16 || rsq == 81 || rsq == 100) return is_difference(L[dist.pa1]);
  return false;
}

// The length of the formula of a distribution without primes as an
// expression of its own (see search::closed_formula), not counting the values
// assigned to the response indicators or omitting the removed proxy
// variables (see input_formula). The terms of products are ordered by these
// lengths (see term_length).
std::size_t dosearch::sort_length(const distr& dist) {
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
      if (rsq == 36) len = term_length(pa1) + term_length(pa2);
      else if ((rsq == 49 || rsq == 64) && compact_division(dist)) len = to_string(compact_atom(dist, empty, empty, pending)).length();
      else if (rsq == 49 || rsq == 64) len = sort_length(pa1) + sort_length(pa2) + 9;
      else if (rsq == 121) len = term_length(pa1) + term_length(pa2) + 19;
      else if (rsq == 144) len = sort_length(pa2) + term_length(pa1) + 3;
      else if (rsq == 169) len = sort_length(pa1) + term_length(pa2) + 3;
    } else if (rsq < 16 || rsq == 81 || rsq == 100) {
      len = sort_length(pa1);
    } else if (rsq == 25 && single_input(pa1, parent_values(dist, pending))) {
      len = to_string(compact_atom(pa1, empty, pa1.pp.a - dist.pp.a, parent_values(dist, pending))).length();
    } else if (rsq == 25) {
      len = sort_length(pa1) + term_length(pa1) + 16 + dec_to_text(dist.pp.a, empty, empty, none).length();
    } else if (rsq == 16 && single_input(pa1, parent_values(dist, pending))) {
      len = to_string(compact_atom(pa1, pa1.pp.a - dist.pp.a, empty, parent_values(dist, pending))).length();
    } else if (rsq == 16) {
      len = term_length(pa1) + 7 + dec_to_text(pa1.pp.a - dist.pp.a, empty, empty, none).length();
    } else if (rsq == 144) {
      len = term_length(pa1) + 4;
    }
  } else len = to_string(dist.pp).length();
  if (memo) lengths[dist.index] = len;
  return len;
}

// The length of the formula of a distribution without primes as a term of a
// product or a case of case-by-case reasoning, where a difference is enclosed
// in parentheses
std::size_t dosearch::term_length(const distr& dist) {
  if (is_difference(dist)) return sort_length(dist) + 13;
  return sort_length(dist);
}

// Variables that are free in the formula of a distribution,
// i.e., variables that appear in the formula outside the sums that bind them.
// Proxy variables that are exchanged for true variables (rule 10) stand for
// the values of the true variables, which are free instead.
varset dosearch::free_vars(const distr& dist) {
  varset vars = dist.pp.a | dist.pp.b;
  if (dist.pa1 == 0) return vars;
  int rsq = dist.rule_num * dist.rule_num;
  const distr& pa1 = L[dist.pa1];
  if (dist.pa2 > 0) {
    if (compact_division(dist)) {
      varset drop, move;
      division_sets(dist, drop, move);
      return free_vars(L[(rsq == 49) ? dist.pa1 : dist.pa2]) - drop;
    }
    varset f = free_vars(pa1) | free_vars(L[dist.pa2]);
    if (rsq == 49 || rsq == 64) return f - removed_proxies(dist);
    return f;
  }
  if (rsq < 16) return free_vars(pa1) - removed_proxies(dist);
  if (rsq == 16) return free_vars(pa1) - (pa1.pp.a - dist.pp.a);
  if (rsq == 100) {
    varset px = exchanged_proxies(dist);
    varset f = free_vars(pa1);
    return (f - px) | (f & px).shift_down(2);
  }
  return free_vars(pa1);
}

// Proxy variables that are exchanged for true variables (rule 10)
varset dosearch::exchanged_proxies(const distr& dist) const {
  const distr& pa1 = L[dist.pa1];
  return ((pa1.pp.a | pa1.pp.b) - (dist.pp.a | dist.pp.b)) & md_p;
}

// Proxy variables that are removed from a distribution by deleting
// observations (rule -1). Divisions (rule -8) only remove missing data
// mechanisms (see valid_rule), but they are treated in the same way. The
// formula of the distribution is the formula of the original distribution at
// any value of the removed variables for which it is defined. A proxy variable is missing
// when its mechanism is disabled and observed when its mechanism is enabled,
// so these values are not arbitrary, and a removed proxy variable must not be
// identified with another variable of the same name, e.g., with a variable
// that a product introduces to the left-hand side or that a summation binds.
// The removed proxy variables are therefore bound by the formula like the
// variables of a summation, without the summation sign, which primes them
// when a variable of the same name is in scope. A removed proxy variable is
// omitted where it is missing (see input_formula).
varset dosearch::removed_proxies(const distr& dist) const {
  const distr& pa1 = L[dist.pa1];
  varset vars = pa1.pp.a | pa1.pp.b;
  if (dist.pa2 > 0) {
    const distr& pa2 = L[dist.pa2];
    vars |= pa2.pp.a | pa2.pp.b;
  }
  return (vars - (dist.pp.a | dist.pp.b)) & md_p;
}

std::string dosearch::dec_to_text(const varset& dec, const varset& enabled, const varset& disabled, const std::vector<int>& scope) const {
  std::string s = "";
  for (int i = dec.first(); i > 0; i = dec.next(i)) {
    if (!s.empty()) s += ",";
    if (enabled.contains(i)) s += label(i, scope) + " = 1";
    else if (disabled.contains(i)) s += label(i, scope) + " = 0";
    else s += label(i, scope);
  }
  return s;
}

std::string dosearch::to_string(const p& pp) const {
  return to_string(pp, std::vector<int>());
}

std::string dosearch::to_string(const p& pp, const std::vector<int>& scope) const {
  std::string s = "";
  s += "p(" + dec_to_text(pp.a, pp.d, pp.e, scope);
  if (!pp.b.empty()) s += "|";
  if (!pp.c.empty()) s += "do(" + dec_to_text(pp.c, pp.d, pp.e, scope) + ")";
  varset b = pp.b - pp.c;
  if (!b.empty()) {
    if (!pp.c.empty()) s += ",";
    s += dec_to_text(b, pp.d, pp.e, scope);
  }
  s += ")";
  return s;
}

bool dosearch::valid_rule(const int& ruleid, const p& pp) const {
  const varset& a = pp.a;
  const varset& b = pp.b;
  const varset& c = pp.c;
  const varset& d = pp.d;
  const varset& e = pp.e;
  switch (ruleid) {
    case -1 : {
      // there must be observations to delete
      return !(b - c).empty();
    }
    case -2 : {
      // there must be actions to exchange
      return !c.empty();
    }
    case -3 : {
      // there must be actions to delete
      return !c.empty();
    }
    case 2 : {
      // there must be observations to exchange
      return !(b - c).empty();
    }
    case 4 : {
      // there must be other variables
      return a.size() != 1;
    }
    case 5 : {
      // there must be other variables
      return a.size() != 1;
    }
    case 6 : {
      // there must be conditioning variables
      return !(b - c).empty();
    }
    case 7 :
    case -7 : {
      // there must be other variables
      if (a.size() == 1) return false;
      // there must be enabled or disabled missing data mechanisms
      return a.intersects(d | e);
    }
    case 8 :
    case -8 : {
      // Every variable must be an enabled or a disabled missing data mechanism
      // Otherwise ordinary conditioning via rule 5 works
      return a.is_subset(d | e);
    }
    case 9 :
    case -9 : {
      // There must be missing data mechanisms
      return (a | b).intersects(md_s);
    }
    case 10 : {
      // There must be enabled missing data mechanisms and proxies
      if (d.empty()) return false;
      return (a | b).intersects(md_p);
    }
    case 11 : {
      // There must be an enabled missing data mechanism
      return a.intersects(d);
    }
    case -11 : {
      // There must be a disabled missing data mechanism
      return a.intersects(e);
    }
    case 12 : {
      // There must be an enabled or a disabled missing data mechanism
      return a.intersects(d | e);
    }
    case 13 :
    case -13 : {
      // There must be observed missing data mechanisms that are not present
      return !((md_s & lhs) - (a | b)).empty();
    }
    default : {
      return true;
    }
  }
  return true;
}

void dosearch::apply_rule(const int& ruleid, const p& pp, const varset& z) {
  const varset& a = pp.a;
  const varset& b = pp.b;
  const varset& c = pp.c;
  const varset& d = pp.d;
  const varset& e = pp.e;
  varset u, v, j, k;
  info.valid = false;
  switch (ruleid) {
    // Insertion of observations
    case 1 : {
      if (z.intersects(a)) return;
      if (z.intersects(b)) return;
      if (md) {
        j = ((a | b) & md_p).shift_down(2);
        if (j.intersects(z)) return; // cannot add x if x* exists
        k = ((a | b) & md_t).shift_up(2);
        if (k.intersects(z)) return; // cannot add x* if x exists
        j = (z & md_p).shift_down(2);
        if (j.intersects(z)) return; // cannot add both x and x*
        k = (z & md_t).shift_up(2);
        if (k.intersects(z)) return; // cannot add both x and x*
      }
      break;
    }
    // Deletion of observations
    case -1 : {
      if (z.intersects(a)) return;
      if (z.intersects(c)) return;
      if (!z.is_subset(b - c)) return;
      break;
    }
    // Exchange actions to observations
    case -2 : {
      if (z.intersects(a)) return;
      if (z.intersects(b - c)) return;
      if (!z.is_subset(c)) return;
      if (z.intersects(md_p)) return;
      break;
    }
    // Exchange observations to actions
    case 2 : {
      if (z.intersects(a)) return;
      if (z.intersects(c)) return;
      if (!z.is_subset(b)) return;
      if (z.intersects(md_p)) return;
      break;
    }
    // Deletion of actions
    case -3 : {
      if (z.intersects(a)) return;
      if (z.intersects(b - c)) return;
      if (!z.is_subset(c)) return;
      if (z.intersects(md_p)) return;
      break;
    }
    // Insertion of actions
    case 3 : {
      if (z.intersects(a)) return;
      if (z.intersects(b)) return;
      if (md) {
        if (z.intersects(md_p)) return; // z intersection m = 0
        j = ((a | b) & md_p).shift_down(2);
        if (j.intersects(z)) return; // cannot add x if x* exists
        k = ((a | b) & md_t).shift_up(2);
        if (k.intersects(z)) return; // cannot add x* if x exists
        j = (z & md_p).shift_down(2);
        if (j.intersects(z)) return; // cannot add both x and x*
        k = (z & md_t).shift_up(2);
        if (k.intersects(z)) return; // cannot add both x and x*
      }
      break;
    }
    // Marginalisation
    case 4 : {
      if (!z.is_subset(a)) return;
      if (z == a) return;
      if (z.intersects(trsb)) return;
      if (md) {
        j = a & md_s;
        if (z.intersects(j) && z.intersects(d | e)) return; // Cannot marginalize over enabled or disabled switch
      }
      break;
    }
    // Conditioning
    case 5 : {
      if (!z.is_subset(a)) return;
      if (z == a) return;
      if (z.intersects(trsb)) return;
      if (md) {
        j = a & md_s;
        if (!j.empty() && a.intersects(d | e)) return; // Cannot condition if switch is enabled or disabled
      }
      break;
    }
    // Product rule
    case 6 : {
      if (!z.is_subset(b)) return;
      if (z.intersects(c)) return;
      if (z.intersects(trsb)) return;
      break;
    }
    // Product rule
    case -6 : {
      // z cannot be in any of the sets
      if (z.intersects(a)) return; // z intersection y = 0
      if (z.intersects(b)) return; // z intersection x = 0
      if (z.intersects(trsb)) return; // z intersection s = 0
      if (md) {
        j = ((a | b) & md_p).shift_down(2);
        if (j.intersects(z)) return; // cannot add x if x* exists
        k = ((a | b) & md_t).shift_up(2);
        if (k.intersects(z)) return; // cannot add x* if x exists
        j = (z & md_p).shift_down(2);
        if (j.intersects(z)) return; // cannot add both x and x*
        k = (z & md_t).shift_up(2);
        if (k.intersects(z)) return; // cannot add both x and x*
      }
      break;
    }
    // Conditioning (Numerator)
    case 7 :
    case -7 : {
      if (!z.is_subset(a)) return;
      if (!z.is_subset(d | e)) return;
      if (z.intersects(trsb)) return;
      if (z == a) return;
      break;
    }
    // Conditioning (Denominator)
    case 8 : {
      if (z.intersects(a)) return;
      if (z.intersects(b)) return;
      if (z.intersects(trsb)) return;
      break;
    }
    // Conditioning (Denominator)
    case -8 : {
      if (!z.is_subset(b)) return;
      if (z.intersects(c)) return; // cannot divide by an intervention
      if (z.intersects(trsb)) return;
      break;
    }
    // Enable (rule 9) or disable (rule -9) missing data mechanisms,
    // i.e., case-by-general reasoning (for missing data problems)
    case 9 :
    case -9 : {
      j = (a & md_s) | ((b - c) & md_s);
      if (!z.is_subset(j)) return;
      if (z.intersects(d | e)) return;
      break;
    }
    // Exchange proxy with true variable (for missing data problems)
    case 10 : {
      u = a & md_p; // Get proxy variables p(u*|.)
      v = b & md_p; // Get proxy variables p(.|v*) or p(.|do(v*))
      if (!z.is_subset(u | v)) return; // z has to contain only proxy variables
      u = u & z;
      v = v & z;
      j = a & d; // Get missing data mechanisms that have been enabled p(r_j = 1|.)
      k = b & d; // Get missing data mechanisms that have been enabled p(.|r_k = 1)
      /* Check whether all proxy variables in z can be replaced
       * Proxy variables p(u*|.) can be replaced by true variables either if p(r_u = 1,u*|.) or p(u*|r_u* = 1)
       * However, proxies p(.|v*) can only be replaced by true variables if p(.|v*,r_v* = 1)
       */
      if (!u.is_subset((j | k).shift_up(1)) || !v.is_subset(k.shift_up(1))) return;
      break;
    }
    // Case-by-case reasoning (for missing data problems)
    case 11 : {
      if (!z.is_subset(a)) return; // z has to be in a
      if (!z.is_subset(d)) return; // z has to be enabled
      break;
    }
    // Case-by-case reasoning (for missing data problems)
    case -11 : {
      if (!z.is_subset(a)) return; // z has to be in a
      if (!z.is_subset(e)) return; // z has to be disabled
      break;
    }
    // General-by-case reasoning (RHS, for missing data problems)
    case 12 : {
      if (!z.is_subset(a)) return; // z has to be in a
      if (!z.is_subset(d) && !z.is_subset(e)) return; // z has to be enabled or disabled
      break;
    }
    // General-by-case reasoning (LHS, for missing data problems)
    case 13 :
    case -13 : {
      if (z.intersects(a)) return; // z intersection a = 0
      if (z.intersects(b)) return; // z intersection b = 0
      if (!z.is_subset(md_s)) return; // z must be a missing data mechanism
      if (!z.is_subset(lhs)) return; // all variables in z must be observed
      break;
    }
    // Unknown rule
    default : {
      return;
    }
  }
  info.valid = true;
  get_ruleinfo(ruleid, pp, z);
  if (md && removes_mechanism(ruleid, pp)) info.valid = false;
}

// A proxy variable is missing when its mechanism is disabled and equal to its
// true variable when its mechanism is enabled, so the joint distribution of
// the two is not positive. A rule that removes a mechanism from a
// distribution while the proxy variable remains in it (rules -1, -3, -7 and
// -8) would give a distribution that is defined for values of the proxy
// variable for which the original distribution is not, e.g., p(y|x*) from
// p(y|x*,r_x = 0) for the values of x* that are not missing.
bool dosearch::removes_mechanism(const int& ruleid, const p& pp) const {
  if (ruleid != -1 && ruleid != -3 && ruleid != -7 && ruleid != -8) return false;
  varset kept = info.to.a | info.to.b;
  varset removed = ((pp.a | pp.b) - kept) & md_s;
  return removed.shift_up(1).intersects(kept & md_p);
}

void dosearch::get_ruleinfo(const int& ruleid, const p& pp, const varset& z) {
  const varset& y = pp.a;
  const varset& xw = pp.b;
  const varset& x = pp.c;
  const varset& d = pp.d;
  const varset& e = pp.e;
  info.from = pp;
  info.rp.a = varset();
  info.ri.x = varset();
  info.ri.intv = false;
  info.enumerate = false;
  switch (ruleid) {
    // Insertion of observations
    case 1 : {
      info.to.a = y; info.to.b = xw | z; info.to.c = x; info.to.d = d; info.to.e = e;
      info.ri.x = y; info.ri.y = z; info.ri.u = xw; info.ri.v = x;
      return;
    }
    // Deletion of observations
    case -1 : {
      varset xwmz = xw - z;
      info.to.a = y; info.to.b = xwmz; info.to.c = x; info.to.d = d - z; info.to.e = e - z;
      info.ri.x = y; info.ri.y = z; info.ri.u = xwmz; info.ri.v = x;
      return;
    }
    // Exchange of observations to actions
    case 2 : {
      info.to.a = y; info.to.b = xw; info.to.c = x | z; info.to.d = d | ((z & md_s) - e); info.to.e = e;
      info.ri.x = y; info.ri.y = z; info.ri.u = xw; info.ri.v = x; info.ri.intv = true;
      return;
    }
    // Exchange of actions to observations
    case -2 : {
      varset xmz = x - z;
      info.to.a = y; info.to.b = xw; info.to.c = xmz; info.to.d = d; info.to.e = e;
      info.ri.x = y; info.ri.y = z; info.ri.u = xw; info.ri.v = xmz; info.ri.intv = true;
      return;
    }
    // Insertion of actions
    case 3 : {
      info.to.a = y; info.to.b = xw | z; info.to.c = x | z; info.to.d = d | (z & md_s); info.to.e = e;
      info.ri.x = y; info.ri.y = z; info.ri.u = xw; info.ri.v = x; info.ri.intv = true;
      return;
    }
    // Deletion of actions
    case -3 : {
      varset xwmz = xw - z;
      varset xmz = x - z;
      info.to.a = y; info.to.b = xwmz; info.to.c = xmz; info.to.d = d - z; info.to.e = e - z;
      info.ri.x = y; info.ri.y = z; info.ri.u = xwmz; info.ri.v = xmz; info.ri.intv = true;
      return;
    }
    // Marginalization
    case 4 : {
      info.to.a = y - z; info.to.b = xw; info.to.c = x; info.to.d = d; info.to.e = e;
      return;
    }
    // Conditioning
    case 5 : {
      info.to.a = y - z; info.to.b = xw | z; info.to.c = x; info.to.d = d; info.to.e = e;
      return;
    }
    // Product-rule
    case 6 : {
      varset xwmz = xw - z;
      info.to.a = y | z; info.to.b = xwmz; info.to.c = x; info.to.d = d;     info.to.e = e;
      info.rp.a = z;     info.rp.b = xwmz; info.rp.c = x; info.rp.d = d - y; info.rp.e = e - y;
      return;
    }
    // Product-rule
    case -6 : {
      info.to.a = y | z; info.to.b = xw;     info.to.c = x; info.to.d = d; info.to.e = e;
      info.rp.a = z;     info.rp.b = xw | y; info.rp.c = x; info.rp.d = d; info.rp.e = e;
      info.enumerate = true;
      return;
    }
    // Conditioning (Numerator) Have P(Y|X), Require P(Z|X), Obtain P(Y|X)/P(Z|X) = P(Y \ Z | X, Z)
    case 7 : {
      varset xwz = xw | z;
      info.to.a = y - z; info.to.b = xwz; info.to.c = x; info.to.d = d;       info.to.e = e;
      info.rp.a = z;     info.rp.b = xw;  info.rp.c = x; info.rp.d = d & xwz; info.rp.e = e & xwz;
      return;
    }
    // Conditioning (Numerator) Have P(Y|X), Require P(Z|X,Y\Z), Obtain P(Y|X)/P(Z|X,Y\Z) = P(Y \ Z | X)
    case -7 : {
      varset ymz = y - z;
      info.to.a = ymz; info.to.b = xw;       info.to.c = x; info.to.d = d - z; info.to.e = e - z;
      info.rp.a = z;   info.rp.b = xw | ymz; info.rp.c = x; info.rp.d = d;     info.rp.e = e;
      return;
    }
    // Conditioning (Denominator) Have P(Y|X), Require P(Y,Z|X), Obtain P(Y,Z|X)/P(Y|X) = P(Z|X,Y)
    case 8 : {
      info.to.a = z;     info.to.b = xw | y; info.to.c = x; info.to.d = d; info.to.e = e;
      info.rp.a = y | z; info.rp.b = xw;     info.rp.c = x; info.rp.d = d; info.rp.e = e;
      info.enumerate = true;
      return;
    }
    // Conditioning (Denominator) Have P(Y|X,Z), Require P(Y,Z|X), Obtain P(Y,Z|X)/P(Y|X,Z) = P(Z|X)
    case -8 : {
      varset xwmz = xw - z;
      info.to.a = z;     info.to.b = xwmz; info.to.c = x; info.to.d = d - y; info.to.e = e - y;
      info.rp.a = y | z; info.rp.b = xwmz; info.rp.c = x; info.rp.d = d;     info.rp.e = e;
      info.enumerate = true;
      return;
    }
    // Enable missing data mechanism (for missing data problems)
    // Case-by-general reasoning, Have P(Y|X), Obtain P(Y|X) for Z = 1
    case 9 : {
      info.to.a = y; info.to.b = xw; info.to.c = x; info.to.d = d | z; info.to.e = e;
      return;
    }
    // Disable missing data mechanism (for missing data problems)
    // Case-by-general reasoning, Have P(Y|X), Obtain P(Y|X) for Z = 0
    case -9 : {
      info.to.a = y; info.to.b = xw; info.to.c = x; info.to.d = d; info.to.e = e | z;
      return;
    }
    // Exchange proxy with true variable (for missing data problems)
    case 10 : {
      varset v = y & z;
      varset k = xw & z;
      info.to.a = (y - v) | v.shift_down(2); info.to.b = (xw - k) | k.shift_down(2); info.to.c = x; info.to.d = d; info.to.e = e;
      return;
    }
    // Case-by-case reasoning (for missing data problems)
    // Have P(Y|X) for Z = 1, Require P(Y|X) for Z = 0, Obtain P(Y|X)
    case 11 : {
      info.to.a = y; info.to.b = xw; info.to.c = x; info.to.d = d - z; info.to.e = e;
      info.rp.a = y; info.rp.b = xw; info.rp.c = x; info.rp.d = d - z; info.rp.e = e | z;
      return;
    }
    // Case-by-case reasoning (for missing data problems)
    // Have P(Y|X) for Z = 0, Require P(Y|X) for Z = 1, Obtain P(Y|X)
    case -11 : {
      info.to.a = y; info.to.b = xw; info.to.c = x; info.to.d = d;     info.to.e = e - z;
      info.rp.a = y; info.rp.b = xw; info.rp.c = x; info.rp.d = d | z; info.rp.e = e - z;
      return;
    }
    // General-by-case reasoning (RHS, for missing data problems)
    // Have P(Y|X) for Z = z, Require P(Y \ Z|X), Obtain P(Y|X) for Z = 1 - z
    case 12 : {
      varset dmz = d - z;
      varset emz = e - z;
      info.to.a = y;     info.to.b = xw; info.to.c = x; info.to.d = dmz | (e & z); info.to.e = emz | (d & z);
      info.rp.a = y - z; info.rp.b = xw; info.rp.c = x; info.rp.d = dmz;           info.rp.e = emz;
      return;
    }
    // General-by-case reasoning (LHS, for missing data problems)
    // Have P(Y|X), Require P(Y,Z|X) for Z = 0, Obtain P(Y,Z|X) for Z = 1
    case 13 : {
      info.to.a = y | z; info.to.b = xw; info.to.c = x; info.to.d = d | z; info.to.e = e;
      info.rp.a = y | z; info.rp.b = xw; info.rp.c = x; info.rp.d = d;     info.rp.e = e | z;
      return;
    }
    // General-by-case reasoning (LHS, for missing data problems)
    // Have P(Y|X), Require P(Y,Z|X) for Z = 1, Obtain P(Y,Z|X) for Z = 0
    case -13 : {
      info.to.a = y | z; info.to.b = xw; info.to.c = x; info.to.d = d;     info.to.e = e | z;
      info.rp.a = y | z; info.rp.b = xw; info.rp.c = x; info.rp.d = d | z; info.rp.e = e;
      return;
    }
  }
}

// dosearch_heuristic

dosearch_heuristic::dosearch_heuristic(const int& n_, const double& tl, const bool& bm, const bool& br, const bool& dd, const bool& da, const bool& fa, const bool& im, const bool& verb):dosearch(n_, tl, bm, br, dd, da, fa, im, verb) {
}

dosearch_heuristic::~dosearch_heuristic() {
}

distr& dosearch_heuristic::next_distribution(const int& i) {
  distr& top = *Q.top();
  Q.pop();
  return top;
}

void dosearch_heuristic::add_distribution(distr& nquery) {
  if (md) nquery.score = compute_score_md(nquery.pp);
  else nquery.score = compute_score(nquery.pp);
  add_to_list(nquery);
  ps[nquery.pp] = index;
  Q.push(&L[index]);
}

void dosearch_heuristic::add_known(const varset& a, const varset& b, const varset& c, const varset& d, const varset& e) {
  index++;
  distr iquery;
  iquery.rule_num = 0;
  iquery.pp.a = a; iquery.pp.b = b; iquery.pp.c = c; iquery.pp.d = d; iquery.pp.e = e;
  iquery.pa1 = 0;
  iquery.pa2 = 0;
  iquery.index = index;
  if (md) iquery.score = compute_score_md(iquery.pp);
  else iquery.score = compute_score(iquery.pp);
  add_to_list(iquery);
  ps[iquery.pp] = index;
  Q.push(&L[index]);
  if (equal_p(iquery.pp, target)) {
    trivial_id = true;
    target_dist.push_back(iquery);
  }
  if (md) lhs = lhs | a | (a & md_p).shift_down(2);
  else lhs = lhs | a;
  if (verbose) Rcpp::Rcout << "Adding known distribution: " << to_string(iquery.pp) << std::endl;
}

// Heuristic for search order
int dosearch_heuristic::compute_score(const p& pp) const {
  int score = 0;
  varset common_y = pp.a & target.a;
  varset common_x = pp.c & target.c;
  varset pp_w = pp.b - pp.c;
  varset target_w = target.b - target.c;
  varset common_z = pp_w & target_w;
  score += 10 * common_y.size();
  score -= 2 * (target.a - common_y).size();
  score += 5 * common_x.size();
  score -= 2 * (pp.c - common_x).size();
  score -= 2 * (target.c - common_x).size();
  score += 3 * common_z.size();
  score -= 1 * (pp_w - common_z).size();
  score -= 1 * (target_w - common_z).size();
  return(score);
}

// Heuristic for search order that takes proxy variables into account.
// Disabled missing data mechanisms do not allow proxy variables to be
// exchanged, so they are only preferred if they are disabled in the target.
int dosearch_heuristic::compute_score_md(const p& pp) const {
  int score = 0;
  varset pp_w = pp.b - pp.c;
  varset proxy_u = pp.a & md_p;
  varset proxy_w = pp_w & md_p;
  varset proxy_total = proxy_u | proxy_w;
  varset switch_total = ((pp.a | pp_w) & md_s) - pp.e;
  varset proxy_needed = switch_total.shift_up(1);
  varset switch_needed = proxy_total.shift_down(1);
  varset proxy_match = proxy_total & proxy_needed;
  varset proxy_mismatch = proxy_total - proxy_needed;
  varset switch_match = switch_total & switch_needed;
  varset switch_mismatch = switch_total - switch_needed;
  varset common_y = ((pp.a - proxy_u) | proxy_u.shift_down(2)) & target.a;
  varset common_x = pp.c & target.c;
  varset target_w = target.b - target.c;
  varset pp_w_true = (pp_w - proxy_w) | proxy_w.shift_down(2); // proxies mapped to true variables
  varset common_z = pp_w_true & target_w;
  varset common_e = pp.e & target.e;
  score += 10 * common_y.size();
  score += 6 * proxy_match.size();
  score += 6 * switch_match.size();
  score -= 2 * proxy_mismatch.size();
  score -= 2 * switch_mismatch.size();
  score -= 2 * (target.a - common_y).size();
  score += 6 * common_x.size();
  score -= 5 * (pp.c - common_x).size();
  score -= 2 * (target.c - common_x).size();
  score += 4 * common_z.size();
  score -= 2 * (pp_w_true - common_z).size();
  score -= 2 * (target_w - common_z).size();
  score += 10 * pp.d.size();
  score += 10 * common_e.size();
  score -= 2 * (pp.e - common_e).size();
  return(score);
}
