#include "dosearch.h"

dosearch::dosearch(const int& n_, const double& tl, const bool& bm, const bool& br, const bool& dd, const bool& da, const bool& fa, const bool& im, const bool& verb):search(n_, tl, bm, br, dd, da, fa, im, verb) {
}

dosearch::~dosearch() {
}

void dosearch::set_target(const int& a, const int& b, const int& c, const int& d) {
  target.a = a; target.b = b; target.c = c; target.d = d;
  if (verbose) Rcpp::Rcout << "Setting target: " << to_string(target) << std::endl;
}

void dosearch::set_graph(dcongraph* g_) {
  g = g_;
}

void dosearch::set_options(const std::vector<int>& rule_vec) {
  trivial_id = false;
  index = 0;
  lhs = 0;

  md_s = g->get_md_switches();
  md_p = g->get_md_proxies();
  md_t = md_s >> 1;
  md = md_s > 0;

  tr = g->get_trnodes();
  sb = g->get_sbnodes();
  trsb = tr | sb;

  if (rule_vec.size() > 0) rules = rule_vec;
  else {
    // Additional rules for missing data problems
    if (md) {
      if (improve) rules = {4, 5, 9, 10, -2, -3, 2, 3, 6, -6, 7, -7, 8, -8};
      else rules = {4, 5, 9, 10, -1, -2, -3, 1, 2, 3, 6, -6, 7, -7, 8, -8};
    } else {
      if (improve) rules = {4, 5, -2, -3, 2, 3, 6, -6};
      else rules = {4, 5, -1, -2, -3, 1, 2, 3, 6, -6};
    }
  }

  rule_times = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
}

void dosearch::set_labels(const Rcpp::StringVector& lab) {
  labels = std::vector<std::string>(2*n);
  for (int i = 0; i < n; i++) {
    labels[i] = lab(i);
    labels[i + n] = "I(" + lab(i) + ")";
  }
}

void dosearch::set_md_symbol(const char& mds) {
  md_sym = mds;
}

void dosearch::add_known(const int& a, const int& b, const int& c, const int& d) {
  index++;
  p pp;
  distr iquery;
  pp.a = a; pp.b = b; pp.c = c; pp.d = d;
  iquery.rule_num = 0;
  iquery.pp = pp;
  iquery.pa1 = 0;
  iquery.pa2 = 0;
  iquery.index = index;
  iquery.primitive = true;
  iquery.score = 0;
  L[index] = iquery;
  ps[make_key(pp)] = index;
  if (equal_p(pp, target)) {
    trivial_id = true;
    target_dist.push_back(L[index]);
  }
  if (md) lhs = (lhs | a) | ((a & md_p) >> 2);
  else lhs = lhs | a;
  if (verbose) Rcpp::Rcout << "Adding known distribution: " << to_string(pp) << std::endl;
}

bool dosearch::check_trivial() {
  if ((lhs & target.a) == target.a) return false;
  return true;
}

void dosearch::assign_candidate(distr& required) {
  info.to.d = required.pp.d & (info.to.a | info.to.b | info.to.c);
}

distr& dosearch::next_distribution(const int& i) {
  return L[i];
}

void dosearch::derive_distribution(const distr& iquery, const distr& required, const int& ruleid, int& remaining, bool& found) {
  index++;
  distr nquery;
  nquery.index = index;
  nquery.pp = info.to;
  nquery.primitive = is_primitive(iquery.primitive, required.primitive, ruleid);
  nquery.pa1 = iquery.index;
  nquery.pa2 = 0;
  nquery.rule_num = ruleid;

  if (info.rp.a > 0) {
    nquery.pa2 = required.index;
  }
  if (equal_p(info.to, target)) {
    if (verbose) {
      if (info.rp.a > 0) Rcpp::Rcout << "Derived: " << to_string(info.to) << " from " << to_string(info.from) << " and " << to_string(info.rp) << " using rule: " << std::to_string(ruleid) << std::endl;
      else Rcpp::Rcout << "Derived: " << to_string(info.to) << " from " << to_string(info.from) << " using rule: " << std::to_string(ruleid) << std::endl;
      Rcpp::Rcout << "Target found" << std::endl;
      Rcpp::Rcout << "Index = " << index << std::endl;
    }
    target_dist.push_back(nquery);
    found = true;
  } else {
    if (verbose) {
      if (info.rp.a > 0) Rcpp::Rcout << "Derived: " << to_string(info.to) << " from " << to_string(info.from) << " and " << to_string(info.rp) << " using rule: " << std::to_string(ruleid) << std::endl;
      else Rcpp::Rcout << "Derived: " << to_string(info.to) << " from " << to_string(info.from) << " using rule: " << std::to_string(ruleid) << std::endl;
    }
    remaining++;
    add_distribution(nquery);
  }
}

void dosearch::add_distribution(distr& nquery) {
  L[index] = nquery;
  ps[make_key(nquery.pp)] = index;
}

void dosearch::enumerate_candidates() {
  int asw = (info.rp.a - (info.rp.a & info.from.a)) & md_s;
  int exist = ps[make_key(info.rp)];
  if (exist > 0) {
    candidates.push(exist);
  }
  if (asw > 0) {
    int d_inc, i_set;
    p rq;
    rq.a = info.rp.a;
    rq.b = info.rp.b;
    rq.c = info.rp.c;
    std::vector<int> elems;
    std::vector<int> total;
    int e = 0;
    for (int i = 1; i <= n; i++) {
      i_set = unary(i);
      if ((i_set & asw) == i_set) {
        elems.push_back(i_set);
        e++;
      }
    }
    for (int i = 0; i <= full_set(e); i++) {
      d_inc = 0;
      for (int j = 1; j <= e; j++) {
        if ((unary(j) & i) > 0) d_inc += elems[j-1];
      }
      rq.d = info.rp.d + d_inc;
      exist = ps[make_key(rq)];
      if (exist > 0) {
        candidates.push(exist);
      }
    }
  }
}

bool dosearch::separation_criterion() {
  return g->dsep_set(info.ri.x, info.ri.y, info.ri.u, info.ri.v);
}

int dosearch::rule_limit(const int& ruleid, const unsigned int& z_size) {
  return z_size;
}

bool dosearch::is_primitive(const bool& pa1_primitive, const bool& pa2_primitive, const int& ruleid) {
  if (pa1_primitive && pa2_primitive) {
    if (ruleid * ruleid < 16) return false;
    return true;
  }
  return false;
}

std::string dosearch::derive_formula(distr& dist) {
  std::string formula = "";
  if (dist.pa1 > 0) {
    int r = dist.rule_num;
    int rsq = r * r;
    distr& pa1 = L[dist.pa1];
    std::string paf1 = derive_formula(pa1);
    if (dist.pa2 > 0) {
      distr& pa2 = L[dist.pa2];
      std::string paf2 = derive_formula(pa2);
      if (dist.primitive) formula = to_string(dist.pp);
      else {
        if (rsq == 36) {
          if (paf1.length() < paf2.length()) formula = "\\left(" + paf1 + paf2 + "\\right)";
          else formula = "\\left(" + paf2 + paf1 + "\\right)";
        } else if (rsq == 49) {
          formula = "\\frac{" + paf1 + "}{" + paf2 + "}";
        } else if (rsq == 64) {
          formula = "\\frac{" + paf2 + "}{" + paf1 + "}";
        }
      }
    } else {
      if (rsq < 16) {
        formula = paf1;
      } else {
        if (dist.primitive) formula = to_string(dist.pp);
        else {
          if (rsq == 25) {
            formula = "\\frac{" + paf1 + "}{\\sum_{" + dec_to_text(dist.pp.a, 0) + "}" + paf1 + "}";
          } else if (rsq == 16) {
            formula =  "\\sum_{" + dec_to_text(pa1.pp.a - dist.pp.a, 0) + "}" + paf1;
          } else if (rsq >= 81) {
            formula = paf1;
          }
        }
      }
    }
  } else {
    formula = to_string(dist.pp);
  }
  return(formula);
}

std::string dosearch::rule_name(const int& rule_num) const {
  std::string rn = "";
  switch (rule_num) {
    case 1  : rn = "R1";
    case -1 : rn = "R1";
    case 2  : rn = "R2";
    case -2 : rn = "R2";
    case 3  : rn = "R3";
    case -3 : rn = "R3";
    case 4  : rn = "M";
    case 5  : rn = "C";
    case 6  : rn = "P";
    case -6 : rn = "P";
    case 7  : rn = "D";
    case -7 : rn = "D";
    case 8  : rn = "D";
    case -8 : rn = "D";
    case 9  : rn = "A";
    case 10 : rn = "EX";
  }
  return rn;
}

std::string dosearch::dec_to_text(const int& dec, const int& enabled) const {
  if (dec == 0) return("");
  std::string s = "";
  int first = 0;
  for (int i = 1; i <= n; i++) {
    if (in_set(i, dec)) {
      first = i;
      if (in_set(i, enabled)) s += labels[i-1] + " = " + md_sym;
      else s += labels[i-1];
      break;
    }
  }
  if (first > 0) {
    for (int i = first + 1; i <= n; i++) {
      if (in_set(i, dec)) {
        s += ",";
        if (in_set(i, enabled)) s += labels[i-1] + " = " + md_sym;
        else s += labels[i-1];
      }
    }
  }
  return s;
}

std::string dosearch::to_string(const p& pp) const {
  int a = pp.a;
  int b = pp.b;
  int c = pp.c;
  int d = pp.d;
  std::string s = "";
  s += "p(" + dec_to_text(a, d);
  if (b != 0) s += "|";
  if (c != 0) s += "do(" + dec_to_text(c, d) + ")";
  b = b & ~c;
  if (b != 0) {
    if (c != 0) s += ",";
    s += dec_to_text(b, d);
  }
  s += ")";
  return s;
}

bool dosearch::valid_rule(const int& ruleid, const int& a, const int& b, const int& c, const int& d, const bool &primi) const {
  switch (ruleid) {
    case -1 : {
      // there must be observations to delete
      if ((b - c) == 0) return false;
      else return true;
    }
    case -2 : {
      // there must be actions to exchange
      if (c == 0) return false;
      else return true;
    }
    case -3 : {
      // there must be actions to delete
      if (c == 0) return false;
      else return true;
    }
    case 2 : {
      // there must be observations to exchange
      if ((b - c) == 0) return false;
      else return true;
    }
    case 4 : {
      // there must be other variables
      if (set_size(a) == 1) return false;
      else return true;
    }
    case 5 : {
      // there must be other variables
      if (set_size(a) == 1) return false;
      else return true;
    }
    case 6 : {
      // there must be conditioning variables
      if ((b - c) == 0) return false;
      else return true;
    }
    case 7 : {
      // there must be other variables
      if (set_size(a) == 1) return false;
      // there must be enabled missing data mechanisms
      if ((a & d) == 0) return false;
      else return true;
    }
    case -7 : {
      // there must be other variables
      if (set_size(a) == 1) return false;
      // there must be enabled missing data mechanisms
      if ((a & d) == 0) return false;
      else return true;
    }
    case 8 : {
      // Every variable must be an enabled missing data mechanism
      // Otherwise ordinary conditioning via rule 5 works
      if ((a & d) != a) return false;
      else return true;
    }
    case -8 : {
      // Every variable must be an enabled missing data mechanism
      // Otherwise ordinary conditioning via rule 5 works
      if ((a & d) != a) return false;
      else return true;
    }
    case 9 : {
      // There must be missing data mechanisms
      if (((a | b) & md_s) == 0) return false;
      else return true;
    }
    case 10 : {
      // There must be enabled missing data mechanisms and proxies
      if (d == 0) return false;
      if (((a | b) & md_p) == 0) return false;
      else return true;
    }
    default : {
      return true;
    }
  }
  return true;
}

void dosearch::apply_rule(const int &ruleid, const int &a, const int &b, const int &c, const int &d, const int &z) {
  int u, v, j, k;
  info.valid = false;
  switch (ruleid) {
    // Insertion of observations
    case 1 : {
      if ((z & a) != 0) return;
      if ((z & b) != 0) return;
      if (md) {
        j = ((a | b) & md_p) >> 2;
        if ((j & z) != 0) return; // cannot add x if x* exists
        k = ((a | b) & md_t) << 2;
        if ((k & z) != 0) return; // cannot add x* if x exists
        j = (z & md_p) >> 2;
        if ((j & z) != 0) return; // cannot add both x and x*
        k = (z & md_t) << 2;
        if ((k & z) != 0) return;// cannot add both x and x*
      }
      break;
    }
    // Deletion of observations
    case -1 : {
      if ((z & a) != 0) return;
      if ((z & c) != 0) return;
      if ((z & (b - c)) != z) return;
      break;
    }
    // Exchange actions to observations
    case -2 : {
      if ((z & a) != 0) return;
      if ((z & (b - c)) != 0) return;
      if ((z & c) != z) return;
      if ((z & md_p) != 0) return;
      break;
    }
    // Exchange observations to actions
    case 2 : {
      if ((z & a) != 0) return;
      if ((z & c) != 0) return;
      if ((z & b) != z) return;
      if ((z & md_p) != 0) return;
      break;
    }
    // Deletion of actions
    case -3 : {
      if ((z & a) != 0) return;
      if ((z & (b - c)) != 0) return;
      if ((z & c) != z) return;
      if ((z & md_p) != 0) return;
      break;
    }
    // Insertion of actions
    case 3 : {
      if ((z & a) != 0) return;
      if ((z & b) != 0) return;
      if (md) {
        if ((z & md_p) != 0) return; // z intersection m = 0
        j = ((a | b) & md_p) >> 2;
        if ((j & z) != 0) return; // cannot add x if x* exists
        k = ((a | b) & md_t) << 2;
        if ((j & z) != 0) return; // cannot add x* if x exists
        j = (z & md_p) >> 2;
        if ((j & z) != 0) return; // cannot add both x and x*
        k = (z & md_t) << 2;
        if ((k & z) != 0) return; // cannot add both x and x*
      }
      break;
    }
    // Marginalisation
    case 4 : {
      if ((z & a) != z) return;
      if (z == a) return;
      if ((z & trsb) != 0) return;
      if (md) {
        j = a & md_s;
        if ((z & j) != 0 && (z & d) != 0) return; // Cannot marginalize over enabled switch
      }
      break;
    }
    // Conditioning
    case 5 : {
      if ((z & a) != z) return;
      if (z == a) return;
      if ((z & trsb) != 0) return;
      if (md) {
        j = a & md_s;
        if ((a & j) != 0 && (a & d) != 0) return; // Cannot condition if switch is enabled
      }
      break;
    }
    // Product rule
    case 6 : {
      if ((z & b) != z) return;
      if ((z & c) != 0) return;
      if ((z & trsb) != 0) return;
      break;
    }
    // Product rule
    case -6 : {
      // z cannot be in any of the sets
      if ((z & a) != 0) return; // z intersection y = 0
      if ((z & b) != 0) return; // z intersection x = 0
      if ((z & trsb) != 0) return; // z intersection s = 0
      if (md) {
        j = ((a | b) & md_p) >> 2;
        if ((j & z) != 0) return; // cannot add x if x* exists
        k = ((a | b) & md_t) << 2;
        if ((k & z) != 0) return; // cannot add x* if x exists
        j = (z & md_p) >> 2;
        if ((j & z) != 0) return; // cannot add both x and x*
        k = (z & md_t) << 2;
        if ((k & z) != 0) return; // cannot add both x and x*
      }
      break;
    }
    // Conditioning (Numerator)
    case 7 : {
      if ((z & a) != z) return;
      if ((z & d) != z) return;
      if ((z & trsb) != 0) return;
      if (z == a) return;
      break;
    }
    // Conditioning (Numerator)
    case -7 : {
      if ((z & a) != z) return;
      if ((z & d) != z) return;
      if ((z & trsb) != 0) return;
      if (z == a) return;
      break;
    }
    // Conditioning (Denominator)
    case 8 : {
      if ((z & a) != 0) return;
      if ((z & b) != 0) return;
      if ((z & trsb) != 0) return;
      break;
    }
    // Conditioning (Denominator)
    case -8 : {
      if ((z & b) != z) return;
      if ((z & trsb) != 0) return;
      break;
    }
    // Enable missing data mechanism (for missing data problems)
    case 9 : {
      j = (a & md_s) | ((b - c) & md_s);
      if ((z & j) != z) return;
      if ((z & d) != 0) return;
      break;
    }
    // Exchange proxy with true variable (for missing data problems)
    case 10 : {
      // if (!iquery.primitive) return;
      u = a & md_p; // Get proxy variables p(u*|.)
      v = b & md_p; // Get proxy variables p(.|v*) or p(.|do(v*))
      if ((z & (u | v)) != z) return; // z has to contain only proxy variables
      u = u & z;
      v = v & z;
      j = a & d; // Get missing data mechanisms that have been enabled p(r_j = 0|.)
      k = b & d; // Get missing data mechanisms that have been enabled p(.|r_k = 0)
      /* Check whether all proxy variables in z can be replaced
       * Proxy variables p(u*|.) can be replaced by true variables either if p(r_u = 0,u*|.) or p(u*|r_u* = 0)
       * However, proxies p(.|v*) can only be replaced by true variables if p(.|v*,r_v* = 0)
       */
      if ((((j | k) << 1) & u) != u || ((k << 1) & v) != v) return;
      break;
    }
  }
  info.valid = true;
  get_ruleinfo(ruleid, a, b, c, d, z);
}

void dosearch::get_ruleinfo(const int& ruleid, const int& y, const int& xw, const int& x, const int& d, const int& z) {
  info.from.a = y; info.from.b = xw; info.from.c = x; info.from.d = d;
  switch (ruleid) {
    // Insertion of observations
    case 1 : {
      info.to.a = y; info.to.b = xw | z; info.to.c = x; info.to.d = d;
      info.rp.a = 0;
      info.ri.x = y; info.ri.y = z; info.ri.u = xw; info.ri.v = x;
      info.enumerate = false;
      return;
    }
    // Deletion of observations
    case -1 : {
      int xwmz = xw & ~z;
      info.to.a = y; info.to.b = xwmz; info.to.c = x; info.to.d = d & ~z;
      info.rp.a = 0;
      info.ri.x = y; info.ri.y = z; info.ri.u = xwmz; info.ri.v = x;
      info.enumerate = false;
      return;
    }
    // Exchange of observations to actions
    case 2 : {
      info.to.a = y; info.to.b = xw; info.to.c = x | z; info.to.d = d | (z & md_s);
      info.rp.a = 0;
      info.ri.x = y; info.ri.y = z << n; info.ri.u = xw; info.ri.v = x;
      info.enumerate = false;
      return;
    }
    // Exchange of actions to observations
    case -2 : {
      int xmz = x & ~z;
      info.to.a = y; info.to.b = xw; info.to.c = xmz; info.to.d = d;
      info.rp.a = 0;
      info.ri.x = y; info.ri.y = z << n; info.ri.u = xw; info.ri.v = xmz;
      info.enumerate = false;
      return;
    }
    // Insertion of actions
    case 3 : {
      info.to.a = y; info.to.b = xw | z; info.to.c = x | z; info.to.d = d | (z & md_s);
      info.rp.a = 0;
      info.ri.x = y; info.ri.y = z << n; info.ri.u = xw; info.ri.v = x;
      info.enumerate = false;
      return;
    }
    // Deletion of actions
    case -3 : {
      int xwmz = xw & ~z;
      int xmz = x & ~z;
      info.to.a = y; info.to.b = xwmz; info.to.c = xmz; info.to.d = d & ~z;
      info.rp.a = 0;
      info.ri.x = y; info.ri.y = z << n; info.ri.u = xwmz; info.ri.v = xmz;
      info.enumerate = false;
      return;
    }
    // Marginalization
    case 4 : {
      info.to.a = y & ~z; info.to.b = xw; info.to.c = x; info.to.d = d;
      info.rp.a = 0;
      info.ri.x = 0;
      info.enumerate = false;
      return;
    }
    // Conditioning
    case 5 : {
      info.to.a = y & ~z; info.to.b = xw | z; info.to.c = x; info.to.d = d;
      info.rp.a = 0;
      info.ri.x = 0;
      info.enumerate = false;
      return;
    }
    // Product-rule
    case 6 : {
      int xwmz = xw & ~z;
      info.to.a = y | z; info.to.b = xwmz; info.to.c = x; info.to.d = d;
      info.rp.a = z;     info.rp.b = xwmz; info.rp.c = x; info.rp.d = d & ~y;
      info.ri.x = 0;
      info.enumerate = false;
      return;
    }
    // Product-rule
    case -6 : {
      info.to.a = y | z; info.to.b = xw;     info.to.c = x; info.to.d = d;
      info.rp.a = z;     info.rp.b = xw | y; info.rp.c = x; info.rp.d = d;
      info.ri.x = 0;
      info.enumerate = true;
      return;
    }
    // Conditioning (Numerator) Have P(Y|X), Require P(Z|X), Obtain P(Y|X)/P(Z|X) = P(Y \ Z | X, Z)
    case 7 : {
      int xwz = xw | z;
      info.to.a = y & ~z; info.to.b = xwz; info.to.c = x; info.to.d = d;
      info.rp.a = z;      info.rp.b = xw;  info.rp.c = x; info.rp.d = d & xwz;
      info.ri.x = 0;
      info.enumerate = false;
      return;
    }
    // Conditioning (Numerator) Have P(Y|X), Require P(Z|X,Y\Z), Obtain P(Y|X)/P(Z|X,Y\Z) = P(Y \ Z | X)
    case -7 : {
      int ymz = y & ~z;
      info.to.a = ymz; info.to.b = xw;       info.to.c = x; info.to.d = d & ~z;
      info.rp.a = z;   info.rp.b = xw | ymz; info.rp.c = x; info.rp.d = d;
      info.ri.x = 0;
      info.enumerate = false;
      return;
    }
    // Conditioning (Denominator) Have P(Y|X), Require P(Y,Z|X), Obtain P(Y,Z|X)/P(Y|X) = P(Z|X,Y)
    case 8 : {
      info.to.a = z;     info.to.b = xw | y; info.to.c = x; info.to.d = d;
      info.rp.a = y | z; info.rp.b = xw;     info.rp.c = x; info.rp.d = d;
      info.ri.x = 0;
      info.enumerate = true;
      return;
    }
    // Conditioning (Denominator) Have P(Y|X,Z), Require P(Y,Z|X), Obtain P(Y,Z|X)/P(Y|X,Z) = P(Z|X)
    case -8 : {
      int xwmz = xw & ~z;
      info.to.a = z;     info.to.b = xwmz; info.to.c = x; info.to.d = d & ~y;
      info.rp.a = y | z; info.rp.b = xwmz; info.rp.c = x; info.rp.d = d;
      info.ri.x = 0;
      info.enumerate = true;
      return;
    }
    // Enable missing data mechanism (for missing data problems)
    case 9 : {
      info.to.a = y; info.to.b = xw; info.to.c = x; info.to.d = d | z;
      info.rp.a = 0;
      info.ri.x = 0;
      info.enumerate = false;
      return;
    }
      // Exchange proxy with true variable (for missing data problems)
    case 10 : {
      int v = y & z;
      int k = xw & z;
      info.to.a = (y & ~v) | (v >> 2); info.to.b = (xw & ~k) | (k >> 2); info.to.c = x; info.to.d = d;
      info.rp.a = 0;
      info.ri.x = 0;
      info.enumerate = false;
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
  nquery.score = compute_score(nquery.pp);
  L[index] = nquery;
  ps[make_key(nquery.pp)] = index;
  Q.push(&L[index]);
}

void dosearch_heuristic::add_known(const int& a, const int& b, const int& c, const int& d) {
  index++;
  p pp;
  distr iquery;
  pp.a = a; pp.b = b; pp.c = c; pp.d = d;
  iquery.rule_num = 0;
  iquery.pp = pp;
  iquery.pa1 = 0;
  iquery.pa2 = 0;
  iquery.index = index;
  iquery.primitive = true;
  iquery.score = 0;
  L[index] = iquery;
  ps[make_key(pp)] = index;
  Q.push(&L[index]);
  if (equal_p(pp, target)) {
    trivial_id = true;
    target_dist.push_back(L[index]);
  }
  if (md) lhs = (lhs | a) | ((a & md_p) >> 2);
  else lhs = lhs | a;
  if (verbose) Rcpp::Rcout << "Adding known distribution: " << to_string(pp) << std::endl;
}

// Heuristic for search order
int dosearch_heuristic::compute_score(const p& pp) const {
  int score = 0;
  int common_y = pp.a & target.a;
  int common_x = pp.c & target.c;
  int pp_w = pp.b - pp.c;
  int target_w = target.b - target.c;
  int common_z = pp_w & target_w;

  score += 10 * set_size(common_y);
  score -= 2 * set_size(target.a - common_y);
  score += 5 * set_size(common_x);
  score -= 2 * set_size(pp.c - common_x);
  score -= 2 * set_size(target.c - common_x);
  score += 3 * set_size(common_z);
  score -= 1 * set_size(pp_w - common_z);
  score -= 1 * set_size(target_w - common_z);

  return(score);
}

// Heuristic for search order that takes proxy variables into account
int dosearch_heuristic::compute_score_md(const p& pp) const {
  int score = 0;
  int pp_w = pp.b - pp.c;

  int proxy_u = pp.a & md_p;
  int proxy_w = pp_w & md_p;

  int proxy_total = proxy_u | proxy_w;
  int switch_total = (pp.a | pp_w) & md_s;
  int proxy_needed = switch_total << 1;
  int switch_needed = proxy_total >> 1;

  int proxy_match = proxy_total & proxy_needed;
  int proxy_mismatch = proxy_total - proxy_needed;
  int switch_match = switch_total & switch_needed;
  int switch_mismatch = switch_total - switch_needed;
  int common_y = ((pp.a - proxy_u) | (proxy_u >> 2)) & target.a;
  int common_x = pp.c & target.c;
  int target_w = target.b - target.c;
  int common_z = ((pp_w - proxy_w) | (proxy_w >> 2)) & target_w;

  score += 10 * set_size(common_y);
  score += 6 * set_size(proxy_match);
  score += 6 * set_size(switch_match);
  score -= 2 * set_size(proxy_mismatch);
  score -= 2 * set_size(switch_mismatch);
  score -= 2 * set_size(target.a - common_y);
  score += 6 * set_size(common_x);
  score -= 5 * set_size(pp.c - common_x);
  score -= 2 * set_size(target.c - common_x);
  score += 4 * set_size(common_z);
  score -= 2 * set_size(pp_w - common_z);
  score -= 2 * set_size(target_w - common_z);
  score += 10 * set_size(pp.d);

  return(score);
}
