#include "csisearch.h"

using namespace std;

csisearch::csisearch(const int& n_, const bool& dd, const bool& da, const bool& fa, const bool& verb):search(n_, dd, da, fa, verb) {
}

csisearch::~csisearch() {
}

void csisearch::set_target(const int& a, const int& b, const int& c, const int& d) {
    target.a = a; target.b = b; target.c = c; target.d = d;
    if ( verbose ) Rcpp::Rcout << "Setting target: " << to_string(target) << endl;
}

void csisearch::set_graph(ldag* g_) {
    g = g_;
}

void csisearch::set_options(const vector<int>& r) {
    trivial_id = false;
    format_do = true;
    index = 0;
    lhs = 0;

    if ( r.size() > 0 ) rules = r;
    else {
        rules = {0, 1, -3, 3, -4, 4, -5, 5, 6, -7, 7, -8, 8, -2, 2};
    }

    rule_times = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};
}

void csisearch::set_labels(const Rcpp::StringVector& lab) {
    labels = vector<string>(n);
    for ( int i = 0; i < n; i++ ) {
        labels[i] = lab(i);
    }
}

void csisearch::set_contexts(const int& con) {
    con_vars = con;
}

void csisearch::set_interventions(const int& intv) {
    intv_vars = intv;
}


void csisearch::add_known(const int& a, const int& b, const int& c, const int& d) {
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
    if ( equal_p(pp, target) ) {
        trivial_id = true;
        target_dist.push_back(L[index]);
    }
    lhs = lhs | a;
    if ( verbose ) Rcpp::Rcout << "Adding known distribution: " << to_string(pp) << endl;
}

bool csisearch::check_trivial() {
    if ( (lhs & target.a) == target.a ) return false;
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
    nquery.pp = info.to;
    nquery.primitive = is_primitive(iquery.primitive, required.primitive, ruleid);
    nquery.pa1 = iquery.index;
    nquery.pa2 = 0;
    nquery.rule_num = ruleid;

    if ( info.rp.a > 0 ) {
        nquery.pa2 = required.index;
    }

    if ( equal_p(info.to, target) ) {
        if ( verbose ) {
            if ( info.rp.a > 0 ) Rcpp::Rcout << "Derived: " << to_string(info.to) << " from " << to_string(info.from) << " and " << to_string(info.rp) << " using rule: " << std::to_string(ruleid) << endl;
            else Rcpp::Rcout << "Derived: " << to_string(info.to) << " from " << to_string(info.from) << " using rule: " << std::to_string(ruleid) << endl;
            Rcpp::Rcout << "!!!! Managed to hit the target !!!!" << endl;
            Rcpp::Rcout << "index = " << index << endl;
        }
        target_dist.push_back(nquery);
        found = true;
    } else {
        if ( verbose ) {
            if ( info.rp.a > 0 ) Rcpp::Rcout << "Derived: " << to_string(info.to) << " from " << to_string(info.from) << " and " << to_string(info.rp) << " using rule: " << std::to_string(ruleid) << endl;
            else Rcpp::Rcout << "Derived: " << to_string(info.to) << " from " << to_string(info.from) << " using rule: " << std::to_string(ruleid) << endl;
        }
        index++;
        remaining++;
        nquery.index = index;
        add_distribution(nquery);
    }
}

void csisearch::add_distribution(distr& nquery) {
    L[index] = nquery;
    ps[make_key(nquery.pp)] = index;
}

void csisearch::enumerate_candidates() {
    int acon = info.rp.a & con_vars;
    int exist = ps[make_key(info.rp)];
    if ( exist > 0 ) {
        candidates.push(exist);
    }
    if ( acon > 0 ) {
        int zero = 0;
        int one = 0;
        int u_inc, v_inc, slice, i_set, z, o;
        bool valid;
        p rq;
        rq.a = info.rp.a;
        rq.b = info.rp.b;
        vector<int> elems;
        vector<int> total;
        int e = 0;
        for ( int i = 1; i <= n; i++ ) {
            i_set = unary(i);
            if ( (i_set & acon) == i_set ) {
                elems.push_back(i_set);
                e++;
            }
        }
        for ( int j = 1; j <= e; j++ ) {
            z = unary(2 * j - 1);
            o = unary(2 * j);
            zero += z;
            one += o;
            total.push_back(z | o);
        }
        for ( int i = 1; i < full_set(2*e); i++ ) {
            valid = true;
            u_inc = 0;
            v_inc = 0;
            for ( int j = 0; j < e; j++ ) {
                slice = i & total[j];
                if ( slice == total[j] ) {
                    valid = false;
                    break;
                }
                if ( (zero & slice) > 0 ) u_inc += elems[j];
                if ( (one & slice) > 0 ) v_inc += elems[j];
            }
            if ( valid ) {
                rq.c = info.rp.c + u_inc;
                rq.d = info.rp.d + v_inc;
                exist = ps[make_key(rq)];
                if ( exist > 0 ) {
                    candidates.push(exist);
                }
            }
        }
    }
}

bool csisearch::separation_criterion() {
    int u = info.ri.u & con_vars;
    int v = info.ri.v & con_vars;
    int iv = info.ri.v & intv_vars;
    return g->csi_criterion(info.ri.x, info.ri.y, info.ri.z, u, v, iv, u | v);
}

int csisearch::rule_limit(const int& ruleid, const unsigned int& z_size) {
    if ( ruleid * ruleid > 3 ) return n;
    else return z_size;
}

bool csisearch::is_primitive(const bool& pa1_primitive, const bool& pa2_primitive, const int& ruleid) {
    if ( pa1_primitive && pa2_primitive ) {
        int r = ruleid * ruleid;
        if ( r == 9 || r == 16 ) return false;
        return true;
    }
    return false;
}

string csisearch::derive_formula(distr& dist) {
    string formula = "";
    if ( dist.pa1 > 0 ) {
        int rsq = dist.rule_num * dist.rule_num;
        distr& pa1 = L[dist.pa1];
        string paf1 = derive_formula(pa1);
        if ( dist.pa2 > 0 ) {
            distr& pa2 = L[dist.pa2];
            string paf2 = derive_formula(pa2);
            if ( dist.primitive ) formula = to_string(dist.pp);
            else {
                if ( rsq == 4 ) {
                    if ( paf1.length() < paf2.length() ) formula = paf1 + "*" + paf2;
                    else formula = paf2 + "*" + paf1;
                } else if ( rsq == 25 ) {
                    formula = "[" + paf1 + " /\\ " + paf2 + "]";
                } else if ( rsq == 36 ) {
                    formula = "[" + paf2 + " - " + paf1 + "]";
                } else if ( rsq == 49 ) {
                    formula = "[" + paf1 + " - " + paf2 + "]";
                }
            }
        } else {
            if ( rsq == 9 || rsq == 16 ) {
                formula = paf1;
            } else {
                if ( dist.primitive ) formula = to_string(dist.pp);
                else {
                    if ( rsq == 0 ) {
                        formula =  "[sum_{" + dec_to_text(pa1.pp.a - dist.pp.a, 0, 0) + "}" + paf1 + "]";
                    } else if ( rsq == 1 ) {
                        formula = "[[" + paf1 + "]/[sum_{" + dec_to_text(dist.pp.a, 0, 0) + "} " + paf1 + "]]";
                    } else if ( rsq == 64 ) {
                        if ( dist.pp.c - pa1.pp.c > 0 ) {
                            formula = "[" + paf1 + "]_{" + dec_to_text(dist.pp.c - pa1.pp.c, 0, 0) + " = 0}";
                        }
                        else if ( dist.pp.d - pa1.pp.d > 0 ) {
                            formula = "[" + paf1 + "]_{" + dec_to_text(dist.pp.d - pa1.pp.d, 0, 0) + " = 1}";
                        }
                    }
                }
            }
        }
    } else {
        formula = to_string(dist.pp);
    }
    return formula;
}

string csisearch::rule_name(const int& rule_num) const {
    switch ( rule_num ) {

        case 0  : return "M";
        case 1  : return "C";
        case 2  : return "P";
        case -2 : return "P";
        case 3  : return "I+";
        case -3 : return "I-";
        case 4  : return "I+0";
        case -4 : return "I+1";
        case 5  : return "CbC";
        case -5 : return "CbC";
        case 6  : return "GbC";
        case 7  : return "GbC";
        case -7 : return "GbC";
        case 8  : return "CbG";
        case -8 : return "CbG";

    }

    return "";
}

string csisearch::dec_to_text(const int& dec, const int& zero, const int& one) const {
    if ( dec == 0 ) return("");
    string s = "";
    int first = 0;
    for ( int i = 1; i <= n; i++ ) {
        if ( in_set(i, dec) ) {
            first = i;
            if ( in_set(i, zero) ) s += labels[i-1] + " = 0";
            else if ( in_set(i, one) ) s += labels[i-1] + " = 1";
            else s += labels[i-1];
            break;
        }
    }
    if ( first > 0 ) {
        for ( int i = first + 1; i <= n; i++) {
            if ( in_set(i, dec) ) {
                s += ",";
                if ( in_set(i, zero) ) s += labels[i-1] + " = 0";
                else if ( in_set(i, one) ) s += labels[i-1] + " = 1";
                else s += labels[i-1];
            }
        }
    }
    return s;
}

string csisearch::to_string(const p& pp) const {
    int a = pp.a;
    int b = pp.b;
    int c = pp.c;
    int d = pp.d;
    string s = "";

    s += "p(" + dec_to_text(a, a & c, a & d);
    if ( b != 0 ) {
      s += "|" + dec_to_text(b, b & c, b & d);
    }
    s += ")";

    return s;
}

bool csisearch::valid_rule(const int& ruleid, const int& a, const int& b, const int& c, const int& d, const bool& primi) const {
    switch ( ruleid ) {

        // Marginalization
        case 0 : {
            // there must be other variables
            if ( set_size(a) == 1 ) return false;
            else return true;
        }

        // Conditioning
        case 1 : {
            // there must be other variables
            if ( set_size(a) == 1 ) return false;
            else return true;
        }

        // Product rule
        case 2 : {
            // there must be conditioning variables
            if ( b == 0 ) return false;
            else return true;
        }

        case -2 : {
            // there must be other variables available
            if ( (a | b) == lhs ) return false;
            else return true;
        }

        // Deletion of observations
        case -3 : {
            // there must be observations to delete
            if ( b == 0 ) return false;
            else return true;
        }

        // Case-by-case reasoning
        case 5 : {
            // There must be a variable z = 0
            if ( c == 0 ) return false;
            else return true;
        }

        // Case-by-case reasoning
        case -5 : {
            // There must be a variable z = 1
            if ( d == 1 ) return false;
            else return true;
        }

        default : {
            return true;
        }
    }

    return true;
}

void csisearch::apply_rule(const int &ruleid, const int &a, const int &b, const int &c, const int &d, const int &z, const int &z_ind) {
    info.valid = false;

    switch ( ruleid ) {

        // Marginalisation
        case 0 : {

            if ( (z & a) != z ) return; // z has to be in a
            if ( z == a ) return; // there has to be something else in a
            if ( (z & (c | d)) != 0 ) return; // cannot marginalize over z = 0 or z = 1
            break;

        }

        // Conditioning
        case 1 : {

            if ( (z & a) != z ) return; // z has to be in a
            if ( z == a ) return; // there has to be something else in a
            if ( ((a - z) & (c | d)) != 0 ) return; // cannot marginalize over y = 0 or y = 1
            break;

        }


        // Product rule
        case 2 : {

            if ( (z & b) != z ) return; // z has to be in b
            if ( (z & lhs) != z ) return; // all variables in z must be observed 
            break;

        }

        // Product rule
        case -2 : {

            // z cannot be in any of the sets
            if ( (z & a) != 0 ) return; // z intersection a = 0
            if ( (z & b) != 0 ) return; // z intersection b = 0
            if ( (z & lhs) != z ) return; // all variables in z must be observed 
            break;

        }

        // Insertion of observations (Unquantified)
        case 3 : {

            // if ( (z & lhs) != z ) return;
            if ( (z & a) != 0 ) return;
            if ( (z & b) != 0 ) return;
            if ( (z & intv_vars) != 0 ) return;
            break;

        }

        // Deletion of observations
        case -3 : {

            if ( (z & a) != 0 ) return;
            if ( (z & b) != z ) return;
            break;

        }

        // Insertion of observations (Z = 0)
        case 4 : {

            // z cannot be in any of the sets
            if ( (z & a) != 0 ) return;
            if ( (z & b) != 0 ) return;
            if ( (z & con_vars) != z ) return; // z must be a context variable
            break;

        }

        // Insertion of observations (Z = 1)
        case -4 : {

            // z cannot be in any of the sets
            if ( (z & a) != 0 ) return;
            if ( (z & b) != 0 ) return;
            if ( (z & (con_vars | intv_vars)) != z ) return; // z must be a context variable or an intervention variable
            break;

        }

        // Case-by-case reasoning
        case 5 : {

            if ( (z & a) != z ) return; // z has to be in a
            if ( (z & c) != z ) return; // z has to be in c
            if ( (z & lhs) != z ) return; // all variables in z must be observed 
            break;

        }

        // Case-by-case reasoning
        case -5 : {

            if ( (z & a) != z ) return; // z has to be in a
            if ( (z & d) != z ) return; // z has to be in d
            if ( (z & lhs) != z ) return; // all variables in z must be observed 
            break;

        }

        // General-by-case reasoning (RHS)
        case 6 : {

            if ( (z & a) != z ) return; // z has to be in a
            if ( (z & c) != z && (z & d) != z ) return; // z = 0 or z = 1 has to hold
            if ( (z & lhs) != z ) return; // all variables in z must be observed 
            break;

        }

        // General-by-case reasoning (LHS, Z = 0)
        case 7 : {

            if ( (z & a) != 0 ) return; // z intersection a = 0
            if ( (z & b) != 0 ) return; // z intersection b = 0
            if ( (z & con_vars) != z ) return; // z must be a context variable
            if ( (z & lhs) != z ) return; // all variables in z must be observed 
            break;

        }

        // General-by-case reasoning (LHS, Z = 1)
        case -7 : {

            if ( (z & a) != 0 ) return; // z intersection a = 0
            if ( (z & b) != 0 ) return; // z intersection b = 0
            if ( (z & con_vars) != z ) return; // z must be a context variable
            if ( (z & lhs) != z ) return; // all variables in z must be observed 
            break;

        }

        // Case-by-general resoning (Z = 0)
        case 8 : {

            if ( (z & a) != z && (z & b) != z ) return; // z either in a or b
            if ( (z & c) != 0 ) return; // z must be unqualified
            if ( (z & d) != 0 ) return; // z must be unqualified
            if ( (z & con_vars) != z ) return; // z must be a context variable
            break;

        }

        // Case-by-general resoning (Z = 1)
        case -8 : {

            if ( (z & a) != z && (z & b) != z ) return; // z either in a or b
            if ( (z & c) != 0 ) return; // z must be unqualified
            if ( (z & d) != 0 ) return; // z must be unqualified
            if ( (z & (con_vars)) != z ) return; // z must be a context variable
            break;

        }

    }

    info.valid = true;
    get_ruleinfo(ruleid, a, b, c, d, z);
}


void csisearch::get_ruleinfo(const int& ruleid, const int& y, const int& x, const int& u, const int& v, const int& z) {
    info.from.a = y; info.from.b = x; info.from.c = u; info.from.d = v;

    switch ( ruleid ) {

        // Marginalisation
        case 0 : {

            info.to.a = y - z; info.to.b = x; info.to.c = u; info.to.d = v;
            info.rp.a = 0;

            info.ri.x = 0;
            info.enumerate = false;

            return;

        }

        // Conditioning
        case 1 : {

            info.to.a = y - z; info.to.b = x + z; info.to.c = u; info.to.d = v;
            info.rp.a = 0;

            info.ri.x = 0;
            info.enumerate = false;

            return;

        }

        // Product rule
        case 2 : {

            info.to.a = y + z; info.to.b = x - z; info.to.c = u;     info.to.d = v;
            info.rp.a = z;     info.rp.b = x - z; info.rp.c = u & x; info.rp.d = v & x;

            info.ri.x = 0;
            info.enumerate = false;

            return;

        }

        // Product rule
        case -2 : {

            info.to.a = y + z; info.to.b = x;     info.to.c = u; info.to.d = v;
            info.rp.a = z;     info.rp.b = x + y; info.rp.c = u; info.rp.d = v;

            info.ri.x = 0;
            info.enumerate = true;

            return;

        }

        // Insertion of observations (Unquantified)
        case 3 : {

            info.to.a = y; info.to.b = x | z; info.to.c = u; info.to.d = v;
            info.rp.a = 0;

            info.ri.y = y; info.ri.x = z; info.ri.z = x;
            info.ri.u = u & x; info.ri.v = v & x;
            info.enumerate = false;

            return;

        }

        // Deletion of observations
        case -3 : {

            info.to.a = y; info.to.b = x - z; info.to.c = u - (u & z); info.to.d = v - (v & z);
            info.rp.a = 0;

            info.ri.y = y; info.ri.x = z; info.ri.z = x - z;
            info.ri.u = (u - (u & z)) & x; info.ri.v = (v - (v & z)) & x;
            info.enumerate = false;

            return;

        }

        // Insertion of observations (Z = 0)
        case 4 : {

            info.to.a = y; info.to.b = x | z; info.to.c = u + z; info.to.d = v;
            info.rp.a = 0;

            info.ri.y = y; info.ri.x = z; info.ri.z = x;
            info.ri.u = u & x; info.ri.v = v & x;
            info.enumerate = false;

            return;

        }

        // Insertion of observations (Z = 1)
        case -4 : {

            info.to.a = y; info.to.b = x | z; info.to.c = u; info.to.d = v + z;
            info.rp.a = 0;

            info.ri.y = y; info.ri.x = z; info.ri.z = x;
            info.ri.u = u & x; info.ri.v = v & x;
            info.enumerate = false;

            return;

        }

        // Case-by-case reasoning
        case 5 : {

            info.to.a = y; info.to.b = x; info.to.c = u - z; info.to.d = v;
            info.rp.a = y; info.rp.b = x; info.rp.c = u - z; info.rp.d = v + z;

            info.ri.x = 0;
            info.enumerate = false;

            return;
        }

        // Case-by-case reasoning
        case -5 : {

            info.to.a = y; info.to.b = x; info.to.c = u;     info.to.d = v - z;
            info.rp.a = y; info.rp.b = x; info.rp.c = u + z; info.rp.d = v - z;

            info.ri.x = 0;
            info.enumerate = false;

            return;
        }

        // General-by-case reasoning (RHS)
        case 6 : {

            info.to.a = y;     info.to.b = x; info.to.c = u - (u & z) + (v & z); info.to.d = v - (v & z) + (u & z);
            info.rp.a = y - z; info.rp.b = x; info.rp.c = u - (u & z);           info.rp.d = v - (v & z);

            info.ri.x = 0;
            info.enumerate = false;

            return;
        }

        // General-by-case reasoning (LHS, Z = 0)
        case 7 : {

            info.to.a = y + z; info.to.b = x; info.to.c = u;     info.to.d = v + z;
            info.rp.a = y + z; info.rp.b = x; info.rp.c = u + z; info.rp.d = v;

            info.ri.x = 0;
            info.enumerate = false;

            return;
        }

        // General-by-case reasoning (LHS, Z = 1)
        case -7 : {

            info.to.a = y + z; info.to.b = x; info.to.c = u + z; info.to.d = v;
            info.rp.a = y + z; info.rp.b = x; info.rp.c = u;     info.rp.d = v + z;

            info.ri.x = 0;
            info.enumerate = false;

            return;
        }

        // Case-by-general resoning (Z = 0)
        case 8 : {

            info.to.a = y; info.to.b = x; info.to.c = u + z; info.to.d = v;
            info.rp.a = 0;

            info.ri.x = 0;
            info.enumerate = false;

            return;
        }

           // Case-by-general resoning (Z = 1)
        case -8 : {

            info.to.a = y; info.to.b = x; info.to.c = u; info.to.d = v + z;
            info.rp.a = 0;

            info.ri.x = 0;
            info.enumerate = false;

            return;
        }

    }
}

// csisearch_heuristic

csisearch_heuristic::csisearch_heuristic(const int& n_, const bool& dd, const bool& da, const bool& fa, const bool& verb):csisearch(n_, dd, da, fa, verb) {
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
    L[index] = nquery;
    ps[make_key(nquery.pp)] = index;
    Q.push(&L[index]);
}

void csisearch_heuristic::add_known(const int& a, const int& b, const int& c, const int& d) {
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
    iquery.score = compute_score(pp);
    L[index] = iquery;
    ps[make_key(pp)] = index;
    if ( equal_p(pp, target) ) {
        trivial_id = true;
        target_dist.push_back(L[index]);
    }
    Q.push(&L[index]);
    lhs = lhs | a;
    if ( verbose ) Rcpp::Rcout << "Adding known distribution: " << to_string(pp) << endl;
}

// Heuristic for search order
int csisearch_heuristic::compute_score(const p& pp) const {
    int score = 0;
    int common_a = pp.a & target.a;
    int common_b = pp.b & target.b;
    int common_c = pp.c & target.c;
    int common_d = pp.d & target.d;

    score += 10 * set_size(common_a);
    score += 5 * set_size(common_b);
    score += 3 * set_size(common_c);
    score += 3 * set_size(common_d);

    score -= 2 * set_size(target.a - common_a);
    score -= 2 * set_size(pp.a - common_a);
    score -= 2 * set_size(pp.b - common_b);
    score -= 2 * set_size(target.b - common_b);
    score -= 1 * set_size(target.c - common_c);
    score -= 1 * set_size(pp.c - common_c);
    score -= 1 * set_size(target.d - common_d);
    score -= 1 * set_size(pp.d - common_d);

    return(score);
}