#ifndef SEARCH_H
#define SEARCH_H

#include <unordered_map>
#include <vector>
#include <string>
#include <queue>
#include <chrono>
#include <stack>
#include <Rcpp.h>
#include "set.h"
#include "derivation.h"

using namespace std;

struct p {
    int a, b, c, d;
};

struct rindep {
    int x, y, z, u, v;
};

struct output {
    p to, from, rp;
    rindep ri;
    bool valid, enumerate;
};

struct distr {
    int rule_num, index, score, pa1, pa2;
    bool primitive;
    p pp;
};

class search {
public:
    search(const int& n_, const double& tl, const bool& bm, const bool& br, const bool& dd, const bool& da, const bool& fa, const bool& im, const bool& verb);
    Rcpp::List initialize();
    void find();
    void set_derivation(derivation* d_);
    void get_candidate(distr& required, const int& req);
    string make_key(const p& pp) const;
    bool equal_p(const p& p1, const p& p2) const;
    void draw(const distr& dist, const bool& recursive, derivation& d);
    void enumerate_distribution(const int& ruleid, const int& a, const int& b, const int& c, const int& d, const int& z, int& cd, int& exist, int& req, bool& found, distr& iquery, distr& required, int& remaining);

    virtual void add_distribution(distr& nquery) = 0;
    virtual void add_known(const int& a, const int& b, const int& c, const int& d) = 0;
    virtual distr& next_distribution(const int& i) = 0;
    virtual void assign_candidate(distr& required) = 0;
    virtual bool check_trivial() = 0;
    virtual bool separation_criterion() = 0;
    virtual int rule_limit(const int& ruleid, const unsigned int& z_size) = 0;
    virtual void set_target(const int& a, const int& b, const int& c, const int& d) = 0;
    virtual void set_options(const vector<int>& rule_vec) = 0;
    virtual void set_labels(const Rcpp::StringVector& lab) = 0;
    virtual bool is_primitive(const bool& pa1_primitive, const bool& pa2_primitive, const int& ruleid) = 0;
    virtual string derive_formula(distr& dist) = 0;
    virtual string to_string(const p& pp) const = 0;
    virtual string rule_name(const int& rule_num) const = 0;
    virtual bool valid_rule(const int& ruleid, const int& a, const int& b, const int& c, const int& d, const bool& primi) const = 0;
    virtual void apply_rule(const int& ruleid, const int& a, const int& b, const int& c, const int& d, const int& z) = 0;
    virtual void derive_distribution(const distr& iquery, const distr& required, const int& ruleid, int& remaining, bool& found) = 0;
    virtual void get_ruleinfo(const int& ruleid, const int& y, const int& x, const int& u, const int& v, const int& z) = 0;
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
    int index, lhs;
    derivation *deriv;
    vector<distr> target_dist;
    vector<string> labels;
    vector<int> z_sets;
    vector<int> rules;
    vector<double> rule_times;
    bool trivial_id, format_do;
    unordered_map<int, distr> L;
    unordered_map<string, int> ps;
    stack<int> candidates;
    output info;
};

#endif