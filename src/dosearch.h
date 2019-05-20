#ifndef DOSEARCH_H
#define DOSEARCH_H

#include "search.h"
#include "dcongraph.h"
#include "derivation.h"

using namespace std;

class dosearch: public search {
public:
    dosearch(const int& n_, const bool& dd, const bool& da, const bool& fa, const bool& verb);
    int md_s, md_p, md_t, md_u, tr, sb, trsb, index, lhs;
    char md_sym;
    bool md, trivial_id, format_do;
    p target;
    dcongraph* g;

    virtual void add_distribution(distr& nquery);
    virtual void add_known(const int& a, const int& b, const int& c, const int& d);
    virtual distr& next_distribution(const int& i);
    void assign_candidate(distr& required);
    bool check_trivial();
    bool separation_criterion();
    int rule_limit(const int& ruleid, const unsigned int& z_size);
    void set_target(const int& a, const int& b, const int& c, const int& d);
    void set_graph(dcongraph* g_);
    void set_options(const vector<int>& r);
    void set_labels(const Rcpp::StringVector& lab);
    void set_md_symbol(const char& mds);
    bool is_primitive(const bool& pa1_primitive, const bool& pa2_primitive, const int& ruleid);
    string derive_formula(distr& dist);
    string dec_to_text(const int& dec, const int& enabled) const;
    string to_string(const p& pp) const;
    string rule_name(const int& rule_num) const;
    bool valid_rule(const int& ruleid, const int& a, const int& b, const int& c, const int& d, const bool& primi) const;
    void apply_rule(const int& ruleid, const int& a, const int& b, const int& c, const int& d, const int& z);
    void derive_distribution(const distr& iquery, const distr& required, const int& ruleid, int& remaining, bool& found);
    void get_ruleinfo(const int& ruleid, const int& y, const int& xw, const int& x, const int& d, const int& z);
    void get_candidate(distr& required, const int& req);
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
    dosearch_heuristic(const int& n_, const bool& dd, const bool& da, const bool& fa, const bool& verb);
    void add_distribution(distr& nquery);
    void add_known(const int& a, const int& b, const int& c, const int& d);
    distr& next_distribution(const int& j);
    ~dosearch_heuristic();
private:
    int compute_score(const p& pp) const;
    int compute_score_md(const p& pp) const;
    priority_queue<distr*, std::vector<distr*>, comp_distr> Q;
};

#endif

