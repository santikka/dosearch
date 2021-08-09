#ifndef CSISEARCH_H
#define CSISEARCH_H

#include "search.h"
#include "ldag.h"
#include "derivation.h"

using namespace std;

class csisearch : public search {
public:
    csisearch(const int& n_, const double& tl, const bool& bm, const bool& br, const bool& dd, const bool& da, const bool& fa, const bool& im, const bool& verb);
    int con_vars, intv_vars;
    ldag* g;

    virtual void add_distribution(distr& nquery);
    virtual void add_known(const int& a, const int& b, const int& c, const int& d);
    virtual distr& next_distribution(const int& i);
    void assign_candidate(distr& required);
    bool check_trivial();
    bool separation_criterion();
    int rule_limit(const int& ruleid, const unsigned int& z_size);
    void set_target(const int& a, const int& b, const int& c, const int& d);
    void set_graph(ldag* g_);
    void set_options(const vector<int>& rule_vec);
    void set_labels(const Rcpp::StringVector& lab);
    void set_contexts(const int& con);
    void set_interventions(const int& intv);
    bool is_primitive(const bool& pa1_primitive, const bool& pa2_primitive, const int& ruleid);
    string derive_formula(distr& dist);
    string dec_to_text(const int& dec, const int& zero, const int& one) const;
    string to_string(const p& pp) const;
    string rule_name(const int& rule_num) const;
    bool valid_rule(const int& ruleid, const int& a, const int& b, const int& c, const int& d, const bool& primi) const;
    void apply_rule(const int& ruleid, const int& a, const int& b, const int& c, const int& d, const int& z);
    void derive_distribution(const distr& iquery, const distr& required, const int& ruleid, int& remaining, bool& found);
    void get_ruleinfo(const int& ruleid, const int& y, const int& x, const int& u, const int& v, const int& z);
    void get_candidate(distr& required, const int& req);
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
    void add_known(const int& a, const int& b, const int& c, const int& d);
    distr& next_distribution(const int& i);
    virtual ~csisearch_heuristic();
private:
    int compute_score(const p& pp) const;
    priority_queue<distr*, std::vector<distr*>, comp_distr> Q;
};

#endif

