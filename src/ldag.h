#ifndef LDAG_H
#define LDAG_H

#include <vector>
#include <stack>
#include <unordered_map>
#include "set.h"

using namespace std;

class ldag {
public:

    struct dirvar {
        bool dir; // true = visited from a child node
        int v;
    };
    struct context {
        vector<int> from;
        vector<int> to;
    };
    struct config {
        int zero, one, equiv;
    };
    struct csi {
        int x, y, z, zero, one;
    };

    bool E[MAX_SIZE][MAX_SIZE];
    int n;
    int con_vars, intv_vars;
    vector<int> context_sets;
    vector<csi> local_csi;
    unordered_map<string, context> C;
    unordered_map<int, vector<config>> context_settings;

    ldag(const int& n);

    void empty();
    bool edge(const int& from, const int& to) const;
    void add_edge(const int& from, const int& to);
    void remove_edge(const int& from, const int& to);

    virtual bool csi_criterion(const int& x, const int& y, const int& z, const int& zero, const int& one, const int& intv, const int& old_con);

    void add_context(const int& zero, const int& one, const int& equiv, const vector<int>& from, const vector<int>& to);
    void add_context_set(const int& set);
    void add_local_csi(const int& x, const int& y, const int& z, const int& zero, const int& one);
    void set_contexts(const int& con, const int& intv);
    int get_ancestors(const int& set, const bool& inc) const;
    void visitable_parents(const int& set, const int& xyz, stack<dirvar>& l) const;
    void visitable_children(const int& set, const int& xyz, stack<dirvar>& l) const;
    void enter_context(const context& con, const context& ivar);
    void exit_context(const context& con, const context& ivar);
    bool in_label(const int& x, const int& y, const int& z, const int& zero, const int& one);
    bool d_sep(const int& x, const int& y, const int& z) const;
    bool csi_sep(const int& x, const int& y, const int& z, const context& con, const context& ivar);
    string context_key(const int& zero, const int& one) const;

    virtual ~ldag();

};

class ldag_cache: public ldag {
public:
    ldag_cache(const int& n);
    bool csi_criterion(const int& x, const int& y, const int& z, const int& zero, const int& one, const int& intv, const int& old_con);
    ~ldag_cache();
private:
    unordered_map<string, int> separations;
    void add_separation(const int& x, const int& y, const int& z, const int& zero, const int& one, const bool& sep);
    int evaluated_separation(const int& x, const int& y, const int& z, const int& zero, const int& one);
    string separation_key(const int& x, const int& y, const int& z, const int& zero, const int& one);
};

#endif  /* LDAG_H */

