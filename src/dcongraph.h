#ifndef DCONGRAPH_H
#define DCONGRAPH_H

#include "set.h"

class dcongraph {
public:
    dcongraph(const int& n);

    void add_edge(const int& from, const int& to);
    void add_conf(const int& from, const int& to);
    void remove_edge(const int& from, const int& to);
    void remove_conf(const int& from, const int& to);
    void add_ivars();
    void empty();
    bool edge(const int& from, const int& to) const;
    bool conf(const int& from, const int& to) const;
    bool dsep(const int& x, const int& y, const int& c, const int& j) const;
    bool dsep_set(const int& xset, const int& yset, const int& c, const int& j) const;
    bool B[MAX_SIZE][MAX_SIZE];
    bool Ce[MAX_SIZE][MAX_SIZE];
    int get_element(const int &set) const;
    int n;

    virtual ~dcongraph();
private:
    struct state {
        bool hh[MAX_SIZE][MAX_SIZE];
        bool tt[MAX_SIZE][MAX_SIZE];
        bool th[MAX_SIZE][MAX_SIZE];
        int c, j, m;
    };
    int tr, sb, md_s, md_p;
    void intervene(state& current, const int& el) const;
    void condition(state& current, const int& el) const;
    void marginalize(state& current, const int& el) const;
};

#endif
