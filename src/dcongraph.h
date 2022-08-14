#ifndef DCONGRAPH_H
#define DCONGRAPH_H

#include "set.h"

class dcongraph {
public:
  dcongraph(const int& n);

  void add_edge(const int& from, const int& to);
  void add_conf(const int& from, const int& to);
  void add_ivars();
  void empty();
  bool dsep(const int& x, const int& y, const int& c, const int& j) const;
  bool dsep_set(const int& xset, const int& yset, const int& c, const int& j) const;
  int get_element(const int &set) const;

  // set/get transportability nodes
  void set_trnodes(const int& t);
  int get_trnodes() const;

  // set/get selection bias nodes
  void set_sbnodes(const int& s);
  int get_sbnodes() const;

  // set/get missing data mechanisms
  void set_md_switches(const int& m);
  void set_md_proxies(const int& m);
  int get_md_switches() const;
  int get_md_proxies() const;

  void initialize_datanodes();

  bool B[MAX_SIZE][MAX_SIZE];
  bool Ce[MAX_SIZE][MAX_SIZE];
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
