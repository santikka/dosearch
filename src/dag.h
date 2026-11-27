#ifndef DAG_H
#define DAG_H

#include <vector>
#include "set.h"

// A directed acyclic graph with bidirected edges (a semi-Markovian causal
// model) of arbitrary size. Vertices are indexed starting from 1. Each vertex
// v has an implicit intervention node I_v whose only edge is I_v -> v.
class dag {
public:
  dag(const int& n_);
  void add_edge(const int& from, const int& to);
  void add_conf(const int& v, const int& w);
  // Is x d-separated from y given c in the graph where the incoming edges
  // of j have been removed? If intv is true, y denotes the set of
  // intervention nodes of the vertices in y instead of the vertices.
  bool dsep(const varset& x, const varset& y, const varset& c, const varset& j, const bool& intv) const;

  // get the parents, the children and the siblings (the vertices connected
  // by a bidirected edge) of a vertex
  const varset& get_parents(const int& v) const;
  const varset& get_children(const int& v) const;
  const varset& get_siblings(const int& v) const;

  // set/get transportability nodes
  void set_trnodes(const varset& t);
  const varset& get_trnodes() const;

  // set/get selection bias nodes
  void set_sbnodes(const varset& s);
  const varset& get_sbnodes() const;

  // set/get missing data mechanisms
  void set_md_switches(const varset& m);
  void set_md_proxies(const varset& m);
  const varset& get_md_switches() const;
  const varset& get_md_proxies() const;

  virtual ~dag();
private:
  int n;
  std::vector<varset> pa, ch, bi;
  varset tr, sb, md_s, md_p;
  // Work space of the graph searches, kept to avoid repeated allocations
  mutable std::vector<int> anc_stack;
  mutable std::vector<std::pair<int, int>> path_stack;
  varset ancestors(const varset& s, const varset& j) const;
  bool reachable(const varset& x, const varset& y, const varset& c, const varset& j, const bool& intv) const;
};

#endif
