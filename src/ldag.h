#ifndef LDAG_H
#define LDAG_H

#include <vector>
#include <stack>
#include <unordered_map>
#include <string>
#include "set.h"

class ldag {
public:
  struct dirvar {
    bool dir; // true = visited from a child node
    int v;
  };
  struct context {
    std::vector<int> from;
    std::vector<int> to;
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
  std::vector<int> context_sets;
  std::vector<csi> local_csi;
  std::unordered_map<std::string, context> C;
  std::unordered_map<int, std::vector<config>> context_settings;
  ldag(const int& n);
  void empty();
  bool edge(const int& from, const int& to) const;
  void add_edge(const int& from, const int& to);
  void remove_edge(const int& from, const int& to);
  virtual bool csi_criterion(const int& x, const int& y, const int& z, const int& zero, const int& one, const int& intv, const int& old_con);
  void add_context(const int& zero, const int& one, const int& equiv, const std::vector<int>& from, const std::vector<int>& to);
  void add_context_set(const int& set);
  void add_local_csi(const int& x, const int& y, const int& z, const int& zero, const int& one);
  void set_contexts(const int& con, const int& intv);
  int get_ancestors(const int& set, const bool& inc) const;
  void visitable_parents(const int& set, const int& xyz, std::stack<dirvar>& l) const;
  void visitable_children(const int& set, const int& xyz, std::stack<dirvar>& l) const;
  void enter_context(const context& con, const context& ivar);
  void exit_context(const context& con, const context& ivar);
  bool in_label(const int& x, const int& y, const int& z, const int& zero, const int& one);
  bool d_sep(const int& x, const int& y, const int& z) const;
  bool csi_sep(const int& x, const int& y, const int& z, const context& con, const context& ivar);
  std::string context_key(const int& zero, const int& one) const;
  virtual ~ldag();

};

class ldag_cache: public ldag {
public:
  ldag_cache(const int& n);
  bool csi_criterion(const int& x, const int& y, const int& z, const int& zero, const int& one, const int& intv, const int& old_con);
  ~ldag_cache();
private:
  std::unordered_map<std::string, bool> separations;
  void add_separation(const int& x, const int& y, const int& z, const int& zero, const int& one);
  int evaluated_separation(const int& x, const int& y, const int& z, const int& zero, const int& one);
  std::string separation_key(const int& x, const int& y, const int& z, const int& zero, const int& one);
};

#endif
