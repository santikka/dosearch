#ifndef LDAG_H
#define LDAG_H

#include <vector>
#include <unordered_map>
#include "set.h"

// A labeled directed acyclic graph of arbitrary size. Vertices are indexed
// starting from 1.
class ldag {
public:
  struct context {
    std::vector<int> from;
    std::vector<int> to;
  };
  struct config {
    varset zero, one, equiv;
  };
  struct csi {
    varset x, y, z, zero, one;
  };
  ldag(const int& n);
  bool edge(const int& from, const int& to) const;
  void add_edge(const int& from, const int& to);
  void remove_edge(const int& from, const int& to);
  virtual bool csi_criterion(const varset& x, const varset& y, const varset& z, const varset& zero, const varset& one, const varset& intv, const varset& old_con);
  void add_context(const varset& zero, const varset& one, const int& equiv, const std::vector<int>& from, const std::vector<int>& to);
  void add_context_set(const varset& set);
  void add_local_csi(const varset& x, const varset& y, const varset& z, const varset& zero, const varset& one);
  void set_contexts(const varset& con, const varset& intv);
  varset get_ancestors(const varset& set) const;
  bool in_label(const varset& x, const varset& y, const varset& z, const varset& zero, const varset& one) const;
  bool d_sep(const varset& x, const varset& y, const varset& z) const;
  bool csi_sep(const varset& x, const varset& y, const varset& z, const context& con, const context& ivar);
  virtual ~ldag();
protected:
  struct set_pair {
    varset a, b;
    bool operator==(const set_pair& s) const {
      return a == s.a && b == s.b;
    }
  };
  struct set_pair_hash {
    std::size_t operator()(const set_pair& s) const {
      std::size_t h = s.a.hash();
      hash_combine(h, s.b.hash());
      return h;
    }
  };
  int n;
  varset con_vars, intv_vars;
  std::vector<varset> pa, ch;
  std::vector<varset> context_sets;
  std::vector<csi> local_csi;
  std::unordered_map<set_pair, context, set_pair_hash> C;
  std::unordered_map<varset, std::vector<config>, varset_hash> context_settings;
  const context& get_context(const varset& zero, const varset& one) const;
  const std::vector<config>& get_settings(const varset& full_con) const;
  std::vector<std::pair<int, int>> enter_context(const context& con, const context& ivar);
  void exit_context(const std::vector<std::pair<int, int>>& removed);
};

// Caches the outcomes of evaluated CSI criteria. Separations that hold are
// valid in any context, but a failed evaluation depends on the contexts that
// were still available (old_con) and on the separations known at the time.
// Failures are therefore keyed by old_con and invalidated whenever a new
// separation is added to the cache.
class ldag_cache: public ldag {
public:
  ldag_cache(const int& n);
  bool csi_criterion(const varset& x, const varset& y, const varset& z, const varset& zero, const varset& one, const varset& intv, const varset& old_con);
  ~ldag_cache();
private:
  struct sep_key {
    varset x, y, z, zero, one, old_con;
    bool operator==(const sep_key& k) const {
      return x == k.x && y == k.y && z == k.z && zero == k.zero && one == k.one && old_con == k.old_con;
    }
  };
  struct sep_key_hash {
    std::size_t operator()(const sep_key& k) const {
      std::size_t h = k.x.hash();
      hash_combine(h, k.y.hash());
      hash_combine(h, k.z.hash());
      hash_combine(h, k.zero.hash());
      hash_combine(h, k.one.hash());
      hash_combine(h, k.old_con.hash());
      return h;
    }
  };
  std::unordered_map<sep_key, bool, sep_key_hash> separations;
  std::unordered_map<sep_key, unsigned long, sep_key_hash> failures;
  unsigned long generation;
  void add_separation(const varset& x, const varset& y, const varset& z, const varset& zero, const varset& one);
  bool evaluated_separation(const varset& x, const varset& y, const varset& z, const varset& zero, const varset& one) const;
  bool known_failure(const sep_key& key) const;
  bool add_failure(const sep_key& key);
};

#endif
