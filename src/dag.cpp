#include "dag.h"

dag::dag(const int& n_):n(n_) {
  pa = std::vector<varset>(n + 1);
  ch = std::vector<varset>(n + 1);
  bi = std::vector<varset>(n + 1);
}

dag::~dag() {
}

void dag::add_edge(const int& from, const int& to) {
  pa[to].insert(from);
  ch[from].insert(to);
}

void dag::add_conf(const int& v, const int& w) {
  bi[v].insert(w);
  bi[w].insert(v);
}

bool dag::dsep(const varset& x, const varset& y, const varset& c, const varset& j, const bool& intv) const {
  return !reachable(x, y, c, j, intv);
}

// Ancestors of s (including s) in the graph where the incoming edges of j
// have been removed
varset dag::ancestors(const varset& s, const varset& j) const {
  varset a = s;
  std::vector<int>& stack = anc_stack;
  s.elements(stack);
  while (!stack.empty()) {
    int v = stack.back();
    stack.pop_back();
    if (j.contains(v)) continue;
    const varset& pv = pa[v];
    for (int p = pv.first(); p > 0; p = pv.next(p)) {
      if (!a.contains(p)) {
        a.insert(p);
        stack.push_back(p);
      }
    }
  }
  return a;
}

// Is there a d-connecting path between x and y given c in the graph where
// the incoming edges of j (directed and bidirected) have been removed?
// If intv is true, y denotes the intervention nodes of the vertices in y.
// The search moves along the edges of the graph and keeps track of whether
// each vertex was entered through a tail (from a child) or through an
// arrowhead (from a parent or a bidirected edge). Each vertex is visited at
// most twice, so the search is linear in the size of the graph.
bool dag::reachable(const varset& x, const varset& y, const varset& c, const varset& j, const bool& intv) const {
  const int start = 0, up = 1, down = 2;
  varset anc = ancestors(c, j);
  varset vis_up, vis_down;
  std::vector<std::pair<int, int>>& stack = path_stack;
  stack.clear();
  for (int v = x.first(); v > 0; v = x.next(v)) {
    stack.push_back(std::make_pair(v, start));
  }
  while (!stack.empty()) {
    int v = stack.back().first;
    int s = stack.back().second;
    stack.pop_back();
    if (s != start) {
      // vertices of x are fully explored as starting points
      if (x.contains(v)) continue;
      varset& vis = (s == up) ? vis_up : vis_down;
      if (vis.contains(v)) continue;
      vis.insert(v);
    }
    bool non_collider = (s == start) || !c.contains(v);
    bool collider = (s == start) || anc.contains(v);
    if (y.contains(v)) {
      if (!intv) return true;
      // Can the path continue along the edge I_v -> v?
      if (!j.contains(v)) {
        if ((s != down && non_collider) || (s == down && collider)) return true;
      }
    }
    const varset& pv = pa[v];
    const varset& cv = ch[v];
    const varset& bv = bi[v];
    if (s != down) {
      // v is a non-collider for every continuation of the path
      if (!non_collider) continue;
      if (!j.contains(v)) {
        for (int w = pv.first(); w > 0; w = pv.next(w)) {
          stack.push_back(std::make_pair(w, up));
        }
        for (int w = bv.first(); w > 0; w = bv.next(w)) {
          if (!j.contains(w)) stack.push_back(std::make_pair(w, down));
        }
      }
      for (int w = cv.first(); w > 0; w = cv.next(w)) {
        if (!j.contains(w)) stack.push_back(std::make_pair(w, down));
      }
    } else {
      // v was entered through an arrowhead
      if (non_collider) {
        for (int w = cv.first(); w > 0; w = cv.next(w)) {
          if (!j.contains(w)) stack.push_back(std::make_pair(w, down));
        }
      }
      if (collider && !j.contains(v)) {
        for (int w = pv.first(); w > 0; w = pv.next(w)) {
          stack.push_back(std::make_pair(w, up));
        }
        for (int w = bv.first(); w > 0; w = bv.next(w)) {
          if (!j.contains(w)) stack.push_back(std::make_pair(w, down));
        }
      }
    }
  }
  return false;
}

const varset& dag::get_parents(const int& v) const {
  return pa[v];
}

const varset& dag::get_children(const int& v) const {
  return ch[v];
}

const varset& dag::get_siblings(const int& v) const {
  return bi[v];
}

void dag::set_trnodes(const varset& t) {
  tr = t;
}

const varset& dag::get_trnodes() const {
  return tr;
}

void dag::set_sbnodes(const varset& s) {
  sb = s;
}

const varset& dag::get_sbnodes() const {
  return sb;
}

void dag::set_md_switches(const varset& m) {
  md_s = m;
}

const varset& dag::get_md_switches() const {
  return md_s;
}

void dag::set_md_proxies(const varset& m) {
  md_p = m;
}

const varset& dag::get_md_proxies() const {
  return md_p;
}
