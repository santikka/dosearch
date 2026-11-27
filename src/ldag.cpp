#include "ldag.h"

ldag::ldag(const int& n_):n(n_) {
  pa = std::vector<varset>(n + 1);
  ch = std::vector<varset>(n + 1);
}

ldag::~ldag() {
}

bool ldag::edge(const int& from, const int& to) const {
  return ch[from].contains(to);
}

void ldag::add_edge(const int& from, const int& to) {
  pa[to].insert(from);
  ch[from].insert(to);
}

void ldag::remove_edge(const int& from, const int& to) {
  pa[to].erase(from);
  ch[from].erase(to);
}

// Ancestors of a set (including the set itself)
varset ldag::get_ancestors(const varset& set) const {
  varset a = set;
  std::vector<int> stack = set.elements();
  while (!stack.empty()) {
    int v = stack.back();
    stack.pop_back();
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

// Reachability-based d-separation restricted to the ancestors of x, y and z
bool ldag::d_sep(const varset& x, const varset& y, const varset& z) const {
  varset a = get_ancestors(z);
  varset xyz = get_ancestors(x | y | z);
  varset vis_up, vis_down;
  std::vector<std::pair<int, bool>> stack; // true = visited from a child
  for (int v = x.first(); v > 0; v = x.next(v)) {
    stack.push_back(std::make_pair(v, true));
  }
  while (!stack.empty()) {
    int v = stack.back().first;
    bool d = stack.back().second;
    stack.pop_back();
    varset& vis = d ? vis_up : vis_down;
    if (vis.contains(v)) continue;
    if (y.contains(v)) return false;
    vis.insert(v);
    bool in_z = z.contains(v);
    bool visit_parents = (d && !in_z) || (!d && a.contains(v));
    bool visit_children = !in_z;
    if (visit_parents) {
      varset pv = pa[v] & xyz;
      for (int w = pv.first(); w > 0; w = pv.next(w)) {
        stack.push_back(std::make_pair(w, true));
      }
    }
    if (visit_children) {
      varset cv = ch[v] & xyz;
      for (int w = cv.first(); w > 0; w = cv.next(w)) {
        stack.push_back(std::make_pair(w, false));
      }
    }
  }
  return true;
}

void ldag::add_context_set(const varset& set) {
  context_sets.push_back(set);
}

void ldag::add_context(const varset& zero, const varset& one, const int& equiv, const std::vector<int>& from, const std::vector<int>& to) {
  context con;
  config cfg;
  cfg.zero = zero; cfg.one = one; cfg.equiv = varset::unit(equiv);
  con.from = from; con.to = to;
  set_pair key = {zero, one};
  C[key] = con;
  context_settings[zero | one].push_back(cfg);
}

void ldag::set_contexts(const varset& con, const varset& intv) {
  con_vars = con;
  intv_vars = intv;
}

void ldag::add_local_csi(const varset& x, const varset& y, const varset& z, const varset& zero, const varset& one) {
  csi loc;
  loc.x = x; loc.y = y; loc.z = z; loc.zero = zero; loc.one = one;
  local_csi.push_back(loc);
}

const ldag::context& ldag::get_context(const varset& zero, const varset& one) const {
  static const context empty_context;
  set_pair key = {zero, one};
  auto it = C.find(key);
  if (it == C.end()) return empty_context;
  return it->second;
}

const std::vector<ldag::config>& ldag::get_settings(const varset& full_con) const {
  static const std::vector<config> empty_settings;
  auto it = context_settings.find(full_con);
  if (it == context_settings.end()) return empty_settings;
  return it->second;
}

bool ldag::in_label(const varset& x, const varset& y, const varset& z, const varset& zero, const varset& one) const {
  for (const csi& loc : local_csi) {
    if (x == loc.x) {
      if (y != loc.y || z != loc.z || zero != loc.zero || one != loc.one) continue;
      return true;
    } else if (y == loc.x) {
      if (x != loc.y || z != loc.z || zero != loc.zero || one != loc.one) continue;
      return true;
    }
  }
  return false;
}

bool ldag::csi_criterion(const varset& x, const varset& y, const varset& z, const varset& zero, const varset& one, const varset& intv, const varset& old_con) {
  varset onei = one | intv;
  if (in_label(x, y, z, zero, onei)) return true; // First check if the CSI is directly encoded in a label
  bool valid_context = false;
  varset empty;
  const context& con = get_context(zero, one);
  const context& ivar = get_context(empty, intv);
  if (csi_sep(x, y, z, con, ivar)) return true;
  varset zero_one = zero | one;
  varset zc = (z - zero_one) & con_vars;
  if (!zc.empty()) {
    for (const varset& w : context_sets) {
      if (w.intersects(old_con)) continue;
      if (!w.is_subset(zc)) continue;
      valid_context = true;
      varset new_con = old_con | w;
      varset equiv_class;
      const std::vector<config>& settings = get_settings(zero_one | w);
      // The case analysis is vacuous if no settings of the contexts exist
      bool covered = false;
      for (const config& setting : settings) {
        if (!zero.is_subset(setting.zero) || !one.is_subset(setting.one)) continue;
        covered = true;
        if (setting.equiv.is_subset(equiv_class)) continue; // A representative of the same equivalence class has already been evaluated
        equiv_class |= setting.equiv;
        if (!csi_criterion(x, y, z, setting.zero, setting.one, intv, new_con)) {
          valid_context = false;
          break;
        }
      }
      if (!covered) valid_context = false;
      if (valid_context) return true;
    }
  }
  for (const varset& w : context_sets) {
    valid_context = false;
    if (w.intersects(old_con)) continue; // Cannot reuse old contexts
    if (w.intersects(intv_vars)) continue; // w cannot contain intervention variables
    if (w.intersects(x) || w.intersects(y) || w.intersects(z)) continue;
    varset new_con = old_con | w;
    varset equiv_class;
    const std::vector<config>& settings = get_settings(zero_one | w);
    if (csi_criterion(x, w, z | y, zero, one, intv, new_con) ||
        csi_criterion(x, w, z, zero, one, intv, new_con) ||
        csi_criterion(y, w, z | x, zero, one, intv, new_con) ||
        csi_criterion(y, w, z, zero, one, intv, new_con)) {
      valid_context = true;
      varset zw = z | w;
      // The case analysis is vacuous if no settings of the contexts exist
      bool covered = false;
      for (const config& setting : settings) {
        if (!zero.is_subset(setting.zero) || !one.is_subset(setting.one)) continue;
        covered = true;
        if (setting.equiv.is_subset(equiv_class)) continue; // A representative of the same equivalence class has already been evaluated
        equiv_class |= setting.equiv;
        if (!csi_criterion(x, y, zw, setting.zero, setting.one, intv, new_con)) {
          valid_context = false;
          break;
        }
      }
      if (!covered) valid_context = false;
    }
    if (valid_context) return true;
  }
  return false;
}

bool ldag::csi_sep(const varset& x, const varset& y, const varset& z, const context& con, const context& ivar) {
  std::vector<std::pair<int, int>> removed = enter_context(con, ivar);
  bool sep = d_sep(x, y, z);
  exit_context(removed);
  return sep;
}

// Remove the edges that vanish in the given context and return them
// so that exactly these edges can be restored afterwards
std::vector<std::pair<int, int>> ldag::enter_context(const context& con, const context& ivar) {
  std::vector<std::pair<int, int>> removed;
  for (unsigned int i = 0; i < con.from.size(); i++) {
    if (edge(con.from[i], con.to[i])) {
      remove_edge(con.from[i], con.to[i]);
      removed.push_back(std::make_pair(con.from[i], con.to[i]));
    }
  }
  for (unsigned int i = 0; i < ivar.from.size(); i++) {
    if (edge(ivar.from[i], ivar.to[i])) {
      remove_edge(ivar.from[i], ivar.to[i]);
      removed.push_back(std::make_pair(ivar.from[i], ivar.to[i]));
    }
  }
  return removed;
}

void ldag::exit_context(const std::vector<std::pair<int, int>>& removed) {
  for (const auto& e : removed) {
    add_edge(e.first, e.second);
  }
}

// ldag_cache

ldag_cache::ldag_cache(const int& n_):ldag(n_), generation(0) {
}

ldag_cache::~ldag_cache() {
}

void ldag_cache::add_separation(const varset& x, const varset& y, const varset& z, const varset& zero, const varset& one) {
  varset empty;
  sep_key k1 = {x, y, z, zero, one, empty};
  sep_key k2 = {y, x, z, zero, one, empty};
  bool added = separations.emplace(k1, true).second;
  added = separations.emplace(k2, true).second || added;
  if (added) generation++;
}

bool ldag_cache::evaluated_separation(const varset& x, const varset& y, const varset& z, const varset& zero, const varset& one) const {
  varset empty;
  sep_key key = {x, y, z, zero, one, empty};
  return separations.find(key) != separations.end();
}

bool ldag_cache::known_failure(const sep_key& key) const {
  auto it = failures.find(key);
  return it != failures.end() && it->second == generation;
}

bool ldag_cache::add_failure(const sep_key& key) {
  failures[key] = generation;
  return false;
}

bool ldag_cache::csi_criterion(const varset& x, const varset& y, const varset& z, const varset& zero, const varset& one, const varset& intv, const varset& old_con) {
  varset onei = one | intv;
  if (evaluated_separation(x, y, z, zero, onei)) return true;
  sep_key fail_key = {x, y, z, zero, onei, old_con};
  if (known_failure(fail_key)) return false;
  // The outcome is reusable only if no separations are added meanwhile
  unsigned long start_generation = generation;
  if (in_label(x, y, z, zero, onei)) { // First check if the CSI is directly encoded in a label
    add_separation(x, y, z, zero, onei);
    return true;
  }
  bool valid_context = false;
  varset empty;
  const context& con = get_context(zero, one);
  const context& ivar = get_context(empty, intv);
  if (csi_sep(x, y, z, con, ivar)) {
    add_separation(x, y, z, zero, onei);
    return true;
  }
  varset zero_one = zero | one;
  varset zc = (z - zero_one) & con_vars;
  if (!zc.empty()) {
    for (const varset& w : context_sets) {
      if (w.intersects(old_con)) continue;
      if (!w.is_subset(zc)) continue;
      valid_context = true;
      varset new_con = old_con | w;
      varset equiv_class;
      const std::vector<config>& settings = get_settings(zero_one | w);
      // The case analysis is vacuous if no settings of the contexts exist
      bool covered = false;
      for (const config& setting : settings) {
        if (!zero.is_subset(setting.zero) || !one.is_subset(setting.one)) continue;
        covered = true;
        if (setting.equiv.is_subset(equiv_class)) continue; // A representative of the same equivalence class has already been evaluated
        equiv_class |= setting.equiv;
        varset sonei = setting.one | intv;
        if (evaluated_separation(x, y, z, setting.zero, sonei)) continue;
        if (!csi_criterion(x, y, z, setting.zero, setting.one, intv, new_con)) {
          valid_context = false;
          break;
        } else add_separation(x, y, z, setting.zero, sonei);
      }
      if (!covered) valid_context = false;
      if (valid_context) {
        add_separation(x, y, z, zero, onei);
        return true;
      }
    }
  }
  for (const varset& w : context_sets) {
    if (w.intersects(old_con)) continue; // Cannot reuse old contexts
    if (w.intersects(intv_vars)) continue; // w cannot contain intervention variables
    if (w.intersects(x) || w.intersects(y) || w.intersects(z)) continue;
    valid_context = false;
    varset new_con = old_con | w;
    varset equiv_class;
    const std::vector<config>& settings = get_settings(zero_one | w);
    if (csi_criterion(x, w, z | y, zero, one, intv, new_con) ||
        csi_criterion(x, w, z, zero, one, intv, new_con) ||
        csi_criterion(y, w, z | x, zero, one, intv, new_con) ||
        csi_criterion(y, w, z, zero, one, intv, new_con)) {
      valid_context = true;
      varset zw = z | w;
      // The case analysis is vacuous if no settings of the contexts exist
      bool covered = false;
      for (const config& setting : settings) {
        if (!zero.is_subset(setting.zero) || !one.is_subset(setting.one)) continue;
        covered = true;
        if (setting.equiv.is_subset(equiv_class)) continue; // A representative of the same equivalence class has already been evaluated
        equiv_class |= setting.equiv;
        varset sonei = setting.one | intv;
        if (evaluated_separation(x, y, zw, setting.zero, sonei)) continue;
        if (!csi_criterion(x, y, zw, setting.zero, setting.one, intv, new_con)) {
          valid_context = false;
          break;
        } else add_separation(x, y, zw, setting.zero, sonei);
      }
      if (!covered) valid_context = false;
    }
    if (valid_context) {
      add_separation(x, y, z, zero, onei);
      return true;
    }
  }
  if (generation == start_generation) add_failure(fail_key);
  return false;
}
