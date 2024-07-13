#include "ldag.h"

ldag::ldag(const int& n_):n(n_) {
  empty();
}

void ldag::empty() {
  for (int i = 0; i < MAX_SIZE; i++) {
    for (int j = 0; j < MAX_SIZE; j++) {
      E[i][j] = false;
    }
  }
  local_csi = std::vector<csi>(0);
}

ldag::~ldag() {
}

bool ldag::d_sep(const int& x, const int& y, const int& z) const {
  int a = get_ancestors(z, true);
  int xyz = get_ancestors(set_union(set_union(x, z), y), true);
  std::stack<dirvar> l;
  std::vector<dirvar> visited;
  for (int i = 1; i <= n; i++) {
    if (in_set(i, x)) {
      dirvar x_el;
      x_el.dir = true;
      x_el.v = i;
      l.push(x_el);
    }
  }
  int v;
  bool d, is_visited;
  while (!l.empty()) {
    is_visited = false;
    dirvar& l_el = l.top();
    d = l_el.dir;
    v = l_el.v;
    l.pop();
    for (auto &vis : visited) {
      if (d == vis.dir && v == vis.v) {
        is_visited = true;
        break;
      }
    }
    if (!is_visited) {
      if (in_set(v, y)) return false;
      dirvar vis_el;
      vis_el.dir = d;
      vis_el.v = v;
      visited.push_back(vis_el);
      if (d && !in_set(v, z)) {
        visitable_parents(v, xyz, l);
        visitable_children(v, xyz, l);
      } else if (!d) {
        if (!in_set(v, z)) {
          visitable_children(v, xyz, l);
        }
        if (in_set(v, a)) {
          visitable_parents(v, xyz, l);
        }
      }
    }
  }
  return true;
}

int ldag::get_ancestors(const int& set, const bool& inc) const {
  int next = 0;
  for (int i = 1; i <= n; i++) {
    if (in_set(i, set)) {
      for (int j = 1; j <= n; j++) {
        if (edge(j, i)) next = set_union(next, unary(j));
      }
    }
  }
  if (next > 0) next = set_union(next, get_ancestors(next, false));
  if (inc) next = set_union(set, next);
  return next;
}

void ldag::visitable_parents(const int& el, const int& xyz, std::stack<dirvar>& l) const {
  for (int i = 1; i <= n; i++) {
    if (edge(i, el) && in_set(i, xyz)) {
      dirvar pa_el;
      pa_el.dir = true;
      pa_el.v = i;
      l.push(pa_el);
    }
  }
}

void ldag::visitable_children(const int& el, const int& xyz, std::stack<dirvar>& l) const {
  for (int i = 1; i <= n; i++) {
    if (edge(el, i) && in_set(i, xyz)) {
      dirvar ch_el;
      ch_el.dir = false;
      ch_el.v = i;
      l.push(ch_el);
    }
  }
}

void ldag::add_edge(const int& from, const int& to) {
  E[from-1][to-1] = true;
}

void ldag::remove_edge(const int& from, const int& to) {
  E[from-1][to-1] = false;
}

bool ldag::edge(const int& from, const int& to) const {
  return E[from-1][to-1];
}

void ldag::add_context_set(const int& set) {
  context_sets.push_back(set);
}

void ldag::add_context(const int& zero, const int& one, const int& equiv, const std::vector<int>& from, const std::vector<int>& to) {
  context con;
  config cfg;
  std::string con_key = context_key(zero, one);
  cfg.zero = zero; cfg.one = one; cfg.equiv = unary(equiv);
  con.from = from; con.to = to;
  C[con_key] = con;
  context_settings[zero | one].push_back(cfg);
}

void ldag::set_contexts(const int& con, const int& intv) {
  con_vars = con;
  intv_vars = intv;
}

void ldag::add_local_csi(const int& x, const int& y, const int& z, const int& zero, const int& one) {
  csi loc;
  loc.x = x; loc.y = y; loc.z = z; loc.zero = zero; loc.one = one;
  local_csi.push_back(loc);
}

bool ldag::in_label(const int& x, const int& y, const int& z, const int& zero, const int& one) {
  for (auto &loc : local_csi) {
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

bool ldag::csi_criterion(const int& x, const int& y, const int& z, const int& zero, const int& one, const int& intv, const int& old_con) {
  int onei = one | intv;
  if (in_label(x, y, z, zero, onei)) return true; // First check if the CSI is directly encoded in a label
  bool valid_context = false;
  int new_con, full_con, equiv_class;
  context& con = C[context_key(zero, one)];
  context& ivar = C[context_key(0, intv)];
  if (csi_sep(x, y, z, con, ivar)) return true;
  int zc = (z - zero - one) & con_vars;
  if (zc > 0) {
    valid_context = false;
    for (auto &w : context_sets) {
      if ((w & old_con) != 0) continue;
      if ((w & zc) != w) continue;
      valid_context = true;
      new_con = old_con | w;
      full_con = (zero | one) | w;
      equiv_class = 0;
      std::vector<config>& settings = context_settings[full_con];
      for (auto &setting : settings) {
        if ((setting.zero & zero) != zero || (setting.one & one) != one) continue;
        if ((setting.equiv & equiv_class) == setting.equiv) continue; // A representative of the same equivalence class has already been evaluated
        equiv_class = equiv_class | setting.equiv;
        if (!csi_criterion(x, y, z, setting.zero, setting.one, intv, new_con)) {
          valid_context = false;
          break;
        }
      }
      if (valid_context) return true;
    }
  }
  for (auto &w : context_sets) {
    valid_context = false;
    if ((w & old_con) != 0) continue; // Cannot reuse old contexts
    if ((w & intv_vars) != 0) continue; // w cannot contain intervention variables
    if ((w & x) != 0 || (w & y) != 0 || (w & z) != 0) continue;
    new_con = old_con | w;
    full_con = (zero | one) | w;
    equiv_class = 0;
    std::vector<config>& settings = context_settings[full_con];
    if (csi_criterion(x, w, z | y, zero, one, intv, new_con) ||
        csi_criterion(x, w, z, zero, one, intv, new_con) ||
        csi_criterion(y, w, z | x, zero, one, intv, new_con) ||
        csi_criterion(y, w, z, zero, one, intv, new_con)) {
      valid_context = true;
      for (auto &setting : settings) {
        if ((setting.zero & zero) != zero || (setting.one & one) != one) continue;
        if ((setting.equiv & equiv_class) == setting.equiv) continue; // A representative of the same equivalence class has already been evaluated
        equiv_class = equiv_class | setting.equiv;
        if (!csi_criterion(x, y, z | w, setting.zero, setting.one, intv, new_con)) {
          valid_context = false;
          break;
        }
      }
    }
    if (valid_context) return true;
  }
  return false;
}

bool ldag::csi_sep(const int& x, const int& y, const int& z, const context& con, const context& ivar) {
  enter_context(con, ivar);
  if (d_sep(x, y, z)) {
    exit_context(con, ivar);
    return true;
  }
  exit_context(con, ivar);
  return false;
}

std::string ldag::context_key(const int& zero, const int& one) const {
  return std::to_string(zero) + "," + std::to_string(one);
}

void ldag::enter_context(const context& con, const context& ivar) {
  for (unsigned int i = 0; i < con.from.size(); i++) {
    remove_edge(con.from[i], con.to[i]);
  }
  for (unsigned int i = 0; i < ivar.from.size(); i++) {
    remove_edge(ivar.from[i], ivar.to[i]);
  }
}

void ldag::exit_context(const context& con, const context& ivar) {
  for (unsigned int i = 0; i < ivar.from.size(); i++) {
    add_edge(ivar.from[i], ivar.to[i]);
  }
  for (unsigned int i = 0; i < con.from.size(); i++) {
    add_edge(con.from[i], con.to[i]);
  }
}

// ldag_cache

ldag_cache::ldag_cache(const int& n_):ldag(n_) {
}

ldag_cache::~ldag_cache() {
}

std::string ldag_cache::separation_key(const int& x, const int& y, const int& z, const int& zero, const int& one) {
  return std::to_string(x) + "," + std::to_string(y) + "," + std::to_string(z) + "," + std::to_string(zero) + "," + std::to_string(one);
}

void ldag_cache::add_separation(const int& x, const int& y, const int& z, const int& zero, const int& one) {
  separations[separation_key(x, y, z, zero, one)] = true;
  separations[separation_key(y, x, z, zero, one)] = true;
}

int ldag_cache::evaluated_separation(const int& x, const int& y, const int& z, const int& zero, const int& one) {
  return separations[separation_key(x, y, z, zero, one)];
}

bool ldag_cache::csi_criterion(const int& x, const int& y, const int& z, const int& zero, const int& one, const int& intv, const int& old_con) {
  int onei = one | intv;
  if (evaluated_separation(x, y, z, zero, onei)) return true;
  if (in_label(x, y, z, zero, onei)) { // First check if the CSI is directly encoded in a label
    add_separation(x, y, z, zero, onei);
    return true;
  }
  bool valid_context = false;
  int new_con, full_con, equiv_class;
  context& con = C[context_key(zero, one)];
  context& ivar = C[context_key(0, intv)];
  if (csi_sep(x, y, z, con, ivar)) {
    add_separation(x, y, z, zero, onei);
    return true;
  }
  int sonei;
  int zc = (z - zero - one) & con_vars;
  if (zc > 0) {
    valid_context = false;
    for (auto &w : context_sets) {
      if ((w & old_con) != 0) continue;
      if ((w & zc) != w) continue;
      valid_context = true;
      new_con = old_con | w;
      full_con = (zero | one) | w;
      equiv_class = 0;
      std::vector<config>& settings = context_settings[full_con];
      for (auto &setting : settings) {
        if ((setting.zero & zero) != zero || (setting.one & one) != one) continue;
        if ((setting.equiv & equiv_class) == setting.equiv) continue; // A representative of the same equivalence class has already been evaluated
        equiv_class = equiv_class | setting.equiv;
        sonei = setting.one | intv;
        if (evaluated_separation(x, y, z, setting.zero, sonei)) continue;
        if (!csi_criterion(x, y, z, setting.zero, setting.one, intv, new_con)) {
          valid_context = false;
          break;
        } else add_separation(x, y, z, setting.zero, sonei);
      }
      if (valid_context) {
        add_separation(x, y, z, zero, onei);
        return true;
      }
    }
  }
  for (auto &w : context_sets) {
    if ((w & old_con) != 0) continue; // Cannot reuse old contexts
    if ((w & intv_vars) != 0) continue; // w cannot contain intervention variables
    if ((w & x) != 0 || (w & y) != 0 || (w & z) != 0) continue;
    valid_context = false;
    new_con = old_con | w;
    full_con = (zero | one) | w;
    equiv_class = 0;
    std::vector<config>& settings = context_settings[full_con];
    if (csi_criterion(x, w, z | y, zero, one, intv, new_con) ||
        csi_criterion(x, w, z, zero, one, intv, new_con) ||
        csi_criterion(y, w, z | x, zero, one, intv, new_con) ||
        csi_criterion(y, w, z, zero, one, intv, new_con)) {
      valid_context = true;
      for (auto &setting : settings) {
        if ((setting.zero & zero) != zero || (setting.one & one) != one) continue;
        if ((setting.equiv & equiv_class) == setting.equiv) continue; // A representative of the same equivalence class has already been evaluated
        equiv_class = equiv_class | setting.equiv;
        sonei = setting.one | intv;
        if (evaluated_separation(x, y, z | w, setting.zero, sonei)) continue;
        if (!csi_criterion(x, y, z | w, setting.zero, setting.one, intv, new_con)) {
          valid_context = false;
          break;
        } else add_separation(x, y, z | w, setting.zero, sonei);
      }
    }
    if (valid_context) {
      add_separation(x, y, z, zero, onei);
      return true;
    }
  }
  return false;
}
