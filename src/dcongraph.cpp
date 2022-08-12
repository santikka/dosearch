#include "dcongraph.h"

dcongraph::dcongraph(const int &n_):n(n_) {
  empty();
}

void dcongraph::empty() {
  for (int i = 0; i < MAX_SIZE; i++) {
    for (int j = 0; j < MAX_SIZE; j++) {
      B[i][j] = false;
      Ce[i][j] = false;
    }
  }
}

dcongraph::~dcongraph() {
}

void dcongraph::add_ivars() {
  //this expands the graph by two
  for (int x = 1; x <= n; x++) {
    add_edge(n+x,x);
  }
  n = 2*n;
}

bool dcongraph::dsep_set(const int& xset, const int& yset, const int& c, const int& j) const {
  bool dseparated = true;
  for (int x = 1; x <= n; x++) {
    if (!in_set(x,xset)) continue;
    for (int y = 1; y <=n; y++) {
      if (!in_set(y,yset)) continue;
      dseparated = dseparated && dsep(x, y, c, j);
      if (!dseparated) return dseparated;
    }
  }
  return dseparated;
}

bool dcongraph::dsep(const int& x, const int& y, const int& c, const int& j) const {

  // note that for encoded indeps an deps we can just check the solution!
  // test this quickly always!

  state current;
  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      current.hh[i][j] = Ce[i][j];
      current.tt[i][j] = false;
      current.th[i][j] = B[j][i]; // notice that index order changes here!!!
    }
  }
  current.j = 0; current.c = 0; current.m = 0;

  // if an edge is added, then the vars will always dependent

  // calculates the m set
  int m = (1 << n) - 1;
  m = m & (~ c); // take out c
  m = m & (~ (1 << (x - 1))); //take out x
  m = m & (~ (1 << (y - 1))); //take out y

  // then need to start applying operations
  while (current.j != j) {
    // find an element that is in j but not in current.j
    int el = get_element(j & ~current.j);
    intervene(current, el);
  }

  // note that cannot do this before the intervened edges are out!!!!
  if ( current.hh[x-1][y-1] || current.hh[y-1][x-1] ||
       current.th[x-1][y-1] || current.th[y-1][x-1] ||
       current.tt[x-1][y-1] || current.tt[y-1][x-1]
  ) return false;
  
  while (current.c != c) {
    // find an element that is in j but not in current.j
    int el = get_element(c & ~current.c);
    condition(current, el);
    
    if (current.hh[x-1][y-1] || current.hh[y-1][x-1] ||
        current.th[x-1][y-1] || current.th[y-1][x-1] ||
        current.tt[x-1][y-1] || current.tt[y-1][x-1]
    ) return false;
  }

  while (current.m != m) {
    // find an element that is in j but not in current.j
    int el = get_element(m & ~current.m);
    marginalize(current, el);
    
    if (current.hh[x-1][y-1] || current.hh[y-1][x-1] ||
        current.th[x-1][y-1] || current.th[y-1][x-1] ||
        current.tt[x-1][y-1] || current.tt[y-1][x-1]
    ) return false;
  }
  if (current.hh[x-1][y-1] || current.hh[y-1][x-1] ||
      current.th[x-1][y-1] || current.th[y-1][x-1] ||
      current.tt[x-1][y-1] || current.tt[y-1][x-1]
  ) return false;

  // no edge was but between x and y, so they are separated! 
  return true;
}

void dcongraph::intervene(state& current, const int& el) const {
  for (int i = 1; i <= n; i++) {
    current.hh[i-1][el-1] = false; //take out the hh edges
    current.th[i-1][el-1] = false;
  }
  current.j = current.j | (1 << (el - 1));
}

void dcongraph::condition(state& current, const int& el) const {
  for (int i = 1; i <= n; i++) {
    for (int j = 1; j <= n; j++) {
      current.hh[i-1][j-1] = current.hh[i-1][j-1] |
        (current.hh[i-1][el-1] & current.hh[el-1][j-1]);
      current.th[i-1][j-1] = current.th[i-1][j-1] |
        (current.th[i-1][el-1] & current.hh[el-1][j-1]);
      current.tt[i-1][j-1] = current.tt[i-1][j-1] |
        (current.th[i-1][el-1] & current.th[j-1][el-1]);
    }
  }
  current.c = current.c | (1 << (el - 1));
}

void dcongraph::marginalize(state& current, const int& el) const {
  for (int i = 1; i <= n; i++) {
    for (int j = 1; j <= n; j++) {
      if (el == i || el == j) continue;
      current.hh[i-1][j-1] = current.hh[i-1][j-1] | 
        (current.th[el-1][i-1] & current.th[el-1][j-1]) | //i<--el-->j
        (current.hh[i-1][el-1] & current.th[el-1][j-1]) | //i<->el-->j
        (current.th[el-1][i-1] & current.hh[el-1][j-1]) | //i<--el<->j
        (current.hh[el-1][i-1] & current.hh[el-1][j-1] &
         current.tt[el-1][el-1]); // i<->el---el<->j

      current.tt[i-1][j-1] = current.tt[i-1][j-1] | 
        (current.tt[i-1][el-1] & current.tt[el-1][j-1]) | //i---el---j
        (current.th[i-1][el-1] & current.tt[el-1][j-1]) | //i-->el---j
        (current.tt[i-1][el-1] & current.th[j-1][el-1]) | //i---el<--j
        (current.th[i-1][el-1] & current.th[j-1][el-1] &
         current.tt[el-1][el-1]); // i-->el---el<--j
      
      current.th[i-1][j-1] = current.th[i-1][j-1] | 
        (current.tt[i-1][el-1] & current.th[el-1][j-1]) | //i---el-->j
        (current.th[i-1][el-1] & current.th[el-1][j-1]) | //i-->el-->j
        (current.tt[i-1][el-1] & current.hh[el-1][j-1]) | //i---el<->j
        (current.th[i-1][el-1] & current.hh[el-1][j-1] &
         current.tt[el-1][el-1]); // i-->el---el<->j
    }
  }
  current.m = current.m | (1 << (el - 1));
}

int dcongraph::get_element(const int& set) const {
  for (int z = 1; z <= n; z++) {
    if (in_set(z, set)) {
      return z;
    }
  }
  return 0;
}

void dcongraph::add_edge(const int& from, const int& to) {
  B[to-1][from-1] = true;
}

void dcongraph::add_conf(const int& from, const int& to) {
  Ce[to-1][from-1] = true;
  Ce[from-1][to-1] = true;
}

void dcongraph::remove_edge(const int& from, const int& to) {
  B[to-1][from-1] = false;
}

void dcongraph::remove_conf(const int& from, const int& to) {
  Ce[to-1][from-1] = false;
  Ce[from-1][to-1] = false;
}

bool dcongraph::edge(const int& from, const int& to) const {
  return B[to-1][from-1];
}

bool dcongraph::conf(const int& from, const int& to) const {
  return Ce[to-1][from-1];
}

void dcongraph::set_trnodes(const int& t) {
  tr = t;
}

int dcongraph::get_trnodes() const {
  return tr;
}

void dcongraph::set_sbnodes(const int& s) {
  sb = s;
}

int dcongraph::get_sbnodes() const {
  return sb;
}

void dcongraph::set_md_switches(const int& m) {
  md_s = m;
}

int dcongraph::get_md_switches() const {
  return md_s;
}

void dcongraph::set_md_proxies(const int& m) {
  md_p = m;
}

int dcongraph::get_md_proxies() const {
  return md_p;
}

void dcongraph::initialize_datanodes() {
  tr = 0; sb = 0; md_s = 0; md_p = 0;
}
