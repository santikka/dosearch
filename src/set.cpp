#include "set.h"

int set_size(const int& set) {
  int size = 0;
  for (int z = 1; z <= MAX_SIZE; z++) {
    if (in_set(z, set)) size++;
  }
  return size;
}

bool in_set(const int& x, const int& set) {
  return set & unary(x);
}

int set_union(const int& set1, const int& set2) {
  return set1 | set2;
}

int full_set(const int& n) {
  return (1 << n) - 1;
}

int unary(const int& x) {
  return 1 << (x - 1);
}

std::vector<int> get_subsets(const int& n) {
  std::vector<int> sets;
  for (int i = 1; i <= n; i++) {
    generate(sets, n, 0, 0, 0, i);
  }
  return sets;
}

void generate(std::vector<int>& sets, const int& n, int z, int j, int a, const int& b) {
  if (a < b) {
    if (a == 0) {
      for (int i = 1; i <= n; i++) {
        generate(sets, n, unary(i), i, a + 1, b);
      }
    } else {
      for (int i = j + 1; i <= n; i++) {
        generate(sets, n, z + unary(i), i, a + 1, b);
      }
    }
  } else {
    sets.push_back(z);
  }
}
