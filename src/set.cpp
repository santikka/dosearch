#include "set.h"

namespace {

inline int popcount64(const uint64_t& w) {
  return __builtin_popcountll(w);
}

inline int ctz64(const uint64_t& w) {
  return __builtin_ctzll(w);
}

}

uint64_t varset::word(const std::size_t& i) const {
  if (i == 0) return w0;
  if (i - 1 < rest.size()) return rest[i - 1];
  return 0;
}

void varset::set_word(const std::size_t& i, const uint64_t& w) {
  if (i == 0) {
    w0 = w;
    return;
  }
  if (i - 1 >= rest.size()) {
    if (w == 0) return;
    rest.resize(i, 0);
  }
  rest[i - 1] = w;
}

void varset::trim() {
  while (!rest.empty() && rest.back() == 0) rest.pop_back();
}

varset varset::unit(const int& x) {
  varset s;
  s.insert(x);
  return s;
}

varset varset::from_vector(const std::vector<int>& v) {
  varset s;
  for (const int& x : v) s.insert(x);
  return s;
}

varset varset::full(const int& n) {
  varset s;
  for (int x = 1; x <= n; x++) s.insert(x);
  return s;
}

std::vector<int> varset::elements() const {
  std::vector<int> v;
  elements(v);
  return v;
}

void varset::elements(std::vector<int>& v) const {
  v.clear();
  for (std::size_t i = 0; i < n_words(); i++) {
    uint64_t w = word(i);
    while (w != 0) {
      v.push_back(static_cast<int>(64 * i) + ctz64(w) + 1);
      w &= w - 1;
    }
  }
}

int varset::size_rest() const {
  int s = popcount64(w0);
  for (const uint64_t& w : rest) s += popcount64(w);
  return s;
}

int varset::first_rest() const {
  for (std::size_t i = 0; i < n_words(); i++) {
    uint64_t w = word(i);
    if (w != 0) return static_cast<int>(64 * i) + ctz64(w) + 1;
  }
  return 0;
}

int varset::next_rest(const int& x) const {
  std::size_t i = static_cast<std::size_t>(x) / 64;
  int b = x % 64;
  if (i >= n_words()) return 0;
  uint64_t w = word(i);
  if (b > 0) w &= ~((static_cast<uint64_t>(1) << b) - 1);
  while (true) {
    if (w != 0) return static_cast<int>(64 * i) + ctz64(w) + 1;
    i++;
    if (i >= n_words()) return 0;
    w = word(i);
  }
}

void varset::insert_rest(const int& x) {
  std::size_t i = static_cast<std::size_t>(x - 1) / 64;
  set_word(i, word(i) | (static_cast<uint64_t>(1) << ((x - 1) % 64)));
}

void varset::erase_rest(const int& x) {
  std::size_t i = static_cast<std::size_t>(x - 1) / 64;
  if (i >= n_words()) return;
  set_word(i, word(i) & ~(static_cast<uint64_t>(1) << ((x - 1) % 64)));
  trim();
}

bool varset::is_subset_rest(const varset& s) const {
  for (std::size_t i = 0; i < rest.size(); i++) {
    if ((rest[i] & ~s.word(i + 1)) != 0) return false;
  }
  return true;
}

bool varset::intersects_rest(const varset& s) const {
  std::size_t m = rest.size() < s.rest.size() ? rest.size() : s.rest.size();
  for (std::size_t i = 0; i < m; i++) {
    if ((rest[i] & s.rest[i]) != 0) return true;
  }
  return false;
}

void varset::or_rest(const varset& s) {
  if (s.rest.size() > rest.size()) rest.resize(s.rest.size(), 0);
  for (std::size_t i = 0; i < s.rest.size(); i++) rest[i] |= s.rest[i];
}

void varset::and_rest(const varset& s) {
  if (rest.size() > s.rest.size()) rest.resize(s.rest.size());
  for (std::size_t i = 0; i < rest.size(); i++) rest[i] &= s.rest[i];
  trim();
}

void varset::diff_rest(const varset& s) {
  std::size_t m = rest.size() < s.rest.size() ? rest.size() : s.rest.size();
  for (std::size_t i = 0; i < m; i++) rest[i] &= ~s.rest[i];
  trim();
}

varset varset::shift_up_rest(const int& k) const {
  varset r;
  std::size_t ws = static_cast<std::size_t>(k) / 64;
  int bs = k % 64;
  for (std::size_t i = 0; i < n_words(); i++) {
    uint64_t w = word(i);
    if (w == 0) continue;
    r.set_word(i + ws, r.word(i + ws) | (w << bs));
    if (bs > 0) {
      uint64_t carry = w >> (64 - bs);
      if (carry != 0) r.set_word(i + ws + 1, r.word(i + ws + 1) | carry);
    }
  }
  r.trim();
  return r;
}

varset varset::shift_down_rest(const int& k) const {
  varset r;
  std::size_t ws = static_cast<std::size_t>(k) / 64;
  int bs = k % 64;
  for (std::size_t i = ws; i < n_words(); i++) {
    uint64_t w = word(i);
    if (w == 0) continue;
    std::size_t j = i - ws;
    r.set_word(j, r.word(j) | (w >> bs));
    if (bs > 0 && j > 0) {
      r.set_word(j - 1, r.word(j - 1) | (w << (64 - bs)));
    }
  }
  r.trim();
  return r;
}

std::size_t varset::hash_rest(std::size_t h) const {
  for (const uint64_t& w : rest) hash_combine(h, static_cast<std::size_t>(w));
  return h;
}
