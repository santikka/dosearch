#ifndef SET_H
#define SET_H

#include <cstdint>
#include <cstddef>
#include <vector>
#include <utility>

// A set of variables represented as a dynamic bitset. Variables are indexed
// starting from 1. The first 64 variables are stored inline so that sets of
// small graphs never allocate memory. Larger variable indices are stored in
// additional words that are allocated only when needed. The additional words
// never have trailing zero words so that equal sets have equal representations.
//
// Operations that only involve the inline word are defined in this header so
// that they can be inlined. Operations on the additional words are in set.cpp.
class varset {
public:
  varset() : w0(0) {}
  varset(const varset& s) : w0(s.w0), rest(s.rest) {}
  varset(varset&& s) noexcept : w0(s.w0), rest(std::move(s.rest)) {}
  varset& operator=(const varset& s) {
    w0 = s.w0;
    if (!rest.empty() || !s.rest.empty()) rest = s.rest;
    return *this;
  }
  varset& operator=(varset&& s) noexcept {
    w0 = s.w0;
    if (!rest.empty() || !s.rest.empty()) rest = std::move(s.rest);
    return *this;
  }
  static varset unit(const int& x);
  static varset from_vector(const std::vector<int>& v);
  static varset full(const int& n);
  std::vector<int> elements() const;
  void elements(std::vector<int>& v) const;
  bool empty() const { return w0 == 0 && rest.empty(); }
  int size() const {
    if (rest.empty()) return __builtin_popcountll(w0);
    return size_rest();
  }
  int first() const {
    if (w0 != 0) return __builtin_ctzll(w0) + 1;
    return first_rest();
  }
  // smallest element greater than x, or 0 if none
  int next(const int& x) const {
    if (x < 64) {
      uint64_t w = w0 & (~static_cast<uint64_t>(0) << x);
      if (w != 0) return __builtin_ctzll(w) + 1;
      if (rest.empty()) return 0;
    }
    return next_rest(x);
  }
  bool contains(const int& x) const {
    if (x <= 64) return (w0 >> (x - 1)) & 1;
    std::size_t i = static_cast<std::size_t>(x - 1) / 64;
    return i - 1 < rest.size() && ((rest[i - 1] >> ((x - 1) % 64)) & 1);
  }
  void insert(const int& x) {
    if (x <= 64) w0 |= static_cast<uint64_t>(1) << (x - 1);
    else insert_rest(x);
  }
  void erase(const int& x) {
    if (x <= 64) w0 &= ~(static_cast<uint64_t>(1) << (x - 1));
    else erase_rest(x);
  }
  bool is_subset(const varset& s) const {
    if ((w0 & ~s.w0) != 0) return false;
    if (rest.empty()) return true;
    return is_subset_rest(s);
  }
  bool intersects(const varset& s) const {
    if ((w0 & s.w0) != 0) return true;
    if (rest.empty() || s.rest.empty()) return false;
    return intersects_rest(s);
  }
  varset& operator|=(const varset& s) {
    w0 |= s.w0;
    if (!s.rest.empty()) or_rest(s);
    return *this;
  }
  varset& operator&=(const varset& s) {
    w0 &= s.w0;
    if (!rest.empty()) and_rest(s);
    return *this;
  }
  varset& operator-=(const varset& s) {
    w0 &= ~s.w0;
    if (!rest.empty() && !s.rest.empty()) diff_rest(s);
    return *this;
  }
  varset operator|(const varset& s) const {
    varset r(*this);
    r |= s;
    return r;
  }
  varset operator&(const varset& s) const {
    varset r(*this);
    r &= s;
    return r;
  }
  varset operator-(const varset& s) const {
    varset r(*this);
    r -= s;
    return r;
  }
  // maps each element x to x + k
  varset shift_up(const int& k) const {
    if (rest.empty() && k < 64 && (k == 0 || (w0 >> (64 - k)) == 0)) {
      varset r;
      r.w0 = w0 << k;
      return r;
    }
    return shift_up_rest(k);
  }
  // maps each element x to x - k, elements x <= k are dropped
  varset shift_down(const int& k) const {
    if (rest.empty() && k < 64) {
      varset r;
      r.w0 = w0 >> k;
      return r;
    }
    return shift_down_rest(k);
  }
  bool operator==(const varset& s) const { return w0 == s.w0 && rest == s.rest; }
  bool operator!=(const varset& s) const { return !(*this == s); }
  std::size_t hash() const {
    std::size_t h = static_cast<std::size_t>(w0 * 0x9e3779b97f4a7c15ULL);
    if (rest.empty()) return h;
    return hash_rest(h);
  }
private:
  uint64_t w0;
  std::vector<uint64_t> rest;
  std::size_t n_words() const { return 1 + rest.size(); }
  uint64_t word(const std::size_t& i) const;
  void set_word(const std::size_t& i, const uint64_t& w);
  void trim();
  int size_rest() const;
  int first_rest() const;
  int next_rest(const int& x) const;
  void insert_rest(const int& x);
  void erase_rest(const int& x);
  bool is_subset_rest(const varset& s) const;
  bool intersects_rest(const varset& s) const;
  void or_rest(const varset& s);
  void and_rest(const varset& s);
  void diff_rest(const varset& s);
  varset shift_up_rest(const int& k) const;
  varset shift_down_rest(const int& k) const;
  std::size_t hash_rest(std::size_t h) const;
};

struct varset_hash {
  std::size_t operator()(const varset& s) const {
    return s.hash();
  }
};

// Combine hash values (boost::hash_combine)
inline void hash_combine(std::size_t& seed, const std::size_t& h) {
  seed ^= h + 0x9e3779b97f4a7c15ULL + (seed << 6) + (seed >> 2);
}

#endif
