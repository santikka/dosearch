#ifndef DERIVATION_H
#define DERIVATION_H

#include <unordered_map>
#include <string>

class derivation {
public:
  derivation();
  void init();
  void finish();
  void add_edge(const std::string& from, const std::string& to, const std::string& st);
  std::string get() const;
  virtual ~derivation();
private:
  std::string deriv;
  std::unordered_map<std::string, int> labels;
  std::string get_label(const std::string& label);
};

#endif
