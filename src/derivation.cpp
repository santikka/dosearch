#include "derivation.h"

derivation::derivation() {
}

derivation::~derivation() {
}

void derivation::init() {
  deriv = "strict digraph InferenceTree {\n";
  labels.clear();
}

void derivation::finish() {
  deriv += "}\n";
}

void derivation::add_edge(const std::string& from, const std::string& to, const std::string& st) {
  deriv += get_label(from) + " -> " + get_label(to) + "[label=\"" + st + "\"]\n";
}

std::string derivation::get_label(const std::string& label) {
  auto it = labels.find(label);
  if (it != labels.end()) return "n" + std::to_string(it->second);
  int id = labels.size() + 1;
  labels.emplace(label, id);
  deriv += "n" + std::to_string(id) + "[shape=polygon,sides=4,label=\"" + label + "\"]\n";
  return "n" + std::to_string(id);
}

std::string derivation::get() const {
  return deriv;
}
