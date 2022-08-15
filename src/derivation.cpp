#include "derivation.h"

derivation::derivation() {
}

derivation::~derivation() {
}

void derivation::init() {
  deriv = "strict digraph InferenceTree {\n";
}

void derivation::finish() {
  deriv += "}\n";
}

void derivation::add_edge(const std::string& from, const std::string& to, const std::string& st) {
  deriv += get_label(from) + " -> " + get_label(to) + "[label=\"" + st + "\"]\n";
}

std::string derivation::get_label(const std::string& label) {
  std::string lab;
  for (unsigned i = 1; i <= labels.size(); i++) {
    lab = labels[i-1];
    if (lab.compare(label) == 0) return ("n" + std::to_string(i));
  }
  labels.push_back(label);
  deriv += "n" + std::to_string(labels.size()) + "[shape=polygon,sides=4,label=\"" + label + "\"]\n";
  return ("n" + std::to_string(labels.size()));
}

std::string derivation::get() const {
  return deriv;
}
