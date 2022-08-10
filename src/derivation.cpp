#include "derivation.h"

using namespace std;

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

void derivation::add_edge(const string& from, const string& to, const string& st) {
  deriv += get_label(from) + " -> " + get_label(to) + "[label=\"" + st + "\"]\n";
}

string derivation::get_label(const string& label) {
  string lab;
  for (unsigned i = 1; i <= labels.size(); i++) {
    lab = labels[i-1];
    if (lab.compare(label) == 0) return ("n" + to_string(i));
  }
  labels.push_back(label);
  deriv += "n" + to_string(labels.size()) + "[shape=polygon,sides=4,label=\"" + label + "\"]\n";
  return ("n" + to_string(labels.size()));
}

string derivation::get() const {
  return deriv;
}