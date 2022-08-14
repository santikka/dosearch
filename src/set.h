#ifndef SET_H
#define SET_H

#include <vector>

const int MAX_SIZE = 30;

int set_size(const int& set);

bool in_set(const int& x, const int& set);

int full_set(const int& n);

int unary(const int& x);

int set_union(const int& set1, const int& set2);

// get non-empty subsets of a set of size n in order of cardinality (banker's sequence)
std::vector<int> get_subsets(const int &n);

// recursive function to generate banker's sequence
void generate(std::vector<int>& sets, const int& n, int z, int j, int a, const int& b);

#endif
