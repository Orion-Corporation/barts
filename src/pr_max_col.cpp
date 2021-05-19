#include "cpp11.hpp"

// For each column in a matrix of samples, determine the probability
// that the sample in that column has the highest value on the row.
[[cpp11::register]]
cpp11::doubles pr_max_col(cpp11::doubles_matrix m) {
  int nrow = m.nrow();
  int ncol = m.ncol();

  cpp11::writable::doubles p(ncol);
  std::fill(p.begin(), p.end(), 0);

  // Count max occurrences for each column
  for (int i = 0; i < nrow; i++) {
    double cur_value = m(i, 0);
    double max_value = m(i, 0);
    int max_index = 0;

    for (int j = 1; j < ncol; j++) {
      cur_value = m(i, j);
      if (cur_value > max_value) {
        max_value = cur_value;
        max_index = j;
      }
    }

    p[max_index] += 1;
  }

  // Normalize counts to probabilities
  for (int j = 0; j < ncol; j++) {
    p[j] /= static_cast<double>(nrow);
  }

  return p;
}
