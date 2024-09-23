#include <Rcpp.h>
using namespace Rcpp;

// Helper function to compute Geweke Z-score
double geweke_z(NumericVector chain, double frac1, double frac2) {
  int n = chain.size();
  int n1 = frac1 * n;  // Number of initial points
  int n2 = frac2 * n;  // Number of last points

  // Extract the first and last parts of the chain
  NumericVector first = chain[Range(0, n1 - 1)];
  NumericVector last = chain[Range(n - n2, n - 1)];

  // Compute means and variances
  double mean1 = mean(first);
  double mean2 = mean(last);

  double var1 = var(first) / n1;
  double var2 = var(last) / n2;

  // Calculate the Geweke Z-score
  double z_score = (mean1 - mean2) / sqrt(var1 + var2);
  return z_score;
}

// [[Rcpp::export]]
NumericVector bootstrapGewekeCpp(NumericVector chain, int B, int n, double frac1, double frac2) {
  // Vector to hold Z-scores
  NumericVector z_scores(B);

  // Perform B bootstrap samples
  for (int i = 0; i < B; i++) {
    // Generate a bootstrap sample with replacement
    NumericVector bootstrap_sample = sample(chain, n, true);

    // Compute the Geweke Z-score for this bootstrap sample
    z_scores[i] = geweke_z(bootstrap_sample, frac1, frac2);
  }

  // Return the Z-scores
  return z_scores;
}
