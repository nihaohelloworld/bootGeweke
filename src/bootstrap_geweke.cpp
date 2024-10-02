#include <Rcpp.h>
#include <algorithm>  // For std::sort

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

// Function to compute mean
double compute_mean(NumericVector x) {
  int n = x.size();
  double sum = 0.0;
  for (int i = 0; i < n; i++) {
    sum += x[i];
  }
  return sum / n;
}

// Function to compute standard deviation
double compute_sd(NumericVector x, double mean) {
  int n = x.size();
  double sum = 0.0;
  for (int i = 0; i < n; i++) {
    sum += (x[i] - mean) * (x[i] - mean);
  }
  return sqrt(sum / (n - 1));
}

// Function to compute confidence intervals
NumericVector compute_confidence_intervals(NumericVector x, double confidence_level) {
  int n = x.size();
  NumericVector sorted_x = clone(x);
  std::sort(sorted_x.begin(), sorted_x.end());

  double lower_index = (1.0 - confidence_level) / 2.0 * (n - 1);
  double upper_index = (1.0 - (1.0 - confidence_level) / 2.0) * (n - 1);

  NumericVector confidence_intervals(2);
  confidence_intervals[0] = sorted_x[std::round(lower_index)];
  confidence_intervals[1] = sorted_x[std::round(upper_index)];

  return confidence_intervals;
}

// [[Rcpp::export]]
List bootstrapGewekeCpp(NumericVector chain, int B, int n, double frac1, double frac2, double confidence_level) {
  // Vector to hold Z-scores
  NumericVector z_scores(B);

  // Perform B bootstrap samples
  for (int i = 0; i < B; i++) {
    // Generate a bootstrap sample with replacement
    NumericVector bootstrap_sample = sample(chain, n, true);

    // Compute the Geweke Z-score for this bootstrap sample
    z_scores[i] = geweke_z(bootstrap_sample, frac1, frac2);
  }

  // Compute mean and standard deviation of Z-scores
  double z_mean = compute_mean(z_scores);
  double z_sd = compute_sd(z_scores, z_mean);

  // Compute confidence intervals
  NumericVector confidence_intervals = compute_confidence_intervals(z_scores, confidence_level);

  // Return results as a list
  return List::create(
    Named("z_scores") = z_scores,
    Named("z_mean") = z_mean,
    Named("z_sd") = z_sd,
    Named("confidence_intervals") = confidence_intervals
  );
}
