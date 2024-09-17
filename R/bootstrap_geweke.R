#' Bootstrap Geweke Diagnostic
#'
#' This function performs a bootstrap-based Geweke diagnostic on an MCMC object.
#' It computes Z-scores for the Geweke diagnostic, allowing for confidence intervals
#' based on the bootstrap samples.
#'
#' @importFrom Rcpp sourceCpp
#' @param x An MCMC object, which can potentially contain more than one chain. If multiple chains are present, they will be combined into a single chain before the bootstrap process.
#' @param B Integer. The number of bootstrap samples to be generated. Default is 1000.
#' @param n Integer. The length of each bootstrap sample. By default, this is set to the same length as the combined chain.
#' @param confidence_level Numeric. The confidence level for calculating confidence intervals of Z-scores. Default is 0.95.
#' @param frac1 Numeric. The proportion of the chain used for the initial segment. Default is 0.1 (the first 10% of the chain).
#' @param frac2 Numeric. The proportion of the chain used for the last segment. Default is 0.5 (the last 50% of the chain).
#'
#' @importFrom stats quantile sd
#' @return A list of class "bootGeweke" containing:
#' \describe{
#'   \item{bootstrap_samples}{A matrix containing all bootstrap samples (B x n).}
#'   \item{z_scores}{A vector of bootstrap Z-scores of length B.}
#'   \item{z_mean}{The mean of the Z-scores.}
#'   \item{z_sd}{The standard deviation of the Z-scores.}
#'   \item{confidence_intervals}{A vector of confidence intervals for the Z-scores based on the provided confidence level.}
#' }
#'
#' @examples
#' \dontrun{
#' mcmc_chain <- rnorm(1000)  # Example MCMC chain
#' result <- bootstrap_geweke(mcmc_chain, B = 500, n = 100, confidence_level = 0.95)
#' summary(result)
#' plot(result)
#' }
#'
#' @export
# bootstrap_geweke function
bootstrap_geweke <- function(x, B = NULL, n = NULL, confidence_level = 0.95, frac1 = 0.1, frac2 = 0.5) {

  # Combine multiple chains into one, if necessary
  if (is.list(x)) {
    x <- unlist(x)
  }

  # Set B to 1000 if not specified
  if (is.null(B)) {
    B <- 1000
  }
  # Set n to the length of the chain if not specified
  if (is.null(n)) {
    n <- length(x)
  }

  # Call the C++ function to do the heavy lifting
  result <- bootstrapGewekeCpp(x, B, n, frac1, frac2)

  # Create a class for the result
  structure(list(
    bootstrap_samples = result$bootstrap_samples,
    z_scores = result$z_scores,
    z_mean = mean(result$z_scores),
    z_sd = sd(result$z_scores),
    confidence_intervals = quantile(result$z_scores, probs = c((1 - confidence_level) / 2, 1 - (1 - confidence_level) / 2))
  ), class = "bootGeweke")
}

#' @method summary bootGeweke
#' @export
# Summary method for bootGeweke class
summary.bootGeweke <- function(object, ...) {
  cat("Summary of Bootstrap Geweke Diagnostic\n")
  cat("Mean Z-score:", object$z_mean, "\n")
  cat("Standard Deviation of Z-scores:", object$z_sd, "\n")
  cat("Confidence Intervals of Z-scores:", object$confidence_intervals, "\n")
}

#' @method plot bootGeweke
#' @importFrom graphics abline hist
#' @export
# Plot method for bootGeweke class
plot.bootGeweke <- function(x, ...) {
  hist(x$z_scores, main = "Bootstrap Geweke Z-scores", xlab = "Z-scores", breaks = 30)
  abline(v = x$z_mean, col = "red", lwd = 2)
}
