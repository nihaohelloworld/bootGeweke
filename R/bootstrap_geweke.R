#' Bootstrap Geweke Diagnostic
#'
#' This function performs a bootstrap-based Geweke diagnostic on an MCMC object.
#' It computes Z-scores for the Geweke diagnostic, allowing for confidence intervals
#' based on the bootstrap samples.
#'
#' @name bootstrap_geweke
#' @useDynLib bootGeweke, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @param x An mcmc.list object where each list element corresponds to an MCMC chain,
#'          and within each chain, there are multiple parameters.
#' @param B Integer. The number of bootstrap samples to be generated. Default is 1000.
#' @param n Integer. The length of each bootstrap sample. Default is the combined length of all chains for each parameter.
#' @param confidence_level Numeric. The confidence level for calculating confidence intervals of Z-scores. Default is 0.95.
#' @param frac1 Numeric. The proportion of the chain used for the initial segment. Default is 0.1 (the first 10% of the chain).
#' @param frac2 Numeric. The proportion of the chain used for the last segment. Default is 0.5 (the last 50% of the chain).
#'
#' @return A list containing the summary statistics for each parameter, including:
#'   - z_mean: The mean of the Z-scores.
#'   - z_sd: The standard deviation of the Z-scores.
#'   - confidence_intervals: Confidence intervals for the Z-scores.
#'
#' @examples
#' \donttest{
#' library(coda)
#' mcmc_chain <- coda::as.mcmc.list(coda::mcmc(rnorm(1000)))  # Example MCMC chain
#' result <- bootstrap_geweke(mcmc_chain, B = 1000, n = 1000, confidence_level = 0.95)
#' summary(result)
#' plot(result)
#' }
#'
#' @importFrom stats quantile sd
#' @export
# bootstrap_geweke function
Rcpp::sourceCpp(file = "../bootGeweke/src/bootstrap_geweke.cpp")
bootstrap_geweke <- function(x, B = NULL, n = NULL, confidence_level = 0.95, frac1 = 0.1, frac2 = 0.5) {

  # Ensure that x is an mcmc.list object
  if (!inherits(x, "mcmc.list")) {
    stop("Input MCMC data must be an mcmc.list object.")
  }

  # Validate B (number of bootstrap samples)
  if (!is.null(B) && (!is.numeric(B) || B <= 100 || B != round(B))) {
    stop("'B' must be a positive integer that larger than 100.")
  }

  # Set default B to 1000 if not specified
  if (is.null(B)) {
    B <- 1000
  }

  # Validate confidence_level
  if (!is.numeric(confidence_level) || confidence_level <= 0 || confidence_level >= 1) {
    stop("'confidence_level' must be a number between 0 and 1.")
  }

  # Validate frac1 and frac2
  if (!is.numeric(frac1) || frac1 <= 0 || frac1 >= 1) {
    stop("'frac1' must be a number between 0 and 1.")
  }

  if (!is.numeric(frac2) || frac2 <= 0 || frac2 >= 1) {
    stop("'frac2' must be a number between 0 and 1.")
  }

  # Ensure frac1 and frac2 segments do not overlap
  if ((frac1 + frac2) > 1) {
    stop("'frac1' and 'frac2' segments must not overlap. Their sum should be <= 1.")
  }

  # Get the number of chains and parameters
  n_chains <- length(x)

  # Check if x is one-dimensional (i.e., only one parameter)
  if (is.null(ncol(x[[1]]))) {
    # One-dimensional case (single parameter)
    n_params <- 1
    combined_chains <- do.call(rbind, lapply(x, as.matrix))  # Combine chains into one column vector
    combined_chains <- matrix(combined_chains, ncol = 1)     # Convert to a 2D matrix with 1 column for compatibility
    param_names <- "Parameter_1"
  } else {
    # Multidimensional case (multiple parameters)
    n_params <- ncol(x[[1]])
    combined_chains <- do.call(rbind, lapply(x, as.matrix))  # Combine all chains into a single matrix
    param_names <- colnames(combined_chains)
  }

  # Combine chains for each parameter into one 2D matrix
  combined_chains <- do.call(rbind, lapply(x, as.matrix))  # Combine all chains into a single matrix

  # Validate n (number of elements in each bootstrap sample)
  if (!is.null(n) && (!is.numeric(n) || n <= 0 || n != round(n))) {
    stop("'n' must be a positive integer.")
  }

  # Set default n if not provided (use the length of the combined chain)
  if (is.null(n)) {
    n <- nrow(combined_chains)
  }

  # Warning if n is not equal to the length of the combined chains
  if (n != nrow(combined_chains)) {
    warning("'n' is not equal to the length of the combined MCMC chain. Proceeding with the specified 'n'.")
  }

  # Initialize list to store results for each parameter
  results <- vector("list", n_params)
  param_names <- colnames(combined_chains)
  names(results) <- param_names

  for (param in 1:n_params) {
    # Extract the combined chain for the current parameter
    param_chain <- combined_chains[, param]

    # Perform the bootstrap sampling and get Z-scores using C++
    z_scores <- bootstrapGewekeCpp(param_chain, B, n, frac1, frac2)

    # Calculate summary statistics for Z-scores
    z_mean <- mean(z_scores)
    z_sd <- sd(z_scores)
    confidence_intervals <- quantile(z_scores, probs = c((1 - confidence_level) / 2, 1 - (1 - confidence_level) / 2))

    # Store the summary results for this parameter
    results[[param]] <- list(
      z_scores = z_scores,
      z_mean = z_mean,
      z_sd = z_sd,
      confidence_intervals = confidence_intervals
    )
  }

  # Assign the class 'bootGeweke' to the results object
  class(results) <- "bootGeweke"

  return(results)
}

#' @method summary bootGeweke
#' @export
# Summary method for bootGeweke class
summary.bootGeweke <- function(object, ...) {
  cat("Bootstrap Geweke Diagnostic Summary:\n")
  for (param in seq_along(object)) {
    cat("\nParameter:", names(object)[param], "\n")
    cat("  Mean Z-score:           ", object[[param]]$z_mean, "\n")
    cat("  Standard Deviation:     ", object[[param]]$z_sd, "\n")
    cat("  Confidence Intervals:   ", object[[param]]$confidence_intervals[1],
        "to", object[[param]]$confidence_intervals[2], "\n")
  }
}


#' @method plot bootGeweke
#' @importFrom graphics abline hist
#' @export
# Plot method for bootGeweke class
plot.bootGeweke <- function(x, param = 1, ...) {
  if (!is.list(x) || !is.numeric(x[[param]]$z_mean)) {
    stop("The input object does not appear to be a valid 'bootGeweke' object.")
  }

  z_scores <- x[[param]]$z_scores
  if (!is.numeric(z_scores)) {
    stop("'z_scores' must be numeric.")
  }

  hist(z_scores, main = paste("Bootstrap Geweke Z-scores (Parameter:", names(x)[param], ")"),
       xlab = "Z-scores", breaks = 30)
  abline(v = x[[param]]$z_mean, col = "red", lwd = 2)
}
