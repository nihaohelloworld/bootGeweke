#' bootGeweke: A Package for Bootstrap Based Geweke Diagnostics on MCMC Chains
#'
#' The `bootGeweke` package provides a function to perform a bootstrap-based Geweke diagnostic
#' on Markov Chain Monte Carlo (MCMC) objects. This package allows users to compute Z-scores
#' for Geweke diagnostics with bootstrap sampling, providing standard deviation and
#' confidence intervals for better interpretation of convergence diagnostics.
#'
#' @section Functions:
#' - `bootstrap_geweke()`: Perform a bootstrap-based Geweke diagnostic on an MCMC object.
#'
#' @name bootGeweke
#' @importFrom Rcpp sourceCpp
#' @useDynLib bootGeweke, .registration = TRUE
NULL
