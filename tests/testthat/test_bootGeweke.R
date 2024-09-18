library(testthat)
library(coda)

# Test for valid inputs
test_that("bootstrap_geweke works for valid inputs", {
  x <- coda::as.mcmc.list(coda::mcmc(rnorm(1000)))
  result <- bootstrap_geweke(x, B = 500, n = 500, confidence_level = 0.95, frac1 = 0.1, frac2 = 0.5)
  expect_s3_class(result, "bootGeweke")
  expect_true(is.numeric(result$var1$z_scores))
  expect_true(length(result$var1$z_scores) == 500)
})

# Test for invalid 'x' (non-numeric input)
test_that("bootstrap_geweke catches invalid x", {
  expect_error(bootstrap_geweke("invalid_x"), "Input MCMC data must be an mcmc.list object.")
})

# Test for invalid 'B' (non-integer, negative, or zero)
test_that("bootstrap_geweke catches invalid B", {
  expect_error(bootstrap_geweke(coda::as.mcmc.list(coda::mcmc(rnorm(1000))), B = -1), "'B' must be a positive integer that larger than 100.")
  expect_error(bootstrap_geweke(coda::as.mcmc.list(coda::mcmc(rnorm(1000))), B = 0), "'B' must be a positive integer that larger than 100.")
  expect_error(bootstrap_geweke(coda::as.mcmc.list(coda::mcmc(rnorm(1000))), B = 10.5), "'B' must be a positive integer that larger than 100.")
})

# Test for invalid 'n' (non-integer, negative)
test_that("bootstrap_geweke catches invalid n", {
  expect_error(bootstrap_geweke(coda::as.mcmc.list(coda::mcmc(rnorm(1000))), n = -1), "'n' must be a positive integer.")
  expect_error(bootstrap_geweke(coda::as.mcmc.list(coda::mcmc(rnorm(1000))), n = 10.5), "'n' must be a positive integer.")
})

# Test for warning when n is not equal to length(x)
test_that("bootstrap_geweke warns when n is not equal to length(x)", {
  expect_warning(bootstrap_geweke(coda::as.mcmc.list(coda::mcmc(rnorm(1000))), n = 500), "'n' is not equal to the length of the combined MCMC chain. Proceeding with the specified 'n'.")
})

# Test for invalid 'confidence_level' (out of bounds)
test_that("bootstrap_geweke catches invalid confidence_level", {
  expect_error(bootstrap_geweke(coda::as.mcmc.list(coda::mcmc(rnorm(1000))), confidence_level = 0), "'confidence_level' must be a number between 0 and 1.")
  expect_error(bootstrap_geweke(coda::as.mcmc.list(coda::mcmc(rnorm(1000))), confidence_level = 1), "'confidence_level' must be a number between 0 and 1.")
})

# Test for invalid 'frac1' and 'frac2' (out of bounds)
test_that("bootstrap_geweke catches invalid frac1 and frac2", {
  expect_error(bootstrap_geweke(coda::as.mcmc.list(coda::mcmc(rnorm(1000))), frac1 = 0), "'frac1' must be a number between 0 and 1.")
  expect_error(bootstrap_geweke(coda::as.mcmc.list(coda::mcmc(rnorm(1000))), frac1 = 1), "'frac1' must be a number between 0 and 1.")
  expect_error(bootstrap_geweke(coda::as.mcmc.list(coda::mcmc(rnorm(1000))), frac2 = 0), "'frac2' must be a number between 0 and 1.")
  expect_error(bootstrap_geweke(coda::as.mcmc.list(coda::mcmc(rnorm(1000))), frac2 = 1), "'frac2' must be a number between 0 and 1.")
})

# Test for overlapping frac1 and frac2
test_that("bootstrap_geweke catches overlapping frac1 and frac2", {
  expect_error(bootstrap_geweke(coda::as.mcmc.list(coda::mcmc(rnorm(1000))), frac1 = 0.6, frac2 = 0.5), "'frac1' and 'frac2' segments must not overlap.")
})
