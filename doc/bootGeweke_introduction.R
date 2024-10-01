## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(bootGeweke)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("bootGeweke_1.0.3.tar.gz", repos = NULL, type = "source")

## ----eval=FALSE---------------------------------------------------------------
#  library(bootGeweke)
#  # Check for vignettes
#  browseVignettes(package = "bootGeweke")

## ----example_geweke_diagnostic, echo = TRUE-----------------------------------
library(bootGeweke)
library(coda)

# Simulate two parameters (mu and sigma) with three MCMC chains each
n_iter <- 1000  # Number of iterations per chain
n_chains <- 3   # Number of chains

# Create matrices for each chain, where each column represents a parameter
# First chain
chain1 <- cbind(mu = rnorm(n_iter, mean = 0, sd = 1),     # mu for chain 1
                sigma = rnorm(n_iter, mean = 2, sd = 0.5))  # sigma for chain 1

# Second chain
chain2 <- cbind(mu = rnorm(n_iter, mean = 0, sd = 1),     # mu for chain 2
                sigma = rnorm(n_iter, mean = 2, sd = 0.5))  # sigma for chain 2

# Third chain
chain3 <- cbind(mu = rnorm(n_iter, mean = 0, sd = 1),     # mu for chain 3
                sigma = rnorm(n_iter, mean = 2, sd = 0.5))  # sigma for chain 3

# Convert each chain to an mcmc object
mcmc_chain1 <- mcmc(chain1)
mcmc_chain2 <- mcmc(chain2)
mcmc_chain3 <- mcmc(chain3)

# Combine the chains into a single mcmc.list object
mcmc_object <- mcmc.list(mcmc_chain1, mcmc_chain2, mcmc_chain3)

# Perform the bootstrap Geweke diagnostic with multiple chains and parameters
# with default n as the length of the mcmc_object
result <- bootstrap_geweke(mcmc_object, B = 1000, confidence_level = 0.95, 
                           frac1 = 0.1, frac2 = 0.5)

## ----fig.width=8, fig.height=6------------------------------------------------
# Plot the Z-scores for the first parameter (mu)
plot(result, param = 1)

## ----fig.width=8, fig.height=6------------------------------------------------
# Plot the Z-scores for the second parameter (sigma)
plot(result, param = 2)

## -----------------------------------------------------------------------------
# Summary of the results
summary(result)

## -----------------------------------------------------------------------------
# Invalid input type
tryCatch({
  bootstrap_geweke("invalid_input")
}, error = function(e) {
  print(e)
})

## -----------------------------------------------------------------------------
# Combined all 3 chains from the previous example into one chain
mcmc_chains <- mcmc.list(mcmc_chain1, mcmc_chain2, mcmc_chain3)
# Use geweke.diag function to compute Z-score
coda::geweke.diag(mcmc_chains)

