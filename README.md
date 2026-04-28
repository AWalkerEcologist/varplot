
<!-- README.md is generated from README.Rmd. Please edit that file -->

# varplot

<!-- badges: start -->
<!-- badges: end -->

varplot provides a variety of quality of life functions for visualizing
the outputs of Integrated Nested Laplace Approximation (INLA) models.
Rather than having to manually derive and plot the credible intervals,
varplot handles the internal derivations to provide rapid assessment of
models.

## Installation

You can install the development version of varplot from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("AWalkerEcologist/varplot")
```

varplot depends on INLA, which is not on CRAN and must be installed
separately:

``` r
install.packages("INLA", repos = c(getOption("repos"), 
                 INLA = "https://inla.r-inla-download.org/R/stable"))
```

All other dependencies are installed automatically with the package.

## Functions

- `get.cred.intervals()` - Extracts the posterior means and specified
  credible intervals for each fixed effect in an INLA model, returning a
  concise data frame for interpretation and visualization.
- `Var.plot()` - Plots the posterior mean and credible intervals of an
  INLA model’s fixed effects, with strong effects highlighted for quick
  interpretation of modeled relationships.

## Usage

``` r
library(INLA)
library(varplot)

# Simulate some toy data
Intercept <- 1
Slope <- 0.5
x <- seq(0, 10, length.out = 1000)
y <- Intercept + Slope*x + rnorm(1000, 0, 1)
df <- data.frame(x, y)

# Fit an INLA model
Model <- inla(y ~ x, family = "normal", data = df)

# Extract credible intervals
get.cred.intervals(Model) # Default credible intervals
get.cred.intervals(Model, Quantiles = c(0.3, 0.5, 0.8, 0.9, 0.95)) # Manually providing credible intervals

# Plot the fixed effects of the model
Var.plot(Model, Plot_Intercept = T, Sig_threshold = 0.9)
```
