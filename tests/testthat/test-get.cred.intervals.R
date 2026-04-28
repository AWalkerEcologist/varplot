# Build a toy model once for all tests
library(INLA)
Intercept <- 1
Slope <- 0.5
x <- seq(0, 10, length.out = 100)
y <- Intercept + Slope * x + rnorm(100, 0, 1)
df <- data.frame(x, y)
Model <- inla(y ~ x, family = "normal", data = df)

test_that("output is a data frame", {
  result <- suppressWarnings(get.cred.intervals(Model))
  expect_s3_class(result, "data.frame")
})

test_that("output has correct columns", {
  result <- suppressWarnings(get.cred.intervals(Model))
  expect_true(all(c("Var", "mu", "CI_50_L", "CI_50_H") %in% names(result)))
})

test_that("output has correct number of rows", {
  result <- suppressWarnings(get.cred.intervals(Model))
  expect_equal(nrow(result), 2)
})

test_that("errors on non-inla object", {
  expect_error(get.cred.intervals(list()), "must be an object of class 'inla'")
})

test_that("errors on invalid quantiles", {
  expect_error(get.cred.intervals(Model, Quantiles = c(0.5, 1.5)),
               "between 0 and 1")
})

test_that("warns when quantiles not provided", {
  expect_warning(get.cred.intervals(Model, Quantiles = NULL),
                 "Quantiles not provided")
})
