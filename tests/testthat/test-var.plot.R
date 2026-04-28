library(INLA)
Intercept <- 1
Slope <- 0.5
x <- seq(0, 10, length.out = 100)
y <- Intercept + Slope * x + rnorm(100, 0, 1)
df <- data.frame(x, y)
Model <- inla(y ~ x, family = "normal", data = df)

test_that("returns a ggplot object", {
  result <- suppressWarnings(Var.plot(Model))
  expect_s3_class(result, "ggplot")
})

test_that("errors on non-inla object", {
  expect_error(Var.plot(list()), "must be an object of class 'inla'")
})

test_that("errors when Sig_threshold doesn't match quantiles", {
  expect_error(
    suppressWarnings(Var.plot(Model, Sig_threshold = 0.95)),
    "Specified quantiles do not contain the specified significance threshold"
  )
})

test_that("errors when Quantiles and CI_width are different lengths", {
  expect_error(
    Var.plot(Model, Quantiles = c(0.5, 0.8), CI_width = c(1), Sig_threshold = 0.8),
    "same length"
  )
})

test_that("errors when Sig_threshold has multiple values", {
  expect_error(
    Var.plot(Model, Sig_threshold = c(0.8, 0.9)),
    "single numeric value"
  )
})

test_that("warns when Sig_threshold not provided", {
  expect_warning(Var.plot(Model), "Significance threshold not provided")
})

test_that("Plot_Intercept = TRUE includes intercept in output", {
  result <- suppressWarnings(Var.plot(Model, Plot_Intercept = TRUE))
  expect_s3_class(result, "ggplot")
  expect_true("(Intercept)" %in% result$data$Var)
})

test_that("Plot_Intercept = FALSE excludes intercept from output", {
  result <- suppressWarnings(Var.plot(Model, Plot_Intercept = FALSE))
  expect_false("(Intercept)" %in% result$data$Var)
})

test_that("Invert_Colors does not error", {
  result <- suppressWarnings(Var.plot(Model, Invert_Colors = TRUE))
  expect_s3_class(result, "ggplot")
})
