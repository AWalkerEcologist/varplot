#' Derives the credible intervals for marginal distributions of fixed-effect variables within an INLA model.
#'
#' @param Model An INLA model of class "inla" that contains fixed effects.
#' @param Quantiles A numeric vector listing the credible intervals of interest. Values must be between 0 and 1. Default vector is c(0.5, 0.8, 0.9)
#'
#' @return Outputs a dataframe containing the fixed effects and the upper and lower bounds of the credibility intervals specified.
#' @export
#' @importFrom INLA inla.emarginal inla.hpdmarginal
#'
#' @examples
#' \donttest{
#' library(INLA)
#' Intercept <- 1
#' Slope <- 0.5
#' x <- seq(0, 10, length.out = 1000)
#' y <- Intercept + Slope * x + rnorm(1000, 0, 1)
#' df <- data.frame(x, y)
#' formula <- y ~ x
#' Model <- inla(formula, family = "normal", data = df)
#' get.cred.intervals(Model)
#' }


get.cred.intervals = function(Model, Quantiles = NULL) {
  if (!inherits(Model, "inla")) {
    stop("Model must be an object of class 'inla'")
  }
  if (is.null(Model$marginals.fixed)) {
    stop("Model contains no fixed effects marginals")
  }
  if (is.null(Quantiles)) {
    Quantiles = c(0.5, 0.8, 0.9)
    warning("Quantiles not provided. Using generic 0.5, 0.8, 0.9 quantiles.")
  }
  if (max(Quantiles) >= 1 || min(Quantiles) <= 0) {
    stop("Quantile values must be between 0 and 1")
  }
  if (any(duplicated(Quantiles))) {
    warning("Duplicated quantile values provided. Output will have duplicated columns.")
  }

  terms = names(Model$marginals.fixed)


  # Creating quantile columns. One for the low value and one for the high value.
  quant_names = c(paste0("CI_", Quantiles*100, "_L"), paste0("CI_", Quantiles*100, "_H"))
  df = data.frame(
    matrix(
      data = rep(NA, (length(quant_names)+2)*length(terms)),
      ncol = length(quant_names)+2)
  )
  names(df) = c("Var", "mu", quant_names)
  df$Var = terms


  for (i in seq_along(terms)) {
    term = terms[i]
    df$mu[i] = inla.emarginal(function(x) x, Model$marginals.fixed[[term]])             #posterior mean for fixed

    for (j in Quantiles) {
      cred_int = inla.hpdmarginal(j, Model$marginals.fixed[[term]])
      cols = which(names(df) %in% c(paste0("CI_", j*100, "_L"), paste0("CI_", j*100, "_H")))
      df[i, cols] = as.numeric(cred_int)
    }
  }
  return(df)
}
