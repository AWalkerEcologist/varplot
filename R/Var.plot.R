#' Plots the credible intervals and mean of an INLA model's fixed effects.
#'
#' @param Model An INLA model of class "inla" that contains fixed effects.
#' @param Plot_Intercept Boolean (optional). Specifies whether to include the intercept in the resulting plot. Default is FALSE.
#' @param Plot_Title Character (optional). Specifies the title of the plot. Default is NULL.
#' @param Invert_Colors Boolean (optional). Specifies whether to invert the color palette when visualizing.
#' @param Quantiles A numeric vector (optional) listing the credible intervals of interest. Values must be between 0 and 1. Default vector is c(0.5, 0.8, 0.9). Must be the same length as the vector provided for CI_width.
#' @param CI_width A numeric vector (optional) listing the line size of the credible intervals. Default vector is c(2.5, 2, 1). Must be the same length as Quantiles.
#' @param Sig_threshold A numeric value (optional) specifying the credible interval threshold for coloring an effect size. Default is 0.9.
#'
#' @return Outputs a ggplot object of the fixed-effect variable effect sizes.
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_hline geom_errorbar
#' @importFrom ggplot2 scale_colour_manual coord_flip xlab ylab theme ggtitle
#' @importFrom stats reorder
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
#' Var.plot(Model, Plot_Intercept = TRUE)
#' }




Var.plot = function(Model,
                    Plot_Intercept = FALSE, Plot_Title = NULL, Invert_Colors = FALSE,
                    Quantiles = c(0.5, 0.8, 0.9), CI_width = c(2.5, 2, 1), Sig_threshold = NULL) {
  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop("Package 'INLA' is required. Install it with:
  install.packages('INLA', repos = c(getOption('repos'),
  INLA = 'https://inla.r-inla-download.org/R/stable'))")
  }


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


  df = get.cred.intervals(Model, Quantiles)

  # If the intercept is to be removed.
  if (!Plot_Intercept) { # Option is specified as false
    df = subset(df, !Var %in% c("Intercept", "(Intercept)", "(intercept)"))
  }

  #Marking if a row is significant.
  if (is.null(Sig_threshold)) {
    warning("Significance threshold not provided. Setting threshold to 0.9.")
    Sig_threshold = 0.9
  }
  if (length(Sig_threshold) > 1) {
    stop("Sig_threshold must be a single numeric value.")
  }
  if (!Sig_threshold %in% Quantiles) {
    stop("Specified quantiles do not contain the specified significance threshold. Please set Sig_threshold to a value from Quantiles.")
  }
  # Locating columns from df to identify significance with
  sig_cols = which(names(df) %in% c(paste0("CI_", Sig_threshold*100, "_L"), paste0("CI_", Sig_threshold*100, "_H")))

  df$sig <- rowSums(sign(df[,sig_cols]))/2

  if (is.null(Plot_Title)) {
    Title = "Coefficient Plot "
  } else {
    Title = Plot_Title
  }


  # Determining colors of the significance plot
  if (!Invert_Colors) {
    cols <- c("-1" = "#EF1414", "0" = "#6F7070", "1" = "#1485EF")
  } else {
    cols <- c("-1" = "#1485EF", "0" = "#6F7070", "1" = "#EF1414")
  }

  # Determining errorbar sizes
  if (length(Quantiles) != length(CI_width)) {
    stop("Quantile and CI_width must be the same length.")
  }

  y <- ggplot(df, aes(x = reorder(Var, mu), y = mu, color = factor(sig))) +
    geom_hline(yintercept = 0) +
    geom_point(size = 3)

  for (CIs in Quantiles) {
    bar_names = c(paste0("CI_", CIs*100, "_L"), paste0("CI_", CIs*100, "_H"))
    size = CI_width[which(CIs == Quantiles)]

    local({
      .ymin <- df[, bar_names[1]]
      .ymax <- df[, bar_names[2]]
      .size <- size
      y <<- y +
        geom_errorbar(aes(ymin = .ymin, ymax = .ymax),
                      width = 0, linewidth = .size)
    })
  }

  y <- y +
    scale_colour_manual(values = cols, breaks = c("-1",  "0",  "1")) +
    coord_flip() +
    xlab("") +
    ylab("Coefficient") +
    theme(legend.position = "none") +
    ggtitle(Title)

  return(y)
}
