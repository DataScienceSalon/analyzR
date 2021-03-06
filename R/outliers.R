#------------------------------------------------------------------------------#
#                                outliers                                      #
#------------------------------------------------------------------------------#
#' outliers
#'
#' \code{outliers} Statistics and plots to evaluate influence of outliers.
#'
#' Regression statistics are computed and plots are rendered with and without
#' each outlier. Regression results with all designated outliers removed
#' are also provided.
#'
#' @param x An LM model for evaluation.
#' @param target Character string containing the column name of outcome or target variable.
#' @param observations Numeric vector of outlier observations
#'
#' @return List containing two objects. The first is a dataframe containing
#' a summary of the adjusted R2 performance with and without each of the
#' designated observations.  The second object is list containing
#' the observation number, the LM object with the observation removed
#' and the regression statistics for the lm objects.
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
outliers <- function(x, target, observations) {

  data <- as.data.frame(x$model)
  fmla <- as.formula(paste(target, "~  ."))
  base <- glance(x)

  obs <- lapply(observations, function(o) { o })
  obs <- c(obs, list(observations))

  analysis <- lapply(obs, function(o) {
    df <- data[-c(o),]
    a <- list()
    a$observation <- c(o)
    a$model <- lm(fmla, data = df)
    a$stats <- glance(a$model)
    a
  })

  overview <- rbindlist(lapply(analysis, function(a) {
    o <- list()
    o$observation <- paste(c(a$observation), collapse = " ")
    o$with <- base$adj.r.squared
    o$without <- a$stats$adj.r.squared
    o$pctChange <- (o$without - o$with) / o$with * 100
    o
  }))
  names(overview) <- c("Observation", "Adj. R2 With Outlier", "Adj. R2 Without Outlier", "Percent Change")

  results <- list()
  results$analysis <- analysis
  results$summary <- overview
  return(results)

}
