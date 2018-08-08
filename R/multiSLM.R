#------------------------------------------------------------------------------#
#                                multiSLM                                      #
#------------------------------------------------------------------------------#
#' multiSLM
#'
#' \code{multiSLM} Performs simple linear regression on multiple variables.
#'
#' Performs simple linear regression on multiple variables
#' vis-a-vis the target variable and returns a list containing the models,
#' the regression results and the statistics. The regression statisics for
#' each variable are summarized in descending order of r squared.
#'
#' @param x Dataframe to be summarized
#' @param vars Character vector containing the names of the variables to analyse
#' @param target Column name of target variable.
#'
#' @return A list containing a list of model information by predictor and a
#' dataframe summarizing regression statistics by predictor. The model information
#' is comprised of a list containing the model object, the regression results,
#' and the regression statistics.
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Analysis Functions
#' @export
multiSLM <- function(x, vars, target) {

  models <- lapply(vars, function(v) {
    m <- list()
    fmla <- as.formula(paste(target, v, sep = "~"))
    m$model <- lm(fmla, data = x)
    m$modelFit <- tidy(m$model)
    m$modelStats <- glance(m$model)
    term <- data.frame(term = v, stringsAsFactors = FALSE)
    m$modelStats <- cbind(term, m$modelStats)
    m
  })
  names(models) <- vars


  tbl <- rbindlist(lapply(models, function(m) {
    m$modelStats
  }))

  tbl <- tbl %>% arrange(-r.squared)


  r <- list()
  r$models <- models
  r$summary <- tbl

  return(r)
}
