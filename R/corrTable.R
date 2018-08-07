
#------------------------------------------------------------------------------#
#                                corrTest                                      #
#------------------------------------------------------------------------------#
#' corrTest
#'
#' \code{corrTest} Computes correlations between numeric predictors and a target variable
#'
#' @param x Dataframe to be analyzed
#' @param target Character string containig the column name of target variable.
#' @param method The correlation formula to use. Valid values include c("pearson", "kendall", "spearman")
#'
#' @return Dataframe of correlations between predictors and the target variable
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family EDA Functions
#' @family Correlation Functions
#' @export
corrTest <- function(x, target, method = "pearson") {

  library(stats)

  vars <- colnames(df)[sapply(df, is.numeric)]
  vars <- vars[vars != target]

  tests <- rbindlist(lapply(vars, function(v) {
    xVar <- x[[v]]
    yVar <- x[[target]]
    ct <- cor.test(x = xVar, y = yVar, method = method)
    data.frame(Target = target,
               Predictor = v,
               Correlation = ct$estimate,
               CI.Lower = ct$conf.int[1],
               CI.Upper = ct$conf.int[2],
               p.value = round(ct$p.value,3))
  }))
  tests <- tests %>% arrange(-abs(Correlation))
  return(tests)
}
