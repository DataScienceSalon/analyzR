#------------------------------------------------------------------------------#
#                                assocMatrix                                   #
#------------------------------------------------------------------------------#
#' assocMatrix
#'
#' \code{assocMatrix} Creates a matrix of associations between nominal variables.
#'
#' @param x Dataframe to be summarized
#' @param vars Character vector containing the names of the variables to analyse
#'
#' @return A matrix of associations between pairs of nominal variables
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Analysis Functions
#' @export
assocMatrix <- function(x, vars) {

  # Initialize empty matrix
  assoc <- matrix(ncol = length(vars),
                  nrow = length(vars),
                  dimnames = list(vars,
                                  vars))

  # Function that accepts matrix for coefficients and data and returns a correlation matrix
  calculate_cramer <- function(m, df) {
    for (r in seq(nrow(m))){
      for (c in seq(ncol(m))){
        m[[r, c]] <- vcd::assocstats(table(df[[r]], df[[c]]))$cramer
      }
    }
    return(m)
  }

  assocMatrix  <- calculate_cramer(assoc ,x)

  return(assocMatrix)
}
