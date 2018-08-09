
#------------------------------------------------------------------------------#
#                                correlation                                   #
#------------------------------------------------------------------------------#
#' correlation
#'
#' \code{correlation} Computes and summarizes a correlation matrix
#'
#' Creates a correlation matrix and summarizes pairwise correlations
#' in a correlation table.
#'
#' @param x dataframe containing data to be analyzed
#' @param target optional character string containing the column name
#' for the target variable.  If this is provided, the target variable
#' will be excluded from the analysis. Submit the target variable
#' when analyzing correlations only among predictors.
#' @param threshold numeric indicating the minimum correlation threshold
#' to be reported in the correlation table.
#' @param bigMatrix Numeric indicating the number of terms above which
#' the terms are replaced with short labels for plotting.
#'
#' @return List containing the correlation matrix and pairwise correlation
#' table. If there are greater than 10 variables, the variable names
#' are replaced by a 2-3 character label for plotting purposes.
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Correlation Functions
#' @export
correlation <- function(x, target = NULL, threshold = 0, bigMatrix = 10) {

  # Extract terms to be analyzed
  vars <- colnames(x)[sapply(x, is.numeric)]
  if (!is.null(target)) vars <- vars[vars != target]

  # Create dataframe containing variables of interest
  quantData <- x[vars]

  if (length(vars) > bigMatrix) {

    # Create new labels for plotting and create cross-reference to original variables
    colnames(quantData) <- paste0("f", 1:ncol(quantData))
    xref <- data.frame(Label = colnames(quantData),
                       Variable = colnames(x[vars]), stringsAsFactors = FALSE)

    # Compute correlation matrix
    correlationMatrix <- cor(quantData)

    # Create pairwise correlation table
    corrTbl <- as.data.frame(as.table(correlationMatrix), stringsAsFactors = FALSE)

    # Add original terms to correlation table
    corrTbl <- merge(corrTbl, xref, by.x = "Var1", by.y = "Label")
    corrTbl <- merge(corrTbl, xref, by.x = "Var2", by.y = "Label")
    names(corrTbl)[names(corrTbl) == "Var1"] <- "Label.1"
    names(corrTbl)[names(corrTbl) == "Var2"] <- "Label.2"
    names(corrTbl)[names(corrTbl) == "Variable.x"] <- "Variable.1"
    names(corrTbl)[names(corrTbl) == "Variable.y"] <- "Variable.2"
    corrTbl <- corrTbl %>% select(Label.1, Variable.1, Label.2, Variable.2, Freq)

  } else {
    # Compute correlation matrix
    correlationMatrix <- cor(quantData)

    # Create pairwise correlation table
    corrTbl <- as.data.frame(as.table(correlationMatrix), stringsAsFactors = FALSE)
    names(corrTbl)[names(corrTbl) == "Var1"] <- "Variable.1"
    names(corrTbl)[names(corrTbl) == "Var2"] <- "Variable.2"
  }

  # If threshold provided, filter correlations
  names(corrTbl)[names(corrTbl) == "Freq"] <- "r"
  if (threshold > 0) {
    corrTbl <- corrTbl %>% filter(r > threshold)
  }

  # Eliminate duplicates and correlations between the same variable
  combinations = combn( colnames(x[vars]) , 2 , FUN = function( x ) { paste( x , collapse = "_" ) } )
  corrTbl = corrTbl[ paste( corrTbl$Variable.1  , corrTbl$Variable.2, sep = "_" ) %in% combinations , ]


  # Sort by the absolute value of the correlation, descending
  corrTbl <- corrTbl %>% arrange(-abs(r))

  result <- list()
  result$matrix <- correlationMatrix
  result$table <- corrTbl
  if (length(vars) > bigMatrix) result$xref <- xref

  return(result)
}
