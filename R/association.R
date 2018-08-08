
#------------------------------------------------------------------------------#
#                                association                                   #
#------------------------------------------------------------------------------#
#' association
#'
#' \code{association} Computes and summarizes an association matrix
#'
#' Creates a association matrix and summarizes pairwise associations
#' in a association table.
#'
#' @param x dataframe containing data to be analyzed. Categorical variables
#' must be factors.
#' @param target optional character string containing the column name
#' for the target variable.  If this is provided, the target variable
#' will be excluded from the analysis. Submit the target variable
#' when analyzing associations only among predictors.
#' @param threshold numeric indicating the minimum association threshold
#' to be reported in the association table.
#'
#' @return List containing the association matrix and pairwise association
#' table. If there are greater than 10 variables, the variable names
#' are replaced by a 2-3 character label for plotting purposes.
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family association Functions
#' @export
association <- function(x, target = NULL, threshold = 0) {

  # Extract terms to be analyzed
  vars <- colnames(x)[sapply(x, is.factor)]
  if (!is.null(target)) vars <- vars[vars != target]

  # Create dataframe containing variables of interest
  qualData <- x[vars]

  if (length(vars) > 10) {

    # Create new labels for plotting and create cross-reference to original variables
    colnames(qualData) <- paste0("f", 1:ncol(qualData))
    xref <- data.frame(Label = colnames(qualData),
                       Variable = colnames(x[vars]), stringsAsFactors = FALSE)

    # Compute association matrix
    associationMatrix <- assocMatrix(qualData, xref$Label)

    # Create pairwise association table
    assocTbl <- as.data.frame(as.table(associationMatrix), stringsAsFactors = FALSE)

    # Add original terms to association table
    assocTbl <- merge(assocTbl, xref, by.x = "Var1", by.y = "Label")
    assocTbl <- merge(assocTbl, xref, by.x = "Var2", by.y = "Label")
    names(assocTbl)[names(assocTbl) == "Var1"] <- "Label.1"
    names(assocTbl)[names(assocTbl) == "Var2"] <- "Label.2"
    names(assocTbl)[names(assocTbl) == "Variable.x"] <- "Variable.1"
    names(assocTbl)[names(assocTbl) == "Variable.y"] <- "Variable.2"
    assocTbl <- assocTbl %>% select(Label.1, Variable.1, Label.2, Variable.2, Freq)

  } else {
    # Compute association matrix
    associationMatrix <- assocMatrix(qualData, vars)

    # Create pairwise association table
    assocTbl <- as.data.frame(as.table(associationMatrix), stringsAsFactors = FALSE)
    names(assocTbl)[names(assocTbl) == "Var1"] <- "Variable.1"
    names(assocTbl)[names(assocTbl) == "Var2"] <- "Variable.2"
  }

  # If threshold provided, filter associations
  names(assocTbl)[names(assocTbl) == "Freq"] <- "r"
  if (threshold > 0) {
    assocTbl <- assocTbl %>% filter(r > threshold)
  }

  # Eliminate duplicates and associations between the same variable
  combinations = combn( colnames(x[vars]) , 2 , FUN = function( x ) { paste( x , collapse = "_" ) } )
  assocTbl = assocTbl[ paste( assocTbl$Variable.1  , assocTbl$Variable.2, sep = "_" ) %in% combinations , ]


  # Sort by the absolute value of the association, descending
  assocTbl <- assocTbl %>% arrange(-abs(r))

  result <- list()
  result$matrix <- associationMatrix
  result$table <- assocTbl
  if (length(vars) > 10) result$xref <- xref

  return(result)
}
