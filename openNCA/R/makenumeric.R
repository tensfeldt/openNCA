#' makenumeric converts/forces individual vectors to numeric values
#' 
#' makenumeric attempts to force a vector's values to all numeric values.
#'
#' @param x vector to force to numeric
#' @return numeric vector
#'
#' @author
#' \itemize{
#'  \item \strong{Thomas Tensfeldt, Pfizer}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{thomas.g.tensfeldt@pfizer.com}
#' }
#'
#' @export
# ------------------------------------------------
# Function to convert dataframe fields that are completely numeric to numeric
makenumeric <- function(x) {
  if(is.factor(x)) { x <- as.character(x) }
  if(!any(is.na(as.numeric(x)))) { x <- as.numeric(x) }
  return(x)
}
