#' valid_models
#'
#' This function returns the list of valid model configurations that are supported
#'
#' @param dl Dependency List (Please refer to \code{\link{create_dependency_list}})
#' 
#' @section Returns:
#' \strong{List} \cr
#' \itemize{
#'  \item List of Valid Model configurations
#' }
#' 
#' @examples
#' 
#' @author
#' \itemize{
#'  \item \strong{Thomas Tensfeldt, Pfizer}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{thomas.g.tensfeldt@pfizer.com}
#' }
#'
valid_models <- function(dl=create_dependency_list()) {
  y <- lapply(dl, FUN=function(x) { x$valid_models } )
  y <- unlist(y)
  y <- as.vector(y[!duplicated(y)])
  return(y)
} 
