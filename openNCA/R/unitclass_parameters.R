#' unitclass_parameters
#'
#' This function returns the list of parameters based on the unit class specified
#'
#' @param parameter Parameter
#' @param dl Dependency List (Please refer to \code{\link{create_dependency_list}})
#' 
#' @section Returns:
#' \strong{List} \cr
#' \itemize{
#'  \item List of Parameters based on Unit Class
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
unitclass_parameters <- function(parameter, dl=create_dependency_list()) {
  y <- lapply(dl, FUN=function(x) { if(parameter_required(parameter, x$unit_class)) { return(TRUE) } else { return(FALSE) }  })
  k <- y[y==TRUE]
  k <- names(k)
  return(k)
}
