#' parameter_regex
#'
#' This function returns the regular expression associated to the specified parameter
#'
#' @param parameter Parameter
#' @param dl Dependency List (Please refer to \code{\link{create_dependency_list}})
#' 
#' @section Returns:
#' \strong{List} \cr
#' \itemize{
#'  \item Regular Expresssion
#' }
#' 
#' @examples
#' parameter_regex("CMAX")
#' #"^CMAX$"
#' 
#' parameter_regex("TMAX")
#' #"^TMAX$"
#' 
#' parameter_regex("AUCT)
#' #"^AUC(T|[0-9]+?)$"
#' 
#' @author
#' \itemize{
#'  \item \strong{Thomas Tensfeldt, Pfizer}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{thomas.g.tensfeldt@pfizer.com}
#' }
#'
parameter_regex <- function(parameter, dl=create_dependency_list()) {
    regex <- dl[[parameter]]$regex
  return(regex)
}
