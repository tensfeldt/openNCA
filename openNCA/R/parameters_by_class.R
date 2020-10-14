#' parameter_by_class
#'
#' This function returns the list of parameters based on the each Unit Class
#'
#' @param unitclass Unit Class 
#' @param checklist The character vector of parameter names from the PARAMETERLIST of the Model Configuration Template
#' @param dl Dependency List (Please refer to \code{\link{create_dependency_list}})
#' 
#' @section Returns:
#' \strong{List} \cr
#' \itemize{
#'  \item List of Parameters based on each Unit Class
#' }
#' 
#' @examples
#' #No appropriate examples
#' 
#' @author
#' \itemize{
#'  \item \strong{Thomas Tensfeldt, Pfizer}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{thomas.g.tensfeldt@pfizer.com}
#' }
#'
parameters_by_class <- function(unitclass, checklist, dl=create_dependency_list()) {
    parameters <- unitclass_parameters(unitclass)
    parameterregex <- unlist(lapply(parameters, FUN=function(x) { return(parameter_regex(x)) }))
    mp <- unlist(lapply(parameterregex, FUN=function(x) { parameter_required(x, checklist, simplify = FALSE) } ))
    mp <- names(mp[mp])
    mp <- mp[!duplicated(mp)]
  return(mp)
}
