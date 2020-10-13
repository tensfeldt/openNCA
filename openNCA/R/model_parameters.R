#' model_parameters
#' 
#' This function will generate a list of parameters based on the specified model and dosing type\cr
#' 
#' @param model Model and Dosing Type
#' @param dl Dependency List (Please refer to \code{\link{create_dependency_list}})
#' 
#' @section Returns:
#' \strong{List} \cr
#' \itemize{
#'  \item List of parameters based on model and dosing type
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
#' @export
model_parameters <- function(model, dl=create_dependency_list()) {
  if(missing(model)) { model <- valid_models() }
  y <- lapply(dl, FUN=function(x) { if(parameter_required(model, x$valid_models)) { return(TRUE) } else { return(FALSE) }  })
  k <- y[y==TRUE]
  k <- names(k)
  return(k)
}
