#' Dependent Parameters
#' 
#' This function will generate a list of dependent parameters based on the specified parameter\cr
#' 
#' @param parameter Parameter Name
#' @param dl Dependency List
#' @param depth Logical Value to enable sub-dependent parameters
#' 
#' @section Returns:
#' \strong{List} \cr
#' \itemize{
#'  \item List of dependent parameters
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
dependent_parameters <- function(parameter, dl=create_dependency_list(), depth=TRUE) {
  y <- lapply(dl, FUN=function(x) { if(parameter_required(parameter, x$predecessors)) { return(TRUE) } else { return(FALSE) }  })
  k <- y[y==TRUE]
  k <- names(k)
  if(depth){
    if(length(k) > 0){
      m <- k
      while(length(m) > 0){
        n <- c()
        for(i in 1:length(m)){
          z <- lapply(dl, FUN=function(x) { if(parameter_required(m[i], x$predecessors)){ return(TRUE) } else { return(FALSE) }  })
          l <- z[z==TRUE]
          l <- names(l)
          if(length(l) > 0  && !(l %in% k)){
            n <- c(n, l)
            k <- c(k, l)
          }
        }
        m <- n
      }
    }
  }
  return(k)
}
