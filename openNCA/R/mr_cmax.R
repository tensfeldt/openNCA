#' Derived Metabolite of the Maximum Observed Concentration
#'
#' This function gets derived metabolite of the maximum observed concentration that is obtained by the inspection of the data.\cr
#' 
#' @details
#' \strong{Equation} \cr
#' \figure{mr_cmax.png} \cr
#'
#' @section Note:
#' \strong{mrcmax_metabolite}: Derived Metabolite of CMAX (metabolite)
#' \strong{mrcmax_parent}: Derived Metabolite of CMAX (parent)
#' \strong{mw_parent}: Molecular Weight (parent)
#' \strong{mw_metabolite}: Molecular Weight (metabolite)
#'
#' @param metabolite_cmax The metabolite maximum observed concentration (numeric value)
#' @param parent_cmax The parent maximum observed concentration (numeric value)
#' @param parent_mw The parent molecular weight (numeric value)
#' @param metabolite_mw The metabolite molecular weight (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item MRCMAX: derived metabolite area under the curve
#' }
#'
#' @examples
#' #No appropriate examples
#' 
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
mr_cmax <- function(metabolite_cmax = NULL, parent_cmax = NULL, parent_mw = NULL, metabolite_mw = NULL){
  if(is.null(metabolite_cmax) || is.null(parent_cmax) || is.null(parent_mw) || is.null(metabolite_mw)){
    count <- 0
    err <- ""
    if(is.null(metabolite_cmax)){
      err <- paste0(err, "'metabolite_cmax'")
      count <- count + 1
    }
    if(is.null(parent_cmax)){
      err <- paste0(err, ifelse(is.null(metabolite_cmax), ", ", ""), "'parent_cmax'")
      count <- count + 1
    }
    if(is.null(parent_mw)){
      err <- paste0(err, ifelse((is.null(metabolite_cmax) || is.null(parent_cmax)), ", ", ""), "'parent_mw'")
      count <- count + 1
    }
    if(is.null(metabolite_mw)){
      err <- paste0(err, ifelse((is.null(metabolite_cmax) || is.null(parent_cmax) || is.null(parent_mw)), ", ", ""), "'metabolite_mw'")
      count <- count + 1
    }
    if(count > 1){
      stop(paste0("Error in mr_cmax: ", err, " vectors are NULL"))
    } else {
      stop(paste0("Error in mr_cmax: ", err, " vector is NULL"))
    }
  }

  if(!(is.numeric(metabolite_cmax) && is.vector(metabolite_cmax)) ){
    stop("Error in mr_cmax: 'metabolite_cmax' is not a numeric vector")
  }
  if(!(is.numeric(parent_cmax) && is.vector(parent_cmax)) ){
    stop("Error in mr_cmax: 'parent_cmax' is not a numeric vector")
  }
  if(!(is.numeric(parent_mw) && is.vector(parent_mw)) ){
    stop("Error in mr_cmax: 'parent_mw' is not a numeric vector")
  }
  if(!(is.numeric(metabolite_mw) && is.vector(metabolite_mw)) ){
    stop("Error in mr_cmax: 'metabolite_mw' is not a numeric vector")
  }
  if(length(metabolite_cmax) != length(parent_cmax) || length(parent_cmax) != length(parent_mw) || length(parent_mw) != length(metabolite_mw)){
    stop("Error in mr_cmax: length of 'metabolite_cmax', 'parent_cmax', 'parent_mw' and 'metabolite_mw' vectors are not equal")
  }
  
  mr_cmax <- (metabolite_cmax/parent_cmax)*parent_mw/metabolite_mw
  return(mr_cmax)
}
