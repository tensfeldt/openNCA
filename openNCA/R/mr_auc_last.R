#' Derived Metabolite of area under the concentration versus time curve from time 0 to time of the last measurable concentration
#'
#' This function gets derived metabolite of the area under the concentration versus time curve from time 0 to time (TLAST) of the
#' last measurable concentration (CLAST).\cr
#' 
#' @details
#' \strong{Equation} \cr
#' \figure{mr_auc_last.png} \cr
#'
#' @section Note:
#' \strong{mrauclast_metabolite}: Derived Metabolite of AUCLAST (metabolite)
#' \strong{mrauclast_parent}: Derived Metabolite of AUCLAST (parent)
#' \strong{mw_parent}: Molecular Weight (parent)
#' \strong{mw_metabolite}: Molecular Weight (metabolite)
#'
#' @param metabolite_auclast The metabolite area under the concentration versus time curve from time 0 to time of the last measurable concentration (numeric value)
#' @param parent_auclast The parent area under the concentration versus time curve from time 0 to time of the last measurable concentration (numeric value)
#' @param parent_mw The parent molecular weight (numeric value)
#' @param metabolite_mw The metabolite molecular weight (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item MRAUCLAST: derived metabolite area under the curve
#' }
#'
#' @examples
#' 
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
mr_auc_last <- function(metabolite_auclast = NULL, parent_auclast = NULL, parent_mw = NULL, metabolite_mw = NULL){
  if(is.null(metabolite_auclast) || is.null(parent_auclast) || is.null(parent_mw) || is.null(metabolite_mw)){
    count <- 0
    err <- ""
    if(is.null(metabolite_auclast)){
      err <- paste0(err, "'metabolite_auclast'")
      count <- count + 1
    }
    if(is.null(parent_auclast)){
      err <- paste0(err, ifelse(is.null(metabolite_auclast), ", ", ""), "'parent_auclast'")
      count <- count + 1
    }
    if(is.null(parent_mw)){
      err <- paste0(err, ifelse((is.null(metabolite_auclast) || is.null(parent_auclast)), ", ", ""), "'parent_mw'")
      count <- count + 1
    }
    if(is.null(metabolite_mw)){
      err <- paste0(err, ifelse((is.null(metabolite_auclast) || is.null(parent_auclast) || is.null(parent_mw)), ", ", ""), "'metabolite_mw'")
      count <- count + 1
    }
    if(count > 1){
      stop(paste0("Error in mr_auc_last: ", err, " vectors are NULL"))
    } else {
      stop(paste0("Error in mr_auc_last: ", err, " vector is NULL"))
    }
  }

  if(!(is.numeric(metabolite_auclast) && is.vector(metabolite_auclast)) ){
    stop("Error in mr_auc_last: 'metabolite_auclast' is not a numeric vector")
  }
  if(!(is.numeric(parent_auclast) && is.vector(parent_auclast)) ){
    stop("Error in mr_auc_last: 'parent_auclast' is not a numeric vector")
  }
  if(!(is.numeric(parent_mw) && is.vector(parent_mw)) ){
    stop("Error in mr_auc_last: 'parent_mw' is not a numeric vector")
  }
  if(!(is.numeric(metabolite_mw) && is.vector(metabolite_mw)) ){
    stop("Error in mr_auc_last: 'metabolite_mw' is not a numeric vector")
  }
  if(length(metabolite_auclast) != length(parent_auclast) || length(parent_auclast) != length(parent_mw) || length(parent_mw) != length(metabolite_mw)){
    stop("Error in mr_auc_last: length of 'metabolite_auclast', 'parent_auclast', 'parent_mw' and 'metabolite_mw' vectors are not equal")
  }
  
  mr_auclast <- (metabolite_auclast/parent_auclast)*parent_mw/metabolite_mw
  return(mr_auclast)
}
