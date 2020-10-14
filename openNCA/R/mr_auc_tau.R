#' Derived Metabolite of the area under the concentration versus time curve from time 0 until the last time point
#'
#' This function gets derived metabolite of the area under the concentration versus time curve from time 0 until the last
#' time point.\cr
#' 
#' @details
#' \strong{Equation} \cr
#' \figure{mr_auc_tau.png} \cr
#'
#' @section Note:
#' \strong{mrauctau_metabolite}: Derived Metabolite of AUCTAUi (metabolite)
#' \strong{mrauctau_parent}: Derived Metabolite of AUCTAUi (parent)
#' \strong{mw_parent}: Molecular Weight (parent)
#' \strong{mw_metabolite}: Molecular Weight (metabolite)
#'
#' @param metabolite_auctau The metabolite area under the concentration versus time curve from time 0 until the last time point (numeric value)
#' @param parent_auctau The parent area under the concentration versus time curve from time 0 until the last time point (numeric value)
#' @param parent_mw The parent molecular weight (numeric value)
#' @param metabolite_mw The metabolite molecular weight (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item MRAUCTAU: derived metabolite area under the curve
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
mr_auc_tau <- function(metabolite_auctau = NULL, parent_auctau = NULL, parent_mw = NULL, metabolite_mw = NULL){
  if(is.null(metabolite_auctau) || is.null(parent_auctau) || is.null(parent_mw) || is.null(metabolite_mw)){
    count <- 0
    err <- ""
    if(is.null(metabolite_auctau)){
      err <- paste0(err, "'metabolite_auctau'")
      count <- count + 1
    }
    if(is.null(parent_auctau)){
      err <- paste0(err, ifelse(is.null(metabolite_auctau), ", ", ""), "'parent_auctau'")
      count <- count + 1
    }
    if(is.null(parent_mw)){
      err <- paste0(err, ifelse((is.null(metabolite_auctau) || is.null(parent_auctau)), ", ", ""), "'parent_mw'")
      count <- count + 1
    }
    if(is.null(metabolite_mw)){
      err <- paste0(err, ifelse((is.null(metabolite_auctau) || is.null(parent_auctau) || is.null(parent_mw)), ", ", ""), "'metabolite_mw'")
      count <- count + 1
    }
    if(count > 1){
      stop(paste0("Error in mr_auc_tau: ", err, " vectors are NULL"))
    } else {
      stop(paste0("Error in mr_auc_tau: ", err, " vector is NULL"))
    }
  }

  if(!(is.numeric(metabolite_auctau) && is.vector(metabolite_auctau)) ){
    stop("Error in mr_auc_tau: 'metabolite_auctau' is not a numeric vector")
  }
  if(!(is.numeric(parent_auctau) && is.vector(parent_auctau)) ){
    stop("Error in mr_auc_tau: 'parent_auctau' is not a numeric vector")
  }
  if(!(is.numeric(parent_mw) && is.vector(parent_mw)) ){
    stop("Error in mr_auc_tau: 'parent_mw' is not a numeric vector")
  }
  if(!(is.numeric(metabolite_mw) && is.vector(metabolite_mw)) ){
    stop("Error in mr_auc_tau: 'metabolite_mw' is not a numeric vector")
  }
  if(length(metabolite_auctau) != length(parent_auctau) || length(parent_auctau) != length(parent_mw) || length(parent_mw) != length(metabolite_mw)){
    stop("Error in mr_auc_tau: length of 'metabolite_auctau', 'parent_auctau', 'parent_mw' and 'metabolite_mw' vectors are not equal")
  }
  
  mr_auctau <- (metabolite_auctau/parent_auctau)*parent_mw/metabolite_mw
  return(mr_auctau)
}
