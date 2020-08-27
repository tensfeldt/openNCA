#' Derived Metabolite of the area under the concentration versus time cruve from time 0 to infinity (Observed)
#'
#' This function gets derived metabolite of the area under the concentration versus time curve from time 0 to
#' infinity (Observed).\cr
#' 
#' @details
#' \strong{Equation} \cr
#' \figure{mr_auc_inf_o.png} \cr
#'
#' @section Note:
#' \strong{mraucinfo_metabolite}: Derived Metabolite of AUCINFO (metabolite)
#' \strong{mraucinfo_parent}: Derived Metabolite of AUCINFO (parent)
#' \strong{mw_parent}: Molecular Weight (parent)
#' \strong{mw_metabolite}: Molecular Weight (metabolite)
#'
#' @param metabolite_aucinfo The metabolite area under the concentration versus time cruve from time 0 to infinity (Observed) (numeric value)
#' @param parent_aucinfo The parent area under the concentration versus time cruve from time 0 to infinity (Observed) (numeric value)
#' @param parent_mw The parent molecular weight (numeric value)
#' @param metabolite_mw The metabolite molecular weight (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item MRAUCINFO: derived metabolite area under the curve
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
mr_auc_inf_o <- function(metabolite_aucinfo = NULL, parent_aucinfo = NULL, parent_mw = NULL, metabolite_mw = NULL){
  if(is.null(metabolite_aucinfo) || is.null(parent_aucinfo) || is.null(parent_mw) || is.null(metabolite_mw)){
    count <- 0
    err <- ""
    if(is.null(metabolite_aucinfo)){
      err <- paste0(err, "'metabolite_aucinfo'")
      count <- count + 1
    }
    if(is.null(parent_aucinfo)){
      err <- paste0(err, ifelse(is.null(metabolite_aucinfo), ", ", ""), "'parent_aucinfo'")
      count <- count + 1
    }
    if(is.null(parent_mw)){
      err <- paste0(err, ifelse((is.null(metabolite_aucinfo) || is.null(parent_aucinfo)), ", ", ""), "'parent_mw'")
      count <- count + 1
    }
    if(is.null(metabolite_mw)){
      err <- paste0(err, ifelse((is.null(metabolite_aucinfo) || is.null(parent_aucinfo) || is.null(parent_mw)), ", ", ""), "'metabolite_mw'")
      count <- count + 1
    }
    if(count > 1){
      stop(paste0("Error in mr_auc_inf_o: ", err, " vectors are NULL"))
    } else {
      stop(paste0("Error in mr_auc_inf_o: ", err, " vector is NULL"))
    }
  }

  if(!(is.numeric(metabolite_aucinfo) && is.vector(metabolite_aucinfo)) ){
    stop("Error in mr_auc_inf_o: 'metabolite_aucinfo' is not a numeric vector")
  }
  if(!(is.numeric(parent_aucinfo) && is.vector(parent_aucinfo)) ){
    stop("Error in mr_auc_inf_o: 'parent_aucinfo' is not a numeric vector")
  }
  if(!(is.numeric(parent_mw) && is.vector(parent_mw)) ){
    stop("Error in mr_auc_inf_o: 'parent_mw' is not a numeric vector")
  }
  if(!(is.numeric(metabolite_mw) && is.vector(metabolite_mw)) ){
    stop("Error in mr_auc_inf_o: 'metabolite_mw' is not a numeric vector")
  }
  if(length(metabolite_aucinfo) != length(parent_aucinfo) || length(parent_aucinfo) != length(parent_mw) || length(parent_mw) != length(metabolite_mw)){
    stop("Error in mr_auc_inf_o: length of 'metabolite_aucinfo', 'parent_aucinfo', 'parent_mw' and 'metabolite_mw' vectors are not equal")
  }
  
  mr_aucinfo <- (metabolite_aucinfo/parent_aucinfo)*parent_mw/metabolite_mw
  return(mr_aucinfo)
}
