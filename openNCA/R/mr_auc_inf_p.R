#' Derived Metabolite of the area under the concentration versus time curve from time 0 to infinity (Predicted)
#'
#' This function gets derived metabolite of the area under the concentration versus time curve from time 0 to
#' infinity (Predicted).\cr
#' 
#' @details
#' \strong{Equation} \cr
#' \figure{mr_auc_inf_p.png} \cr
#'
#' @section Note:
#' \strong{mraucinfp_metabolite}: Derived Metabolite of AUCINFP (metabolite)
#' \strong{mraucinfp_parent}: Derived Metabolite of AUCINFP (parent)
#' \strong{mw_parent}: Molecular Weight (parent)
#' \strong{mw_metabolite}: Molecular Weight (metabolite)
#'
#' @param metabolite_aucinfp The metabolite area under the concentration versus time curve from time 0 to infinity (Predicted) (numeric value)
#' @param parent_aucinfp The parent area under the concentration versus time curve from time 0 to infinity (Predicted) (numeric value)
#' @param parent_mw The parent molecular weight (numeric value)
#' @param metabolite_mw The metabolite molecular weight (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item MRAUCINFP: derived metabolite area under the curve
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
mr_auc_inf_p <- function(metabolite_aucinfp = NULL, parent_aucinfp = NULL, parent_mw = NULL, metabolite_mw = NULL){
  if(is.null(metabolite_aucinfp) || is.null(parent_aucinfp) || is.null(parent_mw) || is.null(metabolite_mw)){
    count <- 0
    err <- ""
    if(is.null(metabolite_aucinfp)){
      err <- paste0(err, "'metabolite_aucinfp'")
      count <- count + 1
    }
    if(is.null(parent_aucinfp)){
      err <- paste0(err, ifelse(is.null(metabolite_aucinfp), ", ", ""), "'parent_aucinfp'")
      count <- count + 1
    }
    if(is.null(parent_mw)){
      err <- paste0(err, ifelse((is.null(metabolite_aucinfp) || is.null(parent_aucinfp)), ", ", ""), "'parent_mw'")
      count <- count + 1
    }
    if(is.null(metabolite_mw)){
      err <- paste0(err, ifelse((is.null(metabolite_aucinfp) || is.null(parent_aucinfp) || is.null(parent_mw)), ", ", ""), "'metabolite_mw'")
      count <- count + 1
    }
    if(count > 1){
      stop(paste0("Error in mr_auc_inf_p: ", err, " vectors are NULL"))
    } else {
      stop(paste0("Error in mr_auc_inf_p: ", err, " vector is NULL"))
    }
  }

  if(!(is.numeric(metabolite_aucinfp) && is.vector(metabolite_aucinfp)) ){
    stop("Error in mr_auc_inf_p: 'metabolite_aucinfp' is not a numeric vector")
  }
  if(!(is.numeric(parent_aucinfp) && is.vector(parent_aucinfp)) ){
    stop("Error in mr_auc_inf_p: 'parent_aucinfp' is not a numeric vector")
  }
  if(!(is.numeric(parent_mw) && is.vector(parent_mw)) ){
    stop("Error in mr_auc_inf_p: 'parent_mw' is not a numeric vector")
  }
  if(!(is.numeric(metabolite_mw) && is.vector(metabolite_mw)) ){
    stop("Error in mr_auc_inf_p: 'metabolite_mw' is not a numeric vector")
  }
  if(length(metabolite_aucinfp) != length(parent_aucinfp) || length(parent_aucinfp) != length(parent_mw) || length(parent_mw) != length(metabolite_mw)){
    stop("Error in mr_auc_inf_p: length of 'metabolite_aucinfp', 'parent_aucinfp', 'parent_mw' and 'metabolite_mw' vectors are not equal")
  }
  
  mr_aucinfp <- (metabolite_aucinfp/parent_aucinfp)*parent_mw/metabolite_mw
  return(mr_aucinfp)
}
