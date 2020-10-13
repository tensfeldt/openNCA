#' The clearance of a substance from the blood by the kidneys
#'
#' @details
#' \strong{Model: Derived}
#' Calculated for single dose studies.
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{clr.png} \cr
#'  }
#' }
#' \eqn{AE = Total amount of drug recovered unchanged in the urine, from time zero to infinity. AEinf is achieved following 5* terminal phase estimation for collection period} \cr
#' \eqn{AUCINFP = Area under the concentration versus time curve from zero time to infinity (Predicted)} \cr
#'
#' @param aucinfp The area under the concentration versus time curve from time 0 to infinity (Predicted) (numeric value)
#' @param a_e Cumulative total amount of drug (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item CLR: the clearance of a substance
#' }
#'
#' @examples
#' #clr()
#' #Error in clr: 'aucinfp' and 'a_e' vectors are NULL
#' 
#' clr(aucinfp = 9.335, a_e = 1.234)
#' #0.1321907
#' 
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
clr <- function(aucinfp = NULL, a_e = NULL){
  if(is.null(aucinfp) && is.null(a_e)){
    stop("Error in clr: 'aucinfp' and 'a_e' vectors are NULL")
  } else if(is.null(aucinfp)) {
    stop("Error in clr: 'aucinfp' vector is NULL")
  } else if(is.null(a_e)) {
    stop("Error in clr: 'a_e' vectors is NULL")
  }

  if(length(a_e) != length(aucinfp) ){
    stop("Error in clr: length of vector arguments do not match")
  }

  if(isTRUE(is.na(a_e) || is.na(aucinfp))) {
    cl_r <- NA
  } else {
    cl_r <- a_e/aucinfp
    cl_r <- replace(cl_r, is.infinite(cl_r), NA)
  }

  return(cl_r)
}
