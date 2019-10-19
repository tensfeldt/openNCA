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
#' @param aucinfp The area under the concentration versus time cruve from time 0 to infinity (Predicted) (numeric value)
#' @param ae Cumulative total amount of drug (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item CLR: the clearance of a substance
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
clr <- function(aucinfp = NULL, ae = NULL){
  if(is.null(aucinfp) && is.null(ae)){
    stop("Error in clr: 'aucinfp' and 'ae' vectors are NULL")
  } else if(is.null(aucinfp)) {
    stop("Error in clr: 'aucinfp' vector is NULL")
  } else if(is.null(ae)) {
    stop("Error in clr: 'ae' vectors is NULL")
  }

  if(length(ae) != length(aucinfp) ){
    stop("Error in clr: length of vector arguments do not match")
  }

  if(is.na(ae) || is.na(aucinfp)) {
    cl_r <- NA
  } else {
    cl_r <- ae/aucinfp
    cl_r <- replace(cl_r, is.infinite(cl_r), NA)
  }

  return(cl_r)
}
