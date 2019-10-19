#' The volume of distribution divided by weight
#'
#' The volume of distribution associated with the terminal slope following extravascular
#' administration divided by the fraction of dose absorbed, calculated using AUCTAUi,
#' divided by the body weight of the subject.
#'
#' @details
#' \strong{Model M1 (SS) Derived}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{vzftauw.png} \cr
#'  }
#' }
#' \eqn{VZFTAUi = The volume of distribution associated with the terminal slope following extravascular administration divided by the fraction of dose absorbed, calculated using AUCTAUi for the ith dosing interval, divided by the body weight of the subject} \cr
#' \eqn{NORMBS = BW = Body weight} \cr
#'
#' @param vzftau The volume of distribution data (numeric value)
#' @param normbs The dose data (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item VZFTAUW: the volume of distribution divided by weight
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
vzftauw <- function(vzftau = NULL, normbs = NULL){
  if(is.null(vzfp) && is.null(normbs)){
    stop("Error in vzftauw: 'vzftau' and 'normbs' vectors are NULL")
  } else if(is.null(clfp)) {
    stop("Error in vzftauw: 'vzftau' vector is NULL")
  } else if(is.null(normbs)) {
    stop("Error in vzftauw: 'normbs' vectors is NULL")
  }

  if(length(vzftau) != length(normbs) ){
    stop("Error in vzftauw: length of vector arguments do not match")
  }

  if(is.na(vzftau) || is.na(normbs)) {
    vzf_tauw <- NA
  } else {
    vzf_tauw <- vzftau/normbs
  }

  return(vzf_tauw)
}
