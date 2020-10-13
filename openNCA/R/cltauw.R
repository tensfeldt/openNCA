#' The total body clearance
#'
#' The total body clearance for intravascular administration, calculated using AUCTAU, divided by the weight.  \cr
#'
#' @details
#' \strong{Model M2 (SS) and M3 (SS) Derived}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{cltauw.png} \cr
#'  }
#' }
#' \eqn{CLTAUi = Total clearance of drug calculated using AUCTAUi from the ith dosing interval} \cr
#' \eqn{NORMBS = BW = Body weight} \cr
#'
#' @section Additional Details:
#'
#' @param cltau The total body clearance (numeric value)
#' @param normbs The dose data (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item CLTAUW: The total body clearance
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
cltauw <- function(cltau = NULL, normbs = NULL){
  if(is.null(cltau) && is.null(normbs)){
    stop("Error in cltauw: 'cltau' and 'normbs' vectors are NULL")
  } else if(is.null(cltau)) {
    stop("Error in cltauw: 'cltau' vector is NULL")
  } else if(is.null(normbs)) {
    stop("Error in cltauw: 'normbs' vectors is NULL")
  }

  if(length(cltau) != length(normbs) ){
    stop("Error in cltauw: length of vector arguments do not match")
  }

  if(is.na(cltau) || is.na(normbs)) {
    cl_tauw <- NA
  } else {
    cl_tauw <- cltau/normbs
  }

  return(cl_tauw)
}
