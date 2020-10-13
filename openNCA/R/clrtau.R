#' The clearance of a substance from the blood by the kidneys
#'
#' @details
#' \strong{Model: Derived}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{clrtau.png} \cr
#'  }
#' }
#' \eqn{AETAUi = Cumulative amount of drug recovered unchanged in the urine during a specified dosing interval, i} \cr
#' \eqn{AUCTAUi = The area under the concentration versus time curve from zero time until the end of the specified dosing interval, i} \cr
#'
#' @param aetau Cumulative amount of drug recovered unchanged in the urine during a specified dosing interval (numeric value)
#' @param auctau The area under the concentration versus time curve from zero time until the end of the specified dosing interval (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item CLRTAU: the clearance of a substance
#' }
#'
#' @examples
#' #clrtau()
#' #Error in clrtau: 'aetau' and 'auctau' vectors are NULL
#' 
#' clrtau(aetau = 9.335, auctau = 1.234)
#' #7.56483
#' 
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
clrtau <- function(aetau = NULL, auctau = NULL){
  if(is.null(aetau) && is.null(auctau)){
    stop("Error in clrtau: 'aetau' and 'auctau' vectors are NULL")
  } else if(is.null(aetau)) {
    stop("Error in clrtau: 'aetau' vector is NULL")
  } else if(is.null(auctau)) {
    stop("Error in clrtau: 'auctau' vectors is NULL")
  }

  if(length(aetau) != length(auctau) ){
    stop("Error in clrtau: length of vector arguments do not match")
  }

  if(is.na(aetau) || is.na(auctau)) {
    cl_rtau <- NA
  } else {
    cl_rtau <- aetau/auctau
    cl_rtau <- replace(cl_rtau, is.infinite(cl_rtau), NA)
  }

  return(cl_rtau)
}
