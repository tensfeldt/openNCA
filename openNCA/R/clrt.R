#' The clearance of a substance from the blood by the kidneys
#'
#' @details
#' \strong{Model: Derived}
#' Calculated for single dose studies.
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{clrt.png} \cr
#'  }
#' }
#' \eqn{AET = Total amount of drug recovered unchanged in the urine, from time zero to time T. AEinf is achieved following 5* terminal phase estimation for collection period.} \cr
#' \eqn{AUCT = Area under the concentration versus time curve from zero time to time T} \cr
#'
#' @param aet Cumulative (running sum) amount of drug (numeric value) recovered unchanged in the urine up to time t post-dose
#' @param auct Area under the concentration versus time curve from time 0 to time T (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item CLRT: the clearance of a substance
#' }
#'
#' @examples
#' #clrt()
#' #Error in clrt: 'aet' and 'auct' vectors are NULL
#' 
#' clrt(aet = 9.335, auct = 1.234)
#' #7.56483
#' 
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
clrt <- function(aet = NULL, auct = NULL){
  if(is.null(aet) && is.null(auct)){
    stop("Error in clrt: 'aet' and 'auct' vectors are NULL")
  } else if(is.null(aet)) {
    stop("Error in clrt: 'aet' vector is NULL")
  } else if(is.null(auct)) {
    stop("Error in clrt: 'auct' vectors is NULL")
  }

  if(length(aet) != length(auct) ){
    stop("Error in clrt: length of vector arguments do not match")
  }

  if(is.na(aet) || is.na(auct)) {
    cl_rt <- NA
  } else {
    cl_rt <- aet/auct
    cl_rt <- replace(cl_rt, is.infinite(cl_rt), NA)
  }

  return(cl_rt)
}
