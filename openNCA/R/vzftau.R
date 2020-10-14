#' The volume of distribution
#'
#' The volume of distribution associated with the terminal slope following extravascular
#' administration divided by the fraction of dose absorbed, calculated using AUCTAUi.
#'
#' @details
#' \strong{Model M1 (SS)}
#' Single Dose Equation:
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{vzftau.png} \cr
#'  }
#' }
#' \eqn{AUCTAUi = Area under the concentration versus time curve from zero time until the end of the ith dosing interval} \cr
#' \eqn{KEL = Terminal or elimination phase rate constant.} \cr
#' \eqn{Dose = dose value for drug dosing interval i} \cr
#'
#' @param kel The terminal phase rate constant for the concentration-time profile of interest (numeric value)
#' @param auctau The area under the concentration versus time curve from time 0 until the end of the ith dosing interval (numeric value)
#' @param dose The dose data (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item VZFTAU: the volume of distribution
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
vzftau <- function(kel = NULL, auctau = NULL, dose = NULL){
  if(is.null(kel) && is.null(auctau) && is.null(dose)){
    stop("Error in vzftau: 'kel', 'auctau' and 'dose' vectors are NULL")
  } else if(is.null(kel) && is.null(auctau)){
    stop("Error in vzftau: 'kel' and 'auctau' vectors are NULL")
  } else if(is.null(kel) && is.null(dose)){
    stop("Error in vzftau: 'kel' and 'dose' vectors are NULL")
  } else if(is.null(auctau) && is.null(dose)){
    stop("Error in vzftau: 'auctau' and 'dose' vectors are NULL")
  } else if(is.null(kel)) {
    stop("Error in vzftau: 'kel' vector is NULL")
  } else if(is.null(auctau)) {
    stop("Error in vzftau: 'auctau' vectors is NULL")
  } else if(is.null(dose)) {
    stop("Error in vzftau: 'dose' vectors is NULL")
  }

  if(length(kel) != length(auctau) && length(auctau) != length(dose)){
    stop("Error in vzftau: length of 'kel', 'auctau' and 'dose' vectors are not equal")
  }

  if(is.na(auctau) || is.na(kel) || (0 %in% dose) || is.na(dose)){
    vz_ftau <- NA
  } else {
    vz_ftau <- dose/(kel * auctau)
    vz_ftau <- replace(vz_ftau, is.infinite(vz_ftau), NA)
  }

  return(vz_ftau)
}
