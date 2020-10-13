#' Volume of distribution (observed) based on the terminal phase.
#'
#' @details
#' \strong{Model M2 and M3}
#' Single Dose Equation only not calculated at steady-state (only VZ/P is calculated using AUCTAU as the correct PK variable)
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{vzo.png} \cr
#'  }
#' }
#' \eqn{F = fraction of dose absorbed [is assumed to be 1]} \cr
#' \eqn{KEL = Terminal or elimination phase rate constant.} \cr
#' \eqn{AUCINFO = Area under the first moment curve from zero time to infinity (Observed)} \cr
#' \eqn{Dosei = dose value for drug dosing interval i} \cr
#'
#' @param kel The terminal phase rate constant for the concentration-time profile of interest (numeric value)
#' @param aucinfo The area under the concentration versus time curve from time 0 to infinity (Observed) (numeric value)
#' @param dose The dose data (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item VZO: The volume of distribution
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
vzo <- function(kel = NULL, aucinfo = NULL, dose = NULL){
  if(is.null(kel) && is.null(aucinfo) && is.null(dose)){
    stop("Error in vzo: 'kel', 'aucinfo' and 'dose' vectors are NULL")
  } else if(is.null(kel) && is.null(aucinfo)){
    stop("Error in vzo: 'kel' and 'aucinfo' vectors are NULL")
  } else if(is.null(kel) && is.null(dose)){
    stop("Error in vzo: 'kel' and 'dose' vectors are NULL")
  } else if(is.null(aucinfo) && is.null(dose)){
    stop("Error in vzo: 'aucinfo' and 'dose' vectors are NULL")
  } else if(is.null(kel)) {
    stop("Error in vzo: 'kel' vector is NULL")
  } else if(is.null(aucinfo)) {
    stop("Error in vzo: 'aucinfo' vectors is NULL")
  } else if(is.null(dose)) {
    stop("Error in vzo: 'dose' vectors is NULL")
  }

  if(length(kel) != length(aucinfo) && length(aucinfo) != length(dose)){
    stop("Error in vzo: length of 'time' and 'conc' vectors are not equal")
  }

  if(is.na(aucinfo) || is.na(kel) || (0 %in% dose)){
    vz_o <- NA
  } else {
    vz_o <- dose/(kel * aucinfo)
    vz_o <- replace(vz_o, is.infinite(vz_o), NA)
  }

  return(vz_o)
}
