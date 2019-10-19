#' Volume of distribution (predicted) based on the terminal phase
#'
#' @details
#' \strong{Model M2 (SD) and M3 (SD)}
#' Single Dose Equation only; not calculated at steady-state (Note that VZ/FP is calculated using AUCTAU):
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{vzp.png} \cr
#'  }
#' }
#' \eqn{KEL = Terminal or elimination phase rate constant.} \cr
#' \eqn{AUCINFO = Area under the first moment curve from zero time to infinity (Observed)} \cr
#' \eqn{Dosei = dose value for drug dosing interval i} \cr
#'
#' @section Additional Details:
#'
#' @param kel The terminal phase rate constant for the concentration-time profile of interest (numeric value)
#' @param aucinfp The area under the concentration versus time cruve from time 0 to infinity (Predicted) (numeric value)
#' @param dose The dose data (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item VZP: The volume of distribution
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
vzp <- function(kel = NULL, aucinfp = NULL, dose = NULL){
  if(is.null(kel) && is.null(aucinfp) && is.null(dose)){
    stop("Error in vzp: 'kel', 'aucinfp' and 'dose' vectors are NULL")
  } else if(is.null(kel) && is.null(aucinfp)){
    stop("Error in vzp: 'kel' and 'aucinfp' vectors are NULL")
  } else if(is.null(kel) && is.null(dose)){
    stop("Error in vzp: 'kel' and 'dose' vectors are NULL")
  } else if(is.null(aucinfp) && is.null(dose)){
    stop("Error in vzp: 'aucinfp' and 'dose' vectors are NULL")
  } else if(is.null(kel)) {
    stop("Error in vzp: 'kel' vector is NULL")
  } else if(is.null(aucinfp)) {
    stop("Error in vzp: 'aucinfp' vectors is NULL")
  } else if(is.null(dose)) {
    stop("Error in vzp: 'dose' vectors is NULL")
  }

  if(length(kel) != length(aucinfp) && length(aucinfp) != length(dose)){
    stop("Error in vzp: length of 'time' and 'conc' vectors are not equal")
  }

  if(is.na(aucinfp) || is.na(kel) || (0 %in% dose)){
    vz_p <- NA
  } else {
    vz_p <- dose/(kel * aucinfp)
    vz_p <- replace(vz_p, is.infinite(vz_p), NA)
  }

  return(vz_p)
}
