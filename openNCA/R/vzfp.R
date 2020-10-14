#' Apparent volume of distribution (predicted) based on the terminal phase for extravascular routes of administration.
#'
#' @details
#' \strong{Model M1 (SD)}
#' Single Dose Equation:
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{vzfp.png} \cr
#'  }
#' }
#' \eqn{F = fraction of dose absorbed [value is unknown for extravascular model} \cr
#' \eqn{KEL = Terminal or elimination phase rate constant} \cr
#' \eqn{AUCINFP = Area under the first moment curve from zero time to infinity (Predicted)} \cr
#' \eqn{Dose = dose value for the profile} \cr
#'
#' @param kel The terminal phase rate constant for the concentration-time profile of interest (numeric value)
#' @param aucinfp The area under the concentration versus time curve from time 0 to infinity (Predicted) (numeric value)
#' @param dose The dose data (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item VZFP: the apparent volume of distribution
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
vzfp <- function(kel = NULL, aucinfp = NULL, dose = NULL){
  if(is.null(kel) && is.null(aucinfp) && is.null(dose)){
    stop("Error in vzfo: 'kel', 'aucinfp' and 'dose' vectors are NULL")
  } else if(is.null(kel) && is.null(aucinfp)){
    stop("Error in vzfo: 'kel' and 'aucinfp' vectors are NULL")
  } else if(is.null(kel) && is.null(dose)){
    stop("Error in vzfo: 'kel' and 'dose' vectors are NULL")
  } else if(is.null(aucinfp) && is.null(dose)){
    stop("Error in vzfo: 'aucinfp' and 'dose' vectors are NULL")
  } else if(is.null(kel)) {
    stop("Error in vzfo: 'kel' vector is NULL")
  } else if(is.null(aucinfp)) {
    stop("Error in vzfo: 'aucinfp' vectors is NULL")
  } else if(is.null(dose)) {
    stop("Error in vzfo: 'dose' vectors is NULL")
  }

  if(length(kel) != length(aucinfp) && length(aucinfp) != length(dose)){
    stop("Error in vzfo: length of 'time' and 'conc' vectors are not equal")
  }

  if(is.na(aucinfp) || is.na(kel) || (0 %in% dose)){
    vz_fp <- NA
  } else {
    vz_fp <- dose/(kel * aucinfp)
    vz_fp <- replace(vz_fp, is.infinite(vz_fp), NA)
  }

  return(vz_fp)
}
