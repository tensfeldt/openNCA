#' Apparent volume of distribution (observed) based on the terminal phase for extravascular routes of administration.
#'
#' @details
#' \strong{Model M1 (SD)}
#' Single Dose Equation only; not calculated at steady-state:
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{vzfo.png} \cr
#'  }
#' }
#' \eqn{F = fraction of dose absorbed [value is unknown for extravascular model} \cr
#' \eqn{KEL = Terminal or elimination phase rate constant} \cr
#' \eqn{AUCINFO = Area under the first moment curve from zero time to infinity (Observed)} \cr
#' \eqn{Dose = dose value for the profile} \cr
#'
#' @param kel The terminal phase rate constant for the concentration-time profile of interest (numeric value)
#' @param aucinfo The area under the concentration versus time curve from time 0 to infinity (Observed) (numeric value)
#' @param dose The dose data (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item VZFO: the apparent volume of distribution
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
vzfo <- function(kel = NULL, aucinfo = NULL, dose = NULL){
  if(is.null(kel) && is.null(aucinfo) && is.null(dose)){
    stop("Error in vzfo: 'kel', 'aucinfo' and 'dose' vectors are NULL")
  } else if(is.na(kel) || is.na(aucinfo)) { # 2019-09-13/TGT/
      return(NA)
  } else if(is.null(kel) && is.null(aucinfo)){
    stop("Error in vzfo: 'kel' and 'aucinfo' vectors are NULL")
  } else if(is.null(kel) && is.null(dose)){
    stop("Error in vzfo: 'kel' and 'dose' vectors are NULL")
  } else if(is.null(aucinfo) && is.null(dose)){
    stop("Error in vzfo: 'aucinfo' and 'dose' vectors are NULL")
  } else if(is.null(kel)) {
    stop("Error in vzfo: 'kel' vector is NULL")
  } else if(is.null(aucinfo)) {
    stop("Error in vzfo: 'aucinfo' vectors is NULL")
  } else if(is.null(dose)) {
    stop("Error in vzfo: 'dose' vectors is NULL")
  }
  
  if(length(kel) != length(aucinfo) && length(aucinfo) != length(dose)){
    stop("Error in vzfo: length of 'time' and 'conc' vectors are not equal")
  }

  if(is.na(aucinfo) || is.na(kel) || (0 %in% dose)){
    vz_fo <- NA
  } else {
    vz_fo <- dose/(kel * aucinfo)
    vz_fo <- replace(vz_fo, is.infinite(vz_fo), NA)
  }

  return(vz_fo)
}
