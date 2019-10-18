#' Steady-state (observed) volume of distribution
#'
#' Steady-state (observed) volume of distribution for non-steady state or steady state data.
#'
#' @details
#' \strong{Model M2 (SD, SS) and M3(SS, SD)}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{vsso.png} \cr
#'  }
#' }
#' \eqn{MRTO = Mean residence time (observed) extrapolated to infinity} \cr
#' \eqn{CLO = Total clearance of drug (observed) } \cr
#'
#' @param clo Total clearance of drug data (given in a vector form)
#' @param mrto Mean residence time data (given in a vector form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item VSSO: steady-state (observed) volume of distribution
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
vsso <- function(clo = NULL, mrto = NULL){
  if(is.null(clo) && is.null(mrto)){
    stop("Error in vsso: 'clo' and 'mrto' vectors are NULL")
  } else if(is.null(clo)) {
    stop("Error in vsso: 'clo' vector is NULL")
  } else if(is.null(mrto)) {
    stop("Error in vsso: 'mrto' vectors is NULL")
  }

  if(length(clo) != length(mrto) ){
    stop("Error in vsso: length of vector arguments do not match")
  }

  if(is.na(clo) || is.na(mrto)) {
    vss_o <- NA
  } else {
    vss_o <- clo * mrto
  }

  return(vss_o)
}
