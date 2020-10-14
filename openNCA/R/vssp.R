#' Steady-state (predicated) volume of distribution
#'
#' Steady-state (predicted) volume of distribution for non-steady state or steady state data.
#'
#' @details
#' \strong{Model M2 (SD, SS) and M3(SS, SD)}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{vssp.png} \cr
#'  }
#' }
#' \eqn{MRTP = Mean residence time (predicted) extrapolated to infinity} \cr
#' \eqn{CLTAUi = Total clearance of drug (predicted) for ith dosing interval} \cr
#'
#' @param cltau Total clearance of drug data (given in a vector form)
#' @param mrtp Mean residence time data (given in a vector form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item VSSP: steady-state (predicated) volume of distribution
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
vssp <- function(cltau = NULL, mrtp = NULL){
  if(is.null(cltau) && is.null(mrtp)){
    stop("Error in vssp: 'cltau' and 'mrtp' vectors are NULL")
  } else if(is.null(cltau)) {
    stop("Error in vssp: 'cltau' vector is NULL")
  } else if(is.null(mrtp)) {
    stop("Error in vssp: 'mrtp' vectors is NULL")
  }

  if(length(cltau) != length(mrtp) ){
    stop("Error in vssp: length of vector arguments do not match")
  }

  if(is.na(cltau) || is.na(mrtp)) {
    vss_p <- NA
  } else {
    vss_p <- cltau * mrtp
  }

  return(vss_p)
}
