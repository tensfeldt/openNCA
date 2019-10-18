#' Steady-state (observed/predicted) volume of distribution
#'
#' Steady-state (observed/predicted) volume of distribution for non-steady state or steady state data.
#'
#' @details
#' \strong{Model M2 (SD, SS) and M3(SS, SD)}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{vss.png} \cr
#'  }
#' }
#' \eqn{MRT = Mean residence time (observed/predicted) extrapolated to infinity} \cr
#' \eqn{CL = Total clearance of drug (observed/predicted) } \cr
#'
#' @param cl Total clearance of drug data (given in a vector form)
#' @param mrt Mean residence time data (given in a vector form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item VSS: steady-state (observed/predicted) volume of distribution
#' }
#'
#' @examples
#'
#' @author
#' \itemize{
#'  \item \strong{Thomas Tensfeldt, Pfizer}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{Thomas.G.Tensfeldt@pfizer.com}
#' }
#' @export
vss <- function(cl = NULL, mrt = NULL){
  if(is.null(cl) && is.null(mrt)){
    stop("Error in vss: 'cl' and 'mrt' vectors are NULL")
  } else if(is.null(cl)) {
    stop("Error in vss: 'cl' vector is NULL")
  } else if(is.null(mrt)) {
    stop("Error in vss: 'mrt' vectors is NULL")
  }

  if(length(cl) != length(mrt) ){
    stop("Error in vss: length of vector arguments do not match")
  }

  if(is.na(cl) || is.na(mrt)) {
    vss <- NA
  } else {
    vss <- cl * mrt
  }

  return(vss)
}
