#' Corrected CMAX/CMAXi for Model M2 (IV Bolus)
#'
#' CMAXi corrected using estimates of KEL and C0, the residual plasma concentration observed at
#' time zero, i.e. the predose concentration. i refers to the value of NDOSEI. The corrected
#' CMAXCi (and the other corrections below) should be triggered for computation when the FLAG
#' for predose concentrations >5% of Cmax are observed has been set. This would ordinarily apply
#' to CMAXC1, i.e. the first dosing interval of a concentration time profile.\cr
#'
#' @details
#' \strong{Equation}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{cmaxc.png} \cr
#'  }
#'  \eqn{C0 = the residual plasma concentration observed at time zero (i.e. the predose concentraton)} \cr
#'  \eqn{KEL = the terminal phase rate constant for the concentration-time profile of interest} \cr
#' }
#'
#' @section Note:
#' \strong{cmax}: Refer to \code{\link{cmax}} for more details
#' \strong{tmax}: Refer to \code{\link{tmax}} for more details
#' \strong{kel}: Refer to \code{\link{kel}} for more details
#'
#' @param kel The terminal phase rate constant for the concentration-time profile of interest (numeric value)
#' @param cmax The cmax data (numeric value)
#' @param c0 The c0 data (numeric value)
#' @param t_max The first time at which CMAXi is observed within the dosing interval (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item CMAXC: corrected CMAX
#' }
#'
#' @examples
#'
#' @author
#' \itemize{
#'  \item \strong{Thomas Tensfeldt, Pfizer Inc & Rudraya Technical Team}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{thomas.g.tensfeldt@pfizer.com}
#' }
#' @export
cmaxc <- function(kel = NULL, cmax = NULL, c0 = NULL, tmax = NULL){
  if(is.null(kel) || is.null(cmax) || is.null(c0) || is.null(tlast)){
    count <- 0
    err <- ""
    if(is.null(kel)){
      err <- paste0(err, "'kel'")
      count <- count + 1
    }
    if(is.null(cmax)){
      err <- paste0(err, ifelse(is.null(kel), ", ", ""), "'cmax'")
      count <- count + 1
    }
    if(is.null(c0)){
      err <- paste0(err, ifelse((is.null(kel) || is.null(cmax)), ", ", ""), "'c0'")
      count <- count + 1
    }
    if(is.null(tmax)){
      err <- paste0(err, ifelse((is.null(kel) || is.null(cmax) || is.null(c0)), ", ", ""), "'tmax'")
      count <- count + 1
    }
    if(count > 1){
      stop(paste0("Error in cmaxc: ", err, " vectors are NULL"))
    } else {
      stop(paste0("Error in cmaxc: ", err, " vector is NULL"))
    }
  }

  if(is.na(kel) || is.na(c0) || is.na(cmax) || is.na(tmax)) {
    cmax_c <- NA
  } else {
    c_pt2 <- c0 * exp(-1*tmax*kel)
    cmax_c <- cmax - (c_pt2)
  }
  return(cmax_c)
}
