#' Corrected CMAXi 
#'
#' CMAXi corrected using estimates of KEL and C0. i refers to the value of NDOSEI. The correct 
#' CMAXCi (and the other corrections below) should be triggered for computation when the FLAG 
#' for predose concentrations >5% of Cmax are observed has been set. This would ordinarily apply 
#' to CMAXC1, i.e. the first dosing interval of a concentration time profile. \cr 
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
#' @param conc The concentration data (given in a vector form) 
#' @param time The time data (given in a vector form)
#' 
#' @section Returns:
#' \strong{Value} \cr 
#' \itemize{
#'  \item AUCINFPC: area under the curve corrected
#' }
#' 
#' @examples 
#' Will add some soon!!
#' 
#' @author
#' \itemize{
#'  \item Kevin McConnell
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
