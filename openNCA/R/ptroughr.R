#' The maximum concentration during the ith dosing interval divided by the pre-dose concentration
#'
#' @details
#' \strong{Equation} \cr
#' \figure{ptroughr.png} \cr
#'
#' @section Note:
#' \strong{cmax}: Refer to \code{\link{cmax}} for more details
#' \strong{ctrough}: Refer to \code{\link{ctrough}} for more details
#'
#' @param cmax The cmax data (numeric value)
#' @param ctrough The pre-dose concentration data (numeric value)
#'
#' @section Returns:
#' \strong{Value or Vector} \cr
#' \itemize{
#'  \item PTROUGH: maximum concentration during the ith dosing interval divided by the pre-dose concentration
#' }
#'
#' @examples
#' ##########
#' ## Data ##
#' ############################################
#' ##  SDEID  ##  TIME  ##   CONC   ##  TAU  ##
#' ############################################
#' ##   30    ##    0   ##   2.89   ##   2   ##
#' ##   30    ##    1   ##   2.49   ##   2   ##
#' ##   30    ##    2   ##   2.47   ##   2   ##
#' ############################################
#'
#' #Data mentioned will be used for the following example
#'
#' ctrough()
#' #Error in ctrough: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- c(2.89, 2.49, 2.47)
#' time_vector <- c(0, 1, 2)
#' tau_value <- 2
#'
#' cmax(conc = conc_vector, time = time_vector)
#' #2.89
#'
#' ctrough(conc = conc_vector, time = time_vector, tau = tau_value)
#' #2.89
#' 
#' ptrough(cmax = 2.89, ctrough = 2.89)
#' #1
#' 
#' ############
#' ## Data 2 ##
#' ############################################
#' ##  SDEID  ##  TIME  ##   CONC   ##  TAU  ##
#' ############################################
#' ##   31    ##    1   ##   2.23   ##   4   ##
#' ##   31    ##    2   ##   2.34   ##   4   ##
#' ##   31    ##    3   ##   2.68   ##   4   ##
#' ############################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(2.23, 2.34, 2.68)
#' time_vector <- c(1, 2, 3)
#' tau_value <- 4
#' 
#' cmax(conc = conc_vector, time = time_vector)
#' #2.68
#'
#' ctrough(conc = conc_vector, time = time_vecto, tau = tau_valuer)
#' #NA
#' 
#' ptrough(cmax = 2.68, ctrough = NA)
#' #NA
#'
#' ############
#' ## Data 3 ##
#' ############################################
#' ##  SDEID  ##  TIME  ##   CONC   ##  TAU  ##
#' ############################################
#' ##   32    ##    0   ##   NA     ##   3   ##
#' ##   32    ##    1   ##   1.23   ##   3   ##
#' ##   32    ##    2   ##   1.34   ##   3   ##
#' ##   32    ##    3   ##   1.19   ##   3   ##
#' ############################################
#'
#' #Data mentioned will be used for the following example
#
#' conc_vector <- c(NA, 1.23, 1.34, 1.19)
#' time_vector <- c(0, 1, 2, 3)
#' tau_value <- 3
#' 
#' cmax(conc = conc_vector, time = time_vector)
#' #1.34
#'
#' ctrough(conc = conc_vector, time = time_vector, tau = tau_value)
#' #NA
#' 
#' ptrough(cmax = 1.34, ctrough = NA)
#' #NA
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
ptroughr <- function(cmax = NULL, ctrough = NULL){
  if(is.null(cmax) && is.null(ctrough)){
    stop("Error in ptroughr: 'cmax' and 'ctrough' vectors are NULL")
  } else if(is.null(cmax)) {
    stop("Error in ptroughr: 'cmax' vector is NULL")
  } else if(is.null(ctrough)) {
    stop("Error in ptroughr: 'ctrough' vectors is NULL")
  }
  
  if(length(ctrough) != length(cmax)){
    stop("Error in ptroughr: length of vector arguments do not match")
  }
  
  p_troughr <- cmax/ctrough
  p_troughr <- replace(p_troughr, is.infinite(p_troughr) || is.nan(p_troughr), NA)
  p_troughr <- replace(p_troughr, p_troughr == 0, NA)
  
  return(p_troughr)
}