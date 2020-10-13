#' The maximum concentration during a dosing interval divided by the concentration at the end of the dosing interval
#'
#' @details
#' \strong{Equation} \cr
#' \figure{ptroughrend.png} \cr
#'
#' @section Note:
#' \strong{cmax}: Refer to \code{\link{cmax}} for more details
#' \strong{ctroughend}: Refer to \code{\link{ctroughend}} for more details
#'
#' @param cmax The cmax data (numeric value)
#' @param ctroughend The concentration at the end of the ith dose interval data (numeric value)
#'
#' @section Returns:
#' \strong{Value or Vector} \cr
#' \itemize{
#'  \item PTROUGHEND: maximum concentration during a dosing interval divided by the concentration at the end of the dosing interval
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
#' #ctroughend()
#' #Error in ctrough: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- c(2.89, 2.49, 2.47)
#' time_vector <- c(0, 1, 2)
#' tau_value <- 2
#' 
#' cmax(conc = conc_vector, time = time_vector)
#' #2.89
#'
#' ctroughend(conc = conc_vector, time = time_vector, tau = tau_value)
#' #2.47
#' 
#' ptroughend(cmax = 2.89, ctrough = 2.47)
#' #1.17004
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
#' ctroughend(conc = conc_vector, time = time_vector, tau = tau_value)
#' #NA
#' 
#' ptroughend(cmax = 2.68, ctrough = NA)
#' #NA
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
ptroughrend <- function(cmax = NULL, ctroughend = NULL){
  if(is.null(cmax) && is.null(ctroughend)){
    stop("Error in ptroughrend: 'cmax' and 'ctroughend' vectors are NULL")
  } else if(is.null(cmax)) {
    stop("Error in ptroughrend: 'cmax' vector is NULL")
  } else if(is.null(ctrough)) {
    stop("Error in ptroughrend: 'ctroughend' vectors is NULL")
  }
  
  if(length(ctroughend) != length(cmax)){
    stop("Error in ptroughrend: length of vector arguments do not match")
  }
  
  p_troughrend <- cmax/ctroughend
  p_troughrend <- replace(p_troughrend, is.infinite(p_troughrend) || is.nan(p_troughrend), NA)
  p_troughrend <- replace(p_troughrend, p_troughrend == 0, NA)
  
  return(p_troughrend)
}