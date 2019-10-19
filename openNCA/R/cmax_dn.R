#' Dose normalized CMAXi
#'
#' @details
#' \strong{Equation} \cr
#' \figure{cmax_dn.png} \cr
#'
#' @section Note:
#' \strong{cmax}: Refer to \code{\link{cmax}} for more details
#'
#' @param cmax The cmax data (numeric value)
#' @param dose The dose data (numeric value)
#'
#' @section Returns:
#' \strong{Value or Vector} \cr
#' \itemize{
#'  \item CMAXDN: dose normalized CMAXi
#' }
#'
#' @examples
#' ##########
#' ## Data ##
#' ###################################
#' ##  SDEID  ##  TIME  ##   CONC   ##
#' ###################################
#' ##   30    ##    0   ##   2.89   ##
#' ##   30    ##    1   ##   2.49   ##
#' ##   30    ##    2   ##   2.47   ##
#' ###################################
#'
#' #Data mentioned will be used for the following example
#'
#' cmax()
#' #Error in cmax: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- c(2.89, 2.49, 2.47)
#' time_vector <- c(0, 1, 2)
#'
#' cmax(conc = conc_vector, time = time_vector)
#' #2.89
#'
#' cmax_dn(cmax = 2.89, dose = 200)
#' #0.01445
#'
#' ############
#' ## Data 2 ##
#' ###################################
#' ##  SDEID  ##  TIME  ##   CONC   ##
#' ###################################
#' ##    31   ##    0   ##     0    ##
#' ##    31   ##    1   ##     0    ##
#' ##    31   ##    2   ##     0    ##
#' ###################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(0, 0, 0)
#' time_vector <- c(0, 1, 2)
#'
#' cmax(conc = conc_vector, time = time_vector)
#' #0
#'
#' cmax_dn(cmax = 0, dose = 200)
#' #0
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
cmax_dn <- function(cmax = NULL, dose = NULL){
  if(is.null(cmax) && is.null(dose)){
    stop("Error in cmax_dn: 'cmax' and 'dose' vectors are NULL")
  } else if(is.null(cmax)) {
    stop("Error in cmax_dn: 'cmax' vector is NULL")
  } else if(is.null(dose)) {
    stop("Error in cmax_dn: 'dose' vectors is NULL")
  }

  if(length(dose) != length(cmax) ){
    stop("Error in auc_dn: length of vector arguments do not match")
  }

  if(is.na(dose) || (0 %in% dose) || is.na(cmax)) {
    cmaxdn <- NA
  } else {
    cmaxdn <- cmax/dose
    cmaxdn <- replace(cmaxdn, is.infinite(cmaxdn), NA)
  }

  return(cmaxdn)
}
