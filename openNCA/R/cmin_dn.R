#' Dose normalized CMINi
#'
#' @details
#' \strong{Equation} \cr
#' \figure{cmin_dn.png} \cr 
#'
#' @section Note:
#' \strong{cmin}: Refer to \code{\link{cmin}} for more details
#'
#' @param cmin The cmin data (numeric value)
#' @param dose The dose data (numeric value)
#'
#' @section Returns:
#' \strong{Value or Vector} \cr
#' \itemize{
#'  \item CMINDN: dose normalized CMINi
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
#' #cmin()
#' #Error in cmin: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- c(2.89, 2.49, 2.47)
#' time_vector <- c(0, 1, 2)
#'
#' cmin(conc = conc_vector, time = time_vector)
#' #2.47
#'
#' cmin_dn(cmin = 2.47, dose = 200)
#' #0.01235
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
#' cmin(conc = conc_vector, time = time_vector)
#' #0
#'
#' cmin_dn(cmin = 0, dose = 200)
#' #0
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
cmin_dn <- function(cmin = NULL, dose = NULL){
  if(is.null(cmin) && is.null(dose)){
    stop("Error in cmin_dn: 'cmin' and 'dose' vectors are NULL")
  } else if(is.null(cmin)) {
    stop("Error in cmin_dn: 'cmin' vector is NULL")
  } else if(is.null(dose)) {
    stop("Error in cmin_dn: 'dose' vectors is NULL")
  }

  if(length(dose) != length(cmin) ){
    stop("Error in auc_dn: length of vector arguments do not match")
  }

  if(is.na(dose) || (0 %in% dose) || is.na(cmin)) {
    cmindn <- NA
  } else {
    cmindn <- cmin/dose
    cmindn <- replace(cmindn, is.infinite(cmindn), NA)
  }

  return(cmindn)
}
