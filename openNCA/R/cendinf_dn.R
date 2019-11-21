#' Dose normalized CENDINF
#'
#' @details
#' \strong{Equation} \cr
#' \figure{cendinf_dn.png} \cr
#'
#' @section Note:
#' \strong{cendinf}: Refer to \code{\link{cendinf}} for more details
#'
#' @param cendinf The cendinf data (numeric value)
#' @param dose The dose data (numeric value)
#'
#' @section Returns:
#' \strong{Value or Vector} \cr
#' \itemize{
#'  \item CENDINFDN: dose normalized observed concentration at the end of infusion
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
#' cendinf()
#' #Error in cendinf: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- c(2.89, 2.49, 2.47)
#' time_vector <- c(0, 1, 2)
#'
#' cendinf(conc = conc_vector, time = time_vector, dof = 1)
#' #2.49
#'
#' cendinf_dn(cendinf = 2.49, dose = 200)
#' #0.01245
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
#' cendinf(conc = conc_vector, time = time_vector)
#' #NA
#'
#' cendinf_dn(cendinf = NA, dose = 200)
#' #NA
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
cendinf_dn <- function(cendinf = NULL, dose = NULL){
  if(is.null(cendinf) && is.null(dose)){
    stop("Error in cendinf_dn: 'cendinf' and 'dose' vectors are NULL")
  } else if(is.null(cendinf)) {
    stop("Error in cendinf_dn: 'cendinf' vector is NULL")
  } else if(is.null(dose)) {
    stop("Error in cendinf_dn: 'dose' vectors is NULL")
  }

  if(length(dose) != length(cendinf) ){
    stop("Error in cendinf_dn: length of vector arguments do not match")
  }

  if(is.na(dose) || (0 %in% dose) || is.na(cendinf)) {
    cendinfdn <- NA
  } else {
    cendinfdn <- cendinf/dose
    cendinfdn <- replace(cendinfdn, is.infinite(cendinfdn), NA)
  }

  return(cendinfdn)
}
