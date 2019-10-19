#' Apparent clearance (predicated) of drug
#'
#' Apparent clearance (predicted) of drug from e.g. plasma, for extravascular routes of administration.
#'
#' @details
#' \strong{Model M1 (SD)}
#' Single Dose Equation:
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{clfp.png} \cr
#'  }
#' }
#' \eqn{F = fraction of dose absorbed assumed to be since value is unknown for extravascular model} \cr
#' \eqn{Dose = dose unit value for drug dosing interval} \cr
#' \eqn{AUCINFP = Area under the first moment curve from zero time to infinity  (Predicted)} \cr
#'
#' @param aucinfo The AUCINFO data (given in a vector form)
#' @param dose The dose data (given in a vector form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item CLFP: Apparent clearance of drug
#' }
#'
#' @examples
#' ##########
#' ## Data ##
#' ###########################################################
#' ##  SID  ##  TIME  ##   CONC   ##  KELFLAG  ##  AUCFLAG  ##
#' ###########################################################
#' ##   30  ##    0   ##   2.89   ##     0     ##     1     ##
#' ##   30  ##    1   ##   2.49   ##     1     ##     0     ##
#' ##   30  ##    2   ##   2.47   ##     0     ##     0     ##
#' ##   30  ##    3   ##   2.38   ##     0     ##     0     ##
#' ##   30  ##    4   ##   2.32   ##     0     ##     1     ##
#' ##   30  ##    5   ##   2.28   ##     1     ##     0     ##
#' ###########################################################
#'
#' #Data mentioned will be used for the following example
#'
#' auc_inf_p()
#' #Error in auc_inf_p: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- c(2.89, 2.49, 2.47, 2.38, 2.32, 2.28)
#' time_vector <- c(0, 1, 2, 3, 4, 5)
#' kelflag_vector <- c(0, 1, 0, 0, 0, 1)
#' aucflag_vector <- c(1, 0, 0, 0, 1, 0)
#'
#' auc_inf_p(conc = conc_vector, time = time_vector, method = NA)
#' #Error in auc_inf_p: the value provided for 'method' is not correct
#'
#' auc_inf_p(conc = conc_vector, time = time_vector, method = 2)
#' #66.50047
#'
#' clfp(aucinfp = 66.50047, dose = 300)
#' #4.511246
#'
#' auc_inf_p(conc = conc_vector, time = time_vector, method = 1, kelflag = kelflag_vector)
#' #50.52326
#'
#' clfp(aucinfp = 50.52326, dose = 300)
#' #5.937859
#'
#' auc_inf_p(conc = conc_vector, time = time_vector, method = 1,  kelflag = kelflag_vector, aucflag = aucflag_vector)
#' #47.84769
#'
#' clfp(aucinfp = 47.84769, dose = 300)
#' #6.269895
#'
#' ############
#' ## Data 2 ##
#' ###########################################################
#' ##  SID  ##  TIME  ##   CONC   ##  KELFLAG  ##  AUCFLAG  ##
#' ###########################################################
#' ##   31  ##    0   ##      0   ##     0     ##     1     ##
#' ##   31  ##    1   ##      0   ##     0     ##     0     ##
#' ##   31  ##    2   ##      0   ##     0     ##     0     ##
#' ###########################################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(0, 0, 0)
#' time_vector <- c(0, 1, 2)
#' kelflag_vector <- c(0, 0, 0)
#' aucflag_vector <- c(0, 0, 0)
#'
#' auc_inf_p(conc = conc_vector, time = time_vector, method = 1)
#' #0
#'
#' clfp(aucinfp = 0, dose = 300)
#' #NA
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
clfp <- function(aucinfp = NULL, dose = NULL){
  if(is.null(aucinfp) && is.null(dose)){
    stop("Error in clfp: 'aucinfp' and 'dose' vectors are NULL")
  } else if(is.null(aucinfp)) {
    stop("Error in clfp: 'aucinfp' vector is NULL")
  } else if(is.null(dose)) {
    stop("Error in clfp: 'dose' vectors is NULL")
  }

  if(length(dose) != length(aucinfp) ){
    stop("Error in clfp: length of vector arguments do not match")
  }

  if(is.na(dose) || (0 %in% dose) || is.na(aucinfp)) {
    clf_p <- NA
  } else {
    clf_p <- dose/aucinfp
    clf_p <- replace(clf_p, is.infinite(clf_p), NA)
  }

  return(clf_p)
}
