#' Apparent clearance (observed) of drug
#'
#' Apparent clearance (observed) of drug from e.g. plasma, for extravascular routes of administration. \cr
#'
#' @details
#' \strong{Model M1}
#' Single Dose Equation only; not calculated at steady-state. Note: For steady-state CLFTAUi is calculated using AUCTAUi:
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{clfo.png} \cr
#'  }
#' }
#' \eqn{F = fraction of dose absorbed value is unknown for extravascular model} \cr
#' \eqn{Dose = sum of dosei to dosen} \cr
#' \eqn{AUCINFO = Area under the first moment curve from zero time to infinity (Observed)} \cr
#'
#' @param aucinfo The AUCINFO data (given in a vector form)
#' @param dose The dose data (given in a vector form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item CLFO: Apparent clearance of drug
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
#' auc_inf_o()
#' #Error in auc_inf_o: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- c(2.89, 2.49, 2.47, 2.38, 2.32, 2.28)
#' time_vector <- c(0, 1, 2, 3, 4, 5)
#' kelflag_vector <- c(0, 1, 0, 0, 0, 1)
#' aucflag_vector <- c(1, 0, 0, 0, 1, 0)
#'
#' auc_inf_o(conc = conc_vector, time = time_vector, method = 2)
#' #67.86757
#'
#' clfo(aucinfo = 67.86757, dose = 300)
#' #4.420373
#'
#' auc_inf_o(conc = conc_vector, time = time_vector, method = 1, kelflag = kelflag_vector)
#' #52.88634
#'
#' clfo(aucinfo = 52.88634, dose = 300)
#' #5.672542
#'
#' auc_inf_o(conc = conc_vector, time = time_vector, method = 1,  kelflag = kelflag_vector, aucflag = aucflag_vector)
#' #50.21078
#'
#' clfo(aucinfo = 50.21078, dose = 300)
#' #5.974813
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
#' auc_inf_o(conc = conc_vector, time = time_vector, method = 1)
#' #0
#'
#' clfo(aucinfo = 0, dose = 300)
#' #NA
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
clfo <- function(aucinfo = NULL, dose = NULL){
  if(is.null(aucinfo) && is.null(dose)){
    stop("Error in clfo: 'aucinfo' and 'dose' vectors are NULL")
  } else if(is.null(aucinfo)) {
    stop("Error in clfo: 'aucinfo' vector is NULL")
  } else if(is.null(dose)) {
    stop("Error in clfo: 'dose' vectors is NULL")
  }

  if(length(dose) != length(aucinfo) ){
    stop("Error in clfo: length of vector arguments do not match")
  }

  if(is.na(dose) || (0 %in% dose) || is.na(aucinfo)) {
    clf_o <- NA
  } else {
    clf_o <- dose/aucinfo
    clf_o <- replace(clf_o, is.infinite(clf_o), NA)
  }

  return(clf_o)
}
