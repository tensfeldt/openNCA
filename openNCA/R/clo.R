#' Total clearance of drug (observed)
#'
#' Total clearance of drug (observed) \cr
#'
#' @details
#' \strong{Models M2 and M3}
#' Single Dose Equation only; not calculated at steady-state. For steady-state CLTAUi is calculated using AUCTAUi
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{clo.png} \cr
#'  }
#' }
#' \eqn{Dose = sum of dosei to dosen} \cr
#' \eqn{AUCINFO = Area under the first moment curve from zero time to infinity (Observed)} \cr
#'
#' @section Note:
#' \strong{auc_inf_o}: Refer to \code{\link{auc_inf_o}} for more details
#'
#' @param aucinfo The AUCINFO data (given in a vector form)
#' @param dose The dose data (given in a vector form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item CLO: total clearance of drug
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
#' #auc_inf_o()
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
#' clo(aucinfo = 67.86757, dose = 300)
#' #4.420373
#'
#' auc_inf_o(conc = conc_vector, time = time_vector, method = 1, kelflag = kelflag_vector)
#' #52.88634
#'
#' clo(aucinfo = 52.88634, dose = 300)
#' #5.672542
#'
#' auc_inf_o(conc = conc_vector, time = time_vector, method = 1,
#'           kelflag = kelflag_vector, aucflag = aucflag_vector)
#' #50.21078
#'
#' clo(aucinfo = 50.21078, dose = 300)
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
#' clo(aucinfo = 0, dose = 300)
#' #NA
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
clo <- function(aucinfo = NULL, dose = NULL){
  if(is.null(aucinfo) && is.null(dose)){
    stop("Error in clo: 'aucinfo' and 'dose' vectors are NULL")
  } else if(is.null(aucinfo)) {
    stop("Error in clo: 'aucinfo' vector is NULL")
  } else if(is.null(dose)) {
    stop("Error in clo: 'dose' vectors is NULL")
  }

  if(length(dose) != length(aucinfo) ){
    stop("Error in clo: length of vector arguments do not match")
  }

  if(is.na(dose) || (0 %in% dose) || is.na(aucinfo)) {
    cl_o <- NA
  } else {
    cl_o <- dose/aucinfo
    cl_o <- replace(cl_o, is.infinite(cl_o), NA)
  }

  return(cl_o)
}
