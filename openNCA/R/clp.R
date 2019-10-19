#' Total clearance (predicted)
#'
#' Total clearance (predicted) of drug from e.g. plasma.  \cr
#'
#' @details
#' \strong{Models M2 and M3}
#' Single Dose Equation only; not calculated at steady-state. For steady-state CLTAUi is calculated using AUCTAUi
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{clp.png} \cr
#'  }
#' }
#' \eqn{Dose = sum of dosei to dosen} \cr
#' \eqn{AUCINFP = Area under the first moment curve from zero time to infinity  (Predicted)} \cr
#'
#' @section Note:
#' \strong{auc_inf_p}: Refer to \code{\link{auc_inf_p}} for more details
#'
#' @param aucinfp The AUCINFP data (given in a vector form)
#' @param dose The dose data (given in a vector form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item CLP: total clearance of drug
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
#' conc_vector <- c(2.89, 2.49, 2.47, 2.38, 2.32, 2.28)
#' time_vector <- c(0, 1, 2, 3, 4, 5)
#' kelflag_vector <- c(0, 1, 0, 0, 0, 1)
#' aucflag_vector <- c(1, 0, 0, 0, 1, 0)
#'
#' auc_inf_p(conc = conc_vector, time = time_vector, method = 2)
#' #66.50047
#'
#' clp(aucinfp = 66.50047, dose = 300)
#' #4.511246
#'
#' auc_inf_p(conc = conc_vector, time = time_vector, method = 1)
#' #66.49503
#'
#' clp(aucinfp = 66.49503, dose = 300)
#' #4.511615
#'
#' auc_inf_p(conc = conc_vector, time = time_vector, method = 1, kelflag = kelflag_vector)
#' #50.52326
#'
#' clp(aucinfp = 50.52326, dose = 300)
#' #5.937859
#'
#' auc_inf_p(conc = conc_vector, time = time_vector, method = 1,  kelflag = kelflag_vector, aucflag = aucflag_vector)
#' #47.84769
#'
#' clp(aucinfp = 47.84769, dose = 300)
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
#' clp(aucinfp = 0, dose = 300)
#' #NA
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
clp <- function(aucinfp = NULL, dose = NULL){
  if(is.null(aucinfp) && is.null(dose)){
    stop("Error in clp: 'aucinfp' and 'dose' vectors are NULL")
  } else if(is.null(aucinfp)) {
    stop("Error in clp: 'aucinfp' vector is NULL")
  } else if(is.null(dose)) {
    stop("Error in clp: 'dose' vectors is NULL")
  }

  if(length(dose) != length(aucinfp)){
    stop("Error in clp: length of vector arguments do not match")
  }

  if(is.na(dose) || (0 %in% dose) || is.na(aucinfp)) {
    cl_p <- NA
  } else {
    cl_p <- dose/aucinfp
    cl_p <- replace(cl_p, is.infinite(cl_p), NA)
  }

  return(cl_p)
}
