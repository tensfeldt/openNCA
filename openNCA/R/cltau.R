#' The total body clearance
#'
#' The total body clearance for intravascular administration, calculated using AUCTAU
#'
#' @details
#' \strong{Model M2 (SS) and M3 (SS)}
#' Steady-State Equation:
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{cltau.png} \cr
#'  }
#' }
#' \eqn{AUCTAUi = Area under the concentration versus time curve from zero time until the end of the ith dosing interval} \cr
#' \eqn{DOSEi = dose unit value for drug dosing interval} \cr
#'
#' @param auctau The area under the concentration versus time curve (numeric value)
#' @param dose The dose data (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item CLTAU: total body clearance
#' }
#'
#' @examples
#' ##########
#' ## Data ##
#' ##########################################
#' ##  SID  ##  TIME  ##   CONC   ##  TAU  ##
#' ##########################################
#' ##   30  ##    0   ##   2.89   ##   5   ##
#' ##   30  ##    1   ##   2.49   ##   5   ##
#' ##   30  ##    2   ##   2.47   ##   5   ##
#' ##   30  ##    3   ##   2.38   ##   5   ##
#' ##   30  ##    4   ##   2.32   ##   5   ##
#' ##   30  ##    5   ##   2.28   ##   5   ##
#' ##########################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(2.89, 2.49, 2.47, 2.38, 2.32, 2.28)
#' time_vector <- c(0, 1, 2, 3, 4, 5)
#' tau_val <- 5
#' told_val <- 0
#'
#' auc_tau(conc = conc_vector, time = time_vector, tau = tau_val, told = told_val,
#'         curr_di = 1, max_di = 2)
#' #12.23956
#'
#' cltau(auctau = 12.23956, dose = 300)
#' #24.51069
#'
#' ############
#' ## Data 2 ##
#' ##########################################
#' ##  SID  ##  TIME  ##   CONC   ##  TAU  ##
#' ##########################################
#' ##   31  ##    0   ##      0   ##   2   ##
#' ##   31  ##    1   ##      0   ##   2   ##
#' ##   31  ##    2   ##      0   ##   2   ##
#' ##########################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(0, 0, 0)
#' time_vector <- c(0, 1, 2)
#' tau_val <- 2
#' told_val <- 0
#'
#' auc_tau(conc = conc_vector, time = time_vector, tau = tau_val, told = told_val,
#'         curr_di = 1, max_di = 2)
#' #0
#'
#' cltau(auctau = 0, dose = 300)
#' #NA
#'
#' ############
#' ## Data 3 ##
#' ##########################################
#' ##  SID  ##  TIME  ##   CONC   ##  TAU  ##
#' ##########################################
#' ##   32  ##    0   ##   1.19   ##   2   ##
#' ##   32  ##    1   ##   1.23   ##   2   ##
#' ##   32  ##    2   ##   1.34   ##   2   ##
#' ##########################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(1.19, 1.23, 1.34)
#' time_vector <- c(0, 1, 2)
#' tau_val <- 2
#' told_val <- 0
#'
#' auc_tau(conc = conc_vector, time = time_vector, tau = tau_val, told = told_val,
#'         curr_di = 1, max_di = 2)
#' #2.495
#'
#' cltau(auctau = 2.495, dose = 300)
#' #120.2405
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
cltau <- function(auctau = NULL, dose = NULL){
  if(is.null(auctau) && is.null(dose)){
    stop("Error in cltau: 'auctau' and 'dose' vectors are NULL")
  } else if(is.null(auctau)) {
    stop("Error in cltau: 'auctau' vector is NULL")
  } else if(is.null(dose)) {
    stop("Error in cltau: 'dose' vectors is NULL")
  }

  if(length(dose) != length(auctau) ){
    stop("Error in cltau: length of vector arguments do not match")
  }

  if(is.na(dose) || (0 %in% dose) || is.na(auctau)) {
    cl_tau <- NA
  } else {
    cl_tau <- dose/auctau
    cl_tau <- replace(cl_tau, is.infinite(cl_tau), NA)
  }

  return(cl_tau)
}
