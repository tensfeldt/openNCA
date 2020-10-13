#' Corrected AUCINFO
#'
#' AUCINFO corrected using estimates of KEL and C0 \cr
#'
#' @details
#' \strong{Equation}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{auc_inf_oc.png} \cr
#'  }
#'  \eqn{C0 = the residual plasma concentration observed at time zero (i.e. the predose concentraton)} \cr
#'  \eqn{KEL = the terminal phase rate constant for the concentration-time profile of interest} \cr
#' }
#'
#' @section Note:
#' \strong{auc_inf_o}: Refer to \code{\link{auc_inf_o}} for more details \cr
#' \strong{kel}: Refer to \code{\link{kel}} for more details \cr
#'
#' @param kel The terminal phase rate constant for the concentration-time profile of interest (numeric value)
#' @param aucinfo The area under the concentration versus time curve from time 0 to infinity (Observed) (numeric value)
#' @param c0 The residual plasma concentration observed at time zero (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item AUCINFOC: area under the curve corrected
#' }
#'
#' @examples
#' ##########
#' ## Data ##
#' ###########################################################
#' ##  SID  ##  TIME  ##   CONC   ##  KELFLAG  ##  AUCFLAG  ##
#' ###########################################################
#' ##   30  ##    0   ##   1.01   ##     0     ##     1     ##
#' ##   30  ##    1   ##   1.16   ##     1     ##     0     ##
#' ##   30  ##    2   ##   1.17   ##     0     ##     0     ##
#' ##   30  ##    3   ##   1.13   ##     0     ##     0     ##
#' ##   30  ##    4   ##   1.21   ##     0     ##     1     ##
#' ##   30  ##    5   ##   0.976  ##     1     ##     0     ##
#' ##   30  ##    6   ##   0.785  ##     1     ##     0     ##
#' ##   30  ##    7   ##   0.55   ##     0     ##     0     ##
#' ##   30  ##    8   ##   0.368  ##     0     ##     0     ##
#' ##   30  ##    9   ##   0.289  ##     0     ##     1     ##
#' ##   30  ##    10  ##   0.118  ##     1     ##     0     ##
#' ##   30  ##    11  ##   0.0608 ##     1     ##     0     ##
#' ###########################################################
#'
#' #Data mentioned will be used for the following example
#'
#' #auc_inf_oc()
#' #Error in auc_inf_oc: 'kel', 'aucinfo' and 'c0' vectors are NULL
#'
#' conc_vector <- c(1.01, 1.16, 1.17, 1.13, 1.21, 0.976, 0.785, 0.55, 0.368, 0.289, 0.118, 0.0608)
#' time_vector <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
#' kelflag_vector <- c(0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1)
#' aucflag_vector <- c(1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0)
#'
#' kel(conc = conc_vector, time = time_vector)
#' #      KEL      KELC0    KELTMLO    KELTMHI    KELNOPT      THALF     THALFF
#' #0.2428728  2.0502221  0.0000000 11.0000000 12.0000000  2.8539516         NA
#'
#' auc_inf_o(conc = conc_vector, time = time_vector, method = 2)
#' #8.541737
#'
#' auc_inf_oc(aucinfo = 8.541737, kel = 0.2428728, c0 = 1.01)
#' #4.383183
#'
#' auc_inf_o(conc = conc_vector, time = time_vector, method = 1, kelflag = kelflag_vector)
#' #8.63335
#'
#' auc_inf_oc(aucinfo = 8.63335, kel = 0.2428728, c0 = 1.01)
#' #4.474795
#'
#' auc_inf_o(conc = conc_vector, time = time_vector, method = 1,  kelflag = kelflag_vector,
#'           aucflag = aucflag_vector)
#' #7.313448
#'
#' auc_inf_oc(aucinfo = 7.313448, kel = 0.2428728, c0 = 1.01)
#' #3.154893
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
auc_inf_oc <- function(kel = NULL, aucinfo = NULL, c0 = NULL){
  if(is.null(kel) && is.null(aucinfo) && is.null(c0)){
    stop("Error in auc_inf_oc: 'kel', 'aucinfo' and 'c0' vectors are NULL")
  } else if(is.null(kel) && is.null(aucinfo)){
    stop("Error in auc_inf_oc: 'kel' and 'aucinfo' vectors are NULL")
  } else if(is.null(kel) && is.null(c0)){
    stop("Error in auc_inf_oc: 'kel' and 'c0' vectors are NULL")
  } else if(is.null(aucinfo) && is.null(c0)){
    stop("Error in auc_inf_oc: 'aucinfo' and 'c0' vectors are NULL")
  } else if(is.null(kel)) {
    stop("Error in auc_inf_oc: 'kel' vector is NULL")
  } else if(is.null(aucinfo)) {
    stop("Error in auc_inf_oc: 'aucinfo' vectors is NULL")
  } else if(is.null(c0)) {
    stop("Error in auc_inf_oc: 'c0' vectors is NULL")
  }

  if(isTRUE(is.na(kel) || is.na(c0) || is.na(aucinfo))) {
    aucinfoc <- NA
  } else {
    aucinfoc <- aucinfo - (c0/kel)
  }
  return(aucinfoc)
}
