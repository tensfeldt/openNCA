#' The mean residence time (MRT) extrapolated to infinity for a substance administered by extravascular
#' dosing, calculated using the observed value of the last non-zero concentration.
#'
#' Note that the parameter naming convention for Release 2 of the Computaton Engine Specifications
#' referred to MRTEVIFO(i) as MRTOi for extravascular administration. This has been changed to MRTEVIFO(i)
#' with Release 3 to distinguish between intra- and extravascular administration.\cr
#'
#' @details
#' \strong{Single Dose Equation:}
#' \strong{For Model M1}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{mrt_evifp_1.png} \cr
#'  }
#' }
#' \strong{Steady-State Equation:}
#' \strong{For Model M1}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{mrt_evifp_2.png} \cr
#'  }
#' }
#' \eqn{AUMCINFO = Area under the first moment curve from zero time to infinity (Observed)} \cr
#' \eqn{AUCINFO = Area under the concentration versus time curve from zero time to infinity (Observed)} \cr
#' \eqn{AUMCTAUi = Area under the first moment curve from zero time until the end of the ith dosing interval} \cr
#' \eqn{AUCTAUi = Area under the concentration versus time curve from zero time until the end of the ith dosing interval} \cr
#' \eqn{TAUi = ith Dosing interval i refers to the value of NDOSEI} \cr
#'
#' @param conc The concentration data (given in a vector form)
#' @param time The time data (given in a vector form)
#' @param method The method that will be used to calculate AUC (use either 1, 2, 3, or 4)\cr
#' \enumerate{
#' \item Linear-Log Trapazoidal Rule (default)
#' \item Linear Trapazoidal Rule
#' \item Log Trapazoidal Rule
#' \item Linear Up - Log DownTrapazoidal Rule
#' }
#' Note: check 'Methods' section below for more details \cr
#' @param parameter This is either single dose (SD) or steady state (SS)
#' @param kelflag The kel exclude flag data (given in a numeric vector)
#' @param aucflag The auc exclude flag data (given in a numeric vector)
#' @param tau The time duration of dosing interval (numeric value)
#' @param orig_conc The original (full) concentration data (given in a numeric vector)
#' @param orig_time The original (full) time data (given in a numeric vector)
#'
#' @section Returns:
#' \strong{Dataset} \cr
#' \itemize{
#'  \item MRTEVIFP: the mean residence time (MRT) extrapolated to infinity
#' }
#'
#' @examples
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
mrt_evif_p <- function(conc = NULL, time = NULL, method = 1, parameter = "SD", kelflag = NULL, aucflag = NULL, tau = NULL, orig_conc = NULL, orig_time = NULL){
  if(is.null(conc) && is.null(time)){
    stop("Error in mrt_evif_p: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in mrt_evif_p: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in mrt_evif_p: 'time' vectors is NULL")
  } else if(all(is.na(time))) { # 2019-09-11/TGT/
      return(NA)
  } else if(all(is.na(conc))) { # 2019-09-11/TGT/
      return(NA)
  }

  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in mrt_evif_p: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in mrt_evif_p: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc)){
    stop("Error in mrt_evif_p: length of 'time' and 'conc' vectors are not equal")
  }
  if(method != 1 && method != 2 && method != 3 && method != 4){
    stop("Error in aumc_XpctP: the value provided for 'method' is not correct")
  }
  if(tolower(parameter) != "sd" && tolower(parameter) != "ss"){
    stop("Error in mrt_evif_p: the value provided for 'parameter' is not correct")
  }

  mrtevifp <- NA
  if(tolower(parameter) == "sd") {
    auc_infp <- auc_inf_p(conc = conc, time = time, method = method, kelflag = kelflag, aucflag = aucflag)
    aumc_infp <- aumc_inf_p(conc = conc, time = time, method = method, kelflag = kelflag, aucflag = aucflag)

    if(auc_infp <= 0 || aumc_infp < 0 || is.na(auc_infp) || is.na(aumc_infp)){
      mrtevifp <- NA
    } else {
      mrtevifp <- aumc_infp/auc_infp
    }
  } else if(tolower(parameter) == "ss") {
      auc_infp <- auc_inf_p(conc = conc, time = time, method = method, kelflag = kelflag, aucflag = aucflag)
### 2019-09-04/TGT/ added orig_conc and orig_time to auc_tau call to permit interpolation
###    auctau <- auc_tau(conc = conc, time = time, method = method, exflag = aucflag, tau = tau)
    auctau <- auc_tau(conc = conc, time = time, method = method, exflag = aucflag, tau = tau, orig_conc = orig_conc, orig_time = orig_time)
### 2019-09-04/TGT/ added orig_conc and orig_time to aumc_tau call to permit interpolation
###    aumctau <- aumc_tau(conc = conc, time = time, method = method, exflag = aucflag, tau = tau)
    aumctau <- aumc_tau(conc = conc, time = time, method = method, exflag = aucflag, tau = tau, orig_conc = orig_conc, orig_time = orig_time)

    if(auc_infp <= 0 || auctau < 0 || aumctau < 0 || is.na(auc_infp) || is.na(auctau) || is.na(aumctau)){
      mrtevifp <- NA
    } else {
### 2019-09-04/TGT/ change auc_tau to auctau, replace mrtevifo with mrtevifp
###      mrtevifo <- ((aumctau + tau)*(auc_infp - auctau))/auc_tau
        mrtevifp <- ((aumctau + tau)*(auc_infp - auctau))/auctau
    }
  }

  return(mrtevifp)
}
