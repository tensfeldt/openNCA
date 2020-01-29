#' The mean residence time (MRT) extrapolated to infinity for a substance administered by intravascular
#' dosing, calculated using the predicted value of the last non-zero concentration
#'
#' The mean residence time (MRT) extrapolated to infinity for a substance administered by intravascular
#' dosing, calculated using the predicted value of the last non-zero concentration. Note that the parameter
#' naming convention for Release 2 of Computation Engine Specificaitons referrred to MRTIVIFP(i) as MRTPi for
#' intravascular administration. This has been changed to MRTIVIFP(i) with Release 3 to distinguish betweeen
#' intra- and extravascular administration\cr
#'
#' @details
#' \strong{Single Dose Equation:}
#' \strong{For Model M2}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{mrt_ivifp_1.png} \cr
#'  }
#' }
#' \strong{For Model M3}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{mrt_ivifp_2.png} \cr
#'  }
#' }
#' \strong{Steady-State Equation:}
#' \strong{For Model M2}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{mrt_ivifp_3.png} \cr
#'  }
#' }
#' \strong{For Model M3}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{mrt_ivifp_4.png} \cr
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
#' @param model This is the model type
#' @param parameter This is either single dose (SD) or steady state (SS)
#' @param kelflag The kel exclude flag data (given in a numeric vector)
#' @param aucflag The auc exclude flag data (given in a numeric vector)
#'
#' @section Returns:
#' \strong{Dataset} \cr
#' \itemize{
#'  \item MRTIVIFP: the mean residence time (MRT) extrapolated to infinity
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
mrt_ivif_p <- function(conc = NULL, time = NULL, method = 1, model = "M2", parameter = "SD", kelflag = NULL, aucflag = NULL, tau = NULL, dof = NULL, spanratio = NULL, kel = NULL, orig_conc = NULL, orig_time = NULL, aucinfp = NULL, aumcinfp = NULL, auctau = NULL, aumctau = NULL){
  if(is.null(conc) && is.null(time)){
    stop("Error in mrt_ivif_p: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in mrt_ivif_p: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in mrt_ivif_p: 'time' vectors is NULL")
  } else if(all(is.na(time))) { # 2019-09-16/TGT/
      return(NA)
  } else if(all(is.na(conc))) { # 2019-09-16/TGT/
      return(NA)
  }

  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in mrt_ivif_p: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in mrt_ivif_p: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc)){
    stop("Error in mrt_ivif_p: length of 'time' and 'conc' vectors are not equal")
  }
  if(method != 1 && method != 2 && method != 3 && method != 4){
    stop("Error in aumc_XpctP: the value provided for 'method' is not correct")
  }
  if(model != "M2" && model != "m2" && model != "M3" && model != "m2"){
    stop("Error in mrt_ivif_p: the value provided for 'model' is not correct")
  }
  if(tolower(parameter) != "sd" && tolower(parameter) != "ss"){
    stop("Error in mrt_ivif_p: the value provided for 'parameter' is not correct")
  }

  mrtivifp <- NA
  if(model == "M2" || model == "m2"){
    if(tolower(parameter) == "sd") {
      if(is.null(aucinfp)){  
        aucinfp <- auc_inf_p(conc = conc, time = time, method = method, kelflag = kelflag, aucflag = aucflag, spanratio = spanratio, kel = kel)
      }
      if(is.null(aumcinfp)){  
        aumcinfp <- aumc_inf_p(conc = conc, time = time, method = method, kelflag = kelflag, aucflag = aucflag, spanratio = spanratio, kel = kel)
      }
      
      if(aucinfp <= 0 || aumcinfp < 0 || is.na(aucinfp) || is.na(aumcinfp)){
        mrtivifp <- NA
      } else {
        mrtivifp <- aumcinfp/aucinfp
      }
    } else if(tolower(parameter) == "ss") {
      if(is.null(tau)){
        stop("Error in mrt_ivif_p: tau is NULL")
      }
      if(is.null(aucinfp)){  
        aucinfp <- auc_inf_p(conc = conc, time = time, method = method, kelflag = kelflag, aucflag = aucflag, spanratio = spanratio, kel = kel)
      }
      if(is.null(auctau)){  
        auctau <- auc_tau(conc = conc, time = time, method = method, exflag = aucflag, tau = tau, orig_conc = orig_conc, orig_time = orig_time)
      }
      if(is.null(aumctau)){  
        aumctau <- aumc_tau(conc = conc, time = time, method = method, exflag = aucflag, tau = tau, orig_conc = orig_conc, orig_time = orig_time)
      }
      
      if(aucinfp <= 0 || auctau < 0 || aumctau < 0 || is.na(aucinfp) || is.na(auctau) || is.na(aumctau)){
        mrtivifp <- NA
      } else {
        mrtivifp <- ((aumctau + tau)*(aucinfp - auctau))/auctau
      }
    }
  } else if(model == "M3" || model == "m3") {
    if(is.null(dof)){
      stop("Error in mrt_ivif_p: dof is NULL")
    }
    if(!is.numeric(dof)){
      stop("Error in mrt_ivif_p: dof is not a numeric value")
    }

    if(tolower(parameter) == "sd") {
      if(is.null(aucinfp)){  
        aucinfp <- auc_inf_p(conc = conc, time = time, method = method, kelflag = kelflag, aucflag = aucflag, spanratio = spanratio, kel = kel)
      }
      if(is.null(aumcinfp)){  
        aumcinfp <- aumc_inf_p(conc = conc, time = time, method = method, kelflag = kelflag, aucflag = aucflag, spanratio = spanratio, kel = kel)
      }
      
      if(aucinfp <= 0 || aumcinfp < 0 || is.na(aucinfp) || is.na(aumcinfp)){
        mrtivifp <- NA
      } else {
        mrtivifp <- aumcinfp/aucinfp - dof/2
      }
    } else if(tolower(parameter) == "ss") {
      if(is.null(tau)){
        stop("Error in mrt_ivif_p: tau is NULL")
      }
      if(is.null(aucinfp)){  
        aucinfp <- auc_inf_p(conc = conc, time = time, method = method, kelflag = kelflag, aucflag = aucflag, spanratio = spanratio, kel = kel)
      }
      if(is.null(auctau)){  
        auctau <- auc_tau(conc = conc, time = time, method = method, exflag = aucflag, tau = tau, orig_conc = orig_conc, orig_time = orig_time)
      }
      if(is.null(aumctau)){  
        aumctau <- aumc_tau(conc = conc, time = time, method = method, exflag = aucflag, tau = tau, orig_conc = orig_conc, orig_time = orig_time)
      }
      
      if(aucinfp <= 0 || auctau < 0 || aumctau < 0 || is.na(aucinfp) || is.na(auctau) || is.na(aumctau)){
        mrtivifp <- NA
      } else {
        mrtivifp <- (((aumctau + tau)*(aucinfp - auctau))/auctau) - dof/2
      }
    }
  }

  return(mrtivifp)
}
