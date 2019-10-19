#' The mean residence time (MRT) extrapolated to infinity for a substance administered by extravascular
#' dosing, calculated using the observed value of the last non-zero concentration.
#'
#' Note that the parameter naming convention for Release 2 of the Computaton Engine Specifications
#' referred to MRTEVIFO(i) as MRTOi for extravascular administration. This has been changed to MRTEVIFO(i)
#' with Release 3 to distinguish between intra- and extravascular administration.\cr
#'
#' @details
#' \strong{Single Dose Equation:}
#' \strong{For Model M2}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{mrt_ivifo_1.png} \cr
#'  }
#' }
#' \strong{For Model M3}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{mrt_ivifo_2.png} \cr
#'  }
#' }
#' \strong{Steady-State Equation:}
#' \strong{For Model M2}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{mrt_ivifo_3.png} \cr
#'  }
#' }
#' \strong{For Model M3}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{mrt_ivifo_4.png} \cr
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
#' @param tau The time duration of dosing interval (numeric value)
#'
#' @section Returns:
#' \strong{Dataset} \cr
#' \itemize{
#'  \item MRTIVIFO: the mean residence time (MRT) extrapolated to infinity
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
mrt_ivif_o <- function(conc = NULL, time = NULL, method = 1, model = "M2", parameter = "SD", kelflag = NULL, aucflag = NULL, tau = NULL, dof = NULL, spanratio = NULL, kel = NULL, orig_conc = NULL, orig_time = NULL){
###    cat("Entered mrt_ivif_o: \n")
###    cat("time: \n"); print(time)
###    cat("conc: \n"); print(conc)
###    cat("model: ", model, "\n")
###    cat("parameter: ", parameter, "\n")
###    cat("dof: \n");    print(dof)
###    cat("tau: \n");    print(tau)
###    cat('orig_time:', orig_time, ' orig_conc: ', orig_conc, '\n')
###    cat('kelflag: ', kelflag, '\n', ' aucflag: ', aucflag, ' spanratio: ', spanratio, '\n')
###    cat('kel: \n'); print(kel)
    
  if(is.null(conc) && is.null(time)){
    stop("Error in mrt_ivif_o: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in mrt_ivif_o: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in mrt_ivif_o: 'time' vectors is NULL")
  } else if(all(is.na(time))) { # 2019-09-16/TGT/
      return(NA)
  } else if(all(is.na(conc))) { # 2019-09-16/TGT/
      return(NA)
  }

  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in mrt_ivif_o: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in mrt_ivif_o: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc)){
    stop("Error in mrt_ivif_o: length of 'time' and 'conc' vectors are not equal")
  }
  if(method != 1 && method != 2 && method != 3 && method != 4){
    stop("Error in mrt_ivif_o: the value provided for 'method' is not correct")
  }
  if(model != "M2" && model != "m2" && model != "M3" && model != "m3"){
    stop("Error in mrt_ivif_o: the value provided for 'model' is not correct")
  }
  if(tolower(parameter) != "sd" && tolower(parameter) != "ss"){
    stop("Error in mrt_ivif_o: the value provided for 'parameter' is not correct")
  }
###cat("mrt_ivif_o: check here1:\n")
  mrtivifo <- NA
    if(model == "M2" || model == "m2") {
###cat("mrt_ivif_o: check here1a:\n")
        
    if(tolower(parameter) == "sd") {
###cat('before auc_inf_o call...\n')
        auc_info <- auc_inf_o(conc = conc, time = time, method = method, kelflag = kelflag, aucflag = aucflag, spanratio = spanratio, kel = kel)
###cat('auc_info: ', auc_info, '\n')
      aumc_info <- aumc_inf_o(conc = conc, time = time, method = method, kelflag = kelflag, aucflag = aucflag, spanratio = spanratio, kel = kel)

      if(auc_info <= 0 || aumc_info < 0 || is.na(auc_info) || is.na(aumc_info)){
        mrtivifo <- NA
      } else {
        mrtivifo <- aumc_info/auc_info
      }
    } else if(tolower(parameter) == "ss") {
###        cat("mrt_ivif_o: check here2:\n")
###        print(tau)
      if(is.null(tau)){
### 2019-08-12/TGT/ Fixed label from mrt_ivif_p to mrt_ivif_o            
###        stop("Error in mrt_ivif_p: tau is NULL")
        stop("Error in mrt_ivif_o: tau is NULL")
      }
###cat("check here2a:\n")
###cat("kelflag:\n")
###print(kelflag)
###    cat('time:', time, ' conc: ', conc, ' method: ', method, ' kelflag: ', kelflag, ' aucflag: ', aucflag, ' spanratio: ', spanratio, ' kel: ', kel, '\n')
        
      auc_info <- auc_inf_o(conc = conc, time = time, method = method, kelflag = kelflag, aucflag = aucflag, spanratio = spanratio, kel = kel)
###       cat("mrt_ivif_o: auc_info:", auc_info, "\n")
###        cat("mrt_ivif_o: orig_conc: ", orig_conc, " orig_time: ", orig_time, "\n")
      auctau <- auc_tau(conc = conc, time = time, method = method, exflag = aucflag, tau = tau, orig_conc = orig_conc, orig_time = orig_time)
###        cat('mrt_ivif_o: auctau: ', auctau, '\n')
      aumctau <- aumc_tau(conc = conc, time = time, method = method, exflag = aucflag, tau = tau, orig_conc = orig_conc, orig_time = orig_time)
###        cat('mrt_ivif_o: aumctau: ', aumctau, '\n')

###cat("check here5:\n")
      
      if(auc_info <= 0 || auctau < 0 || aumctau < 0 || is.na(auc_info) || is.na(auctau) || is.na(aumctau)){
        mrtivifo <- NA
      } else {
###cat("mrt_ivif_o: check here6:\n")
          
          mrtivifo <- ((aumctau + tau)*(auc_info - auctau))/auctau
###          cat('mrtivifo: ', mrtivifo, '\n')
      }
    }
  } else if(model == "M3" || model == "m3") {
    if(is.null(dof)){
      stop("Error in mrt_ivif_o: dof is NULL")
    }
    if(!is.numeric(dof)){
      stop("Error in mrt_ivif_o: dof is not a numeric value")
    }
    if(tolower(parameter) == "sd") {
      auc_info <- auc_inf_o(conc = conc, time = time, method = method, kelflag = kelflag, aucflag = aucflag, spanratio = spanratio, kel = kel)
      aumc_info <- aumc_inf_o(conc = conc, time = time, method = method, kelflag = kelflag, aucflag = aucflag, spanratio = spanratio, kel = kel)

      if(auc_info <= 0 || aumc_info < 0 || is.na(auc_info) || is.na(aumc_info)){
        mrtivifo <- NA
      } else {
        mrtivifo <- aumc_info/auc_info - dof/2
      }
    } else if(tolower(parameter) == "ss") {
      if(is.null(tau)){
### 2019-08-12/TGT/ Fixed label from mrt_ivif_p to mrt_ivif_o            
###        stop("Error in mrt_ivif_p: tau is NULL")
        stop("Error in mrt_ivif_o: tau is NULL")
      }
      auc_info <- auc_inf_o(conc = conc, time = time, method = method, kelflag = kelflag, aucflag = aucflag, spanratio = spanratio, kel = kel)
###
###      cat('mrt_ivif_o: auc_info: ', auc_info, '\n')
      auctau <- auc_tau(conc = conc, time = time, method = method, exflag = aucflag, tau = tau, orig_conc = orig_conc, orig_time = orig_time)
###      cat('mrt_ivif_o: auctau: ', auctau, '\n')
      aumctau <- aumc_tau(conc = conc, time = time, method = method, exflag = aucflag, tau = tau, orig_conc = orig_conc, orig_time = orig_time)
###      cat('mrt_ivif_o: aumc: ', aumctau, '\n')
      
      if(auc_info <= 0 || auctau < 0 || aumctau < 0 || is.na(auc_info) || is.na(auctau) || is.na(aumctau)){
        mrtivifo <- NA
      } else {
        mrtivifo <- (((aumctau + tau)*(auc_info - auctau))/auctau) - dof/2
      }
    }
  }

  return(mrtivifo)
}
