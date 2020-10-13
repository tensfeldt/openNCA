#' Area under the concentration versus time curve from time 0 to infinity (Predicted)
#'
#' This function gets the area under the concentration versus time curve from time 0 to
#' infinity (Predicted). Estimated concentration at the time of the last measurable concentration
#' is used to calculate extrapolated AUC.\cr
#'
#' @details
#' \strong{Equation}
#' \enumerate{
#'  \item Calculate the estimated concentration (Cest) at the time of the last measurable concentration: \cr
#'  \tabular{rl}{
#'   \tab \figure{auc_inf_p1.png} \cr
#'  }
#'  \eqn{Cest = Estimated concentration at time Tlast} \cr
#'  \eqn{Tlast = time of the last measurable concentration} \cr
#'  \eqn{KEL = Terminal or elminatination phase rate constant} \cr
#'  \eqn{C_{0} = Y-intercept (concentration at time zero) from log-linear regression for KEL}{C0 =
#'  Y-intercept (concentration at time zero) from log-linear regression for KEL} \cr
#'  \item Calculate the extrapolated AUC: \cr
#'  \tabular{rl}{
#'   \tab \figure{auc_inf_p2.png} \cr
#'  }
#'  \item \figure{auc_inf_p3.png} \cr
#' }
#' @section Additional Details:
#' \strong{Linear Method} \cr
#' \figure{auc_1.png} \cr
#' \strong{Log Method} \cr
#' \figure{auc_2.png} \cr
#' \eqn{AUC = Area under the cruve} \cr
#' \eqn{C_{i} = Concentration 1}{Ci = Concentration 1} \cr
#' \eqn{C_{i+1} = Concentration 2}{Ci+1 = Concentration 2} \cr
#' \eqn{T_{i} = Time 1}{Ti = Time 1} \cr
#' \eqn{T_{i+1} = Time 2}{Ti+1 = Time 2} \cr
#' \eqn{ln = Natural Logarithm} \cr \cr
#' \strong{Methods:} You can use the following methods to calculate AUC: \cr
#' \enumerate{
#'  \item \strong{Linear-Log Trapazoidal Rule}(default method): The linear method is used up to Tmax (the
#'  first occurrence of Cmax) and the log trapezoidal method is used for the remainder of the profile. If
#'  Ci or Ci+1 is 0 then the linear trapezoidal rule is used.
#'  \item \strong{Linear Trapazoidal Rule}: The linear method is used for the entire profile.
#'  \item \strong{Log Trapazoidal Rule}: The log trapezoidal method is used for the entire profile. If
#'  Ci or Ci+1 is 0 then the linear trapezoidal rule is used.
#'  \item \strong{Linear Up - Log Down Trapazoidal Rule}: Linear trapezoidal while the concentrations
#'  are increasing and log trapezoidal while the concentration are decreasing, the assessment is made on
#'  a step basis for each portion of the profile i.e. t1 to t2. If Ci or Ci+1 is 0 then the linear
#'  trapezoidal rule is used.
#' }
#'
#' @section Note:
#' The inputs 'kelflag', 'aucflag', 'auclast' and 't_last' are optional. If 'auclast' input is not provided then it will generate the AUCLASTi using the concentration and time data provided. \cr
#' If 't_last' input is not provided then it will generate the CLAST using the concentration and time data provided.
#' \strong{auclast}: Refer to \code{\link{auc_last}} for more details \cr
#' \strong{clast}: Refer to \code{\link{clast}} for more details \cr
#'
#' @param conc The concentration data (given in a vector form)
#' @param time The time data (given in a vector form)
#' @param method The method that will be used to calculate AUC (use either 1, 2, 3, or 4)\cr
#' \enumerate{
#' \item Linear-Log Trapezoidal Rule (default)
#' \item Linear Trapezoidal Rule
#' \item Log Trapezoidal Rule
#' \item Linear Up - Log Down Trapezoidal Rule
#' }
#' Note: check 'Methods' section below for more details \cr
#' @param kelflag The KEL exclude flag data (given in a numeric vector)
#' @param aucflag The AUC exclude flag data (given in a numeric vector)
#' @param auclast The area under the concentration versus time curve from zero time until the time (TLAST) of the last measurable concentration (CLASTi) during the ith dosing interval (numeric value)
#' @param t_last The time of last measurable (non-zero) plasma concentration (numeric value)
#' @param spanratio The SPAN Ratio (numeric value)
#' @param kel The terminal phase rate constant for the concentration-time profile of interest (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item AUC: area under the curve
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
#' #auc_inf_p()
#' #Error in auc_inf_p: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- c(2.89, 2.49, 2.47, 2.38, 2.32, 2.28)
#' time_vector <- c(0, 1, 2, 3, 4, 5)
#' kelflag_vector <- c(0, 1, 0, 0, 0, 1)
#' aucflag_vector <- c(1, 0, 0, 0, 1, 0)
#'
#' #auc_inf_p(conc = conc_vector, time = time_vector, method = NA)
#' #Error in auc_inf_p: the value provided for 'method' is not correct
#'
#' auc_inf_p(conc = conc_vector, time = time_vector, method = 2)
#' #66.50047
#'
#' auc_inf_p(conc = conc_vector, time = time_vector, method = 1)
#' #66.49503
#'
#' auc_inf_p(conc = conc_vector, time = time_vector, method = 1, kelflag = kelflag_vector)
#' #50.52326
#'
#' auc_inf_p(conc = conc_vector, time = time_vector, method = 1,  kelflag = kelflag_vector,
#'           aucflag = aucflag_vector)
#' #47.84769
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
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
auc_inf_p <- function(conc = NULL, time = NULL, method = 1, kelflag = NULL, aucflag = NULL, auclast = NULL, t_last = NULL, spanratio = NULL, kel = NULL){
  if(is.null(conc) && is.null(time)){
    stop("Error in auc_inf_p: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in auc_inf_p: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in auc_inf_p: 'time' vectors is NULL")
  } else if(all(is.na(time))) { # 2019-09-11/TGT/
      return(NA)
  } else if(all(is.na(conc))) { # 2019-09-11/TGT/
      return(NA)
  }

  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in auc_inf_p: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in auc_inf_p: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc)){
    stop("Error in auc_inf_p: length of 'time' and 'conc' vectors are not equal")
  }
  if((method != 1 && method != 2 && method != 3 && method != 4) || (is.null(method) || is.na(method))){
    stop("Error in auc_inf_p: the value provided for 'method' is not correct")
  }

  if(is.null(kel)){
    if(!(is.null(spanratio))){
      kel <- kel(conc = conc, time = time, exflag = kelflag, spanratio = spanratio)
    } else {
      kel <- kel(conc = conc, time = time, exflag = kelflag)
    }
  }
  if(is.null(t_last)){
    t_last <- tlast(conc = conc, time = time)
  }
  if(sum(conc, na.rm = T) == 0){
    return(NA)
  }

  if(is.na(kel[['KEL']])){
### 2019-09-01/TGT/ the following hsould be auc_infp NOT aumc_infp
###    aumc_infp <- NA
###    return(aumc_infp)
    auc_infp <- NA
    return(auc_infp)
  } else {
    c_est <- exp(-1*t_last*kel[['KEL']]) * kel[['KELC0']]
    auc_pred <- c_est/kel[['KEL']]
    if(is.null(auclast)){
      auclast <- auc_last(conc = conc, time = time, method = method, exflag = aucflag)
    }
    auc_infp <- auclast + auc_pred

    if(auc_infp < 0 || is.na(auc_infp)){
      auc_infp <- NA
      return(auc_infp)
    } else {
      return(auc_infp)
    }
  }
}
