#' Area under the concentration versus time curve from time 0 to infinity (Observed)
#'
#' This function gets the area under the concentration versus time curve from time 0 to
#' infinity (Observed). Observed value for the time of the last measurable concentration is used
#' to calculate the extrapolated AUC.\cr
#'
#' @details
#' \strong{Equation}
#' \enumerate{
#'  \item Calculate the extrapolated AUC: \cr
#'  \tabular{rl}{
#'   \tab \figure{auc_inf_o1.png} \cr
#'  }
#'  \eqn{CLAST = Last measurable (non-zero) plasma concentration} \cr
#'  \eqn{KEL = Terminal phase rate constant} \cr
#'  \item Final Equation: \cr
#'  \tabular{rl}{
#'   \tab \figure{auc_inf_o2.png} \cr
#'  }
#'  \eqn{AUCLAST = Area under the concentration versus time curve from zero until time of last
#' measurable concentration} \cr
#' }
#' @section Additional Details:
#' \strong{Linear Method} \cr
#' \figure{auc_1.png} \cr
#' \strong{Log Method} \cr
#' \figure{auc_2.png} \cr
#' \eqn{AUC = Area under the curve} \cr
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
#' The inputs 'kelflag', 'aucflag', 'auclast' and 'c_last' are optional. If 'auclast' input is not provided then it will generate the AUCLASTi using the concentration and time data provided. \cr
#' If 'c_last' input is not provided then it will generate the CLAST using the concentration and time data provided. \cr \cr
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
#' @param c_last The last measurable (non-zero) plasma concentration (numeric value)
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
#' #auc_inf_o()
#' #Error in auc_inf_o: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- c(2.89, 2.49, 2.47, 2.38, 2.32, 2.28)
#' time_vector <- c(0, 1, 2, 3, 4, 5)
#' kelflag_vector <- c(0, 1, 0, 0, 0, 1)
#' aucflag_vector <- c(1, 0, 0, 0, 1, 0)
#'
#' #auc_inf_o(conc = conc_vector, time = time_vector, method = NA)
#' #Error in auc_inf_o: the value provided for 'method' is not correct
#'
#' auc_inf_o(conc = conc_vector, time = time_vector, method = 2)
#' #67.86757
#'
#' auc_inf_o(conc = conc_vector, time = time_vector, method = 1)
#' #67.86212
#'
#' auc_inf_o(conc = conc_vector, time = time_vector, method = 1, kelflag = kelflag_vector)
#' #52.88634
#'
#' auc_inf_o(conc = conc_vector, time = time_vector, method = 1,  kelflag = kelflag_vector,
#'           aucflag = aucflag_vector)
#' #50.21078
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
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
auc_inf_o <- function(conc = NULL, time = NULL, method = 1, kelflag = NULL, aucflag = NULL, auclast = NULL, c_last = NULL, spanratio = NULL, kel = NULL){
  if(is.null(conc) && is.null(time)){
    stop("Error in auc_inf_o: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in auc_inf_o: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in auc_inf_o: 'time' vectors is NULL")
  } else if(all(is.na(time))) { # 2019-09-11/TGT/
      return(NA)
  } else if(all(is.na(conc))) { # 2019-09-11/TGT/
      return(NA)
  }

  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in auc_inf_o: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in auc_inf_o: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc)){
    stop("Error in auc_inf_o: length of 'time' and 'conc' vectors are not equal")
  }
  if((method != 1 && method != 2 && method != 3 && method != 4) || (is.null(method) || is.na(method))){
    stop("Error in auc_inf_o: the value provided for 'method' is not correct")
  }

  if(is.null(kel)){
    if(!(is.null(spanratio))){
      kel <- kel(conc = conc, time = time, exflag = kelflag, spanratio = spanratio)
    } else {
      kel <- kel(conc = conc, time = time, exflag = kelflag)
    }
  }
  if(is.null(c_last)){
    c_last <- clast(conc = conc, time = time)
  }
  if(sum(conc, na.rm = T) == 0){
    return(0)
  }
###
###  cat('auc_inf_o.R: time: ', time, ' conc: ', conc, ' method: ', method, ' kelflag: ', kelflag, ' aucflag: ', aucflag, ' auclast: ', auclast, ' c_last: ', c_last, ' spanratio: ', spanratio, ' kel: ', kel, '\n')
###  cat(names(kel),'\n')

  if(is.na(kel[['KEL']])){
    auc_info <- NA
    return(auc_info)
  } else {
    auc_obs <- c_last/kel[['KEL']]
    if(is.null(auclast)){
      auclast <- auc_last(conc = conc, time = time, method = method, exflag = aucflag)
    }
    auc_info <- auclast + auc_obs
###cat('auc_info_o.R: auc_info: ', auc_info, '\n')
    if(auc_info < 0 || is.na(auc_info)){
      auc_info <- NA
      return(auc_info)
    } else {
      return(auc_info)
    }
  }
}
