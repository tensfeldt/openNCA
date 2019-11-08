#' Area under the concentration (AUC) versus time cruve from time 0 to time T
#'
#' This function gets the area under the concentration versus time curve from time 0 to time T for
#' all recorded times T.
#'
#' @details
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
#'  first occurance of Cmax) and the log trapezoidal method is used for the remainder of the profile. If
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
#' The input 'exflag' is optional.
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
#' @param exflag The exclude flag data (given in a numeric vector)
#' @param interpolate The value to determine whether to interpolate data points (given in a logical form)
#' @param model The model specification (either 'M1', 'M2', 'M3', or 'M4')
#' @param dosing_type The dosing type specification (either 'SD' or 'SS')
#' @param told The time of last dose (given in a numeric value)
#' @param orig_conc The original (full) concentration data (given in a numeric vector)
#' @param orig_time The original (full) time data (given in a numeric vector)
#' 
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item AUC: area under the curve for all recorded times
#' }
#'
#' @examples
#' ##########
#' ## Data ##
#' #################################
#' ##  SID  ##  TIME  ##   CONC   ##
#' #################################
#' ##   30  ##    0   ##   2.89   ##
#' ##   30  ##    1   ##   2.49   ##
#' ##   30  ##    2   ##   2.47   ##
#' ##   30  ##    3   ##   2.38   ##
#' ##   30  ##    4   ##   2.32   ##
#' ##   30  ##    5   ##   2.28   ##
#' #################################
#'
#' #Data mentioned will be used for the following example
#'
#' auc_t()
#' #Error in auc_t: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- c(2.89, 2.49, 2.47, 2.38, 2.32, 2.28)
#' time_vector <- c(0, 1, 2, 3, 4, 5)
#'
#' auc_t(conc = conc_vector, time = time_vector)
#' #12.23956
#'
#' ############
#' ## Data 2 ##
#' #################################
#' ##  SID  ##  TIME  ##   CONC   ##
#' #################################
#' ##   31  ##    0   ##      0   ##
#' ##   31  ##    1   ##      0   ##
#' ##   31  ##    2   ##      0   ##
#' #################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(0, 0, 0)
#' time_vector <- c(0, 1, 2)
#'
#' auc_t(conc = conc_vector, time = time_vector)
#' #0
#'
#' ############
#' ## Data 3 ##
#' #################################
#' ##  SID  ##  TIME  ##   CONC   ##
#' #################################
#' ##   32  ##    0   ##   1.19   ##
#' ##   32  ##    1   ##   1.23   ##
#' ##   32  ##    2   ##   1.34   ##
#' ##   32  ##    3   ##   NA     ##
#' #################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(1.19, 1.23, 1.34, NA)
#' time_vector <- c(0, 1, 2, 3)
#'
#' auc_t(conc = conc_vector, time = time_vector)
#' #NA
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
auc_t <- function(conc = NULL, time = NULL, method = 1, exflag = NULL, interpolate = NULL, model = NULL, dosing_type = NULL, told = NULL, orig_conc = NULL, orig_time = NULL){
  if(is.null(conc) && is.null(time)){
    stop("Error in auc_t: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in auc_t: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in auc_t: 'time' vectors is NULL")
  }

  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in auc_t: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in auc_t: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc)){
    stop("Error in auc_t: length of 'time' and 'conc' vectors are not equal")
  }
  if(method != 1 && method != 2 && method != 3 && method != 4){
    stop("Error in auc_t: the value provided for 'method' is not correct")
  }
  if(any(is.na(conc))){
    return(NA)
  }
  if(sum(conc, na.rm = T) == 0){
    return(0)
  }

  if(method == 1){
    return(auc_lin_log(conc = conc, time = time, exflag = exflag, interpolate = interpolate, model = model, dosing_type = dosing_type, told = told, orig_conc = orig_conc, orig_time = orig_time))
  } else if(method == 2){
    return(auc_lin(conc = conc, time = time, exflag = exflag, interpolate = interpolate, model = model, dosing_type = dosing_type, told = told, orig_conc = orig_conc, orig_time = orig_time))
  } else if(method == 3){
    return(auc_log(conc = conc, time = time, exflag = exflag, interpolate = interpolate, model = model, dosing_type = dosing_type, told = told, orig_conc = orig_conc, orig_time = orig_time))
  } else if(method == 4){
    return(auc_lin_up_log_down(conc = conc, time = time, exflag = exflag, interpolate = interpolate, model = model, dosing_type = dosing_type, told = told, orig_conc = orig_conc, orig_time = orig_time))
  }
}
