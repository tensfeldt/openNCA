#' Partial Area under the concentration (AUC) versus time cruve from time T1 until time T2
#'
#' This function gets the area under the concentration versus time curve from time T1 untill time T2, where
#' T1 and T2 are user-specified.
#'
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
#' Please note that if T2 is less than or equal to T1 then AUCT1_T2 will return NA. If all the concentrations are 0's then AUCT1_T2 will return NA.
#' For concentration corresponding to time T1 or T2 is not available then AUCT1_T2 will return NA. \cr
#' The inputs 'exflag' and 't_max' are optional. If 't_max' input is not provided then it will generate the TMAXi using the concentration and time data provided.\cr
#' \strong{tmax}: Refer to \code{\link{tmax}} for more details \cr
#'
#' @param conc The concentration data (given in a vector form)
#' @param time The time data (given in a vector form)
#' @param t1 The start time that will be used to calculate AUC
#' @param t2 The end time that will be used to calculate AUC
#' @param method The method that will be used to calculate AUC (use either 1, 2, 3, or 4)\cr
#' \enumerate{
#' \item Linear-Log Trapazoidal Rule (default)
#' \item Linear Trapazoidal Rule
#' \item Log Trapazoidal Rule
#' \item Linear Up - Log DownTrapazoidal Rule
#' }
#' Note: check 'Methods' section below for more details \cr
#' @param exflag The exclude flag data (given in a numeric vector)
#' @param t_max The first time at which CMAXi is observed within the dosing interval (numeric value)
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
#'  \item AUC: area under the curve from times provided by user
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
#' auc_t1_t2()
#' #Error in auc_t1_t2: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- c(2.89, 2.49, 2.47, 2.38, 2.32, 2.28)
#' time_vector <- c(0, 1, 2, 3, 4, 5)
#'
#' auc_t1_t2(conc = conc_vector, time = time_vector, t1 = 1)
#' #NA
#'
#' auc_t1_t2(conc = conc_vector, time = time_vector, t1 = 1, t2 = 4)
#' #7.254581
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
#' auc_t1_t1(conc = conc_vector, time = time_vector, t1 = 0, t2 = 2)
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
#' #################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(1.19, 1.23, 1.34)
#' time_vector <- c(0, 1, 2)
#'
#' auc_t1_t2(conc = conc_vector, time = time_vector, t1 = 0, t2 = 1)
#' #1.21
#'
#' auc_t1_t2(conc, time, t1 = 1, t2 = 1)
#' #Error in auc_t1_t2(conc = conc_vector, time = time_vector, t1 = 1, t2 = 1) :
#' #  Error in auc_t1_t2: 't1' value is greater than 't2' value
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
auc_t1_t2 <- function(conc = NULL, time = NULL, t1 = NULL, t2 = NULL, method = 1, exflag = NULL, t_max = NULL, dose_time = NULL, interpolate = NULL, model = NULL, dosing_type = NULL, told = NULL, orig_conc = NULL, orig_time = NULL){
  if(is.null(conc) && is.null(time)){
    stop("Error in auc_t1_t2: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in auc_t1_t2: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in auc_t1_t2: 'time' vectors is NULL")
  } else if(all(is.na(time))) { # 2019-09-11/TGT/
      return(NA)
  } else if(all(is.na(conc))) { # 2019-09-11/TGT/
      return(NA)
  } else if(is.na(t_max) && method==1) { # 2019-09-11/TGT/ - update 2019-09-25/TGT/ updated to also consider method
      return(NA)
  }

  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in auc_t1_t2: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in auc_t1_t2: 'time' is not a numeric vector")
  }
  if(is.null(t1) || is.na(t1)){
    return(NA)
  }
  if(is.null(t2) || is.na(t2)){
    return(NA)
  }
  if(!is.null(t_max) && !(t_max %in% time)){
    stop("Error in auc_t1_t2: 't_max' value is not in the 'time' data")
  }
  if(as.numeric(t1) >= as.numeric(t2)){
    stop("Error in auc_t1_t2: 't1' value is greater than 't2' value")
  }
  if(method != 1 && method != 2 && method != 3 && method != 4){
    stop("Error in auc_t1_t2: the value provided for 'method' is not correct")
  }
  if(!is.null(exflag)){
    if(!(is.logical(exflag) || is.numeric(exflag))){
      stop("Error in auc_t1_t2: 'exflag' is not a logical vector")
    }
  }
  if(!is.null(dose_time)){
    if(!(is.numeric(dose_time))){
      stop("Error in auc_t1_t2: 'dose_time' is not a numeric value")
    } else {
      if(t1 < dose_time){
        return(NA)
      }
    }
  }

  #Formatting data to subset data based on time values
  if(!is.null(exflag)){
    exflag <- exflag[time >= t1 & time <= t2]
  }
  conc <- conc[time >= t1 & time <= t2]
  time <- time[time >= t1 & time <= t2]

  if(length(time) != length(conc)){
    stop("Error in auc_t1_t2: length of 'time' and 'conc' vectors are not equal")
  }
  if(sum(conc, na.rm = T) == 0){
    return(0)
  }

  if(method == 1){
    return(auc_lin_log(conc = conc, time = time, exflag = exflag, t_max = t_max, interpolate = interpolate, model = model, dosing_type = dosing_type, told = told, orig_conc = orig_conc, orig_time = orig_time))
  } else if(method == 2){
    return(auc_lin(conc = conc, time = time, exflag = exflag, interpolate = interpolate, model = model, dosing_type = dosing_type, told = told, orig_conc = orig_conc, orig_time = orig_time))
  } else if(method == 3){
    return(auc_log(conc = conc, time = time, exflag = exflag, interpolate = interpolate, model = model, dosing_type = dosing_type, told = told, orig_conc = orig_conc, orig_time = orig_time))
  } else if(method == 4){
    return(auc_lin_up_log_down(conc = conc, time = time, exflag = exflag, interpolate = interpolate, model = model, dosing_type = dosing_type, told = told, orig_conc = orig_conc, orig_time = orig_time))
  }
}
