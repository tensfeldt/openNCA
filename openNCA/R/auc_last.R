#' Area under the concentration versus time cruve from time 0 to time of the last mesurable concentration
#'
#' This function gets the area under the concentration versus time curve from time 0 to time (TLAST) of the
#' last mesuarable concentraation (CLAST). As illustrated in the following figure, AUC_LAST includes the
#' trapezoidal area upto the time of the last measurable concentration. Although there may be additional
#' time points, there is no additonal AUC since by defination all subsequent concentrations are zero.\cr
#' \figure{auc_last.png}
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
#' The inputs 'exflag', 't_last' and 't_max' are optional. If 't_last' input is not provided then it will generate the TLAST using the concentration and time data provided. \cr
#' If 't_max' input is not provided then it will generate the TMAXi using the concentration and time data provided. \cr \cr
#' \strong{tlast}: Refer to \code{\link{tlast}} for more details \cr
#' \strong{tmax}: Refer to \code{\link{tmax}} for more details \cr
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
#' @param t_last The time of last measureable (non-zero) plasma concentration (numeric value)
#' @param t_max The first time at which CMAXi is observed within the dosing interval (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item AUCLAST: area under the curve until time of last mesurable concentration
#' }
#'
#' @examples
#' ##########
#' ## Data ##
#' #############################################
#' ##  SID  ##  TIME  ##   CONC   ##  EXFLAG  ##
#' #############################################
#' ##   30  ##    0   ##   2.89   ##    0     ##
#' ##   30  ##    1   ##   2.49   ##    1     ##
#' ##   30  ##    2   ##   2.47   ##    0     ##
#' ##   30  ##    3   ##   2.38   ##    0     ##
#' ##   30  ##    4   ##   2.32   ##    0     ##
#' ##   30  ##    5   ##   2.28   ##    1     ##
#' ##   30  ##    6   ##   0      ##    1     ##
#' #############################################
#'
#' #Data mentioned will be used for the following example
#'
#' auc_last()
#' #Error in auc_last: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- c(2.89, 2.49, 2.47, 2.38, 2.32, 2.28, 0)
#' time_vector <- c(0, 1, 2, 3, 4, 5, 6)
#' exflag_vector <- c(0, 1, 0, 0, 0, 1, 1)
#'
#' auc_last(conc = conc_vector, time = time_vector, method = NA)
#' #Error in auc_last: the value provided for 'method' is not correct
#'
#' auc_last(conc = conc_vector, time = time_vector, method = 2)
#' #12.245
#'
#' auc_last(conc = conc_vector, time = time_vector, method = 1)
#' #12.23956
#'
#' auc_last(conc = conc_vector, time = time_vector, method = 1, exflag = exflag_vector)
#' #10.12361
#'
#' auc_last(conc = conc_vector, time = time_vector, method = 1, t_max = 2)
#' #12.24454
#'
#' ############
#' ## Data 2 ##
#' #############################################
#' ##  SID  ##  TIME  ##   CONC   ##  EXFLAG  ##
#' #############################################
#' ##   31  ##    0   ##      0   ##    0     ##
#' ##   31  ##    1   ##      0   ##    0     ##
#' ##   31  ##    2   ##      0   ##    0     ##
#' #############################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(0, 0, 0)
#' time_vector <- c(0, 1, 2)
#' exflag_vector <- c(0, 0, 0)
#'
#' auc_last(conc = conc_vector, time = time_vector, method = 1)
#' #0
#'
#' ############
#' ## Data 3 ##
#' #############################################
#' ##  SID  ##  TIME  ##   CONC   ##  EXFLAG  ##
#' #############################################
#' ##   32  ##    0   ##   1.19   ##    0     ##
#' ##   32  ##    1   ##   1.23   ##    1     ##
#' ##   32  ##    2   ##   1.34   ##    1     ##
#' #############################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(1.19, 1.23, 1.34)
#' time_vector <- c(0, 1, 2)
#' exflag_vector <- c(0, 1, 1)
#'
#' auc_last(conc = conc_vector, time = time_vector)
#' #2.495
#'
#' auc_last(conc = conc_vector, time = time_vector, exflag = exflag_vector)
#' #NA
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
auc_last <- function(conc = NULL, time = NULL, method = 1, exflag = NULL, t_last = NULL, t_max = NULL){
  if(is.null(conc) && is.null(time)){
    stop("Error in auc_last: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in auc_last: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in auc_last: 'time' vectors is NULL")
  } else if(all(is.na(time))) { # 2019-09-13/TGT/
      return(NA)
  } else if(all(is.na(conc))) { # 2019-09-13/TGT/
      return(NA)
  }

  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in auc_last: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in auc_last: 'time' is not a numeric vector")
  }
  if((method != 1 && method != 2 && method != 3 && method != 4) || (is.null(method) || is.na(method))){
    stop("Error in auc_last: the value provided for 'method' is not correct")
  }

### 2019-09-11/TGT/ return NA if all conc is NA or all time is NA
  if(length(time) == 0 || length(conc) == 0 || all(is.na(time)) || all(is.na(conc))) { 
    return(NA)
  }

  #Formatting data to subset data based on time values
  if(is.null(t_last)){
    t_last <- tlast(conc = conc, time = time)
  }

### 2019-09-11/TGT/ return NA if t_last is NA
  if(is.na(t_last)) { return(NA) }
  
  if(!(t_last %in% time)){
    stop("Error in auc_last: 't_last' value is not in the 'time' data")
  }

  conc <- conc[time <= t_last]
  time <- time[time <= t_last]

  if(length(time) != length(conc)){
    stop("Error in auc_last: length of 'time' and 'conc' vectors are not equal")
  }
  if(sum(conc, na.rm = T) == 0){
    return(0)
  }

  if(method == 1){
    return(auc_lin_log(conc = conc, time = time, exflag = exflag, t_max = t_max))
  } else if(method == 2){
    return(auc_lin(conc = conc, time = time, exflag = exflag))
  } else if(method == 3){
    return(auc_log(conc = conc, time = time, exflag = exflag))
  } else if(method == 4){
    return(auc_lin_up_log_down(conc = conc, time = time, exflag = exflag))
  }
}
