#' Area under the concentration versus time cruve from time 0 until the last time point
#'
#' This function gets the area under the concentration versus time curve from time 0 until the last
#' time point. As illustrated in the following figure, AUC_ALL includes the trapezoidal area from the
#' time of the last measurable concentration to the next time point. Although there may be additional
#' time points, there is no additonal AUC since by defination all subsequent concentrations are zero.\cr
#' \figure{auc_all.png}
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
#' The inputs 'exflag', 't_max', 'orig_conc' and 'orig_time' are optional. If 't_max' input is not provided then it will generate the TMAXi using the concentration and time data provided. \cr
#' If 'orig_conc' input is optional and is needed to interpolate missing data points. If 'orig_time' input is optional and is needed to interpolate missing data points.\cr
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
#' @param tau The time duration of dosing interval (numeric value)
#' @param t_max The first time at which CMAXi us observed within a dosing interval (numeric value)s
#' @param orig_conc The original (full) concentration data (given in a numeric vector)
#' @param orig_time The original (full) time data (given in a numeric vector)
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
#' auc_tau()
#' #Error in auc_tau: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- c(2.89, 2.49, 2.47, 2.38, 2.32, 2.28)
#' time_vector <- c(0, 1, 2, 3, 4, 5)
#' tau_val <- 5
#'
#' auc_tau(conc = conc_vector, time = time_vector, tau = tau_val)
#' #12.23956
#'
#' tau_val <- 2
#'
#' auc_tau(conc = conc_vector, time = time_vector, tau = tau_val)
#' #Error in auc_tau(conc = conc_vector, time = time_vector, tau = tau_val) :
#' #  Error in auc_tau: 'orig_conc' and 'orig_time' vectors are NULL, cannot interpolate data if original data is not provided!
#'
#' auc_tau(conc = conc_vector, time = time_vector, tau = tau_val, orig_conc = conc_vector,
#'         orig_time = time_vector)
#' #12.23956
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
#'
#' auc_tau(conc = conc_vector, time = time_vector, tau = tau_val)
#' #0
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
#'
#' auc_tau(conc = conc_vector, time = time_vector, tau = tau_val)
#' #2.495
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
auc_tau <- function(conc = NULL, time = NULL, method = 1, exflag = NULL, tau = NULL, t_max = NULL, orig_conc = NULL, orig_time = NULL){
  if(is.null(conc) && is.null(time)){
    stop("Error in auc_tau: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in auc_tau: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in auc_tau: 'time' vectors is NULL")
  } else if(is.null(tau)){
    stop("Error in auc_tau: 'tau' value is NULL")
  } else if(all(is.na(time))) { # 2019-09-11/TGT/
      return(NA)
  } else if(all(is.na(conc))) { # 2019-09-11/TGT/
      return(NA)
  }

  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in auc_tau: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in auc_tau: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc)){
    stop("Error in auc_tau: length of 'time' and 'conc' vectors are not equal")
  }
  if(method != 1 && method != 2 && method != 3 && method != 4){
    stop("Error in auc_tau: the value provided for 'method' is not correct")
  }
  if(sum(conc, na.rm = T) == 0){
    return(0)
  }
###
###cat('auc_tau.R: tau: ', tau, ' time: ', time, ' conc: ', conc, ' method: ', method, ' exflag: ', exflag, ' t_max: ', t_max, ' orig_conc: ', orig_conc, ' orig_time: ', orig_time, '\n')
  
  if(tau %in% time && time[length(time)] == tau){
    if(method == 1){
      return(auc_lin_log(conc = conc, time = time, exflag = exflag, t_max = t_max))
    } else if(method == 2){
      return(auc_lin(conc = conc, time = time, exflag = exflag))
    } else if(method == 3){
      return(auc_log(conc = conc, time = time, exflag = exflag))
    } else if(method == 4){
      return(auc_lin_up_log_down(conc = conc, time = time, exflag = exflag))
    }
  } else {
    if(is.null(orig_conc) && is.null(orig_time)){
      stop("Error in auc_tau: 'orig_conc' and 'orig_time' vectors are NULL, cannot interpolate data if original data is not provided!")
    } else if(is.null(orig_conc)) {
      stop("Error in auc_tau: 'orig_conc' vector is NULL, cannot interpolate data if original conc is not provided!")
    } else if(is.null(orig_time)) {
      stop("Error in auc_tau: 'orig_time' vectors is NULL, cannot interpolate data if original time is not provided!")
    }

    if(!(is.numeric(orig_conc) && is.vector(orig_conc)) ){
      stop("Error in auc_tau: 'orig_conc' is not a numeric vector")
    }
    if(!(is.numeric(orig_time) && is.vector(orig_time)) ){
      stop("Error in auc_tau: 'orig_time' is not a numeric vector")
    }
    if(length(orig_time) != length(orig_conc)){
      stop("Error in auc_tau: length of 'orig_time' and 'orig_conc' vectors are not equal")
    }

### 2019-08-28/TGT/ There doesn't appear to be any implement extrapolation method for auc_tau.R if tau>max(time)
    if(tau < orig_time[length(orig_time)]){
      time_1 <- time[length(time)]
      conc_1 <- conc[length(conc)]
      time_2 <- orig_time[!orig_time < tau][1]
      conc_2 <- orig_conc[orig_time %in% time_2]

###
###      cat('auc_tau.R: t_max: ', t_max,' class(tau): ', class(tau), ' tau: ', tau, '\n')

      tau_conc <- NA
      if(method == 1){
        if(is.null(t_max)){
          t_max <- tmax(conc = conc, time = time)
        }
        if(!is.na(t_max) & tau <= t_max){
          tau_conc <- interpolate_lin(conc1 = conc_1, time1 = time_1, conc2 = conc_2, time2 = time_2, est_time = tau)
        } else {
          tau_conc <- interpolate_log(conc1 = conc_1, time1 = time_1, conc2 = conc_2, time2 = time_2, est_time = tau)
        }
      } else if(method == 2){
        tau_conc <- interpolate_lin(conc1 = conc_1, time1 = time_1, conc2 = conc_2, time2 = time_2, est_time = tau)
      } else if(method == 3){
        tau_conc <- interpolate_log(conc1 = conc_1, time1 = time_1, conc2 = conc_2, time2 = time_2, est_time = tau)
      } else if(method == 4){
        if(time_1 <= time_2){
          tau_conc <- interpolate_lin(conc1 = conc_1, time1 = time_1, conc2 = conc_2, time2 = time_2, est_time = tau)
        } else {
          tau_conc <- interpolate_log(conc1 = conc_1, time1 = time_1, conc2 = conc_2, time2 = time_2, est_time = tau)
        }
      }
      new_time <- c(time, tau)
      new_time <- sort(new_time)
      index <- sum(new_time <= tau)
      if(index > length(conc)){
        new_conc <- c(conc[1:(index-1)], tau_conc)
      } else {
        new_conc <- c(conc[1:(index-1)], tau_conc, conc[index:length(conc)])
      }
      if(sum(new_conc, na.rm = T) == 0){
        return(0)
      }
###      cat('new_conc: ', new_conc, '\n')
      
      if(method == 1){
        return(auc_lin_log(conc = new_conc, time = new_time, exflag = exflag, t_max = t_max))
      } else if(method == 2){
        return(auc_lin(conc = new_conc, time = new_time, exflag = exflag))
      } else if(method == 3){
        return(auc_log(conc = new_conc, time = new_time, exflag = exflag))
      } else if(method == 4){
        return(auc_lin_up_log_down(conc = new_conc, time = new_time, exflag = exflag))
      }
    }
  }
}
