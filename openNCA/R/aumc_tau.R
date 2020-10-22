#' Area under the concentration versus time curve from time 0 until the dosing interval i
#'
#' This function gets the area under the concentration versus time curve from time 0 until the dosing
#' interval i.
#'
#' @details
#' \strong{Linear Method} \cr
#' \figure{aumc_1.png} \cr
#' \strong{Log Method} \cr
#' \figure{aumc_2.png} \cr
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
#' The inputs 'exflag', 't_max', 'orig_conc' and 'orig_time' are optional. If 't_max' input is not provided then it will generate the TMAXi using the concentration and time data provided. \cr
#' If 'orig_conc' input is optional and is needed to interpolate missing data points. If 'orig_time' input is optional and is needed to interpolate missing data points. \cr
#' \strong{tmax}: Refer to \code{\link{tmax}} for more details
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
#' @param exflag The exclude flag data (given in a numeric vector)
#' @param told The time duration of last dose (numeric value)
#' @param tau The time duration of dosing interval (numeric value)
#' @param t_max The first time at which CMAXi us observed within a dosing interval (numeric value)s
#' @param orig_conc The original (full) concentration data (given in a numeric vector)
#' @param orig_time The original (full) time data (given in a numeric vector)
#' @param last_crit_factor The criteria value for last time acceptance criteria (numeric value)
#' @param kel The terminal phase rate constant for the concentration-time profile of interest (numeric value)
#' @param aumclast The area under the concentration versus time curve from zero time until the time (TLAST) of the last measurable concentration (CLASTi) during the ith dosing interval (numeric value)
#' @param lasttime The time of the last measured concentration point of the profile (numeric value)
#' @param orgtime The original time value from the map data ('nominal' or 'actual')
#' @param nom_time The nominal time data (given in a vector form)
#' @param ctoldest The estimated concentration at the time of last dose (numeric value)
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
#' #aumc_tau()
#' #Error in aumc_tau: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- c(2.89, 2.49, 2.47, 2.38, 2.32, 2.28)
#' time_vector <- c(0, 1, 2, 3, 4, 5)
#' tau_val <- 5
#' told_val <- 0
#'
#' aumc_tau(conc = conc_vector, time = time_vector, tau = tau_val, told = told_val)
#' #29.64777
#'
#' tau_val <- 2
#'
#' #aumc_tau(conc = conc_vector, time = time_vector, tau = tau_val, told = told_val)
#' #Error in aumc_tau(conc = conc_vector, time = time_vector, tau = tau_val) :
#' #  Error in aumc_tau: 'orig_conc' and 'orig_time' vectors are NULL, 
#' #  cannot interpolate data if original data is not provided!
#'
#' aumc_tau(conc = conc_vector, time = time_vector, tau = tau_val, 
#'          orig_conc = conc_vector, orig_time = time_vector, told = told_val)
#' #29.64777
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
#' told_val <- 0
#'
#' aumc_tau(conc = conc_vector, time = time_vector, tau = tau_val, told = told_val)
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
#' told_val <- 0
#'
#' aumc_tau(conc = conc_vector, time = time_vector, tau = tau_val, told = told_val)
#' #2.57
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
aumc_tau <- function(conc = NULL, time = NULL, method = 1, exflag = NULL, told = NULL, tau = NULL, t_max = NULL, orig_conc = NULL, orig_time = NULL, last_crit_factor = NULL, kel = NULL, aumclast = NULL, lasttime = NULL, orgtime = "nominal", nom_time = NULL, ctoldest = NULL){
  if(is.null(conc) && is.null(time)){
    stop("Error in aumc_tau: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in aumc_tau: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in aumc_tau: 'time' vectors is NULL")
  } else if(is.null(told)){
    stop("Error in aumc_tau: 'told' value is NULL")
  } else if(is.null(tau)){
    stop("Error in aumc_tau: 'tau' value is NULL")
  } else if(all(is.na(time))) { # 2019-09-11/TGT/
      return(NA)
  } else if(all(is.na(conc))) { # 2019-09-11/TGT/
      return(NA)
  }

  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in aumc_tau: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in aumc_tau: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc)){
    stop("Error in aumc_tau: length of 'time' and 'conc' vectors are not equal")
  }
  if(method != 1 && method != 2 && method != 3 && method != 4){
    stop("Error in aumc_tau: the value provided for 'method' is not correct")
  }
  if(!(is.numeric(told) && is.vector(told)) && !is.na(told)){
    stop("Error in aumc_tau: 'told' is not a numeric vector")
  }
  if(!(is.numeric(tau) && is.vector(tau)) && !is.na(tau)){
    stop("Error in aumc_tau: 'tau' is not a numeric vector")
  }
  if(tolower(orgtime) != "nominal" && tolower(orgtime) != "actual"){
    stop("Error in aumc_tau: 'orgtime' is not 'nominal' or 'actual'")
  }
  if(length(time) == 0 || length(conc) == 0){
    return(NA)
  }
###  cat('aumc_tau.R: told: ', told, ' tau: ', tau, ' time: ', time, ' nomtime: ', nom_time,' conc: ', conc, ' method: ', method, ' exflag: ', exflag, ' t_max: ', t_max, ' orig_conc: ', orig_conc, ' orig_time: ', orig_time, '\n')
  
  #Replace the time point closest to TAU if TAU is not present in the dataset
  time_min_range <- ifelse(!is.null(last_crit_factor), as.numeric(last_crit_factor) * tau, NA)
  if(tolower(orgtime) == "actual"){
    if(!isTRUE(tau %in% time)){
      if(length(which(time == tau)) > 0){
        tmp_told <- time[which(time == tau)]
        tmp_ctold <- conc[which(time == tmp_told)]
      } else if(length(which(time < tau)) > 0){
        tmp_told <- time[which(time < tau)]
        tmp_told <- ifelse(isTRUE(length(tmp_told) > 0), tmp_told[length(tmp_told)], NA)
        tmp_ctold <- conc[which(time == tmp_told)]
      } else {
        tmp_told <- tau
        tmp_ctold <- NA
      }
###      cat('aumc_tau.R: tmp_told: ', tmp_told, ' tmp_ctold: ', tmp_ctold, 'time_min_range: ', time_min_range, 'ctoldest: ', ctoldest, '\n')
      if(!is.na(tmp_ctold)){
        if(isTRUE(time_min_range <= tmp_told && tmp_told <= tau)){
          ctold <- ifelse(isTRUE(length(tmp_ctold) > 0), tmp_ctold[length(tmp_ctold)], NA)
          time[which(time == tmp_told)] <- tau
        } else {
          ctold <- ifelse(!is.null(ctoldest), as.numeric(ctoldest), NA)
          time[length(time)+1] <- tau
          conc[length(time)] <- ctold
        }
      }
    } 
  } else {
    if(length(which(time == tau)) > 0){
      tmp_told <- time[which(time == tau)]
      tmp_ctold <- conc[which(time == tmp_told)]
    } else {
      tmp_told <- tau
      tmp_ctold <- NA
    }
    if(!is.na(tmp_ctold)){
      time[which(time == tau)] <- tau
    }
  }
  #Remove any time points that are before TOLD
  tmptold <- 0
  idx <- which(time < tmptold)
  if(length(idx) > 0){
    idx <- which(time >= tmptold)
    time <- time[idx]
    conc <- conc[idx]
  }
  
###  cat('aumc_tau.R: told: ', told, ' tau: ', tau, ' time: ', time, ' nomtime: ', nom_time,' conc: ', conc, ' method: ', method, ' exflag: ', exflag, ' t_max: ', t_max, ' orig_conc: ', orig_conc, ' orig_time: ', orig_time, '\n')
  tmp_tmax <- t_max - told
  if(isTRUE(tau %in% time && time[length(time)] == tau)){
    if(method == 1){
      return(aumc_lin_log(conc = conc, time = time, exflag = exflag, t_max = tmp_tmax))
    } else if(method == 2){
      return(aumc_lin(conc = conc, time = time, exflag = exflag))
    } else if(method == 3){
      return(aumc_log(conc = conc, time = time, exflag = exflag))
    } else if(method == 4){
      return(aumc_lin_up_log_down(conc = conc, time = time, exflag = exflag))
    }
  } else {
    if(is.null(orig_conc) && is.null(orig_time)){
      stop("Error in aumc_tau: 'orig_conc' and 'orig_time' vectors are NULL, cannot interpolate data if original data is not provided!")
    } else if(is.null(orig_conc)) {
      stop("Error in aumc_tau: 'orig_conc' vector is NULL, cannot interpolate data if original conc is not provided!")
    } else if(is.null(orig_time)) {
      stop("Error in aumc_tau: 'orig_time' vectors is NULL, cannot interpolate data if original time is not provided!")
    }

    if(!(is.numeric(orig_conc) && is.vector(orig_conc)) ){
      stop("Error in aumc_tau: 'orig_conc' is not a numeric vector")
    }
    if(!(is.numeric(orig_time) && is.vector(orig_time)) ){
      stop("Error in aumc_tau: 'orig_time' is not a numeric vector")
    }
    if(length(orig_time) != length(orig_conc)){
      stop("Error in aumc_tau: length of 'orig_time' and 'orig_conc' vectors are not equal")
    }

    curr_tau <- as.numeric(told + tau)
    if(isTRUE(curr_tau < orig_time[length(orig_time)])){
      idx <- which(orig_time < curr_tau)
      idx <- idx[length(idx)]
      time_1 <- orig_time[idx]
      conc_1 <- orig_conc[idx]
      time_2 <- orig_time[idx+1]
      conc_2 <- orig_conc[idx+1]
      
###      cat('aumc_tau.R: time_1: ', time_1,' conc_1: ', conc_1, ' time_2: ', time_2,' conc_2: ', conc_2, '\n')
      
      tau_conc <- NA
      if(method == 1){
        if(is.null(t_max)){
          tmp_tmax <- tmax(conc = conc, time = time)
        }
        if(!is.na(tmp_tmax) & tau <= tmp_tmax){
          tau_conc <- interpolate_lin(conc1 = conc_1, time1 = time_1, conc2 = conc_2, time2 = time_2, est_time = curr_tau)
        } else {
          tau_conc <- interpolate_log(conc1 = conc_1, time1 = time_1, conc2 = conc_2, time2 = time_2, est_time = curr_tau)
        }
      } else if(method == 2){
        tau_conc <- interpolate_lin(conc1 = conc_1, time1 = time_1, conc2 = conc_2, time2 = time_2, est_time = curr_tau)
      } else if(method == 3){
        tau_conc <- interpolate_log(conc1 = conc_1, time1 = time_1, conc2 = conc_2, time2 = time_2, est_time = curr_tau)
      } else if(method == 4){
        if(time_1 <= time_2){
          tau_conc <- interpolate_lin(conc1 = conc_1, time1 = time_1, conc2 = conc_2, time2 = time_2, est_time = curr_tau)
        } else {
          tau_conc <- interpolate_log(conc1 = conc_1, time1 = time_1, conc2 = conc_2, time2 = time_2, est_time = curr_tau)
        }
      }
      tmp_df <- data.frame(conc = conc, time = time)
      tmp_df <- tmp_df[tmp_df$time < tau,]
      tmp_df[nrow(tmp_df),] <- c(tau_conc, tau)
      new_conc <- tmp_df$conc
      new_time <- tmp_df$time
      
###      cat('new_conc: ', new_conc, '\n')
###      cat('new_time: ', new_time, '\n')

      if(method == 1){
        return(aumc_lin_log(conc = new_conc, time = new_time, exflag = exflag, t_max = tmp_tmax))
      } else if(method == 2){
        return(aumc_lin(conc = new_conc, time = new_time, exflag = exflag))
      } else if(method == 3){
        return(aumc_log(conc = new_conc, time = new_time, exflag = exflag))
      } else if(method == 4){
        return(aumc_lin_up_log_down(conc = new_conc, time = new_time, exflag = exflag))
      }
    } else {
###      cat('last_crit_factor: ', last_crit_factor, '\n')
      time_min_range <- ifelse(!is.null(last_crit_factor), as.numeric(last_crit_factor) * curr_tau, NA)
###      cat('time_min_range: ', time_min_range, '\n')
      aumctau <- NA
      if(!is.na(time_min_range)){
        if(!is.null(kel) && "KEL" %in% names(kel) && "KELC0" %in% names(kel)){
          if(!is.na(kel[["KEL"]])){
            tau_conc <- NA
            tau_conc <- cest(conc = conc, time = time, t_last = tau, kel = kel[["KEL"]], kelc0 = kel[["KELC0"]])
            new_time <- c(time, tau)
            new_conc <- c(conc, tau_conc)
            
###            cat('new_conc: ', new_conc, '\n')
###            cat('new_time: ', new_time, '\n')
            
            if(method == 1){
              aumctau <- aumc_lin_log(conc = new_conc, time = new_time, exflag = exflag, t_max = tmp_tmax)
            } else if(method == 2){
              aumctau <- aumc_lin(conc = new_conc, time = new_time, exflag = exflag)
            } else if(method == 3){
              aumctau <- aumc_log(conc = new_conc, time = new_time, exflag = exflag)
            } else if(method == 4){
              aumctau <- aumc_lin_up_log_down(conc = new_conc, time = new_time, exflag = exflag)
            }
###            cat('aumctau: ', aumctau, '\n')
          } else if(!is.null(aumclast)){
###            cat('aumclast: ', aumclast, '\n')
            aumctau <- aumclast
          }
        }
        if(isTRUE(time_min_range <= lasttime)){
          return(aumctau)
        } else {
          aumctau <- ifelse(!is.null(aumclast), ifelse(isTRUE(aumctau <= (as.numeric(aumclast) * 1.20)), aumctau, NA), NA)
        }
      }
      return(aumctau)
    }
  }
}
