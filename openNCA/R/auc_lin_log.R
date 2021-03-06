#' Linear-Log Trapezoidal Area Under the Concentration-Time Curve (AUC) Calculation Method
#'
#' The Linear-Log Trapezoidal Rule is applied with this AUC method. The linear method is used
#' up to Tmax (the first occurrence of Cmax), and the log trapezoidal method is used for the
#' remainder of the profile. If Ci or Ci+1 is 0 then the linear trapezoidal rule is used.
#' 
#' @param conc The concentration data (given in a vector form)
#' @param time The time data (given in a vector form)
#' @param exflag The exclude flag data (given in a numeric vector)
#' @param t_max The first time at which CMAXi is observed within the dosing interval (numeric value)
#' 
#! @export
auc_lin_log <- function(conc = NULL, time = NULL, exflag = NULL, t_max = NULL){
  if(is.null(conc) && is.null(time)){
    stop("Error in auc_lin_log: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in auc_lin_log: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in auc_lin_log: 'time' vectors is NULL")
  } else if(all(is.na(time))) { # 2019-09-25/TGT/
      return(NA)
  } else if(all(is.na(conc))) { # 2019-09-25/TGT/
      return(NA)
  }

  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in auc_lin_log: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in auc_lin_log: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc) ){
    stop("Error in auc_lin_log: length of 'time' and 'conc' vectors are not equal")
  }
  if(!is.null(exflag)){
    if(!(is.logical(exflag) || is.numeric(exflag))){
      stop("Error in auc_lin_log: 'exflag' is not a logical vector")
    }
  }

  #Formatting data to remove any NA or less than 0 concentration values and corresponding time values
  if(!is.null(exflag)) {
    exflag <- !as.logical(exflag)
    time <- time[exflag]
    conc <- conc[exflag]
  }
  time <- time[!is.na(conc)]
  conc <- conc[!is.na(conc)]

  tmp <- data.frame(time, conc)
  if(nrow(tmp) < 2){
    auc <- NA
    return(auc)
  } else {
    if(is.null(t_max)){
      t_max <- tmax(conc = conc, time = time)
    }
    auc_df <- ""

    if(!is.na(t_max)){
      for(i in 1:(nrow(tmp)-1)){
        if(tmp$time[i+1] <= t_max || tmp$conc[i] == 0 || tmp$conc[i+1] == 0 || tmp$conc[i] == tmp$conc[i+1]){
          auc_df[i] <- ((tmp$conc[i] + tmp$conc[i+1])/2)*(tmp$time[i+1]-tmp$time[i])
        } else {
          tmp_ln <- tmp$conc[i]/tmp$conc[i+1]
          #print(paste0("tmp_ln:", tmp_ln))
          auc_df[i] <- ((tmp$conc[i] - tmp$conc[i+1])/log(tmp_ln))*(tmp$time[i+1]-tmp$time[i])
        }
      }
    } else {
      stop("Error in auc_lin_log: 'tmax' is NA")
    }
    auc_df <- as.numeric(auc_df)
    auc <- sum(auc_df, na.rm = TRUE)
  }
  return(auc)
}
