#' Linear Up-Log Down Trapezoidal Area Under the Concentration-Time Curve (AUC) Calculation Method
#'
#' Linear trapezoidal while concentrations are increasing, and log trapezoidal while
#' concentrations are decreasing; the assessment is made on a step by step basis for
#' each portion of the profile i.e. t1 to t2. If Ci or Ci+1 is 0 then the linear trapezoidal
#' rule is used.
#' 
#' @param conc The concentration data (given in a vector form)
#' @param time The time data (given in a vector form)
#' @param exflag The exclude flag data (given in a numeric vector)
#'
#! @export
auc_lin_up_log_down <- function(conc = NULL, time = NULL, exflag = NULL){
  if(is.null(conc) && is.null(time)){
    stop("Error in auc_lin_up_log_down: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in auc_lin_up_log_down: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in auc_lin_up_log_down: 'time' vectors is NULL")
  } else if(all(is.na(time))) { # 2019-09-25/TGT/
      return(NA)
  } else if(all(is.na(conc))) { # 2019-09-25/TGT/
      return(NA)
  }

  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in auc_lin_up_log_down: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in auc_lin_up_log_down: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc) ){
    stop("Error in auc_lin_up_log_down: length of 'time' and 'conc' vectors are not equal")
  }
  if(!is.null(exflag)){
    if(!(is.logical(exflag) || is.numeric(exflag))){
      stop("Error in auc_lin_up_log_down: 'exflag' is not a logical vector")
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
    auc_df <- ""

    for(i in 1:(nrow(tmp)-1)){
      curr_c <- as.numeric(conc[i])
      next_c <- as.numeric(conc[i+1])
      if(next_c >= curr_c){
        auc_df[i] <- ((conc[i] + conc[i+1])/2)*(time[i+1]-time[i])
      } else {
        if(conc[i] == 0 || conc[i+1] == 0){
          auc_df[i] <- ((conc[i] + conc[i+1])/2)*(time[i+1]-time[i])
        } else {
          tmp_ln <- conc[i]/conc[i+1]
          #print(paste0("tmp_ln:", tmp_ln))
          auc_df[i] <- ((conc[i] - conc[i+1])/log(tmp_ln))*(time[i+1]-time[i])
        }
      }
    }
    auc_df <- as.numeric(auc_df)
    auc <- sum(auc_df, na.rm = TRUE)
  }
  return(auc)
}
