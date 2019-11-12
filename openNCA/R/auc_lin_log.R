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
#' @param interpolate The value to determine whether to interpolate data points (given in a logical form)
#' @param extrapolate The value to determine whether to extrapolate data points (given in a logical form)
#' @param model The model specification (either 'M1', 'M2', 'M3', or 'M4')
#' @param dosing_type The dosing type specification (either 'SD' or 'SS')
#' @param told The time of last dose (given in a numeric value)
#' @param orig_conc The original (full) concentration data (given in a numeric vector)
#' @param orig_time The original (full) time data (given in a numeric vector)
#' 
#! @export
auc_lin_log <- function(conc = NULL, time = NULL, exflag = NULL, t_max = NULL, interpolate = NULL, extrapolate = NULL, model = NULL, dosing_type = NULL, told = NULL, orig_conc = NULL, orig_time = NULL){
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
##  2019-11-07/RD Added for Interpolation to account for error handling
##
  if(!is.null(interpolate) && !is.logical(interpolate)){
    stop("Error in auc_lin_log: 'interpolate' is not a logical value")
  }
  if(!is.null(model) && (model != "M1" && model != "M2" && model != "M3" && model != "M4")){
    stop("Error in auc_lin_log: 'model' is not either 'M1', 'M2', 'M3' or 'M4'")
  }
  if(!is.null(dosing_type) && (dosing_type != "SD" && dosing_type != "SS")){
    stop("Error in auc_lin_log: 'dosing_type' is not either 'SD' or 'SS'")
  }
  if(!is.null(told) && !is.numeric(told)){
    stop("Error in auc_lin_log: 'told' is not a numeric value")
  }
  if(!is.null(interpolate) && is.null(model)){
    stop("Error in auc_lin_log: 'model' is NULL")
  }
  if(!is.null(interpolate) && is.null(dosing_type)){
    stop("Error in auc_lin_log: 'dosing_type' is NULL")
  }
  if(!is.null(interpolate) && is.null(told)){
    stop("Error in auc_lin_log: 'told' is NULL")
  }
  if(!is.null(interpolate) && (is.null(orig_conc) || is.null(orig_time))){
    if(is.null(orig_conc) && is.null(orig_time)){
      stop("Error in auc_lin_log: 'orig_conc' and 'orig_time' vectors are NULL")
    } else if(is.null(orig_conc)) {
      stop("Error in auc_lin_log: 'orig_conc' vector is NULL")
    } else if(is.null(orig_time)) {
      stop("Error in auc_lin_log: 'orig_time' vectors is NULL")
    }
  }
  if((!is.null(orig_conc)) && (!(is.numeric(orig_conc) && is.vector(orig_conc)))){
    stop("Error in auc_lin_log: 'orig_conc' is not a numeric vector")
  }
  if((!is.null(orig_time)) && (!(is.numeric(orig_time) && is.vector(orig_time)))){
    stop("Error in auc_lin_log: 'orig_time' is not a numeric vector")
  }
  if(((!is.null(orig_time)) && !is.null(orig_conc)) && (length(orig_time) != length(orig_conc))){
    stop("Error in auc_lin_log: length of 'orig_time' and 'orig_conc' vectors are not equal")
  }

  #Formatting data to remove any NA or less than 0 concentration values and corresponding time values
  if(!is.null(exflag)) {
    exflag <- !as.logical(exflag)
    time <- time[exflag]
    conc <- conc[exflag]
  }
  
  if(!isTRUE(interpolate)){
    time <- time[!is.na(conc)]
    conc <- conc[!is.na(conc)]  
  }
  
##  2019-11-07/RD Commenting this as interpolation since there is no need to remove all NA's 
##  
##  time <- time[!is.na(conc)]
##  conc <- conc[!is.na(conc)]

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
##      2019-11-11/RD Added for Interpolation/Extrapolation to check for triggers for interpolation/extrapolation
##
      if(isTRUE(interpolate) || isTRUE(extrapolate)){
##        2019-11-08/RD Added helper function for Interpolation
##
        est_tmp <- estimate_missing_concentration(conc = conc, time = time, auc_method = "LIN", model = model, dosing_type = dosing_type, told = told, orig_conc = orig_conc, orig_time = orig_time)
        conc <- est_tmp[[1]]
      }
      for(i in 1:(nrow(tmp)-1)){
        if(tmp$time[i+1] <= t_max || tmp$conc[i] == 0 || tmp$conc[i+1] == 0 || tmp$conc[i] == tmp$conc[i+1]){
          auc_df[i] <- ((tmp$conc[i] + tmp$conc[i+1])/2)*(tmp$time[i+1]-tmp$time[i])
        } else {
          tmp_ln <- tmp$conc[i]/tmp$conc[i+1]
          auc_df[i] <- ((tmp$conc[i] - tmp$conc[i+1])/log(tmp_ln))*(tmp$time[i+1]-tmp$time[i])
        }
      }
    } else {
      stop("Error in auc_lin_log: 'tmax' is NA")
    }
    auc_df <- as.numeric(auc_df)
    auc <- sum(auc_df, na.rm = TRUE)
  }
##  2019-11-11/RD Returning interpolated/extrapolated data that will be used as an output
##
  if(isTRUE(interpolate) || isTRUE(extrapolate)){
    return(list(auc, est_tmp[[2]]))
  } else {
    return(auc)
  }
##  2019-11-08/RD Commenting this as interpolation return call will replace it 
##
##  return(auc)
}
