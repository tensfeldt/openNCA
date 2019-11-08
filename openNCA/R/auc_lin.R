#' Linear Trapezoidal Area Under the Concentration-Time Curve (AUC) Calculation Method
#'
#' The AUC linear method is used for the entire profile.
#' 
#' @param conc The concentration data (given in a vector form)
#' @param time The time data (given in a vector form)
#' @param exflag The exclude flag data (given in a numeric vector)
#' @param interpolate The value to determine whether to interpolate data points (given in a logical form)
#' @param model The model specification (either 'M1', 'M2', 'M3', or 'M4')
#' @param dosing_type The dosing type specification (either 'SD' or 'SS')
#' @param told The time of last dose (given in a numeric value)
#' @param orig_conc The original (full) concentration data (given in a numeric vector)
#' @param orig_time The original (full) time data (given in a numeric vector)
#' 
#! @export
auc_lin <- function(conc = NULL, time = NULL, exflag = NULL, interpolate = NULL, model = NULL, dosing_type = NULL, told = NULL, orig_conc = NULL, orig_time = NULL){
  if(is.null(conc) && is.null(time)){
    stop("Error in auc_lin: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in auc_lin: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in auc_lin: 'time' vectors is NULL")
  } else if(all(is.na(time))) { # 2019-09-25/TGT/
      return(NA)
  } else if(all(is.na(conc))) { # 2019-09-25/TGT/
      return(NA)
  }

  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in auc_lin: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in auc_lin: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc) ){
    stop("Error in auc_lin: length of 'time' and 'conc' vectors are not equal")
  }
  if(!is.null(exflag)){
    if(!(is.logical(exflag) || is.numeric(exflag))){
      stop("Error in auc_lin: 'exflag' is not a logical vector")
    }
  }
##  2019-11-07/RD Added for Interpolation to account for error handling
##
  if(!is.null(interpolate) && !is.logical(interpolate)){
    stop("Error in auc_lin: 'interpolate' is not a logical value")
  }
  if(!is.null(model) && (model != "M1" && model != "M2" && model != "M3" && model != "M4")){
    stop("Error in auc_lin: 'model' is not either 'M1', 'M2', 'M3' or 'M4'")
  }
  if(!is.null(dosing_type) && (dosing_type != "SD" && dosing_type != "SS")){
    stop("Error in auc_lin: 'dosing_type' is not either 'SD' or 'SS'")
  }
  if(!is.null(told) && !is.numeric(told)){
    stop("Error in auc_lin: 'told' is not a numeric value")
  }
  if(!is.null(interpolate) && is.null(model)){
    stop("Error in auc_lin: 'model' is NULL")
  }
  if(!is.null(interpolate) && is.null(dosing_type)){
    stop("Error in auc_lin: 'dosing_type' is NULL")
  }
  if(!is.null(interpolate) && is.null(told)){
    stop("Error in auc_lin: 'told' is NULL")
  }
  if(!is.null(interpolate) && (is.null(orig_conc) || is.null(orig_time))){
    if(is.null(orig_conc) && is.null(orig_time)){
      stop("Error in auc_lin: 'orig_conc' and 'orig_time' vectors are NULL")
    } else if(is.null(orig_conc)) {
      stop("Error in auc_lin: 'orig_conc' vector is NULL")
    } else if(is.null(orig_time)) {
      stop("Error in auc_lin: 'orig_time' vectors is NULL")
    }
  }
  if((!is.null(orig_conc)) && (!(is.numeric(orig_conc) && is.vector(orig_conc)))){
    stop("Error in auc_lin: 'orig_conc' is not a numeric vector")
  }
  if((!is.null(orig_time)) && (!(is.numeric(orig_time) && is.vector(orig_time)))){
    stop("Error in auc_lin: 'orig_time' is not a numeric vector")
  }
  if(((!is.null(orig_time)) && !is.null(orig_conc)) && (length(orig_time) != length(orig_conc))){
    stop("Error in auc_lin: length of 'orig_time' and 'orig_conc' vectors are not equal")
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
    auc_df <- ""

    for(i in 1:(nrow(tmp)-1)){
##      2019-11-07/RD Added for Interpolation to check for triggers for interpolation
##
      if(isTRUE(interpolate)){
##        2019-11-07/RD Added for Interpolation for AUC Start time has no data
##        
        if(is.na(conc[1]) && i == 1){
          if(time[1] <= orig_time[1]){
            if(model == "M2"){
              #Extrapolate conc by log-linear regression 
            } else {
              if(dosing_type == "SD"){
                conc_s_tmp <- 0
              } else if(dosing_type == "SS"){
                conc_s_tmp <- cmin(conc = conc, time = time)
              }
            }
            if(time[1] == told){
              conc[1] <- conc_s_tmp
            } else {
              cold <- ifelse(!is.na(orig_conc[which(told == orig_time)]), orig_conc[which(told == orig_time)], NA)
              conc[1] <- interpolation_lin(conc1 = cold, time1 = told, conc2 = orig_conc[1], time2 = orig_time[1], est_time = time[1])
            }
          } else if((orig_time[1] < time[1]) && (time[1] < orig_time[length(orig_time)])){
            idx <- which(time[1] == orig_time)
            conc[1] <- interpolation_lin(conc1 = orig_conc[idx-1], time1 = orig_time[idx-1], conc2 = orig_conc[idx+1], time2 = orig_time[i+1], est_time = time[1])
          } else if(time[1] > orig_time[length(orig_time)]){
            kel_v <- kel(conc = conc, time = time)
            if(!is.na(kel_v[["KEL"]])){
              conc[1] <- cest(conc = conc, time = time)
            } else {
              conc[1] <- NA
            }
          }
##        2019-11-07/RD Added for Interpolation for AUC End time has no data
##  
        } else if(is.na(conc[nrow(tmp)]) && (i+1) == nrow(tmp)){
          if((orig_time[1] < time[nrow(tmp)]) && (time[nrow(tmp)] < orig_time[length(orig_time)])){
            idx <- which(time[nrow(tmp)] == orig_time)
            conc[nrow(tmp)] <- interpolation_lin(conc1 = orig_conc[idx-1], time1 = orig_time[idx-1], conc2 = orig_conc[idx+1], time2 = orig_time[i+1], est_time = time[nrow(tmp)])
          } else {
            kel_v <- kel(conc = conc, time = time)
            if(!is.na(kel_v[["KEL"]])){
              conc[nrow(tmp)] <- cest(conc = conc, time = time)
            } else {
              min_lim <- orig_time[length(orig_time)] - (orig_time[length(orig_time)] * 0.05)
              max_lim <- (orig_time[length(orig_time)] * 0.05) + orig_time[length(orig_time)]
              if(min_lim < time[nrow(tmp)] && time[nrow(tmp)] < max_lim){
                conc[nrow(tmp)] <- orig_conc[length(orig_conc)]
              } else {
                conc[nrow(tmp)] <- NA
              }
            }
          }
        }
      }
      auc_df[i] <- ((conc[i] + conc[i+1])/2)*(time[i+1]-time[i])
    }
    auc_df <- as.numeric(auc_df)
    auc <- sum(auc_df, na.rm = TRUE)
  }
##  2019-11-07/RD Returning interpolated data that will be used as an output
##
  if(isTRUE(interpolate)){
    return(list(auc, conc, time))
  } else {
    return(auc)
  }
##  2019-11-07/RD Commenting this as interpolation return call will replace it 
##
##  return(auc)
}
