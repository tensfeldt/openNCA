#' Helper Function for Estimating Missing Concentration Data for AUC Start and End Times
#' 
#' @param conc The concentration data (given in a vector form)
#' @param time The time data (given in a vector form)
#' @param interpolate The value to determine whether to interpolate data points (given in a logical form)
#' @param extrapolate The value to determine whether to extrapolate data points (given in a logical form)
#' @param auc_method The method specified for AUC (either 'LIN' or 'LOG')
#' @param model The model specification (either 'M1', 'M2', 'M3', or 'M4')
#' @param dosing_type The dosing type specification (either 'SD' or 'SS')
#' @param told The time of last dose (given in a numeric value)
#' @param kel The KEL value (given as a numeric)
#' @param orig_conc The original (full) concentration data (given in a numeric vector)
#' @param orig_time The original (full) time data (given in a numeric vector)
#' 
#' 
estimate_missing_concentration <- function(conc = NULL, time = NULL, interpolate = NULL, extrapolate = NULL, auc_method = NULL, model = NULL, dosing_type = NULL, told = NULL, kel = NULL, orig_conc = NULL, orig_time = NULL) {
  if(is.null(auc_method)){
    stop("Error in estimate_missing_concentration: 'auc_method' is NULL")
  }
  if(auc_method != "LIN" && auc_method != "LOG"){
    stop("Error in estimate_missing_concentration: 'auc_method' is not either 'LIN' or 'LOG'")
  }
  
## 2019-11-24/RD Removing any CONC value that is equal to 0
  sel_idx <- which(orig_conc == 0)
  if(length(sel_idx) > 0){
    orig_conc <- orig_conc[-sel_idx]
    orig_time <- orig_time[-sel_idx] 
  }
  
  tmp <- data.frame("CONC" = conc, "TIME" = time, "INT_EXT" = NA)
  
  for(i in 1:(nrow(tmp)-1)){
##    2019-11-08/RD Added for Interpolation for AUC Start time has no data
##        
    if(is.na(conc[1]) && i == 1){
      if(time[1] <= orig_time[1]){
        if(model == "M2"){
          k <- (log(na.omit(tmp)[["conc"]][2])-log(na.omit(tmp)[["conc"]][2]))/(na.omit(tmp)[["time"]][2]-na.omit(tmp)[["time"]][1])
          if(k >= 0){
            conc_s_tmp <- conc[1]
          } else {
            conc_s_tmp <- exp(-1*k*na.omit(tmp)[["time"]][1]) * na.omit(tmp)[["conc"]][1] 
          }
        } else {
          if(model != "M4"){
            if(dosing_type == "SD"){
              conc_s_tmp <- 0
            } else if(dosing_type == "SS"){
              conc_s_tmp <- cmin(conc = conc, time = time)
            } 
          } else {
            conc_s_tmp <- NULL
          }
        }
        if(time[1] == told){
          if(isTRUE(extrapolate) && !is.null(conc_s_tmp)){
            conc[1] <- conc_s_tmp
            tmp$INT_EXT[1] <- "EXT" 
          }
        } else {
          if(isTRUE(interpolate)){
            cold <- ifelse(!is.na(orig_conc[which(told == orig_time)]), orig_conc[which(told == orig_time)], NA)
            if(auc_method == "LIN"){
              conc[1] <- interpolate_lin(conc1 = cold, time1 = told, conc2 = orig_conc[1], time2 = orig_time[1], est_time = time[1])
            } else if(auc_method == "LOG"){
              conc[1] <- interpolate_log(conc1 = cold, time1 = told, conc2 = orig_conc[1], time2 = orig_time[1], est_time = time[1])
            }
            tmp$INT_EXT[1] <- "INT"
          }
        }
      } else if((orig_time[1] < time[1]) && (time[1] < orig_time[length(orig_time)])){
        if(isTRUE(interpolate)){
          idx <- which(orig_time < time[1])
          idx <- idx[length(idx)]
          if(auc_method == "LIN"){
            conc[1] <- interpolate_lin(conc1 = orig_conc[idx], time1 = orig_time[idx], conc2 = orig_conc[idx+1], time2 = orig_time[idx+1], est_time = time[1])
          } else if(auc_method == "LOG"){
            conc[1] <- interpolate_log(conc1 = orig_conc[idx], time1 = orig_time[idx], conc2 = orig_conc[idx+1], time2 = orig_time[idx+1], est_time = time[1])
          }
          tmp$INT_EXT[1] <- "INT"
        }
      } else if(time[1] >= orig_time[length(orig_time)]){
        if(!is.na(kel[["KEL"]])){
          if(isTRUE(extrapolate)){
            conc[1] <- cest(conc = conc, time = time, t_last = time[1], kel = kel[["KEL"]], kelc0 = kel[["KELC0"]])
            tmp$INT_EXT[1] <- "EXT"
          }
        } else {
          if(isTRUE(extrapolate)){
            conc[1] <- NA
            tmp$INT_EXT[1] <- "EXT"
          }
        }
      }
##    2019-11-08/RD Added for Interpolation for AUC End time has no data
##  
    } else if(is.na(conc[nrow(tmp)]) && (i+1) == nrow(tmp)){
      if((orig_time[1] < time[nrow(tmp)]) && (time[nrow(tmp)] < orig_time[length(orig_time)])){
        if(isTRUE(interpolate)){
          idx <- which(orig_time < time[nrow(tmp)])
          idx <- idx[length(idx)]
          if(auc_method == "LIN"){
            conc[nrow(tmp)] <- interpolate_lin(conc1 = orig_conc[idx], time1 = orig_time[idx], conc2 = orig_conc[idx+1], time2 = orig_time[idx+1], est_time = time[nrow(tmp)])
          } else if(auc_method == "LOG"){
            conc[nrow(tmp)] <- interpolate_log(conc1 = orig_conc[idx], time1 = orig_time[idx], conc2 = orig_conc[idx+1], time2 = orig_time[idx+1], est_time = time[nrow(tmp)])
          }
          tmp$INT_EXT[nrow(tmp)] <- "INT"
        }
      } else {
        if(!is.na(kel[["KEL"]])){
          if(isTRUE(extrapolate)){
            conc[nrow(tmp)] <- cest(conc = conc, time = time, t_last = time[length(time)], kel = kel[["KEL"]], kelc0 = kel[["KELC0"]])
            tmp$INT_EXT[nrow(tmp)] <- "EXT"
          }
        } else {
          if(isTRUE(extrapolate)){
            min_lim <- orig_time[length(orig_time)] - (orig_time[length(orig_time)] * 0.05)
            max_lim <- (orig_time[length(orig_time)] * 0.05) + orig_time[length(orig_time)]
            if(min_lim < time[nrow(tmp)] && time[nrow(tmp)] < max_lim){
              conc[nrow(tmp)] <- orig_conc[length(orig_conc)]
              tmp$INT_EXT[nrow(tmp)] <- "EXT"
            } else {
              conc[nrow(tmp)] <- NA
              tmp$INT_EXT[nrow(tmp)] <- "EXT"
            }
          }
        }
      }
    }
  }
  tmp$CONC <- conc
  return(list(conc, tmp))
}
