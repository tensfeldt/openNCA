#' Helper Function for Estimating TOLD Concentration Data
#' 
#' @param conc The concentration data (given in a vector form)
#' @param time The time data (given in a vector form)
#' @param interpolate The value to determine whether to interpolate data points (given in a logical form)
#' @param extrapolate The value to determine whether to extrapolate data points (given in a logical form)
#' @param auc_method The method specified for AUC (either 'LIN' or 'LOG')
#' @param model The model specification (either 'M1', 'M2', 'M3', or 'M4')
#' @param dosing_type The dosing type specification (either 'SD' or 'SS')
#' @param told The time of last dose (given in a numeric value)
#' @param orig_conc The original (full) concentration data (given in a numeric vector)
#' @param orig_time The original (full) time data (given in a numeric vector)
#' 
estimate_told_concentration <- function(conc = NULL, time = NULL, interpolate = NULL, extrapolate = NULL, auc_method = NULL, model = NULL, dosing_type = NULL, told = NULL, orig_conc = NULL, orig_time = NULL, tmp = NULL) {
  if(is.null(auc_method)){
    stop("Error in estimate_told_concentration: 'auc_method' is NULL")
  }
  if(auc_method != "LIN" && auc_method != "LOG"){
    stop("Error in estimate_told_concentration: 'auc_method' is not either 'LIN' or 'LOG'")
  }
  
## 2019-11-24/RD Removing any CONC value that is equal to 0
  sel_idx <- which(orig_conc == 0)
  if(length(sel_idx) > 0){
    orig_conc <- orig_conc[-sel_idx]
    orig_time <- orig_time[-sel_idx] 
  }
  if(is.null(tmp)){
    tmp <- data.frame("CONC" = conc, "TIME" = time, "INT_EXT" = NA) 
  }

  if(model == "M2"){
    if(nrow(tmp) >= 2){
      k <- (log(na.omit(tmp)[["conc"]][2])-log(na.omit(tmp)[["conc"]][1]))/(na.omit(tmp)[["time"]][2]-na.omit(tmp)[["time"]][1])
      if(k >= 0){
        conc_s_tmp <- conc[1]
      } else {
        conc_s_tmp <- exp(-1*k*na.omit(tmp)[["time"]][1]) * na.omit(tmp)[["conc"]][1] 
      }
    } else if(nrow(tmp) == 1){
      conc_s_tmp <- conc[1]
    } else {
      conc_s_tmp <- NA
    }
  } else {
    if(dosing_type == "SD"){
      conc_s_tmp <- 0
    } else if(dosing_type == "SS"){
      conc_s_tmp <- cmin(conc = conc, time = time)
    }
  }
  if(isTRUE(time[1] == told)){
    if(isTRUE(extrapolate) && !is.null(conc_s_tmp)){
      conc[1] <- conc_s_tmp
      tmp$INT_EXT[1] <- "EXT" 
    }
  } else {
    if(isTRUE(interpolate)){
      cold <- ifelse(!is.na(orig_conc[which(told == orig_time)]), orig_conc[which(told == orig_time)], NA)
      if(!is.na(cold) && length(cold) > 0){
        if(auc_method == "LIN"){
          conc[1] <- interpolate_lin(conc1 = cold, time1 = told, conc2 = orig_conc[1], time2 = orig_time[1], est_time = time[1])
        } else if(auc_method == "LOG"){
          conc[1] <- interpolate_log(conc1 = cold, time1 = told, conc2 = orig_conc[1], time2 = orig_time[1], est_time = time[1])
        }
        tmp$INT_EXT[1] <- "INT"
      }
    }
  }
  tmp$CONC <- conc
  return(list(conc, tmp))
}
