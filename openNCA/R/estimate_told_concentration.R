#' Helper Function for Estimating TOLD Concentration Data
#' 
#' @param conc The concentration data (given in a vector form)
#' @param time The time data (given in a vector form)
#' @param interpolate The value to determine whether to interpolate data points (given in a logical form)
#' @param extrapolate The value to determine whether to extrapolate data points (given in a logical form)
#' @param auc_method The method specified for AUC (either 'LIN' or 'LOG')
#' @param model The model specification (either 'M1', 'M2', 'M3', or 'M4')
#' @param dosing_type The dosing type specification (either 'SD' or 'SS')
#' @param dosing_interval The current dosing interval (numeric value)
#' @param told The time of last dose (given in a numeric value)
#' @param prev_told The time of last dose from previous interval (given in a numeric value)
#' @param prev_tau The time duration of previous dosing interval (given in a numeric value)
#' @param last_crit_factor The criteria value for last time acceptance criteria (numeric value)
#' @param orig_conc The original (full) concentration data (given in a numeric vector)
#' @param orig_time The original (full) time data (given in a numeric vector)
#' @param orgtime The original time value from the map data ('nominal' or 'actual')
#' @param tmp The helper variable (used for internal calls)
#' 
estimate_told_concentration <- function(conc = NULL, time = NULL, interpolate = NULL, extrapolate = NULL, auc_method = NULL, model = NULL, dosing_type = NULL, dosing_interval = NULL, told = NULL, prev_told = NULL, prev_tau = NULL, last_crit_factor = NULL, orig_conc = NULL, orig_time = NULL, orgtime = NULL, tmp = NULL) {
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

  conc_s_tmp <- NULL
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
      if(tolower(orgtime) == "actual"){
        time_min_range <- ifelse(!is.null(last_crit_factor), as.numeric(last_crit_factor) * prev_tau, NA)
        
        #Find the concentration/time value closest to TOLD
        if(length(which(time == told)) > 0){
          tmp_told <- time[which(time == told)]
          tmp_ctold <- conc[which(time == told)]
          if(isTRUE(is.na(tmp_ctold))){
            if(length(which(time < told)) > 0){
              tmp_told <- time[which(time < told)]
              tmp_told <- ifelse(isTRUE(length(tmp_told) > 0), tmp_told[length(tmp_told)], NA)
              tmp_ctold <- conc[which(time == tmp_told)]
            }
          }
        } else if(length(which(time < told)) > 0){
          tmp_told <- time[which(time < told)]
          tmp_told <- ifelse(isTRUE(length(tmp_told) > 0), tmp_told[length(tmp_told)], NA)
          tmp_ctold <- conc[which(time == tmp_told)]
        } else {
          tmp_told <- told
          tmp_ctold <- NA
        }
        if(isTRUE(dosing_interval == 1)){
          adj_time <- as.numeric(tmp_told)
        } else {
          adj_time <- as.numeric(tmp_told - prev_told)
        }
        if(!is.na(tmp_ctold)){
          if(isTRUE(time_min_range <= tmp_told && tmp_told <= prev_tau)){
            conc_s_tmp <- ifelse(isTRUE(length(tmp_ctold) > 0), tmp_ctold[length(tmp_ctold)], NA)
          } else {
            conc_s_tmp <- cmin(conc = conc, time = time)
          }
        } else {
          conc_s_tmp <- cmin(conc = conc, time = time)
        }
      } else {
        conc_s_tmp <- cmin(conc = conc, time = time)
      }
    }
  }
  
  ctold_missing <- TRUE
  if(length(which(time == told)) > 0){
    orig_told <- time[which(time == told)]
    orig_ctold <- conc[which(time == told)]
    if(isTRUE(!is.na(orig_ctold))){
      ctold_missing <- TRUE
    }
  }
  if(isTRUE(ctold_missing)){
    if(isTRUE(extrapolate) && !is.null(conc_s_tmp)){
      conc[which(time == told)] <- conc_s_tmp
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
