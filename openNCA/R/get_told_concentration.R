#' Helper Function for Obtaining TOLD Concentration Data
#' 
#' @param conc The concentration data (given in a vector form)
#' @param time The time data (given in a vector form)
#' @param nom_time The nominal time data (given in a vector form)
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
#' 
get_told_concentration <- function(conc = NULL, time = NULL, nom_time = NULL, interpolate = NULL, extrapolate = NULL, auc_method = NULL, model = NULL, dosing_type = NULL, dosing_interval = NULL, told = NULL, prev_told = NULL, prev_tau = NULL, last_crit_factor = NULL, orig_conc = NULL, orig_time = NULL, orgtime = NULL) {
  tmp_conc_di <- conc
  tmp_time_di <- time
  tmp_nom_time_di <- nom_time
  
  af_idx <- which(time > told)
  b4_idx <- which(time < told)
  if(isTRUE(length(af_idx) > 0)){
    tmp_af_conc_di <- tmp_conc_di[af_idx]
    tmp_af_time_di <- tmp_time_di[af_idx]
    tmp_af_nom_time_di <- tmp_nom_time_di[af_idx]
  } else {
    tmp_af_conc_di <- c()
    tmp_af_time_di <- c()
    tmp_af_nom_time_di <- c()
  }
  if(isTRUE(length(b4_idx) > 0)){
    tmp_b4_conc_di <- tmp_conc_di[b4_idx]
    tmp_b4_time_di <- tmp_time_di[b4_idx]
    tmp_b4_nom_time_di <- tmp_nom_time_di[b4_idx]
  } else {
    tmp_b4_conc_di <- c()
    tmp_b4_time_di <- c()
    tmp_b4_nom_time_di <- c()
  }
  tmp_conc_di <- c(tmp_b4_conc_di, NA, tmp_af_conc_di)
  tmp_time_di <- c(tmp_b4_time_di, told, tmp_af_time_di)
  tmp_nom_time_di <- c(tmp_b4_nom_time_di, told, tmp_af_nom_time_di)
  
  est_tmp <- estimate_told_concentration(conc = tmp_conc_di, time = tmp_time_di, interpolate = interpolate, extrapolate = extrapolate, auc_method = auc_method, model = model, dosing_type = dosing_type, dosing_interval = dosing_interval, told = told, prev_told = prev_told, prev_tau = prev_tau, last_crit_factor = last_crit_factor, orig_conc = orig_conc, orig_time = orig_time, orgtime = orgtime)
  tmp_conc_di <- est_tmp[[1]]
  idx <- which(tmp_time_di == told)
  if(isTRUE(length(idx) > 0)){
    ctoldest <- tmp_conc_di[idx]
  } else {
    ctoldest <- NA
  }
  return(list(tmp_conc_di, tmp_time_di, tmp_nom_time_di, ctoldest))
}