interpolate_lin <- function(conc1 = NULL, time1 = NULL, conc2 = NULL, time2 = NULL, est_time = NULL){
  if(is.null(conc1) || is.na(conc1)) {
    stop("Error in auc_interpolate_lin: 'conc1' value is NULL or NA")
  } else if(is.null(time1) || is.na(time1)) {
    stop("Error in auc_interpolate_lin: 'time1' value is NULL or NA")
  } else if(is.null(conc2) || is.na(conc2)) {
    stop("Error in auc_interpolate_lin: 'conc2' value is NULL or NA")
  } else if(is.null(time2) || is.na(time2)) {
    stop("Error in auc_interpolate_lin: 'time2' value is NULL or NA")
  } else if(is.null(est_time) || is.na(est_time)) {
    stop("Error in auc_interpolate_lin: 'est_time' value is NULL or NA")
  }

  if(!is.numeric(conc1)) {
    stop("Error in auc_interpolate_lin: 'conc1' value is not numeric")
  } else if(!is.numeric(time1)) {
    stop("Error in auc_interpolate_lin: 'time1' value is not numeric")
  } else if(!is.numeric(conc2)) {
    stop("Error in auc_interpolate_lin: 'conc2' value is not numeric")
  } else if(!is.numeric(time2)) {
    stop("Error in auc_interpolate_lin: 'time2' value is not numeric")
  } else if(!is.numeric(est_time)) {
    stop("Error in auc_interpolate_lin: 'est_time' value is not numeric")
  }

  est_conc <- NA
  if(time1 < est_time && est_time < time2){
    tmp_t <- abs(((est_time - time1)/(time2 - time1)))
    est_conc <- (conc1 + (tmp_t * (conc2 - conc1)))
  }
  return(est_conc)
}
