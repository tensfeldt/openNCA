#' interpolate_log performs log-linear interpolation between two data points
#' 
#' @export
interpolate_log <- function(conc1 = NULL, time1 = NULL, conc2 = NULL, time2 = NULL, est_time = NULL){
### 2019-10-18/TGT/ change error messages from "inerpolate_log" to "interpolate_log"
##  2019-11-06/RD Fixing typo, changed 'interpolate_lin' to 'interpolate_log' for errors
  if(is.null(conc1) || is.na(conc1)) {
    stop("Error in interpolate_log: 'conc1' value is NULL or NA")
  } else if(is.null(time1) || is.na(time1)) {
    stop("Error in interpolate_log: 'time1' value is NULL or NA")
  } else if(is.null(conc2) || is.na(conc2)) {
    stop("Error in interpolate_log: 'conc2' value is NULL or NA")
  } else if(is.null(time2) || is.na(time2)) {
    stop("Error in interpolate_log: 'time2' value is NULL or NA")
  } else if(is.null(est_time) || is.na(est_time)) {
    stop("Error in interpolate_log: 'est_time' value is NULL or NA")
  }

  if(!is.numeric(conc1)) {
    stop("Error in interpolate_log: 'conc1' value is not numeric")
  } else if(!is.numeric(time1)) {
    stop("Error in interpolate_log: 'time1' value is not numeric")
  } else if(!is.numeric(conc2)) {
    stop("Error in interpolate_log: 'conc2' value is not numeric")
  } else if(!is.numeric(time2)) {
    stop("Error in interpolate_log: 'time2' value is not numeric")
  } else if(!is.numeric(est_time)) {
    stop("Error in interpolate_log: 'est_time' value is not numeric")
  }

  est_conc <- NA
  if(time1 < est_time && est_time < time2){
    tmp_t <- abs(((est_time - time1)/(time1 - time2)))
    est_conc <- exp(log(conc1) + (tmp_t * (log(conc2) - log(conc1))))
  }
  return(est_conc)
}
