#' Linear Trapezoidal Area Under the Concentration-Time Curve (AUC) Calculation Method
#'
#' The AUC linear method is used for the entire profile.
#' 
#' @param conc The concentration data (given in a vector form)
#' @param time The time data (given in a vector form)
#' @param exflag The exclude flag data (given in a numeric vector)
#'
#! @export
auc_lin <- function(conc = NULL, time = NULL, exflag = NULL){
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
      auc_df[i] <- ((conc[i] + conc[i+1])/2)*(time[i+1]-time[i])
    }
    auc_df <- as.numeric(auc_df)
    auc <- sum(auc_df, na.rm = TRUE)
  }
  return(auc)
}
