aumc_log <- function(conc = NULL, time = NULL, exflag = NULL){
  if(is.null(conc) && is.null(time)){
    stop("Error in aumc_log: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in aumc_log: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in aumc_log: 'time' vectors is NULL")
  }

  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in aumc_log: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in aumc_log: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc) ){
    stop("Error in aumc_log: length of 'time' and 'conc' vectors are not equal")
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
    aumc <- NA
    return(aumc)
  } else {
    t_max <- tmax(conc = conc, time = time)
    aumc_df <- ""

    for(i in 1:(nrow(tmp)-1)){
      if(conc[i] == 0 || conc[i+1] == 0){
        aumc_df[i] <- (((conc[i]*time[i]) + (conc[i+1]*time[i+1]))/2)*(time[i+1]-time[i])
      } else {
        tmp_ln <- conc[i+1]/conc[i]
        #print(paste0("tmp_ln:", tmp_ln))
        aumc_df_p1 <- (((conc[i+1]*time[i+1]) - (conc[i]*time[i]))/log(tmp_ln))*(time[i+1]-time[i])
        aumc_df_p2 <- ((time[i+1]-time[i])*(time[i+1]-time[i]))*((conc[i+1] - conc[i])/((log(tmp_ln)*log(tmp_ln))))
        aumc_df[i] <- aumc_df_p1 - aumc_df_p2
      }
    }
    aumc_df <- as.numeric(aumc_df)
    aumc <- sum(aumc_df, na.rm = TRUE)
  }
  return(aumc)
}
