#' Concentrations at the end of the ith dose interval
#'
#' This function gets the concentrations at the end of the ith dosing interval, defined as the observed concentration 
#' at the time of nominal time TAU during multiple dosing.
#'
#' @details If the concentration at the end of the ith dose interval is not provided then it returns NA. \cr
#' The function will ensure that the concentration data and the time data provided are numeric vectors and
#' are also of the same length. \cr
#'
#' @param conc The concentration data (given in a vector form)
#' @param time The time data (given in a vector form)
#' @param tau The tau data (given in a vector form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item CTROUGHEND: concentrations at the end of ith dose interval
#' }
#'
#' @examples
#' ##########
#' ## Data ##
#' ############################################
#' ##  SDEID  ##  TIME  ##   CONC   ##  TAU  ##
#' ############################################
#' ##   30    ##    0   ##   2.89   ##   2   ##
#' ##   30    ##    1   ##   2.49   ##   2   ##
#' ##   30    ##    2   ##   2.47   ##   2   ##
#' ############################################
#'
#' #Data mentioned will be used for the following example
#'
#' ctroughend()
#' #Error in ctrough: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- c(2.89, 2.49, 2.47)
#' time_vector <- c(0, 1, 2)
#' tau_value <- 2
#'
#' ctroughend(conc = conc_vector, time = time_vector, tau = tau_value)
#' #2.47
#'
#' ############
#' ## Data 2 ##
#' ############################################
#' ##  SDEID  ##  TIME  ##   CONC   ##  TAU  ##
#' ############################################
#' ##   31    ##    1   ##   2.23   ##   4   ##
#' ##   31    ##    2   ##   2.34   ##   4   ##
#' ##   31    ##    3   ##   2.68   ##   4   ##
#' ############################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(2.23, 2.34, 2.68)
#' time_vector <- c(1, 2, 3)
#' tau_value <- 4
#'
#' ctroughend(conc = conc_vector, time = time_vector, tau = tau_value)
#' #NA
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
ctroughend <- function(conc = NULL, time = NULL, tau = NULL, told = NULL){
  if(is.null(conc) && is.null(time) && is.null(tau)){
    stop("Error in ctroughend: 'conc', 'time' and 'tau' vectors are NULL")
  } else if(is.null(conc) && is.null(time)){
    stop("Error in ctroughend: 'conc' and 'time' vectors are NULL")
  } else if(is.null(time) && is.null(tau)){
    stop("Error in ctroughend: 'time' and 'tau' vectors are NULL")
  } else if(is.null(conc) && is.null(tau)){
    stop("Error in ctroughend: 'conc' and 'tau' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in ctroughend: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in ctroughend: 'time' vectors is NULL")
  } else if(is.null(tau)) {
    stop("Error in ctroughend: 'tau' vectors is NULL")
  } else if(all(is.na(time))) { 
    return(NA)
  } else if(all(is.na(conc))) {
    return(NA)
  } else if(all(is.na(tau))) {
    return(NA)
  }

  if(!(is.numeric(conc) && is.vector(conc))){
    stop("Error in ctroughend: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time))){
    stop("Error in ctroughend: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc)){
    stop("Error in ctroughend: length of 'time' and 'conc' vectors are not equal")
  }
  if(length(told) != length(tau)){
    stop("Error in ctrough: length of 'told' and 'tau' vectors are not equal")
  }

  tmp <- data.frame(time, conc)
  c_troughend <- NULL
  
  # Find the concentration at the end of the ith dosing interval
  for(i in 1:length(tau)){
    tmp_start <- tmp[tmp$time <= tau[i],]
    start_time <- told[i]
    end_time <- start_time + tau[i]
    tmp_df <- tmp[start_time <= tmp$time && tmp$time <= end_time,]
    if(is.null(c_troughend)){
      c_troughend <- tmp_df[nrow(tmp_df),]$conc[1]
    } else {
      c_troughend <- c(c_troughend, tmp_df[nrow(tmp_df),]$conc[1])
    }
  }
  return(c_troughend)
}
