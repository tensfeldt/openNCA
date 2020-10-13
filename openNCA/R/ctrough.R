#' Pre-dose concentration
#'
#' This function gets the pre-dose concentration defined as nominal time 0 during multiple dosing.
#'
#' @details If the concentration at nominal time 0 is not provided then it returns NA. \cr
#' The function will ensure that the concentration data and the time data provided are numeric vectors and
#' are also of the same length. \cr
#'
#' @param conc The concentration data (given in a vector form)
#' @param time The time data (given in a vector form)
#' @param tau The time duration of dosing interval (numeric value)
#' @param told The time of last dose (given in a numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item CTROUGH: pre-dose concentration
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
#' #ctrough()
#' #Error in ctrough: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- c(2.89, 2.49, 2.47)
#' time_vector <- c(0, 1, 2)
#' tau_value <- 2
#'
#' ctrough(conc = conc_vector, time = time_vector, tau = tau_value)
#' #2.89
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
#' ctrough(conc = conc_vector, time = time_vector, tau = tau_value)
#' #NA
#'
#' ############
#' ## Data 3 ##
#' ############################################
#' ##  SDEID  ##  TIME  ##   CONC   ##  TAU  ##
#' ############################################
#' ##   32    ##    0   ##   NA     ##   3   ##
#' ##   32    ##    1   ##   1.23   ##   3   ##
#' ##   32    ##    2   ##   1.34   ##   3   ##
#' ##   32    ##    3   ##   1.19   ##   3   ##
#' ############################################
#'
#' #Data mentioned will be used for the following example
#
#' conc_vector <- c(NA, 1.23, 1.34, 1.19)
#' time_vector <- c(0, 1, 2, 3)
#' tau_value <- 3
#'
#' ctrough(conc = conc_vector, time = time_vector, tau = tau_value)
#' #NA
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
ctrough <- function(conc = NULL, time = NULL, tau = NULL, told = NULL){
  if(is.null(conc) && is.null(time) && is.null(tau)){
    stop("Error in ctrough: 'conc', 'time' and 'tau' vectors are NULL")
  } else if(is.null(conc) && is.null(time)){
    stop("Error in ctrough: 'conc' and 'time' vectors are NULL")
  } else if(is.null(time) && is.null(tau)){
    stop("Error in ctrough: 'time' and 'tau' vectors are NULL")
  } else if(is.null(conc) && is.null(tau)){
    stop("Error in ctrough: 'conc' and 'tau' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in ctrough: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in ctrough: 'time' vectors is NULL")
  } else if(is.null(tau)) {
    stop("Error in ctrough: 'tau' vectors is NULL")
  } else if(all(is.na(time))) { 
    return(NA)
  } else if(all(is.na(conc))) {
    return(NA)
  } else if(all(is.na(tau))) {
    return(NA)
  }

  if(!(is.numeric(conc) && is.vector(conc))){
    stop("Error in ctrough: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time))){
    stop("Error in ctrough: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc)){
    stop("Error in ctrough: length of 'time' and 'conc' vectors are not equal")
  }
  if(length(told) != length(tau)){
    stop("Error in ctrough: length of 'told' and 'tau' vectors are not equal")
  }

  tmp <- data.frame(time, conc)
  c_trough <- NULL
  
  # Find the pre-dose concentration value
  for(i in 1:length(tau)){
    start_time <- told[i]
    end_time <- start_time + tau[i]
    tmp_df <- tmp[start_time <= tmp$time & tmp$time <= end_time,]
    if(is.null(c_trough)){
      c_trough <- tmp_df$conc[1]
    } else {
      c_trough <- c(c_trough, tmp_df$conc[1])
    }
  }
  return(c_trough)
}
