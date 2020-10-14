#' Lag Time
#'
#' This function gets the lag time, which is the time before the start of absorption phase
#' and is directly observed from the concentration-time profile. This time is defined s the sample time
#' immediately prior to the first quantifiable concentration.
#'
#' @details If all concentrations have a value greater than 0 then TLAG is 0.
#' You must provide a valid SDEID with respect to the data. \cr
#' The function will ensure that the concentration data and the time data provided are numeric vectors and
#' are also of the same length. \cr
#'
#' @param conc The concentration data (given in a vector form)
#' @param time The time data (given in a vector form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item TLAG: lag time
#' }
#'
#' @examples
#' ##########
#' ## Data ##
#' ###################################
#' ##  SDEID  ##  TIME  ##   CONC   ##
#' ###################################
#' ##   30    ##    0   ##   2.89   ##
#' ##   30    ##    1   ##   2.49   ##
#' ##   30    ##    2   ##   2.47   ##
#' ###################################
#'
#' #Data mentioned will be used for the following example
#'
#' #tlag()
#' #Error in tlag: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- c(2.89, 2.49, 2.47)
#' time_vector <- c(0, 1, 2)
#'
#' tlag(conc = conc_vector, time = time_vector)
#' #0
#'
#' ############
#' ## Data 2 ##
#' ###################################
#' ##  SDEID  ##  TIME  ##   CONC   ##
#' ###################################
#' ##   31    ##    0   ##      0   ##
#' ##   31    ##    1   ##      0   ##
#' ##   31    ##    2   ##      0   ##
#' ###################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(0, 0, 0)
#' time_vector <- c(0, 1, 2)
#'
#' tlag(conc = conc_vector, time = time_vector)
#' #NA
#'
#' ############
#' ## Data 3 ##
#' ###################################
#' ##  SDEID  ##  TIME  ##   CONC   ##
#' ###################################
#' ##   32    ##    0   ##   NA     ##
#' ##   32    ##    1   ##   1.23   ##
#' ##   32    ##    2   ##   1.34   ##
#' ##   32    ##    3   ##   1.19   ##
#' ###################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(NA, 1.23, 1.34, 1.19)
#' time_vector <- c(0, 1, 2, 3)
#'
#' tlag(conc = conc_vector, time = time_vector)
#' #1
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
tlag <- function(conc = NULL, time = NULL){
  if(is.null(conc) && is.null(time)){
    stop("Error in tlag: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in tlag: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in tlag: 'time' vectors is NULL")
  } else if(all(is.na(time))) { # 2019-09-13/TGT/
      return(NA)
  } else if(all(is.na(conc))) { # 2019-09-13/TGT/
      return(NA)
  }

  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in tlag: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in tlag: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc) ){
    stop("Error in tlag: length of 'time' and 'conc' vectors are not equal")
  }

  tmp <- data.frame(time, conc)
  t_lag <- NA
  if(nrow(tmp) < 1){
    return(t_lag)
  }
  if(sum(tmp$conc, na.rm=T) == 0){
    return(t_lag)
  }
  if(isTRUE(all(tmp$conc > 0))){
    return(0)
  }
  tmp <- tmp[order(tmp$time),]

  for(i in 1:nrow(tmp)){
    tmp_c <- tmp$conc[i]
    tmp_next_c <- tmp$conc[i+1]
    if(!is.na(tmp_c) && isTRUE(tmp_next_c != 0) && is.numeric(tmp_c)){
      t_lag <- tmp$time[i]
      break
    }
  }
  return(t_lag)
}
