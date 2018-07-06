#' Lag Time
#'
#' This function gets the lag time, which is described as the time before the start of absorption phase
#' and is directly observed from the concentration-time profile. This time is defined s the sample time 
#' immidiately prior to the first quantifiable concentration.
#' 
#' @details If all concentrations have a value greater than 0 then TLAG is 0. 
#' You must provide a valid sid with respect to the data.
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
#' #################################
#' ##  SID  ##  TIME  ##   CONC   ## 
#' #################################
#' ##   30  ##    0   ##   2.89   ##
#' ##   30  ##    1   ##   2.49   ##
#' ##   30  ##    2   ##   2.47   ##
#' #################################
#' 
#' data <- data.frame(
#'     SID = ...,
#'     TIME = ...,
#'     RESULT = ...
#' )
#' #Same data as above, just represented as a dataframe
#' 
#' tlag()   
#' #Error in tlag: 'conc' and 'time' vectors are NULL
#' 
#' conc_vector <- data$CONC
#' time_vector <- data$TIME
#' 
#' tlag(conc = conc_vector, time = time_vector)
#' #2
#'  
#' ############
#' ## Data 2 ##
#' #################################
#' ##  SID  ##  TIME  ##   CONC   ## 
#' #################################
#' ##   31  ##    0   ##      0   ## 
#' ##   31  ##    1   ##      0   ##
#' ##   31  ##    2   ##      0   ##
#' #################################
#' 
#' data2 <- data.frame(
#'     SID = ...,
#'     TIME = ...,
#'     RESULT = ...
#' )
#' #Same data as above, just represented as a dataframe
#' 
#' conc_vector <- data2$CONC
#' time_vector <- data2$TIME
#' 
#' tlag(conc = conc_vector, time = time_vector)
#' #NA
#' 
#' ############
#' ## Data 3 ##
#' #################################
#' ##  SID  ##  TIME  ##   CONC   ## 
#' #################################
#' ##   32  ##    0   ##   1.19   ## 
#' ##   32  ##    1   ##   1.23   ##
#' ##   32  ##    2   ##   1.34   ##
#' ##   32  ##    3   ##   BLQ    ##
#' #################################
#' 
#' data3 <- data.frame(
#'     SID = ...,
#'     TIME = ...,
#'     RESULT = ...
#' )
#' #Same data as above, just represented as a dataframe
#' 
#' conc_vector <- data3$CONC
#' time_vector <- data3$TIME
#' 
#' tlag(conc = conc_vector, time = time_vector)
#' #2
#' 
#' @author
#' \itemize{
#'  \item Ronish Desai: \email{rbdesai@@rudraya.com} 
#' }
#' Other contributors: \cr
#' \itemize{
#'  \item Rudraya Corporation: \url{http://rudraya.com}
#' }
#' \figure{rudraya.png} 
#' @export
tlag <- function(conc = NULL, time = NULL){
  if(is.null(conc) && is.null(time)){
    stop("Error in tlag: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in tlag: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in tlag: 'time' vectors is NULL")
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
  tmp <- tmp[order(tmp$time),]
  
  for(i in 1:nrow(tmp)){
    tmp_c <- tmp$conc[i]
    if(!is.na(tmp_c) && is.numeric(tmp_c)){
      t_lag <- tmp$time[i]
      break
    }
  }
  return(t_lag) 
}
