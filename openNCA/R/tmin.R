#' Time at Minimum Observed Concentration
#'
#' This function gets the first time at which CMINi is observed within the dosing interval and
#' is obtained by the inspection of the data. In the case of multiple dosing, TMINi 
#' is obtained by inspection of the data during each dosing interval i. 
#' 
#' @details If all the concentrations are 0's then TMINi will return NA. 
#'
#' @param conc The concentration data (given in a vector form) 
#' @param time The time data (given in a vector form)
#' 
#' @section Returns:
#' \strong{Value} \cr 
#' \itemize{
#'  \item CMIN: minimum observed concentration
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
#' tmin()   
#' #Error in tmin: 'conc' and 'time' vectors are NULL
#' 
#' conc_vector <- data$CONC
#' time_vector <- data$TIME
#' 
#' tmin(conc = conc_vector, time = time_vector)
#' #1
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
#' tmin(conc = conc_vector, time = time_vector)
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
#' ##   32  ##    3   ##   1.19   ##
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
#' tmin(conc = conc_vector, time = time_vector)
#' #0
#' 
#' @author
#' \itemize{
#'  \item Kevin McConnell
#' }
#' @export
tmin <- function(conc = NULL, time = NULL){
  if(is.null(conc) && is.null(time)){
    stop("Error in tmin: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in tmin: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in tmin: 'time' vectors is NULL")
  }
  
  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in tmin: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in tmin: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc) ){
    stop("Error in tmin: length of 'time' and 'conc' vectors are not equal")
  }
  
  tmp <- data.frame(time, conc)
  t_min <- NA
  if(nrow(tmp) < 1){
    return(t_min)
  }
  if(sum(tmp$conc, na.rm=T) == 0){
    return(t_min)
  }
  # Find the maximum concentration value and ignore NA values.
  c_min <- min(tmp$conc, na.rm = TRUE)
  # Find the time of the max concentration 
  t_min <- min(tmp[tmp$conc == c_min,]$time, na.rm = TRUE)
  return(t_min)
}
