#' Time at Maximum Observed Concentration
#'
#' This function gets the first time at which CMAXi is observed within a dosing interval 
#' and is obtained by the inspection of the data. In the case of multiple dosing, TMAXi is obtained
#' by inspection of the data during the dose inteval i.
#'
#' @details If all the concentrations are 0's then TMAXi will return NA. 
#' Also the interval must be in the range of the times listed in the data. 
#'
#' @param conc The concentration data (given in a vector form) 
#' @param time The time data (given in a vector form)
#' 
#' @section Returns:
#' \strong{Value} \cr 
#' \itemize{
#'  \item TMAX: time at maximum observed concentration
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
#' tmax()   
#' #Error in tmax: 'conc' and 'time' vectors are NULL
#' 
#' conc_vector <- data$CONC
#' time_vector <- data$TIME
#' 
#' tmax(conc = conc_vector, time = time_vector)
#' #0
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
#' tmax(conc = conc_vector, time = time_vector)
#' #NA
#' 
#' ############
#' ## Data 3 ##
#' #################################
#' ##  SID  ##  TIME  ##   CONC   ## 
#' #################################
#' ##   32  ##    0   ##   1.19   ## 
#' ##   32  ##    1   ##   1.27   ##
#' ##   32  ##    2   ##   1.24   ##
#' ##   32  ##    3   ##   1.27   ##
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
#' tmax(conc = conc_vector, time = time_vector)
#' #1
#' 
#' @author
#' \itemize{
#'  \item Kevin McConnell
#' }
#' @export
tmax <- function(conc = NULL, time = NULL){
  if(is.null(conc) && is.null(time)){
    stop("Error in tmax: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in tmax: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in tmax: 'time' vectors is NULL")
  }
  
  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in tmax: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in tmax: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc) ){
    stop("Error in tmax: length of 'time' and 'conc' vectors are not equal")
  }
  
  tmp <- data.frame(time, conc)
  t_max <- NA
  if(nrow(tmp) < 1){
    return(t_max)
  }
  if(sum(tmp$conc, na.rm=T) == 0){
    return(t_max)
  }
  # Find the maximum concentration value and ignore NA values.
  c_max <- max(tmp$conc, na.rm = TRUE)
  # Find the time of the max concentration 
  t_max <- min(tmp[tmp$conc == c_max,]$time, na.rm = TRUE)
  return(t_max)
}
