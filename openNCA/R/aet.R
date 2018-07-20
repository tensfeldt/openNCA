#' Cumulative (running sum) amount of drug recovered unchanged in the urine up to time t post-dose,
#' where t can have a numerical value. 
#'
#' @details 
#' \strong{Model M4} \cr
#' \strong{Equation} \cr
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{aet.png} \cr
#'  }
#'  \eqn{AET = cumulative amount of drug (wt) up to time t post dose} \cr
#'  \eqn{ATt = amount of drug (wt), where t can have a numerical value} \cr
#' }
#' 
#' @param time The time data (given in a vector form)
#' @param amt The amount data (given in a vector form) either represents the volume or weight 
#' @param t The post-dose time value (numeric value)
#' 
#' @section Returns:
#' \strong{Value} \cr 
#' \itemize{
#'  \item AET: Cumulative amount of drug 
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
#' cmax()   
#' #Error in cmax: 'conc' and 'time' vectors are NULL
#' 
#' conc_vector <- data$CONC
#' time_vector <- data$TIME
#' 
#' cmax(conc = conc_vector, time = time_vector)
#' #2.89
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
#' cmax(conc = conc_vector, time = time_vector)
#' #0
#' 
#' ############
#' ## Data 3 ##
#' #################################
#' ##  SID  ##  TIME  ##   CONC   ## 
#' #################################
#' ##   32  ##    0   ##   1.19   ## 
#' ##   32  ##    1   ##   1.23   ##
#' ##   32  ##    2   ##   1.34   ##
#' ##   32  ## "None" ##   1.32   ##
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
#' cmax(conc = conc_vector, time = time_vector)
#' #Error in cmax: 'time' is not a numeric vector
#' 
#' @author
#' \itemize{
#'  \item Kevin McConnell
#' }
#' @export
aet <- function(time = NULL, amt = NULL, t = NULL){
  if(is.null(amt) && is.null(time) && is.null(t)) {
    stop("Error in aet: 'amt' and 'time' vectors and value 't' are NULL")
  } else if(is.null(amt) && is.null(time)) {
    stop("Error in aet: 'amt' and 'time' vectors are NULL")
  } else if(is.null(amt) && is.null(t)) {
    stop("Error in aet: 'amt' vector and value 't' are NULL")
  } else if(is.null(time) && is.null(t)) {
    stop("Error in aet: 'time' vector and value 't' are NULL")
  } else if(is.null(amt)) {
    stop("Error in aet: 'amt' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in aet: 'time' vector is NULL")
  } else if(is.null(t)) {
    stop("Error in aet: value 't' is NULL")
  }
  
  if(!(is.numeric(amt) && is.vector(amt))){
    stop("Error in aet: 'amt' is not a numeric vector")
  }
  
  if(any(is.na(amt))) {
    a_e <- NA
  } else {
    if(t %in% time) {
      tmp_time <- time[time <= t]
      if(length(tmp) > 0) {
        tmp_amt <- amt[1:length(tmp_time)]
      } else {
        stop("Error in aet: value 't' cannot be used to subset 'time' vector")
      }
      a_e <- sum(tmp_amt[!is.na(tmp_time)])
    } else {
      stop("Error in ae: value 't' is not present in 'time' vector")
    }
  }
  
  return(a_e)
}
