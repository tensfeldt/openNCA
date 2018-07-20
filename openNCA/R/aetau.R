#' Cumulative amount of drug recovered unchanged in the urine during a dosing interval i post dose. 
#' i refers to the value of NDOSEI.
#'
#' @details 
#' \strong{Model M4} \cr
#' \strong{Equation} \cr
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{aetau.png} \cr
#'  }
#'  \eqn{AETAUi = Cumulative amount at of drug during a dosing interval (wt)} \cr
#'  \eqn{ATt = amount of drug (wt), where t can have a numerical value} \cr
#'  \eqn{ATtau = amount of drug (wt) at end of the dosing interval} \cr
#' }
#' 
#' @param time The time data (given in a vector form)
#' @param aet Cumulative (running sum) amount of drug
#' @param t The post-dose time value (numeric value)
#' 
#' @section Returns:
#' \strong{Value} \cr 
#' \itemize{
#'  \item AETAU: Cumulative amount of drug 
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
aetau <- function(time = NULL, aet = NULL, t = NULL){
  if(is.null(aet) && is.null(time) && is.null(t)) {
    stop("Error in aetau: 'aet' and 'time' vectors and value 't' are NULL")
  } else if(is.null(aet) && is.null(time)) {
    stop("Error in aetau: 'aet' and 'time' vectors are NULL")
  } else if(is.null(aet) && is.null(t)) {
    stop("Error in aetau: 'aet' vector and value 't' are NULL")
  } else if(is.null(time) && is.null(t)) {
    stop("Error in aetau: 'time' vector and value 't' are NULL")
  } else if(is.null(aet)) {
    stop("Error in aetau: 'aet' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in aetau: 'time' vector is NULL")
  } else if(is.null(t)) {
    stop("Error in aetau: value 't' is NULL")
  }
  
  if(!(is.numeric(aet) && is.vector(aet))){
    stop("Error in aetau: 'amt' is not a numeric vector")
  }
  
  if(any(is.na(aet))) {
    ae_tau <- NA
  } else {
    if(t %in% time) {
      tmp_time <- time[time <= t]
      if(length(tmp) > 0) {
        tmp_aet <- aet[1:length(tmp_time)]
      } else {
        stop("Error in aetau: value 't' cannot be used to subset 'time' vector")
      }
      ae_tau <- sum(tmp_aet[!is.na(tmp_time)])
    } else {
      stop("Error in aetau: value 't' is not present in 'time' vector")
    }
  }
  
  return(ae_tau)
}
