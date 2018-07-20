#' Cumulative amount of drug recovered unchanged in the urine up to time T post-dose, where t 
#' can have a numerical value, expressed as percentage of administered dose.
#'
#' @details 
#' \strong{Model M4, Derived} \cr
#' \strong{Equation} \cr
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{aetpct.png} \cr
#'  }
#'  \eqn{AET = cumulative amount at of drug at time t (wt)} \cr
#'  \eqn{Dose = administered dose  (wt)} \cr
#' }
#' 
#' @param aet Cumulative (running sum) amount of drug
#' @param dose The dose data (given in a vector form)
#' 
#' @section Returns:
#' \strong{Value} \cr 
#' \itemize{
#'  \item AETPCT: Cumulative amount of drug in percentage
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
aetpct <- function(aet = NULL, dose = NULL){
  if(is.null(aet) && is.null(dose)) {
    stop("Error in aetpct: 'aet' and 'dose' vectors are NULL")
  } else if(is.null(aet)) {
    stop("Error in aetpct: 'aet' vector is NULL")
  } else if(is.null(dose)) {
    stop("Error in aetpct: 'dose' vector is NULL")
  } 
  
  if(!(is.numeric(aet) && is.vector(aet))){
    stop("Error in aetpct: 'aet' is not a numeric vector")
  }
  if(!(is.numeric(dose) && is.vector(dose))){
    stop("Error in aetpct: 'dose' is not a numeric vector")
  }
  
  if(is.na(dose) || (0 %in% dose) || is.na(aet)) {
    aet_pct <- NA
  } else {
    aet_pct <- (aet/dose) * 100
    aet_pct <- replace(aet_pct, is.infinite(aet_pct), NA)
  }
  
  return(aet_pct)
}
