#' Total amount of drug recovered unchanged in the urine, from time zero to infinity 
#'
#' @details 
#' \strong{Model M4} \cr
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{ae.png} \cr
#'  }
#'  \eqn{AE = cumulative total amount from time zero to infinity (wt)} \cr
#'  \eqn{ATt = amount of drug (wt), where t can have a numerical value} \cr
#'  \eqn{ATlast = Last measurable amount of drug (wt)} \cr 
#' }
#' 
#' @param amt The amount data (given in a vector form)
#' 
#' @section Returns:
#' \strong{Value} \cr 
#' \itemize{
#'  \item AE: total amount of drug recovered
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
ae <- function(time = NULL, amt = NULL){
  if(is.null(amt) && is.null(time)) {
    stop("Error in ae: 'amt' and 'time' vectors is NULL")
  } else if(is.null(amt)) {
    stop("Error in ae: 'amt' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in ae: 'time' vector is NULL")
  }
  if(!(is.numeric(amt) && is.vector(amt))){
    stop("Error in ae: 'amt' is not a numeric vector")
  }
  
  if(any(is.na(amt))) {
    a_e <- NA
  } else {
    a_e <- sum(amt[!is.na(time)])
  }
  
  return(a_e)
}
