#' Initial Volume of Distribution 
#'
#' @details 
#' \strong{Model M2 (IV Bolus)} \cr
#' \figure{v_0.png} \cr 
#' 
#' @param c0 The c0 data (given in a vector form) 
#' @param dose The dose data (given in a vector form)
#' 
#' @section Returns:
#' \strong{Value} \cr 
#' \itemize{
#'  \item V0: initial volume of distribution
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
v0 <- function(c0 = NULL, dose = NULL){
  if(is.null(c0) && is.null(dose)){
    stop("Error in v0: 'c0' and 'dose' vectors are NULL")
  } else if(is.null(c0)) {
    stop("Error in v0: 'c0' vector is NULL")
  } else if(is.null(dose)) {
    stop("Error in v0: 'dose' vectors is NULL")
  }
  
  if(!(is.numeric(c0) && is.vector(c0)) ){
    stop("Error in v0: 'c0' is not a numeric vector")
  }
  if(!(is.numeric(dose) && is.vector(dose)) ){
    stop("Error in v0: 'dose' is not a numeric vector")
  }
  if(length(dose) != length(c0) ){
    stop("Error in v0: length of 'dose' and 'c0' vectors are not equal")
  }
  
  if(is.na(dose) || (0 %in% dose) || is.na(c0)) {
    v_0 <- NA
  } else {
    v_0 <- c0/dose
    v_0 <- replace(v_0, is.infinite(v_0), NA)
  }
  
  return(v_0)
}
