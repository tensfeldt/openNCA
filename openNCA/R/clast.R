#' Last Measurable Plasma Concentration
#'
#' This function gets the last measurable plasma concentration that is obtained by the 
#' inspection of the data.
#' 
#' @details If all the concentrations are 0's then CLAST will return NA. 
#' 
#' @param conc The concentration data (given in a vector form) 
#' @param time The time data (given in a vector form)
#' 
#' @section Returns:
#' \strong{Value} \cr 
#' \itemize{
#'  \item CLAST: last measureable plasma concentration
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
#' clast()   
#' #Error in clast: 'conc' and 'time' vectors are NULL
#' 
#' conc_vector <- data$CONC
#' time_vector <- data$TIME
#' 
#' clast(conc = conc_vector, time = time_vector)
#' #2.47
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
#' clast(conc = conc_vector, time = time_vector)
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
#' clast(conc = conc_vector, time = time_vector)
#' #1.34
#' 
#' @author
#' \itemize{
#'  \item Kevin McConnell
#' }
#' @export
clast <- function(conc = NULL, time = NULL){
  if(is.null(conc) && is.null(time)){
    stop("Error in clast: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in clast: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in clast: 'time' vectors is NULL")
  }
  
  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in clast: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in clast: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc) ){
    stop("Error in clast: length of 'time' and 'conc' vectors are not equal")
  }
  
  tmp <- data.frame(time, conc)
  c_last <- NA
  if(nrow(tmp) < 1){
    return(c_last)
  }
  if(sum(tmp$conc, na.rm=T) == 0){
    return(c_last)
  }
  # Find the maximum concentration value that does not have a NULL value.
  tmp_df <- tmp[!is.na(tmp$conc),]
  tmp_df <- tmp_df[tmp_df$conc > 0,]
  c_last <- tmp_df[nrow(tmp_df),]$conc
  return(c_last)
}
