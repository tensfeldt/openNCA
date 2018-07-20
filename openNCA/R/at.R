#' Amount of Drug in the body at time T, where T can have a numerical value. 
#'
#' @details 
#' \strong{Model M4} \cr
#' Use the appropriate equation to calculate the amount of drug recovered unchanged in the urine at each time post-dose \cr \cr
#' \strong{Equation} \cr
#' If urine is reported as volume then the following equation should be used: \cr
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{at_1.png} \cr
#'  }
#'  \eqn{V = urine volume at each time post-dose of time interval} \cr
#'  \eqn{C = urince concentration at each time post-dose of time interval} \cr
#' }
#' If urine is reported as weight, then system automatically will apply specific gravity corrections where 1.020a g/mL is the approximate specific gravity of urine. \cr
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{at_2.png} \cr
#'  }
#'  \eqn{W = urine weight at each time post-dose of time interval} \cr
#'  \eqn{C = urince concentration at each time post-dose of time interval} \cr
#'  \eqn{1.020 g/ml = specify gravity of urine} \cr
#' }
#' 
#' @param time The time data (given in a vector form)
#' @param conc The concentration data (given in a vector form)
#' @param amt The amount data (given in a vector form) either represents the volume or weight 
#' @param amt_units The units data for the amount (given in a vector form)
#' 
#' @section Returns:
#' \strong{Value} \cr 
#' \itemize{
#'  \item AT: Amount of Drug in the body
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
at <- function(time = NULL, conc = NULL, amt = NULL, amt_units = NULL){
  if(is.null(time) || is.null(conc) || is.null(amt) || is.null(amt_units)){
    count <- 0
    err <- ""
    if(is.null(time)){
      err <- paste0(err, "'time'")
      count <- count + 1
    } 
    if(is.null(conc)){
      err <- paste0(err, ifelse(is.null(time), ", ", ""), "'conc'")
      count <- count + 1
    } 
    if(is.null(amt)){
      err <- paste0(err, ifelse((is.null(time) || is.null(conc)), ", ", ""), "'amt'")
      count <- count + 1
    } 
    if(is.null(amt_units)){
      err <- paste0(err, ifelse((is.null(time) || is.null(conc) || is.null(amt)), ", ", ""), "'amt_units'")
      count <- count + 1
    }
    if(count > 1){
      stop(paste0("Error in at: ", err, " vectors are NULL")) 
    } else {
      stop(paste0("Error in at: ", err, " vector is NULL"))
    }
  } 
  
  if(any(is.na(amt)) || any(is.na(conc))) {
    a_t <- NA
  } else {
    wt_units <- c("kg", "gm", "dg", "cg", "mg", "ug", "ng", "pg", "fg", "") 
    v_units <- c("kl", "l", "dl", "cl", "ml", "ul", "nl", "pl", "fl", "") 
    
    unit <- tolower(unique(amt_units))
    if(unit %in% v_units) {
      a_t <- conc[!is.na(time)] * amt[!is.na(time)]
    } else if(unit %in% wt_units) {
      a_t <- conc[!is.na(time)] * (amt[!is.na(time)]/1.020)
    } else {
      a_t <- NA
    }
  }
  
  return(a_t)
}
