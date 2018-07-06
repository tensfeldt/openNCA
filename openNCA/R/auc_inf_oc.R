#' Corrected AUCINFO 
#'
#' AUCINFO corrected using estimates of KEL and C0 \cr 
#' 
#' @details
#' \strong{Equation}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{auc_inf_oc.png} \cr
#'  }
#'  \eqn{C0 = the residual plasma concentration observed at time zero (i.e. the predose concentraton)} \cr
#'  \eqn{KEL = the terminal phase rate constant for the concentration-time profile of interest} \cr
#' }
#' 
#' @param conc The concentration data (given in a vector form) 
#' @param time The time data (given in a vector form)
#' 
#' @section Returns:
#' \strong{Value} \cr 
#' \itemize{
#'  \item AUCINFOC: area under the curve corrected
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
#' ##   30  ##    3   ##   2.38   ##
#' ##   30  ##    4   ##   2.32   ##
#' ##   30  ##    5   ##   2.28   ##
#' #################################
#' 
#' data <- data.frame(
#'     SID = ...,
#'     TIME = ...,
#'     RESULT = ...
#' )
#' #Same data as above, just represented as a dataframe
#' 
#' auc_inf_o()   
#' #Error in auc_all: 'conc' and 'time' vectors are NULL
#' 
#' conc_vector <- data$CONC
#' time_vector <- data$TIME
#' 
#' auc_inf_o(conc = conc_vector, time = time_vector)
#' #67.86212
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
#' auc_inf_o(conc = conc_vector, time = time_vector)
#' #NA
#' 
#' @author
#' \itemize{
#'  \item Kevin McConnell
#' }
#' @export
auc_inf_oc <- function(kel = NULL, aucinfo = NULL, c0 = NULL){
  if(is.null(kel) && is.null(aucinfo) && is.null(c0)){
    stop("Error in auc_inf_oc: 'kel', 'aucinfo' and 'c0' vectors are NULL")
  } else if(is.null(kel) && is.null(aucinfo)){
    stop("Error in auc_inf_oc: 'kel' and 'aucinfo' vectors are NULL")
  } else if(is.null(kel) && is.null(c0)){
    stop("Error in auc_inf_oc: 'kel' and 'c0' vectors are NULL")
  } else if(is.null(aucinfo) && is.null(c0)){
    stop("Error in auc_inf_oc: 'aucinfo' and 'c0' vectors are NULL")
  } else if(is.null(kel)) {
    stop("Error in auc_inf_oc: 'kel' vector is NULL")
  } else if(is.null(aucinfo)) {
    stop("Error in auc_inf_oc: 'aucinfo' vectors is NULL")
  } else if(is.null(c0)) {
    stop("Error in auc_inf_oc: 'c0' vectors is NULL")
  }
  
  if(is.na(kel) || is.na(c0) || is.na(aucinfo)) {
    aucinfoc <- NA
  } else {
    aucinfoc <- aucinfo - (c0/kel) 
  }
  return(aucinfoc)
}
