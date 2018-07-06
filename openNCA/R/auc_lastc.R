#' Corrected AUCLASTi  
#'
#' AUCLAST corrected using estimates of KEL and C0. i refers to the value of NDOSEI. \cr 
#' 
#' @details
#' \strong{Equation}
#' CLAST(res) is the predicted residual concentration at TLAST
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{auc_lastc2.png} \cr
#'  }
#' }
#' AUCLAST corrected using estimates of KEL and C0 
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{auc_lastc1.png} \cr
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
#'  \item AUCINFPC: area under the curve corrected
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
auc_lastc <- function(kel = NULL, auclast = NULL, c0 = NULL, tlast = NULL){
  if(is.null(kel) || is.null(auclast) || is.null(c0) || is.null(tlast)){
    count <- 0
    err <- ""
    if(is.null(kel)){
      err <- paste0(err, "'kel'")
      count <- count + 1
    } 
    if(is.null(auclast)){
      err <- paste0(err, ifelse(is.null(kel), ", ", ""), "'auclast'")
      count <- count + 1
    } 
    if(is.null(c0)){
      err <- paste0(err, ifelse((is.null(kel) || is.null(auclast)), ", ", ""), "'c0'")
      count <- count + 1
    } 
    if(is.null(tlast)){
      err <- paste0(err, ifelse((is.null(kel) || is.null(auclast) || is.null(c0)), ", ", ""), "'tlast'")
      count <- count + 1
    }
    if(count > 1){
      stop(paste0("Error in auc_lastc: ", err, " vectors are NULL")) 
    } else {
      stop(paste0("Error in auc_lastc: ", err, " vector is NULL"))
    }
  } 
  
  if(is.na(kel) || is.na(c0) || is.na(auclast) || is.na(tlast)) {
    auclastc <- NA
  } else {
    c_last_res <- c0 * exp(-1*tlast*kel)
    auclastc <- auclast - (c0/kel) - (c_last_res/kel)
  }
  return(auclastc)
}
