#' Area under the concentration versus time cruve from time 0 to infinity (Observed)
#'
#' This function gets the area under the concentration versus time curve from time 0 to 
#' infinity (Observed). Observed value for the time of the last mesurable concentration is used
#' to calculate the extrapolated AUC.\cr 
#' 
#' @details
#' \strong{Equation}
#' \enumerate{
#'  \item Calculate the extrapolated AUC: \cr 
#'  \tabular{rl}{
#'   \tab \figure{auc_inf_o1.png} \cr
#'  }
#'  \eqn{CLAST = Last mesurable (non-zero) plasma concentration} \cr
#'  \eqn{KEL = Terminal phase rate constant} \cr 
#'  \item Final Equation: \cr
#'  \tabular{rl}{ 
#'   \tab \figure{auc_inf_o2.png} \cr
#'  }
#'  \eqn{AUCLAST = Area under the concentration versus time curve from zero until time of last
#' mesurable concentration} \cr
#' }
#' @section Additional Details:
#' \strong{Linear Method} \cr  
#' \figure{auc_1.png} \cr
#' \strong{Log Method} \cr  
#' \figure{auc_2.png} \cr
#' \eqn{AUC = Area under the curve} \cr
#' \eqn{C_{i} = Concentration 1}{Ci = Concentration 1} \cr
#' \eqn{C_{i+1} = Concentration 2}{Ci+1 = Concentration 2} \cr
#' \eqn{T_{i} = Time 1}{Ti = Time 1} \cr
#' \eqn{T_{i+1} = Time 2}{Ti+1 = Time 2} \cr
#' \eqn{ln = Natural Logarithm} \cr \cr
#' \strong{Methods:} You can use the following methods to calculate AUC: \cr
#' \enumerate{
#'  \item \strong{Linear-Log Trapazoidal Rule}(default method): The linear method is used up to Tmax (the 
#'  first occurance of Cmax) and the log trapezoidal method is used for the remainder of the profile. If
#'  Ci or Ci+1 is 0 then the linear trapezoidal rule is used.
#'  \item \strong{Linear Trapazoidal Rule}: The linear method is used for the entire profile.
#'  \item \strong{Log Trapazoidal Rule}: The log trapezoidal method is used for the entire profile. If
#'  Ci or Ci+1 is 0 then the linear trapezoidal rule is used.
#'  \item \strong{Linear Up - Log Down Trapazoidal Rule}: Linear trapezoidal while the concentrations
#'  are increasing and log trapezoidal while the concentration are decreasing, the assessment is made on
#'  a step basis for each portion of the profile i.e. t1 to t2. If Ci or Ci+1 is 0 then the linear 
#'  trapezoidal rule is used.
#' }
#' \strong{Equation} \cr
#' If the user selects the option to have dose normalized AUC then the following equation is applied: \cr 
#' \figure{auc_dn.png} \cr 
#' 
#' @param conc The concentration data (given in a vector form) 
#' @param time The time data (given in a vector form)
#' @param method The method that will be used to calculate AUC (use either 1, 2, 3, or 4)\cr
#' \enumerate{
#' \item Linear-Log Trapazoidal Rule (default)
#' \item Linear Trapazoidal Rule 
#' \item Log Trapazoidal Rule 
#' \item Linear Up - Log DownTrapazoidal Rule 
#' } 
#' Note: check 'Methods' section below for more details \cr
#' 
#' @section Returns:
#' \strong{Value} \cr 
#' \itemize{
#'  \item AUC: area under the curve
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
auc_inf_o <- function(conc = NULL, time = NULL, method = 1){
  if(is.null(conc) && is.null(time)){
    stop("Error in auc_inf_o: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in auc_inf_o: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in auc_inf_o: 'time' vectors is NULL")
  }
  
  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in auc_inf_o: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in auc_inf_o: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc)){
    stop("Error in auc_inf_o: length of 'time' and 'conc' vectors are not equal")
  }
  if(method != 1 && method != 2 && method != 3 && method != 4){
    stop("Error in auc_inf_o: the value provided for 'method' is not correct")
  }
  
  kel <- kel(conc = conc, time = time)
  c_last <- clast(conc = conc, time = time)
  auc_obs <- c_last/kel[['KEL']]

  if(is.na(kel[['KEL']])){
    aumc_info <- NA
    return(aumc_info)
  } else {
    auclast <- auc_last(conc = conc, time = time, method = method)
    auc_info <- auclast + auc_obs
    
    if(auc_info < 0){
      auc_info <- NA
      return(auc_info) 
    } else {
      return(auc_info)  
    }
  }
}
