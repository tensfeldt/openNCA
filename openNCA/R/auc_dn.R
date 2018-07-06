#' Dose normalized area under the concentration versus time curve
#'
#' This function gets the dose normalized area under the concentration versus time curve from time 0 
#' until the last time point. 
#' 
#' @details
#' \strong{Equation} \cr
#' \figure{auc_dn.png} \cr
#' \strong{Linear Method} \cr  
#' \figure{auc_1.png} \cr
#' \strong{Log Method} \cr  
#' \figure{auc_2.png} \cr
#' \eqn{AUC = Area under the cruve} \cr
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
#' 
#' @param auc The AUC data (given in a vector form) 
#' @param dose The dose data (given in a vector form)
#' 
#' @section Returns:
#' \strong{Value or Vector} \cr 
#' \itemize{
#'  \item AUC: dose normalized area under the curve
#' }
#' 
#' @examples 
#' ##########
#' ## Data ##
#' #################################
#' ##  SID  ##  TIME  ##  RESULT  ##
#' #################################
#' ##   30  ##    0   ##   2.89   ##
#' ##   30  ##    1   ##   2.49   ##
#' ##   30  ##    2   ##   2.47   ##
#' ##   31  ##    0   ##      0   ##
#' ##   31  ##    1   ##   1.00   ##
#' ##   31  ##    2   ##      0   ##
#' ##   32  ##    0   ##   1.19   ##
#' ##   32  ##    1   ##   1.23   ##
#' ##   32  ##    2   ##   1.34   ##
#' ##   32  ##    4   ##   1.32   ##
#' #################################
#' 
#' data <- data.frame(
#'     SID = ...,
#'     TIME = ...,
#'     RESULT = ...
#' )
#' #Same data as above, just represented as a dataframe
#' 
#' auc_dn()   
#' #No data found!
#' 
#' auc_dn(data)  
#' #Object not of class NCA
#' 
#' mod <- model("~/data.csv")  
#' #Creates an NCA object with data represented in 'data' above
#' auc_dn(mod)  
#' #Please specify for which subject you want to get the AUC_DN for!
#' 
#' auc_dn(mod, sid = "all") 
#' #  SID              AUC     METHOD
#' #   30 4.94993257703106 Linear-Log
#' #   31                1 Linear-Log
#' #   32 5.15416479501756 Linear-Log
#' 
#' auc_dn(mod, sid = 32)  
#' #  SID              AUC     METHOD
#' #   32 5.15416479501756 Linear-Log
#' 
#' auc_dn(mod, sid = 10)  
#' #Invaild subject ID! Subject ID not found in the data provided!
#' 
#' auc_dn(mod, sid = 31, method = 5)
#' #Invalid method number! Please provide a valid value for method!
#' 
#' @author
#' \itemize{
#'  \item Kevin McConnell
#' }
#' @export
auc_dn <- function(auc = NULL, dose = NULL){
  if(is.null(auc) && is.null(dose)){
    stop("Error in auc_dn: 'auc' and 'dose' vectors are NULL")
  } else if(is.null(auc)) {
    stop("Error in auc_dn: 'auc' vector is NULL")
  } else if(is.null(dose)) {
    stop("Error in auc_dn: 'dose' vectors is NULL")
  }
  
  if(length(dose) != length(auc) ){
    stop("Error in auc_dn: length of vector arguments do not match")
  }
 
  if(is.na(dose) || (0 %in% dose) || is.na(auc)) {
    aucdn <- NA
  } else {
    aucdn <- auc/dose
    aucdn <- replace(aucdn, is.infinite(aucdn), NA)
  }
  
  return(aucdn)
}