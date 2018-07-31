#' The total body clearance 
#' 
#' The total body clearance for extravascular administration divided by the fraction of dose absorbed, 
#' calculated using the predicted value of the last non-zero concentration, divided by the weight.  \cr
#' 
#' @details
#' \strong{Model M1 (SD) Derived}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{clfpw.png} \cr
#'  }
#' }
#' \eqn{CLFP = Apparent clearance (predicted) of drug for extravascular routes of administration} \cr  
#' \eqn{NORMBS = BW = Body weight} \cr   
#' 
#' @section Additional Details:
#' 
#' @param clfp Apparent clearance (observed) of drug  (given in a vector form) 
#' @param normbs The dose data (given in a vector form)
#' 
#' @section Returns:
#' \strong{Value} \cr 
#' \itemize{
#'  \item CLFPW: The total body clearance 
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
#' aumc_XpctP()   
#' #No data found!
#' 
#' aumc_XpctP(data)  
#' #Object not of class NCA
#' 
#' mod <- model("~/data.csv")  
#' #Creates an NCA object with data represented in 'data' above
#' aumc_XpctP(mod)  
#' #Please specify for which subject you want to get the AUMC_XPCTP for!
#' 
#' aumc_XpctP(mod, sid = "all") 
#' #  SID              AUC     METHOD
#' #   30 4.94993257703106 Linear-Log
#' #   31                1 Linear-Log
#' #   32 5.15416479501756 Linear-Log
#' 
#' aumc_XpctP(mod, sid = 32)  
#' #  SID              AUC     METHOD
#' #   32 5.15416479501756 Linear-Log
#' 
#' aumc_XpctP(mod, sid = 10)  
#' #Invaild subject ID! Subject ID not found in the data provided!
#' 
#' aumc_XpctP(mod, sid = 31, method = 5)
#' #Invalid method number! Please provide a valid value for method!
#' 
#' @author
#' \itemize{
#'  \item Kevin McConnell
#' }
#' @export
clftauw <- function(clftau = NULL, normbs = NULL){
  if(is.null(clftau) && is.null(normbs)){
    stop("Error in clftauw: 'clftau' and 'normbs' vectors are NULL")
  } else if(is.null(clfp)) {
    stop("Error in clftauw: 'clftau' vector is NULL")
  } else if(is.null(normbs)) {
    stop("Error in clftauw: 'normbs' vectors is NULL")
  }
  
  if(length(clftau) != length(normbs) ){
    stop("Error in clftauw: length of vector arguments do not match")
  }
  
  if(is.na(clftau) || is.na(normbs)) {
    clf_tauw <- NA
  } else {
    clf_tauw <- clftau/normbs
  }
  
  return(clf_tauw)
}