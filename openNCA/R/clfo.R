#' Apparent clearance (observed) of drug 
#' 
#' Apparent clearance (observed) of drug from e.g. plasma, for extravascular routes of administration. \cr
#' 
#' @details
#' \strong{Model M1}
#' Single Dose Equation only; not calculated at steady-state. Note: For steady-state CLFTAUi is calculated using AUCTAUi:
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{clfo.png} \cr
#'  }
#' }
#' \eqn{F = fraction of dose absorbed [value is unknown for extravascular model} \cr  
#' \eqn{Dose = sum of dosei to dosen} \cr   
#' \eqn{AUCINFO = Area under the first moment curve from zero time to infinity (Observed)} \cr  
#' 
#' @section Additional Details:
#' 
#' @param aucinfo The AUCINFO data (given in a vector form) 
#' @param dose The dose data (given in a vector form)
#' 
#' @section Returns:
#' \strong{Value} \cr 
#' \itemize{
#'  \item CLFO: Apparent clearance of drug
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
clfo <- function(aucinfo = NULL, dose = NULL){
  if(is.null(aucinfo) && is.null(dose)){
    stop("Error in clfo: 'aucinfo' and 'dose' vectors are NULL")
  } else if(is.null(aucinfo)) {
    stop("Error in clfo: 'aucinfo' vector is NULL")
  } else if(is.null(dose)) {
    stop("Error in clfo: 'dose' vectors is NULL")
  }
  
  if(length(dose) != length(aucinfo) ){
    stop("Error in clfo: length of vector arguments do not match")
  }
  
  if(is.na(dose) || (0 %in% dose) || is.na(aucinfo)) {
    clf_o <- NA
  } else {
    clf_o <- dose/aucinfo
    clf_o <- replace(clf_o, is.infinite(clf_o), NA)
  }
  
  return(clf_o)
}