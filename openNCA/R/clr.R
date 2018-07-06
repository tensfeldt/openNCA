#' The clearance of a substance from the blood by the kidneys. 
#' 
#' @details
#' \strong{Model: Derived}
#' Calculated for single dose studies.
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{clr.png} \cr
#'  }
#' } 
#' \eqn{AE = Total amount of drug recovered unchanged in the urine, from time zero to infinity. AEinf is achieved following 5* terminal phase estimation for collection period} \cr   
#' \eqn{AUCINFP = Area under the concentration versus time curve from zero time to infinity (Predicted)} \cr  
#' 
#' @section Additional Details:
#' 
#' @param aucinfo The AUCINFO data (given in a vector form) 
#' @param dose The dose data (given in a vector form)
#' 
#' @section Returns:
#' \strong{Value} \cr 
#' \itemize{
#'  \item CLO: total clearance of drug 
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
clr <- function(aucinfp = NULL, ae = NULL){
  if(is.null(aucinfp) && is.null(ae)){
    stop("Error in clr: 'aucinfp' and 'ae' vectors are NULL")
  } else if(is.null(aucinfp)) {
    stop("Error in clr: 'aucinfp' vector is NULL")
  } else if(is.null(ae)) {
    stop("Error in clr: 'ae' vectors is NULL")
  }
  
  if(length(ae) != length(aucinfp) ){
    stop("Error in clr: length of vector arguments do not match")
  }

  if(is.na(ae) || is.na(aucinfp)) {
    cl_r <- NA
  } else {
    cl_r <- ae/aucinfp
    cl_r <- replace(cl_r, is.infinite(cl_r), NA)
  }
  
  return(cl_r)
}
