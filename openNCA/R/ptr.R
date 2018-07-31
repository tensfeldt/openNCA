#' Total clearance of drug (observed)
#' 
#' Total clearance of drug (observed) \cr
#' 
#' @details
#' \strong{Models M2 and M3}
#' Single Dose Equation only; not calculated at steady-state. For steady-state CLTAUi is calculated using AUCTAUi
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{clo.png} \cr
#'  }
#' } 
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
ptr <- function(cmax = NULL, cmin = NULL){
  if(is.null(cmax) && is.null(cmin)){
    stop("Error in ptr: 'cmax' and 'cmin' vectors are NULL")
  } else if(is.null(cmax)) {
    stop("Error in ptr: 'cmax' vector is NULL")
  } else if(is.null(cmin)) {
    stop("Error in ptr: 'cmin' vectors is NULL")
  }
  
  if(length(cmax) != length(cmin) ){
    stop("Error in ptr: length of vector arguments do not match")
  }

  if(is.na(cmax) || (0 %in% cmin) || is.na(cmin)) {
    pt_r <- NA
  } else {
    pt_r <- cmax/cmin
    pt_r <- replace(pt_r, is.infinite(pt_r), NA)
  }
  
  return(pt_r)
}
