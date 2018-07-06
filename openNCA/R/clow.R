#' The total body clearance 
#' 
#' The total body clearance for intravascular administration, calculated using the observed value 
#' of the last non-zero concentration, divided by the weight.  \cr
#' 
#' @details
#' \strong{Model M2 (SD) and M3 (SD) Derived}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{clow.png} \cr
#'  }
#' }
#' \eqn{CLO = Total clearance of drug (observed)} \cr  
#' \eqn{NORMBS = BW = Body weight} \cr   
#' 
#' @section Additional Details:
#' 
#' @param clo The AUCINFO data (given in a vector form) 
#' @param normbs The dose data (given in a vector form)
#' 
#' @section Returns:
#' \strong{Value} \cr 
#' \itemize{
#'  \item CLOW: The total body clearance 
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
clow <- function(clo = NULL, normbs = NULL){
  if(is.null(clo) && is.null(normbs)){
    stop("Error in clow: 'clo' and 'normbs' vectors are NULL")
  } else if(is.null(clo)) {
    stop("Error in clow: 'clo' vector is NULL")
  } else if(is.null(normbs)) {
    stop("Error in clow: 'normbs' vectors is NULL")
  }
  
  if(length(clo) != length(normbs) ){
    stop("Error in clfow: length of vector arguments do not match")
  }
  
  if(is.na(clo) || is.na(normbs)) {
    cl_ow <- NA
  } else {
    cl_ow <- clo/normbs
  }
  
  return(cl_ow)
}