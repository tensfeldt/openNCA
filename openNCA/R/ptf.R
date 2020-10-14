#' Fluctuation between the peak and trough concentrations during a dosing interval i
#' 
#' @details
#' Steady-State Equation
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{ptf.png} \cr
#'  }
#' } 
#' \eqn{CMAXi = Maximum observed concentration} \cr   
#' \eqn{CMINi = Minimum observed concentration} \cr  
#' \eqn{CAVi = Average concentration at steady-state for interval i} \cr 
#' 
#' @param cmax The maximum observed concentration data (given in a vector form) 
#' @param cmin The minimum observed concentration data (given in a vector form)
#' @param cav The average concentration data (given in a vector form)
#' 
#' @section Returns:
#' \strong{Value} \cr 
#' \itemize{
#'  \item PTF: fluctuation between the peak and trough
#' }
#' 
#' @examples
#' #No appropriate examples
#' 
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
ptf <- function(cmax = NULL, cmin = NULL, cav = NULL){
  if(is.null(cmax) && is.null(cmin) && is.null(cav)) {
    stop("Error in ptf: 'cmax', 'cmin' and vectors are NULL")
  } else if(is.null(cmax) && is.null(cmin)){
    stop("Error in ptf: 'cmax' and 'cmin' vectors are NULL")
  } else if(is.null(cmax) && is.null(cav)){
    stop("Error in ptf: 'cmax' and 'cav' vectors are NULL")
  } else if(is.null(cmin) && is.null(cav)){
    stop("Error in ptf: 'cmin' and 'cav' vectors are NULL")
  } else if(is.null(cmax)) {
    stop("Error in ptf: 'cmax' vector is NULL")
  } else if(is.null(cmin)) {
    stop("Error in ptf: 'cmin' vectors is NULL")
  } else if(is.null(cav)) {
    stop("Error in ptf: 'cav' vector is NULL")
  } 

  if((0 %in% cmax) ||is.na(cmax) || (0 %in% cmin) || is.na(cmin) || (0 %in% cav) || is.na(cav)) {
    pt_f <- NA
  } else {
    pt_f <- (cmax - cmin)/cav
    pt_f <- replace(pt_f, is.infinite(pt_f), NA)
  }
  
  return(pt_f)
}
