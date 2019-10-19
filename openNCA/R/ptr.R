#' Peak/Trough (CMAXi to CMINi) Ratio over the dosing interval i
#' 
#' @details
#' Steady-State Equation
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{ptr.png} \cr
#'  }
#' } 
#' \eqn{CMAXi = Maximum observed concentration} \cr   
#' \eqn{CMINi = Minimum observed concentration} \cr  
#' 
#' @param cmax The maxium observed concentration data (given in a vector form) 
#' @param cmin The minimum observed concentration data (given in a vector form)
#' 
#' @section Returns:
#' \strong{Value} \cr 
#' \itemize{
#'  \item PTR: peak/trough ratio
#' }
#' 
#' @examples 
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
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
