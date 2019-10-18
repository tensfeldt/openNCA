#' The volume of distribution divided by weight
#'
#' The volume of distribution at steady state based on the predicted last measurable plasma
#' concentration for a substance administered by intravascular dosing divided by the weight.
#'
#' @details
#' \strong{Model M2 (SD, SS) and M3(SS, SD) Derived}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{vsspw.png} \cr
#'  }
#' }
#' \eqn{VSSPi = Steady-state (predicted) volume of distribution for non-steady state or steady state data} \cr
#' \eqn{NORMBS = BW = Body weight} \cr
#'
#' @param vssp The volume of distribution (numeric value)
#' @param normbs The dose data (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item VSSPW: the volume of distribution divided by weight
#' }
#'
#' @examples
#'
#' @author
#' \itemize{
#'  \item \strong{Thomas Tensfeldt, Pfizer & Rudraya Technical Team}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{thomas.g.tensfeldt@pfizer.com}
#' }
#' @export
vsspw <- function(vssp = NULL, normbs = NULL){
### 2019-08-17/TGT/ assume normbs is 1/unit value if not available
  if(is.null(vssp) && is.null(normbs)){
    stop("Error in vsspw: 'vssp' and 'normbs' vectors are NULL")
  } else if(is.null(vssp)) {
    stop("Error in vsspw: 'vssp' vector is NULL")
  } else if(is.null(normbs)) {
### 2019-08-17/TGT/ assume unit NORMBS value if missing or non-numeric
    normbs <- rep(1.0, length(vssp))
    warning("Warning in vsspw: supplied 'normbs' vector is NULL, assuming unit normbs==1")
###    stop("Error in vsspw: 'normbs' vectors is NULL")
  }

  normbs <- type.convert(normbs, as.is=TRUE)

  if(length(vssp) != length(normbs) ){
    stop("Error in vsspw: length of vector arguments do not match")
  }
  
###  if(is.na(vssp) || is.na(normbs)) {
  if(is.na(vssp) || is.na(normbs) || mode(normbs)!="numeric") {
    vss_pw <- NA
  } else {
    vss_pw <- vssp/normbs
  }

  return(vss_pw)
}
