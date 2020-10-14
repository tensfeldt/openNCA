#' The volume of distribution divided by weight
#'
#' The volume of distribution at steady state based on the observed last measurable plasma
#' concentration for a substance administered by intravascular dosing divided by the weight.
#'
#' @details
#' \strong{Model M2 and M3 (SD) Derived}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{vssow.png} \cr
#'  }
#' }
#' \eqn{VSSO = Steady-state (observed) volume of distribution for non-steady state or steady state data} \cr
#' \eqn{NORMBS = BW = Body weight} \cr
#'
#' @param vsso The volume of distribution (given in a vector form)
#' @param normbs The dose data (given in a vector form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item VSSOW: the volume of distribution divided by weight
#' }
#'
#' @examples
#' #No appropriate examples
#'
#' @author
#' \itemize{
#'  \item \strong{Thomas Tensfeldt, Pfizer & Rudraya Technical Team}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{thomas.g.tensfeldt@pfizer.com}
#' }
#' @export
### 2019-08-17/TGT/ vsspo <- function(vsso = NULL, normbs = NULL){
vssow <- function(vsso = NULL, normbs = NULL){
### 2019-08-17/TGT/ assume normbs is 1/unit value if not available
  if(is.null(vsso) && is.null(normbs)){
    stop("Error in vssow: 'vsso' and 'normbs' vectors are NULL")
  } else if(is.null(vsso)) {
    stop("Error in vssow: 'vsso' vector is NULL")
  } else if(is.null(normbs)) {
### 2019-08-17/TGT/ assume unit NORMBS value if missing or non-numeric
    normbs <- rep(1.0, length(vssp))
    warning("Warning in vsspw: supplied 'normbs' vector is NULL, assuming unit normbs==1")
###    stop("Error in vssow: 'normbs' vectors is NULL")
  }

  normbs <- type.convert(normbs, as.is=TRUE)

  if(length(vsso) != length(normbs) ){
    stop("Error in vssow: length of vector arguments do not match")
  }

###  if(is.na(vsso) || is.na(normbs)) {
  if(is.na(vsso) || is.na(normbs) || mode(normbs)!="numeric") {
    vss_ow <- NA
  } else {
    vss_ow <- vsso/normbs
  }

  return(vss_ow)
}
