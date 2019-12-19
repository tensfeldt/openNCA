#' The total body clearance
#'
#' The total body clearance for intravascular administration, calculated using the predicted value of
#' the last non-zero concentration, divided by the weight.   \cr
#'
#' @details
#' \strong{Model M2 (SD) and M3 (SD) Derived}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{clpw.png} \cr
#'  }
#' }
#' \eqn{CLP = Total clearance of drug (predicted)} \cr
#' \eqn{NORMBS = BW = Body weight} \cr
#'
#' @section Note:
#' \strong{clp}: Refer to \code{\link{clp}} for more details
#'
#' @param clp Apparent clearance (observed) of drug  (given in a vector form)
#' @param normbs The dose data (given in a vector form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item CLPW: The total body clearance
#' }
#'
#' @examples
#' clpw()
#' #Error in clftauw: 'clftau' and 'normbs' vectors are NULL
#' 
#' clpw(clp = 10.34535, normbs = 1.234)
#' #8.38359
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
clpw <- function(clp = NULL, normbs = NULL){
  if(is.null(clp) && is.null(normbs)){
    stop("Error in clpw: 'clp' and 'normbs' vectors are NULL")
  } else if(is.null(clp)) {
    stop("Error in clpw: 'clp' vector is NULL")
  } else if(is.null(normbs)) {
    stop("Error in clpw: 'normbs' vectors is NULL")
  }

  if(length(clp) != length(normbs) ){
    stop("Error in clpw: length of vector arguments do not match")
  }

  if(is.na(clp) || is.na(normbs)) {
    cl_pw <- NA
  } else {
    cl_pw <- clp/normbs
  }

  return(cl_pw)
}
