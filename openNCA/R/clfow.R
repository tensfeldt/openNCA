#' The total body clearance
#'
#' The total body clearance for extravascular administration divided by the fraction of dose absorbed,
#' calculated using the observed value of the last non-zero concentration, divided by the weight. \cr
#'
#' @details
#' \strong{Model M1 (SD) Derived}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{clfow.png} \cr
#'  }
#' }
#' \eqn{CLFO = Apparent clearance (observed) of drug for extravascular routes of administration} \cr
#' \eqn{NORMBS = BW = Body weight} \cr
#'
#' @param clfo The AUCINFO data (numeric value)
#' @param normbs The dose data (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item CLFOW: The total body clearance
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
clfow <- function(clfo = NULL, normbs = NULL){
  if(is.null(clfo) && is.null(normbs)){
    stop("Error in clfow: 'clfo' and 'normbs' vectors are NULL")
  } else if(is.null(clfo)) {
    stop("Error in clfow: 'clfo' vector is NULL")
  } else if(is.null(normbs)) {
    stop("Error in clfow: 'normbs' vectors is NULL")
  }

  if(length(clfo) != length(normbs) ){
    stop("Error in clfow: length of vector arguments do not match")
  }

  if(is.na(clfo) || is.na(normbs)) {
    clf_ow <- NA
  } else {
    clf_ow <- clfo/normbs
  }

  return(clf_ow)
}
