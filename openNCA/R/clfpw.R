#' The total body clearance
#'
#' The total body clearance for extravascular administration divided by the fraction of dose absorbed,
#' calculated using the predicted value of the last non-zero concentration, divided by the weight. \cr
#'
#' @details
#' \strong{Model M1 (SD) Derived}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{clfpw.png} \cr
#'  }
#' }
#' \eqn{CLFP = Apparent clearance (predicted) of drug for extravascular routes of administration} \cr
#' \eqn{NORMBS = BW = Body weight} \cr
#'
#' @param clfp Apparent clearance (observed) of drug (numeric value)
#' @param normbs The dose data (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item CLFPW: The total body clearance
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
clfpw <- function(clfp = NULL, normbs = NULL){
  if(is.null(clfp) && is.null(normbs)){
    stop("Error in clfpw: 'clfp' and 'normbs' vectors are NULL")
  } else if(is.null(clfp)) {
    stop("Error in clfpw: 'clfp' vector is NULL")
  } else if(is.null(normbs)) {
    stop("Error in clfpw: 'normbs' vectors is NULL")
  }

  if(length(clfp) != length(normbs) ){
    stop("Error in clfpw: length of vector arguments do not match")
  }

  if(is.na(clfp) || is.na(normbs)) {
    clf_pw <- NA
  } else {
    clf_pw <- clfp/normbs
  }

  return(clf_pw)
}
