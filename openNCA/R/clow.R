#' The total body clearance
#'
#' The total body clearance for intravascular administration, calculated using the observed value
#' of the last non-zero concentration, divided by the weight. \cr
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
#' @section Note:
#' \strong{clo}: Refer to \code{\link{clo}} for more details
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
#' #No appropriate examples
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
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
