#' The volume of distribution divided by the weight
#'
#' The volume of distribution associated with the terminal slope following intravascular administration,
#' calculated using the observed value of the last non-zero concentration, divided by the weight.  \cr
#'
#' @details
#' \strong{Model M2 (SD) and M3 (SD) Derived}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{vzow.png} \cr
#'  }
#' }
#' \eqn{VZO = The volume of distribution associated with the terminal slope following intravascular administration, calculated using the observed value of the last non-zero concentration} \cr
#' \eqn{NORMBS = BW = Body weight} \cr
#'
#' @section Additional Details:
#'
#' @param vzo The The volume of distribution data (given in a vector form)
#' @param normbs The dose data (given in a vector form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item VZOW: The volume of distribution divided by the weight
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
vzow <- function(vzo = NULL, normbs = NULL){
  if(is.null(vzo) && is.null(normbs)){
    stop("Error in vzow: 'vzo' and 'normbs' vectors are NULL")
  } else if(is.null(vzo)) {
    stop("Error in vzow: 'vzo' vector is NULL")
  } else if(is.null(normbs)) {
    stop("Error in vzow: 'normbs' vectors is NULL")
  }

  if(length(vzo) != length(normbs) ){
    stop("Error in vzow: length of vector arguments do not match")
  }

  if(is.na(vzo) || is.na(normbs)) {
    vz_ow <- NA
  } else {
    vz_ow <- vzo/normbs
  }

  return(vz_ow)
}
