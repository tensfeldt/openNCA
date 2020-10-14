#' The volume of distribution divided by the weight
#'
#' The volume of distribution associated with the terminal slope following intravascular administration,
#' calculated using the predicted value of the last non-zero concentration, divided by the weight. \cr
#'
#' @details
#' \strong{Model M2 (SD) and M3 (SD) Derived}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{vzpw.png} \cr
#'  }
#' }
#' \eqn{VZP = The volume of distribution associated with the terminal slope following intravascular administration, calculated using the predicted value of the last non-zero concentration} \cr
#' \eqn{NORMBS = BW = Body weight} \cr
#'
#' @param vzp The volume of distribution data (given in a vector form)
#' @param normbs The dose data (given in a vector form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item VZFPW: The volume of distribution divided by the weight
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
vzpw <- function(vzp = NULL, normbs = NULL){
  if(is.null(vzp) && is.null(normbs)){
    stop("Error in vzpw: 'vzp' and 'normbs' vectors are NULL")
  } else if(is.null(vzp)) {
    stop("Error in vzpw: 'vzp' vector is NULL")
  } else if(is.null(normbs)) {
    stop("Error in vzpw: 'normbs' vectors is NULL")
  }

  if(length(vzp) != length(normbs) ){
    stop("Error in vzpw: length of vector arguments do not match")
  }

  if(is.na(vzp) || is.na(normbs)) {
    vz_pw <- NA
  } else {
    vz_pw <- vzp/normbs
  }

  return(vz_pw)
}
