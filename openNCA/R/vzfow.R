#' The volume of distribution divided by weight
#'
#' The volume of distribution associated with the terminal slope following extravascular administration
#' divided by the fraction of dose absorbed, calculated using the observed value of the last non-zero
#' concentration, divided by the body weight of the subject. \cr
#'
#' @details
#' \strong{Model M1 (SD) Derived}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{vzfow.png} \cr
#'  }
#' }
#' \eqn{VZFO = The volume of distribution associated with the terminal slope following extravascular administration, calculated using the observed value of the last non-zero concentration} \cr
#' \eqn{NORMBS = BW = Body weight} \cr
#'
#' @param vzfo The The volume of distribution data (numeric value)
#' @param normbs The dose data (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item VZFOW: The volume of distribution divided by weight
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
vzfow <- function(vzfo = NULL, normbs = NULL){
  if(is.null(vzfo) && is.null(normbs)){
    stop("Error in vzfow: 'vzfo' and 'normbs' vectors are NULL")
  } else if(is.na(vzfo) || is.na(normbs)) { # 2019-09-13/TGT/
      return(NA)
  } else if(is.null(vzfo)) {
    stop("Error in vzfow: 'vzfo' vector is NULL")
  } else if(is.null(normbs)) {
    stop("Error in vzfow: 'normbs' vectors is NULL")
  }

  if(length(vzfo) != length(normbs) ){
    stop("Error in vzfow: length of vector arguments do not match")
  }

  if(is.na(vzfo) || is.na(normbs)) {
    vzf_ow <- NA
  } else {
    vzf_ow <- vzfo/normbs
  }

  return(vzf_ow)
}
