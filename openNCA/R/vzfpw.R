#' The volume of distribution divided by weight
#'
#' The volume of distribution associated with the terminal slope following extravascular administration
#' divided by the fraction of dose absorbed, calculated using the predicted value of the last non-zero
#' concentration, divided by the body weight of the subject  \cr
#'
#' @details
#' \strong{Model M1 (SD) Derived}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{vzfpw.png} \cr
#'  }
#' }
#' \eqn{VZFP = The volume of distribution associated with the terminal slope following extravascular administration, calculated using the predicted value of the last non-zero concentration} \cr
#' \eqn{NORMBS = BW = Body weight} \cr
#'
#' @section Additional Details:
#'
#' @param vzfp The volume of distribution data (numeric value)
#' @param normbs The dose data (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item VZFPW: The volume of distribution divided by weight
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
vzfpw <- function(vzfp = NULL, normbs = NULL){
  if(is.null(vzfp) && is.null(normbs)){
    stop("Error in vzfpw: 'vzfp' and 'normbs' vectors are NULL")
  } else if(is.null(clfp)) {
    stop("Error in vzfpw: 'vzfp' vector is NULL")
  } else if(is.null(normbs)) {
    stop("Error in vzfpw: 'normbs' vectors is NULL")
  }

  if(length(vzfp) != length(normbs) ){
    stop("Error in vzfpw: length of vector arguments do not match")
  }

  if(is.na(vzfp) || is.na(normbs)) {
    vzf_pw <- NA
  } else {
    vzf_pw <- vzfp/normbs
  }

  return(vzf_pw)
}
