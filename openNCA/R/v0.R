#' Initial Volume of Distribution
#'
#' @details
#' \strong{Model M2 (IV Bolus)} \cr
#' \figure{v_0.png} \cr
#' 
#' @section Note:
#' \strong{c0}: Refer to \code{\link{c0}} for more details \cr
#'
#' @param c0 The c0 data (given in a vector form)
#' @param dose The dose data (given in a vector form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item V0: initial volume of distribution
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
v0 <- function(c0 = NULL, dose = NULL){
  if(is.null(c0) && is.null(dose)){
    stop("Error in v0: 'c0' and 'dose' vectors are NULL")
  } else if(is.null(c0)) {
    stop("Error in v0: 'c0' vector is NULL")
  } else if(is.null(dose)) {
    stop("Error in v0: 'dose' vectors is NULL")
  } else if(is.na(c0)) {   # 2019-09-16/TGT/
      return(NA)
  } else if(is.na(dose)) { # 2019-09-16/TGT/
      return(NA)
  }

  if(!(is.numeric(c0) && is.vector(c0)) ){
    stop("Error in v0: 'c0' is not a numeric vector")
  }
  if(!(is.numeric(dose) && is.vector(dose)) ){
    stop("Error in v0: 'dose' is not a numeric vector")
  }
  if(length(dose) != length(c0) ){
    stop("Error in v0: length of 'dose' and 'c0' vectors are not equal")
  }

  if(is.na(dose) || (0 %in% dose) || is.na(c0)) {
    v_0 <- NA
  } else {
    v_0 <- dose/c0
    v_0 <- replace(v_0, is.infinite(v_0), NA)
  }

  return(v_0)
}
