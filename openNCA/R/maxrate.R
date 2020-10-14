#' Maximum observed excretion rate
#'
#' @param rate The rate data (given in a vector form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item MAXRATE: maximum rate
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
maxrate <- function(rate = NULL){
  if(is.null(rate)) {
    stop("Error in maxrate: 'rate' vector is NULL")
  } else if(all(is.na(rate))) { # 2019-11-24/RD/
    return(NA)
  }
  
  if(!(is.numeric(rate) && is.vector(rate))){
    stop("Error in maxrate: 'rate' is not a numeric vector")
  }
  
  max_rate <- max(rate, na.rm = TRUE)
  if(is.infinite(max_rate)){
    max_rate <- NA
  }

  return(max_rate)
}
