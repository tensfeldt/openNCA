#' Midpoint of collection interval associated with the maximum observed excretion rate
#' 
#' @details If all the RATE's are 0's then RATELAST will return 'NA'. 
#'
#' @param rate The rate data (given in a vector form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item TMAXRATE: maximum observed excretion rate
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
tmaxrate <- function(midpt = NULL, rate = NULL){
  if(is.null(midpt) && is.null(rate)) {
    stop("Error in tmaxrate: 'midpt' vector and value 'rate' are NULL")
  } else if(is.null(midpt)) {
    stop("Error in tmaxrate: 'midpt' vector is NULL")
  } else if(is.null(rate)) {
    stop("Error in tmaxrate: 'rate' vector is NULL")
  }
  if(!(is.numeric(midpt) && is.vector(midpt))){
    stop("Error in tmaxrate: 'midpt' is not a numeric vector")
  }
  if(!(is.numeric(rate) && is.vector(rate))){
    stop("Error in tmaxrate: 'rate' is not a numeric vector")
  }
  if(length(midpt) != length(rate)){
    stop("Error in tmaxrate: length of 'midpt' and 'rate' vectors are not equal")
  }

  tmax_rate <- midpt[rate %in% max(rate)]
  return(tmax_rate)
}
