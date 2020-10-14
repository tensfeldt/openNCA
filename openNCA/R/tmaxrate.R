#' Midpoint of collection interval associated with the maximum observed excretion rate
#' 
#' @details If all the RATE's are 0's then RATELAST will return 'NA'. 
#'
#' @param midpt The midpoint data (given in a vector form)
#' @param rate The rate data (given in a vector form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item TMAXRATE: maximum observed excretion rate
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
tmaxrate <- function(midpt = NULL, rate = NULL){
  if(is.null(midpt) && is.null(rate)) {
    stop("Error in tmaxrate: 'midpt' vector and value 'rate' are NULL")
  } else if(is.null(midpt)) {
    stop("Error in tmaxrate: 'midpt' vector is NULL")
  } else if(is.null(rate)) {
    stop("Error in tmaxrate: 'rate' vector is NULL")
  } else if(all(is.na(midpt))) { # 2019-11-24/RD/
    return(NA)
  } else if(all(is.na(rate))) { # 2019-11-24/RD/
    return(NA)
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

  tmax_rate <- midpt[rate %in% max(rate, na.rm = TRUE)][1]
  return(tmax_rate)
}
