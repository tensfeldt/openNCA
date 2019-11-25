#' Midpoint of collection interval associated with last measurable (nonzero) rate
#' 
#' @param midpt The midpoint data (given in a vector form)
#' @param rate The rate data (given in a vector form)
#' 
#' @section Returns:
#' \strong{Value} \cr 
#' \itemize{
#'  \item MIDPTLAST: last measureable rate
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
midptlast <- function(midpt = NULL, rate = NULL){
  if(is.null(midpt) && is.null(rate)) {
    stop("Error in midptlast: 'midpt' vector and value 'rate' are NULL")
  } else if(is.null(midpt)) {
    stop("Error in midptlast: 'midpt' vector is NULL")
  } else if(is.null(rate)) {
    stop("Error in midptlast: 'rate' vector is NULL")
  } else if(all(is.na(midpt))) { # 2019-11-24/RD/
    return(NA)
  } else if(all(is.na(rate))) { # 2019-11-24/RD/
    return(NA)
  }
  
  
  if(!(is.numeric(midpt) && is.vector(midpt))){
    stop("Error in midptlast: 'midpt' is not a numeric vector")
  }
  if(!(is.numeric(rate) && is.vector(rate))){
    stop("Error in midptlast: 'rate' is not a numeric vector")
  }
  if(length(midpt) != length(rate)){
    stop("Error in midptlast: length of 'midpt' and 'rate' vectors are not equal")
  }
  
  midpt_last <- midpt[length(rate[rate > 0])]
  return(midpt_last)
}
