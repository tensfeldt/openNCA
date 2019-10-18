#' Last measurable (nonzero) rate
#' 
#' @details If all the RATE's are 0's then RATELAST will return 'NA'. 
#' 
#' @param rate The rate data (given in a vector form)
#' 
#' @section Returns:
#' \strong{Value} \cr 
#' \itemize{
#'  \item RATELAST: last measureable rate
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
ratelast <- function(rate = NULL){
  if(is.null(rate)) {
    stop("Error in ratelast: 'rate' vector is NULL")
  }
  
  if(!(is.numeric(rate) && is.vector(rate)) ){
    stop("Error in tlast: 'conc' is not a numeric vector")
  }
  
  tmp <- rate[!is.na(rate)]
  tmp <- tmp[tmp > 0]
  rate_last <- tmp[length(tmp)]
  return(rate_last)
}
