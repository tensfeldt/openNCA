#' The total body clearance
#'
#' The total body clearance for extravascular administration divided by the fraction of dose absorbed,
#' calculated using AUCTAU, divided by the weight
#'
#' @details
#' \strong{Model M1 (SS) Derived}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{clftauw.png} \cr
#'  }
#' }
#' \eqn{CLFTAUi = The total body clearance for extravascular administration divided by the fraction of dose absorbed, calculated using AUCTAUi for the ith dosing interval} \cr
#' \eqn{NORMBS = BW = Body weight} \cr
#'
#' @param clftau The total body clearance (numeric value)
#' @param normbs The dose data (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item CLFTAUW: The total body clearance divided by weight
#' }
#'
#' @examples
#' #clftauw()
#' #Error in clftauw: 'clftau' and 'normbs' vectors are NULL
#' 
#' clftauw(clftau = 10.34535, normbs = 1.234)
#' #8.38359
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
clftauw <- function(clftau = NULL, normbs = NULL){
  if(is.null(clftau) && is.null(normbs)){
    stop("Error in clftauw: 'clftau' and 'normbs' vectors are NULL")
  } else if(is.null(clftau)) {
    stop("Error in clftauw: 'clftau' vector is NULL")
  } else if(is.null(normbs)) {
    stop("Error in clftauw: 'normbs' vectors is NULL")
  }

  if(length(clftau) != length(normbs) ){
    stop("Error in clftauw: length of vector arguments do not match")
  }

  if(is.na(clftau) || is.na(normbs)) {
    clf_tauw <- NA
  } else {
    clf_tauw <- clftau/normbs
  }

  return(clf_tauw)
}
