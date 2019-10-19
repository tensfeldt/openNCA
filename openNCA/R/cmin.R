#' Minimum Observed Concentration
#'
#' This function gets the minimum observed concentration that is obtained by the inspection of the data.
#' It is designed as the first occurance within a dosing interval. In the case of multiple dosing, CMINi
#' is obtained by inspection of the data during each dosing interval i.
#'
#' @details If all the concentrations are 0's then CMINi will return NA.
#' Also the interval must be in the range of the times listed in the data. \cr
#' The function will ensure that the concentration data and the time data provided are numeric vectors and
#' are also of the same length. \cr
#'
#' @param conc The concentration data (given in a vector form)
#' @param time The time data (given in a vector form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item CMIN: minimum observed concentration
#' }
#'
#' @examples
#' ##########
#' ## Data ##
#' ###################################
#' ##  SDEID  ##  TIME  ##   CONC   ##
#' ###################################
#' ##   30    ##    0   ##   2.89   ##
#' ##   30    ##    1   ##   2.49   ##
#' ##   30    ##    2   ##   2.47   ##
#' ###################################
#'
#' #Data mentioned will be used for the following example
#'
#' cmin()
#' #Error in cmin: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- c(2.89,2.49, 2.47)
#' time_vector <- c(0, 1, 2)
#'
#' cmin(conc = conc_vector, time = time_vector)
#' #2.47
#'
#' ############
#' ## Data 2 ##
#' ###################################
#' ##  SDEID  ##  TIME  ##   CONC   ##
#' ###################################
#' ##   31    ##    0   ##      0   ##
#' ##   31    ##    1   ##      0   ##
#' ##   31    ##    2   ##      0   ##
#' ###################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(0, 0, 0)
#' time_vector <- c(0, 1, 2)
#'
#' cmin(conc = conc_vector, time = time_vector)
#' #NA
#'
#' ############
#' ## Data 3 ##
#' ###################################
#' ##  SDEID  ##  TIME  ##   CONC   ##
#' ###################################
#' ##   32    ##    0   ##   1.19   ##
#' ##   32    ##    1   ##   1.23   ##
#' ##   32    ##    2   ##   1.34   ##
#' ##   32    ## "None" ##   1.32   ##
#' ###################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(1.19, 1.23, 1.34, 1.32)
#' time_vector <- c(0, 1, 2, "None")
#'
#' cmin(conc = conc_vector, time = time_vector)
#' #Error in cmin: 'time' is not a numeric vector
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
cmin <- function(conc = NULL, time = NULL){
  if(is.null(conc) && is.null(time)){
    stop("Error in cmin: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in cmin: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in cmin: 'time' vectors is NULL")
  } else if(all(is.na(time))) { # 2019-09-13/TGT/
      return(NA)
  } else if(all(is.na(conc))) { # 2019-09-13/TGT/
      return(NA)
  }

  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in cmin: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in cmin: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc) ){
    stop("Error in cmin: length of 'time' and 'conc' vectors are not equal")
  }

  tmp <- data.frame(time, conc)
  c_min <- NA
  if(nrow(tmp) < 1){
    return(c_min)
  }
  if(sum(tmp$conc, na.rm=T) == 0){
    return(c_min)
  }
  # Find the maximum concentration value and ignore NA values.
  c_min <- min(tmp$conc, na.rm = TRUE)
  return(c_min)
}
