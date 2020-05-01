#' First Time at Maximum Observed Concentration
#'
#' This function gets the first time at which CMAXi is observed within a dosing interval
#' and is obtained by the inspection of the data. In the case of multiple dosing, TMAXi is obtained
#' by inspection of the data during the dose inteval i.
#'
#' @details If all the concentrations are 0's then TMAXi will return NA.
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
#'  \item TMAX: time at maximum observed concentration
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
#' tmax()
#' #Error in tmax: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- c(2.89, 2.49, 2.47)
#' time_vector <- c(0, 1, 2)
#'
#' tmax(conc = conc_vector, time = time_vector)
#' #0
#'
#' ############
#' ## Data 2 ##
#' ###################################
#' ##  SDEID  ##  TIME  ##   CONC   ##
#' ###################################
#' ##   31    ##    0   ##     0    ##
#' ##   31    ##    1   ##     0    ##
#' ##   31    ##    2   ##     0    ##
#' ###################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(0, 0, 0)
#' time_vector <- c(0, 1, 2)
#'
#' tmax(conc = conc_vector, time = time_vector)
#' #NA
#'
#' ############
#' ## Data 3 ##
#' ###################################
#' ##  SDEID  ##  TIME  ##   CONC   ##
#' ###################################
#' ##   32    ##    0   ##   1.19   ##
#' ##   32    ##    1   ##   1.27   ##
#' ##   32    ##    2   ##   1.24   ##
#' ##   32    ##    3   ##   1.27   ##
#' ###################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(1.19, 1.27, 1.24, 1.27)
#' time_vector <- c(0, 1, 2, 3)
#'
#' tmax(conc = conc_vector, time = time_vector)
#' #1
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
tmax <- function(conc = NULL, time = NULL, told = NULL){
  if(is.null(conc) && is.null(time)){
    stop("Error in tmax: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in tmax: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in tmax: 'time' vectors is NULL")
  } else if(all(is.na(time))) { # 2019-09-13/TGT/
      return(NA)
  } else if(all(is.na(conc))) { # 2019-09-13/TGT/
      return(NA)
  }

  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in tmax: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in tmax: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc) ){
    stop("Error in tmax: length of 'time' and 'conc' vectors are not equal")
  }

  tmp <- data.frame(time, conc)
  t_max <- NA
  if(nrow(tmp) < 1){
    return(t_max)
  }
  if(sum(tmp$conc, na.rm=T) == 0){
    return(t_max)
  }
  # Find the maximum concentration value and ignore NA values.
  c_max <- max(tmp$conc, na.rm = TRUE)
  # Find the time of the max concentration
  t_max <- min(tmp[tmp$conc == c_max,]$time, na.rm = TRUE)
  if(!is.null(told) && is.numeric(told)){
    t_max <- ifelse(isTRUE(t_max < told), told, t_max)
  }
  return(t_max)
}
