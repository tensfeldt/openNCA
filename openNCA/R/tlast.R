#' Time of Last Measurable (non-zero) Plasma Concentration
#'
#' This function gets the time of last measurable plasma concentration that is obtained by the
#' inspection of the data.
#'
#' @details If all the concentrations are 0's then TLAST will return NA.
#' You must provide a valid sid with respect to the data. \cr
#' The function will ensure that the concentration data and the time data provided are numeric vectors and
#' are also of the same length. \cr
#'
#' @param conc The concentration data (given in a vector form)
#' @param time The time data (given in a vector form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item TLAST: time at last measureable plasma concentration
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
#' tlast()
#' #Error in tlast: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- c(2.89, 2.49, 2.47)
#' time_vector <- c(0, 1, 2)
#'
#' tlast(conc = conc_vector, time = time_vector)
#' #2
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
#' tlast(conc = conc_vector, time = time_vector)
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
#' ##   32    ##    3   ##   NA     ##
#' ###################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(1.19, 1.23, 1.34, NA)
#' time_vector <- c(0, 1, 2, 3)
#'
#' tlast(conc = conc_vector, time = time_vector)
#' #2
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
tlast <- function(conc = NULL, time = NULL){
  if(is.null(conc) && is.null(time)){
    stop("Error in tlast: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in tlast: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in tlast: 'time' vectors is NULL")
  } else if(all(is.na(time))) { # 2019-09-13/TGT/
      return(NA)
  } else if(all(is.na(conc))) { # 2019-09-13/TGT/
      return(NA)
  }

  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in tlast: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in tlast: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc) ){
    stop("Error in tlast: length of 'time' and 'conc' vectors are not equal")
  }

  tmp <- data.frame(time, conc)
  t_last <- NA
  if(nrow(tmp) < 1){
    return(t_last)
  }
  if(sum(tmp$conc, na.rm=T) == 0){
    return(t_last)
  }
  # Find the maximum concentration value that does not have a NULL or zero value.
  tmp_df <- tmp[!is.na(tmp$conc),]
  tmp_df <- tmp_df[tmp_df$conc > 0,]
  t_last <- tmp_df[nrow(tmp_df),]$time
  return(t_last)
}
