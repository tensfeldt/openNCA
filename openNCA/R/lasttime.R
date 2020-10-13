#' Time of Last Measurable Concentration
#'
#' This is the time of the last measured concentration point of the profile INCLUDING any points at
#' or below the Lower limit of Quantitation, BLQ's. This observation can be later than TLAST. \cr
#'
#' @details LASTTIME serves, in one use case as a criteria for acceptability or quality flags to ensure
#' there's sufficient data to report certain parameters, for example, AUCTAU. If AUCTAU not of
#' sufficient quality, then analyst would also not likely report CL. \cr
#' The function will ensure that the concentration data and the time data provided are numeric vectors and
#' are also of the same length. \cr
#'
#' @param conc The concentration data (given in a vector form)
#' @param time The time data (given in a vector form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item LASTTIME: time at last measurable concentration
#' }
#'
#' @examples
#' ##########
#' ## Data ##
#' #################################
#' ##  SID  ##  TIME  ##   CONC   ##
#' #################################
#' ##   30  ##    0   ##   2.89   ##
#' ##   30  ##    1   ##   2.49   ##
#' ##   30  ##    2   ##   2.47   ##
#' #################################
#'
#' #Data mentioned will be used for the following example
#'
#' #tlast()
#' #Error in tlast: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- c(2.89, 2.49, 2.47)
#' time_vector <- c(0, 1, 2)
#'
#' lasttime(conc = conc_vector, time = time_vector)
#' #2
#'
#' ############
#' ## Data 2 ##
#' #################################
#' ##  SID  ##  TIME  ##   CONC   ##
#' #################################
#' ##   31  ##    0   ##      0   ##
#' ##   31  ##    1   ##      0   ##
#' ##   31  ##    2   ##      0   ##
#' #################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(0, 0, 0)
#' time_vector <- c(0, 1, 2)
#'
#' lasttime(conc = conc_vector, time = time_vector)
#' #NA
#'
#' ############
#' ## Data 3 ##
#' #################################
#' ##  SID  ##  TIME  ##   CONC   ##
#' #################################
#' ##   32  ##    0   ##   1.19   ##
#' ##   32  ##    1   ##   1.23   ##
#' ##   32  ##    2   ##   1.34   ##
#' ##   32  ##    3   ##   NA     ##
#' #################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(1.19, 1.23, 1.34, NA)
#' time_vector <- c(0, 1, 2, 3)
#'
#' lasttime(conc = conc_vector, time = time_vector)
#' #2
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
lasttime <- function(conc = NULL, time = NULL){
  if(is.null(conc) && is.null(time)){
    stop("Error in lasttime: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in lasttime: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in lasttime: 'time' vectors is NULL")
  } else if(all(is.na(time))) { # 2019-09-13/TGT/
      return(NA)
  } else if(all(is.na(conc))) { # 2019-09-13/TGT/
      return(NA)
  }

  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in lasttime: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc) ){
    stop("Error in lasttime: length of 'time' and 'conc' vectors are not equal")
  }

  tmp <- data.frame(time, conc)
  last_time <- NA

  # Find the maximum concentration value that does not have a NULL
  tmp_df <- tmp[!is.na(tmp$conc),]
  if(nrow(tmp_df) > 0){
    last_time <- tmp_df[nrow(tmp_df),]$time
  } else {
    last_time <- NA
  }
  return(last_time)
}
