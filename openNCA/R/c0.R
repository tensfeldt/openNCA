#' Initial concentration resulting immediately after an IV bolus administration.
#'
#' @param conc The concentration data (given in a vector form)
#' @param time The time data (given in a vector form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item C0: initial concentration
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
#' ##   30  ##    3   ##   2.38   ##
#' ##   30  ##    4   ##   2.32   ##
#' ##   30  ##    5   ##   2.28   ##
#' #################################
#'
#' #data <- data.frame(
#' #    SID = ...,
#' #    TIME = ...,
#' #    RESULT = ...
#' #)
#' data <- data.frame(SID=rep(30, 6), TIME = c(0,1,2,3,4,5), CONC=c(2.89,2.49,2.47,2.38,2.32,2.28))
#' #Same data as above, just represented as a dataframe
#'
#' #c0()
#' #Error in c0: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- data$CONC
#' time_vector <- data$TIME
#'
#' c0(conc = conc_vector, time = time_vector)
#' #2.89
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
#' #data2 <- data.frame(
#' #    SID = ...,
#' #    TIME = ...,
#' #    RESULT = ...
#' #)
#' data2 <- data.frame(SID=rep(31,3), TIME=c(0,1,2), CONC=rep(0,3))
#' #Same data as above, just represented as a dataframe
#'
#' conc_vector <- data2$CONC
#' time_vector <- data2$TIME
#'
#' c0(conc = conc_vector, time = time_vector)
#' #0
#'
#' ############
#' ## Data 3 ##
#' #################################
#' ##  SID  ##  TIME  ##   CONC   ##
#' #################################
#' ##   32  ##    0   ##   1.19   ##
#' ##   32  ##    1   ##   1.23   ##
#' ##   32  ##    2   ##   1.34   ##
#' #################################
#'
#' #data3 <- data.frame(
#' #    SID = ...,
#' #    TIME = ...,
#' #    RESULT = ...
#' #)
#' data3 <- data.frame(SID=rep(32,3), TIME=c(0,1,2), CONC=c(1.19,1.23,1.34))
#' #Same data as above, just represented as a dataframe
#'
#' conc_vector <- data3$CONC
#' time_vector <- data3$TIME
#'
#' c0(conc = conc_vector, time = time_vector)
#' #1.19
#' 
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
c0 <- function(conc = NULL, time = NULL){
  if(is.null(conc) && is.null(time)){
    stop("Error in c0: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in c0: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in c0: 'time' vectors is NULL")
  } else if(all(is.na(time))) { # 2019-09-13/TGT/
      return(NA)
  } else if(all(is.na(conc))) { # 2019-09-13/TGT/
      return(NA)
  }

  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in c0: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in c0: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc) ){
    stop("Error in c0: length of 'time' and 'conc' vectors are not equal")
  }

  tmp <- data.frame(time, conc)
  c_0 <- NA
  if(nrow(tmp) < 1){
    return(c_0)
  }
  if(sum(tmp$conc, na.rm=T) == 0){
    return(0)
  }
  # Find the initial concentration value and ignore NA values (SD only at the moment).
  c_0 <- na.omit(conc)[1]
  return(c_0)
}
