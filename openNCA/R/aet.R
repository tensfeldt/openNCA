#' Cumulative (running sum) amount of drug recovered unchanged in the urine up to time t post-dose,
#' where t can have a numerical value.
#'
#' @details
#' \strong{Model M4} \cr
#' \strong{Equation} \cr
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{aet.png} \cr
#'  }
#'  \eqn{AET = cumulative amount of drug (wt) up to time t post dose} \cr
#'  \eqn{ATt = amount of drug (wt), where t can have a numerical value} \cr
#' }
#'
#' @param amt The amount data (given in a vector form) either represents the volume or weight
#' @param time The time data (given in a vector form)
#' @param t The post-dose time value (numeric value)
#' @param orig_time The original time data (given in a vector form)
#' @param curr_time The current time data (given in a vector form)
#' @param all_time The time data for given profile (given in a vector form)
#' @param end_time The end time data (given in a vector form)
#' @param returnNA Determines if NAs should be returned or not (given in a logical form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item AET: Cumulative amount of drug
#' }
#'
#' @examples
#' ##########
#' ## Data ##
#' #################################
#' ##  SID  ##  TIME  ##  AMOUNT  ##
#' #################################
#' ##   30  ##    0   ##   2.89   ##
#' ##   30  ##    1   ##   2.49   ##
#' ##   30  ##    2   ##   2.47   ##
#' #################################
#'
#' #Data mentioned will be used for the following example
#'
#' #aet()
#' #Error in aet: 'amt' and 'time' vectors are NULL
#'
#' amt_vector <- c(2.89, 2.49, 2.47)
#' time_vector <- c(0, 1, 2)
#'
#' aet(amt = amt_vector, time = time_vector, t = 1)
#' #5.38
#'
#' aet(amt = amt_vector, time = time_vector, t = 2)
#' #7.85
#'
#' ############
#' ## Data 2 ##
#' #################################
#' ##  SID  ##  TIME  ##  AMOUNT  ##
#' #################################
#' ##   31  ##    0   ##     0    ##
#' ##   31  ##    1   ##     0    ##
#' ##   31  ##    2   ##     0    ##
#' #################################
#'
#' #Data mentioned will be used for the following example
#'
#' amt_vector <- c(0, 0, 0)
#' time_vector <- c(0, 1, 2)
#'
#' aet(amt = amt_vector, time = time_vector, t = 1)
#' #0
#'
#' ############
#' ## Data 3 ##
#' #################################
#' ##  SID  ##  TIME  ##  AMOUNT  ##
#' #################################
#' ##   32  ##    0   ##   1.19   ##
#' ##   32  ##    1   ##   1.23   ##
#' ##   32  ##    2   ##   1.34   ##
#' ##   32  ## "None" ##   1.32   ##
#' #################################
#'
#' #Data mentioned will be used for the following example
#'
#' amt_vector <- c(1.19, 1.23, 1.34, 1.32)
#' time_vector <- c(0, 1, 2, "None")
#'
#' aet(amt = amt_vector, time = time_vector, t = 2)
#' #3.76
#'
#' aet(amt = amt_vector, time = time_vector, t = 2)
#' #Error in aet: value 't' is not present in 'time' vector
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
aet <- function(amt = NULL, time = NULL, t = NULL, orig_time=NULL, curr_time=NULL, all_time=NULL, end_time=NULL, returnNA=FALSE){
  if(is.null(amt) && is.null(time) && is.null(t)) {
    stop("Error in aet: 'amt' and 'time' vectors and value 't' are NULL")
  } else if(is.null(amt) && is.null(time)) {
    stop("Error in aet: 'amt' and 'time' vectors are NULL")
  } else if(is.null(amt) && is.null(t)) {
    stop("Error in aet: 'amt' vector and value 't' are NULL")
  } else if(is.null(time) && is.null(t)) {
    stop("Error in aet: 'time' vector and value 't' are NULL")
  } else if(is.null(amt)) {
    stop("Error in aet: 'amt' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in aet: 'time' vector is NULL")
  } else if(is.null(t)) {
    stop("Error in aet: value 't' is NULL")
  } else if(all(is.na(time))) { # 2019-11-24/RD/
    return(NA)
  } else if(all(is.na(amt))) { # 2019-11-24/RD/
    return(NA)
  }
  
  if(!(is.numeric(amt) && is.vector(amt))){
    stop("Error in aet: 'amt' is not a numeric vector")
  }
  
  amt_check <- FALSE
  if(!is.null(all_time) && !is.null(curr_time)){
    tmp_all_rows_tmp <- apply(all_time, 1, function(x) { 
      tmp <- NA
      apply(curr_time, 1, function(y) {
        test_match <- TRUE
        for(i in 1:length(all_time)){
          if(x[i] != y[i]){
            test_match <- FALSE
            return(test_match)
          }
        }
        if(isTRUE(test_match)){
          return(test_match)
        }
      })
    })
    tmp_all_rows <- c()
    if(is.matrix(tmp_all_rows_tmp)){
      for(j in 1:ncol(tmp_all_rows_tmp)){
        tmp_all_rows <- c(tmp_all_rows, any(tmp_all_rows_tmp[,j]))
      }
    } else {
      tmp_all_rows <- tmp_all_rows_tmp
    }
    tmp_end_time <- all_time[tmp_all_rows,][,2]
    tmp_rows <- end_time %in% tmp_end_time
    tmp_time <- end_time[tmp_rows] %in% t
    tmp_lim <- seq(1:length(end_time[tmp_rows]))[tmp_time]
    if(length(tmp_lim) > 0){
      tmp_lim <- max(tmp_lim)
      tmp_time[1:tmp_lim] <- TRUE
    }
    amt_check <- ifelse(any(tmp_rows), ifelse(all(is.na(amt[tmp_rows])), TRUE, FALSE), TRUE)
  }
    
  if(isTRUE(amt_check)) {
    a_e <- NA
  } else {
    if(t %in% time) {
      if(is.null(all_time) || is.null(curr_time)){
        tmp_time <- time[time <= t]
        if(length(tmp_time) > 0) {
          tmp_amt <- amt[1:length(tmp_time)]
        } else {
          stop("Error in aet: value 't' cannot be used to subset 'time' vector")
        }
        if(is.logical(returnNA) && isTRUE(returnNA)){
          a_e <- sum(tmp_amt[!is.na(tmp_time)][1:length(tmp_time)])
        } else {
          a_e <- sum(tmp_amt[!is.na(tmp_time)][1:length(tmp_time)], na.rm = TRUE)
          a_e <- ifelse(a_e == 0, NA, a_e)
        }
      } else {
        if(any(tmp_time)) {
          tmp_amt <- amt[tmp_rows][tmp_time]
          if(is.logical(returnNA) && isTRUE(returnNA)){
            a_e <- sum(tmp_amt)
          } else {
            a_e <- sum(tmp_amt, na.rm = TRUE)
            #a_e <- ifelse(a_e == 0, NA, a_e)
          }
        } else {
          a_e <- NA
        }
      }
    } else {
      a_e <- NA
    }
  }
  return(a_e)
}
