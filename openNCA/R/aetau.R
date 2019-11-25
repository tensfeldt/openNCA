#' Cumulative amount of drug recovered unchanged in the urine during a dosing interval i post dose.
#' i refers to the value of NDOSEI.
#'
#' @details
#' \strong{Model M4} \cr
#' \strong{Equation} \cr
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{aetau.png} \cr
#'  }
#'  \eqn{AETAUi = Cumulative amount at of drug during a dosing interval (wt)} \cr
#'  \eqn{ATt = amount of drug (wt), where t can have a numerical value} \cr
#'  \eqn{ATtau = amount of drug (wt) at end of the dosing interval} \cr
#' }
#'
#' @section Note:
#' \strong{aet}: Refer to \code{\link{aet}} for more details
#'
#' @param aet Cumulative (running sum) amount of drug
#' @param time The time data (given in a vector form)
#' @param t The post-dose time value (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item AETAU: Cumulative amount of drug
#' }
#'
#' @examples
#'  ##########
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
#' #aetau()
#' #Error in aetau: 'aet' and 'time' vectors and value 't' are NULL
#'
#' amt_vector <- c(2.89, 2.49, 2.47)
#' time_vector <- c(0, 1, 2)
#'
#' aet(amt = amt_vector, time = time_vector, t = 0)
#' #2.89
#'
#' aet(amt = amt_vector, time = time_vector, t = 1)
#' #5.38
#'
#' aet(amt = amt_vector, time = time_vector, t = 2)
#' #7.85
#'
#' aet_vector <- c(2.89, 5.38, 7.85)
#'
#' aetau(aet = aet_vector, time = time_vector, t = 1)
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
aetau <- function(aet = NULL, time = NULL, t = NULL){
  if(is.null(aet) && is.null(time) && is.null(t)) {
    stop("Error in aetau: 'aet' and 'time' vectors and value 't' are NULL")
  } else if(is.null(aet) && is.null(time)) {
    stop("Error in aetau: 'aet' and 'time' vectors are NULL")
  } else if(is.null(aet) && is.null(t)) {
    stop("Error in aetau: 'aet' vector and value 't' are NULL")
  } else if(is.null(time) && is.null(t)) {
    stop("Error in aetau: 'time' vector and value 't' are NULL")
  } else if(is.null(aet)) {
    stop("Error in aetau: 'aet' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in aetau: 'time' vector is NULL")
  } else if(is.null(t)) {
    stop("Error in aetau: value 't' is NULL")
  } else if(all(is.na(time))) { # 2019-11-24/RD/
    return(NA)
  } else if(all(is.na(aet))) { # 2019-11-24/RD/
    return(NA)
  }

  if(!(is.numeric(aet) && is.vector(aet))){
    stop("Error in aetau: 'amt' is not a numeric vector")
  }

  if(any(is.na(aet))) {
    ae_tau <- NA
  } else {
    if(t %in% time) {
      tmp_time <- time[time <= t]
      if(length(tmp_time) > 0) {
        tmp_aet <- aet[1:length(tmp_time)]
      } else {
        stop("Error in aetau: value 't' cannot be used to subset 'time' vector")
      }
      ae_tau <- sum(tmp_aet[!is.na(tmp_time)])
    } else {
      stop("Error in aetau: value 't' is not present in 'time' vector")
    }
  }

  return(ae_tau)
}
