#' Cumulative amount of drug recovered unchanged in the urine up to time T post-dose, where t
#' can have a numerical value, expressed as percentage of administered dose.
#'
#' @details
#' \strong{Model M4, Derived} \cr
#' \strong{Equation} \cr
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{aetpct.png} \cr
#'  }
#'  \eqn{AET = cumulative amount at of drug at time t (wt)} \cr
#'  \eqn{Dose = administered dose  (wt)} \cr
#' }
#'
#' @section Note:
#' \strong{aet}: Refer to \code{\link{aet}} for more details
#'
#' @param aet Cumulative (running sum) amount of drug
#' @param dose The dose data (given in a vector form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item AETPCT: Cumulative amount of drug in percentage
#' }
#'
#' @examples
#' ##########
#' ## Data ##
#' ############################################
#' ##  SID  ##  TIME  ##  AMOUNT  ##   DOSE  ##
#' ############################################
#' ##   30  ##    0   ##   2.89   ##   400   ##
#' ##   30  ##    1   ##   2.49   ##   450   ##
#' ##   30  ##    2   ##   2.47   ##   500   ##
#' ############################################
#'
#' #Data mentioned will be used for the following example
#'
#' #aetpct()
#' #Error in aetpct: 'aet' and 'dose' vectors are NULL
#'
#' amt_vector <- c(2.89, 2.49, 2.47)
#' time_vector <- c(0, 1, 2)
#' dose_vector <- c(400, 450, 500)
#'
#' aet(amt = amt_vector, time = time_vector, t = 1)
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
#' aetpct(aet = aet_vector, dose = dose_vector)
#' #0.722500 1.195556 1.570000
#'
#' ############
#' ## Data 2 ##
#' ############################################
#' ##  SID  ##  TIME  ##  AMOUNT  ##   DOSE  ##
#' ############################################
#' ##   31  ##    0   ##     0    ##   400   ##
#' ##   31  ##    1   ##     0    ##   450   ##
#' ##   31  ##    2   ##     0    ##   500   ##
#' ############################################
#'
#' #Data mentioned will be used for the following example
#'
#' amt_vector <- c(0, 0, 0)
#' time_vector <- c(0, 1, 2)
#' dose_vector <- c(400, 450, 500)
#' aet_vector <- c(0, 0, 0)
#'
#' aetpct(aet = aet_vector, dose = dose_vector)
#' #0 0 0
#'
#' ############
#' ## Data 3 ##
#' ############################################
#' ##  SID  ##  TIME  ##  AMOUNT  ##   DOSE  ##
#' ############################################
#' ##   32  ##    0   ##   1.19   ##   400   ##
#' ##   32  ##    1   ##   1.23   ##   450   ##
#' ##   32  ##    2   ##   1.34   ##   500   ##
#' ##   32  ## "None" ##   1.32   ##   520   ##
#' ############################################
#'
#' #Data mentioned will be used for the following example
#'
#' amt_vector <- c(1.19, 1.23, 1.34, 1.32)
#' time_vector <- c(0, 1, 2, "None")
#' dose_vector <- c(400, 450, 500, 520)
#'
#' aet(amt = amt_vector, time = time_vector, t = 0)
#' #1.19
#'
#' aet(amt = amt_vector, time = time_vector, t = 1)
#' #2.42
#'
#' aet(amt = amt_vector, time = time_vector, t = 2)
#' #3.76
#'
#' #aet(amt = amt_vector, time = time_vector, t = 3)
#' #Error in aet: value 't' is not present in 'time' vector
#'
#' aet_vector <- c(1.19, 2.42, 3.76, NA)
#'
#' aet(amt = amt_vector, time = time_vector, t = 2)
#' #3.76
#'
#' #aetpct(aet = aet_vector, dose = dose_vector)
#' #0.2975000 0.5377778 0.7520000 NA
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
aetpct <- function(aet = NULL, dose = NULL){
  if(is.null(aet) && is.null(dose)) {
    stop("Error in aetpct: 'aet' and 'dose' vectors are NULL")
  } else if(is.null(aet)) {
    stop("Error in aetpct: 'aet' vector is NULL")
  } else if(is.null(dose)) {
    stop("Error in aetpct: 'dose' vector is NULL")
  }

  if(!(is.numeric(aet) && is.vector(aet))){
    stop("Error in aetpct: 'aet' is not a numeric vector")
  }
  if(!(is.numeric(dose) && is.vector(dose))){
    stop("Error in aetpct: 'dose' is not a numeric vector")
  }

  if(is.na(dose) || (0 %in% dose) || is.na(aet)) {
    aet_pct <- NA
  } else {
    aet_pct <- (aet/dose) * 100
    aet_pct <- replace(aet_pct, is.infinite(aet_pct), NA)
  }

  return(aet_pct)
}
