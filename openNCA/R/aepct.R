#' Cumulative total amount of drug recovered unchanged in the urine, from time zero to infinity,
#' expressed as percentage of administered dose
#'
#' @details
#' \strong{Model M4, Derived} \cr
#' \strong{Equation} \cr
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{aepct.png} \cr
#'  }
#'  \eqn{AE = Cumulative total amount of drug from time zero to infinity (wt)} \cr
#'  \eqn{Dose = administered dose  (wt)} \cr
#' }
#'
#' @section Note:
#' \strong{ae}: Refer to \code{\link{ae}} for more details
#'
#' @param ae Cumulative total amount of drug
#' @param dose The dose data (given in a vector form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item AEPCT: Cumulative amount of drug in percentage
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
#' #aepct()
#' #Error in aepct: 'amt' and 'time' vectors are NULL
#'
#' amt_vector <- c(2.89, 2.49, 2.47)
#' time_vector <- c(0, 1, 2)
#' ae_vector <- amt_vector
#' dose_vector <- c(400, 450, 500)
#'
#' aepct(ae = ae_vector, dose = dose_vector)
#' #0.7225000 0.5533333 0.4940000
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
#' ae_vector <- amt_vector
#' dose_vector <- c(400, 450, 500)
#'
#' aepct(ae = ae_vector, dose = dose_vector)
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
#' ae_vector <- amt_vector
#' dose_vector <- c(400, 450, 500, 520)
#'
#' aepct(ae = ae_vector, dose = dose_vector)
#' #0.2975000 0.2733333 0.2680000 0.2538462
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
aepct <- function(ae = NULL, dose = NULL){
  if(is.null(ae) && is.null(dose)) {
    stop("Error in aepct: 'ae' and 'dose' vectors are NULL")
  } else if(is.null(ae)) {
    stop("Error in aepct: 'ae' vector is NULL")
  } else if(is.null(dose)) {
    stop("Error in aepct: 'dose' vector is NULL")
  } else if(all(is.na(ae))) { # 2019-11-24/RD/
    return(NA)
  } else if(all(is.na(dose))) { # 2019-11-24/RD/
    return(NA)
  }

  if(!(is.numeric(ae) && is.vector(ae))){
    stop("Error in aepct: 'ae' is not a numeric vector")
  }
  if(!(is.numeric(dose) && is.vector(dose))){
    stop("Error in aepct: 'dose' is not a numeric vector")
  }

  if(is.na(dose) || (0 %in% dose) || is.na(ae)) {
    ae_pct <- NA
  } else {
    ae_pct <- (ae/dose) * 100
    ae_pct <- replace(ae_pct, is.infinite(ae_pct), NA)
  }

  return(ae_pct)
}
