#' Total amount of drug recovered unchanged in the urine, from time zero to infinity
#'
#' @details
#' \strong{Model M4} \cr
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{ae.png} \cr
#'  }
#'  \eqn{AE = cumulative total amount from time zero to infinity (wt)} \cr
#'  \eqn{ATt = amount of drug (wt), where t can have a numerical value} \cr
#'  \eqn{ATlast = Last measurable amount of drug (wt)} \cr
#' }
#'
#' @param amt The amount data (given in a vector form)
#' @param time The time data (given in a vector form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item AE: total amount of drug recovered
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
#' #ae()
#' #Error in ae: 'amt' and 'time' vectors are NULL
#'
#' amt_vector <- c(2.89, 2.49, 2.47)
#' time_vector <- c(0, 1, 2)
#'
#' ae(amt = amt_vector, time = time_vector)
#' #7.85
#'
#' ############
#' ## Data 2 ##
#' #################################
#' ##  SID  ##  TIME  ##  AMOUNT  ##
#' #################################
#' ##   31  ##    0   ##      0   ##
#' ##   31  ##    1   ##      0   ##
#' ##   31  ##    2   ##      0   ##
#' #################################
#'
#' #Data mentioned will be used for the following example
#'
#' amt_vector <- c(0, 0, 0)
#' time_vector <- c(0, 1, 2)
#'
#' ae(amt = amt_vector, time = time_vector)
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
#' ae(amt = amt_vector, time = time_vector)
#' #Error in ae: 'time' is not a numeric vector
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
ae <- function(amt = NULL, time = NULL){
  if(is.null(amt) && is.null(time)) {
    stop("Error in ae: 'amt' and 'time' vectors is NULL")
  } else if(is.null(amt)) {
    stop("Error in ae: 'amt' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in ae: 'time' vector is NULL")
  } else if(all(is.na(time))) { # 2019-11-24/RD/
    return(NA)
  } else if(all(is.na(amt))) { # 2019-11-24/RD/
    return(NA)
  } 
  
  if(!(is.numeric(amt) && is.vector(amt))){
    stop("Error in ae: 'amt' is not a numeric vector")
  }

  if(any(is.na(amt))) {
    a_e <- NA
  } else {
    a_e <- sum(amt[!is.na(time)])
  }

  return(a_e)
}
