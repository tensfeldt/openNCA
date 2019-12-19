#' Estimated concentration at time of occurrence of Time of Last Dose (TLAST)
#'
#' Calculate the predicted concentration (CEST) at the time of the last measurable concentration
#'
#' @details
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{cest.png} \cr
#'  }
#' }
#' \eqn{CEST  = Predicted concentration at time TLAST} \cr
#' \eqn{KEL = Terminal or elimination phase rate constant} \cr
#' \eqn{TLAST = Time of the last observed measurable concentration} \cr
#' \eqn{KELC0 = Y-intercept (concentration at time zero) from log-linear regression for KEL} \cr
#'
#' @section Note:
#' The inputs 'kelflag', 'tlast', 'spanratio' and 'kel' are optional.\cr
#' \strong{tlast}: Refer to \code{\link{tlast}} for more details \cr
#' \strong{kel}: Refer to \code{\link{kel}} for more details \cr
#'
#' @param conc The concentration data (given in a vector form)
#' @param time The time data (given in a vector form)
#' @param kelflag The KEL exclude flag data (given in a numeric vector)
#' @param t_last The time of last measureable (non-zero) plasma concentration (numeric value)
#' 
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item CEST: estimated concentration at time of occurrence of TLAST
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
#' data <- data.frame(
#'     SID = ...,
#'     TIME = ...,
#'     RESULT = ...
#' )
#' #Same data as above, just represented as a dataframe
#'
#' cest()
#' #Error in cest: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- data$CONC
#' time_vector <- data$TIME
#' 
#' kel(conc_vector, time_vector)
#' #       KEL       KELC0     KELTMLO     KELTMHI     KELNOPT       THALF      THALFF 
#' #0.04099056  2.72984039  0.00000000  5.00000000  6.00000000 16.90992299          NA
#' 
#' cest(conc_vector, time_vector, t_last = 5, kel = 0.04099056, kelc0 = 2.72984039)
#' #2.223962
#' 
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
cest <- function(conc = NULL, time = NULL, kelflag = NULL, t_last = NULL, spanratio = NULL, kel = NULL, kelc0 = NULL){
  if(is.null(conc) && is.null(time)){
    stop("Error in cest: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in cest: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in cest: 'time' vectors is NULL")
  } else if(all(is.na(time))) { # 2019-09-13/TGT/
      return(NA)
  } else if(all(is.na(conc))) { # 2019-09-13/TGT/
      return(NA)
  }

  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in cest: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in cest: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc)){
    stop("Error in cest: length of 'time' and 'conc' vectors are not equal")
  }

  if(is.null(kel) || is.null(kelc0)){
    if(!(is.null(spanratio))){
      kel_v <- kel(conc = conc, time = time, exflag = kelflag, spanratio = spanratio)
      kel <- kel_v[["KEL"]]
      kelc0 <- kel_v[["KELC0"]]
    } else {
      kel_v <- kel(conc = conc, time = time, exflag = kelflag)
      kel <- kel_v[["KEL"]]
      kelc0 <- kel_v[["KELC0"]]
    }
  }
  if(is.null(t_last)){
    t_last <- tlast(conc = conc, time = time)
  }

  if(is.na(kel) || is.na(kelc0)){
    c_est <- NA
  } else {
    c_est <- exp(-1*t_last*kel) * kelc0
  }
  return(c_est)
}
