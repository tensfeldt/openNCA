#' Area under the urinary excretion rate curve from time 0 to the last rate.
#'
#' This function gets the area under urinary excretion rate curve from time 0 until the last
#' time point. As illustrated in the following figure, AUC_ALL includes the trapezoidal area from the
#' time of the last measurable concentration to the next time point. Although there may be additional
#' time points, there is no additional AUC since by definition all subsequent concentrations are zero.\cr
#' \figure{auc_all.png}
#'
#' @details
#' \strong{Linear Method} \cr
#' \figure{auc_1.png} \cr
#' \strong{Log Method} \cr
#' \figure{auc_2.png} \cr
#' \eqn{AUC = Area under the cruve} \cr
#' \eqn{C_{i} = Concentration 1}{Ci = Concentration 1} \cr
#' \eqn{C_{i+1} = Concentration 2}{Ci+1 = Concentration 2} \cr
#' \eqn{T_{i} = Time 1}{Ti = Time 1} \cr
#' \eqn{T_{i+1} = Time 2}{Ti+1 = Time 2} \cr
#' \eqn{ln = Natural Logarithm} \cr \cr
#' \strong{Methods:} You can use the following methods to calculate AUC: \cr
#' \enumerate{
#'  \item \strong{Linear-Log Trapazoidal Rule}(default method): The linear method is used up to Tmax (the
#'  first occurrence of Cmax) and the log trapezoidal method is used for the remainder of the profile. If
#'  Ci or Ci+1 is 0 then the linear trapezoidal rule is used.
#'  \item \strong{Linear Trapazoidal Rule}: The linear method is used for the entire profile.
#'  \item \strong{Log Trapazoidal Rule}: The log trapezoidal method is used for the entire profile. If
#'  Ci or Ci+1 is 0 then the linear trapezoidal rule is used.
#'  \item \strong{Linear Up - Log Down Trapazoidal Rule}: Linear trapezoidal while the concentrations
#'  are increasing and log trapezoidal while the concentration are decreasing, the assessment is made on
#'  a step basis for each portion of the profile i.e. t1 to t2. If Ci or Ci+1 is 0 then the linear
#'  trapezoidal rule is used.
#' }
#' \strong{Equation} \cr
#' If the user selects the option to have dose normalized AUC then the following equation is applied: \cr
#' \figure{auc_dn.png} \cr
#'
#' @param conc The concentration data (given in a vector form)
#' @param time The time data (given in a vector form)
#' @param method The method that will be used to calculate AUC (use either 1, 2, 3, or 4)\cr
#' \enumerate{
#' \item Linear-Log Trapezoidal Rule (default)
#' \item Linear Trapezoidal Rule
#' \item Log Trapezoidal Rule
#' \item Linear Up - Log Down Trapezoidal Rule
#' }
#' Note: check 'Methods' section below for more details \cr
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item AURC: area under the curve
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
#' #aurc_all()
#' #Error in aurc_all: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- data$CONC
#' time_vector <- data$TIME
#'
#' aurc_all(conc = conc_vector, time = time_vector)
#' #12.23956
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
#' #aurc_all(conc = conc_vector, time = time_vector)
#' #Error in auc_lin_log(conc, time) :
#' #  Error in auc_lin_log: 'tmax' is NA
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
#' aurc_all(conc = conc_vector, time = time_vector)
#' #2.494215
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
aurc_all <- function(conc = NULL, time = NULL, method = 1){
  if(is.null(conc) && is.null(time)){
    stop("Error in aurc_all: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in aurc_all: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in aurc_all: 'time' vectors is NULL")
  }

  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in aurc_all: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in aurc_all: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc)){
    stop("Error in aurc_all: length of 'time' and 'conc' vectors are not equal")
  }
  if(method != 1 && method != 2 && method != 3 && method != 4){
    stop("Error in aurc_all: the value provided for 'method' is not correct")
  }

  if(method == 1){
    return(auc_lin_log(conc = conc, time = time))
  } else if(method == 2){
    return(auc_lin(conc = conc, time = time))
  } else if(method == 3){
    return(auc_log(conc = conc, time = time))
  } else if(method == 4){
    return(auc_lin_up_log_down(conc = conc, time = time))
  }
}
