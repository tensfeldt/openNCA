#' Percentage of AUCINFO obtained by forward extrapolation
#'
#' Percentage of AUCINFO obtained by forward extrapolation.\cr
#'
#' @details
#' \strong{Equation}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{auc_XpctO1.png} \cr
#'  }
#' }
#' @section Additional Details:
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
#'  first occurance of Cmax) and the log trapezoidal method is used for the remainder of the profile. If
#'  Ci or Ci+1 is 0 then the linear trapezoidal rule is used.
#'  \item \strong{Linear Trapazoidal Rule}: The linear method is used for the entire profile.
#'  \item \strong{Log Trapazoidal Rule}: The log trapezoidal method is used for the entire profile. If
#'  Ci or Ci+1 is 0 then the linear trapezoidal rule is used.
#'  \item \strong{Linear Up - Log Down Trapazoidal Rule}: Linear trapezoidal while the concentrations
#'  are increasing and log trapezoidal while the concentration are decreasing, the assessment is made on
#'  a step basis for each portion of the profile i.e. t1 to t2. If Ci or Ci+1 is 0 then the linear
#'  trapezoidal rule is used.
#' }
#'
#' @section Note:
#' The inputs 'kelflag', 'auclast', 'aucinfo' and 'auclast' are optional. If 'aucinfo' input is not provided then it will generate the AUCINFO using the concentration and time data provided. \cr
#' If 'auclast' input is not provided then it will generate the AUCLASTi using the concentration and time data provided.
#' \strong{auc_inf_o}: Refer to \code{\link{auc_inf_o}} for more details \cr
#' \strong{auc_last}: Refer to \code{\link{auc_last}} for more details \cr
#'
#' @param conc The concentration data (given in a vector form)
#' @param time The time data (given in a vector form)
#' @param method The method that will be used to calculate AUC (use either 1, 2, 3, or 4)\cr
#' \enumerate{
#' \item Linear-Log Trapazoidal Rule (default)
#' \item Linear Trapazoidal Rule
#' \item Log Trapazoidal Rule
#' \item Linear Up - Log DownTrapazoidal Rule
#' }
#' Note: check 'Methods' section below for more details \cr
#'
#' @param kelflag The KEL exclude flag data (given in a numeric vector)
#' @param aucflag The AUC exclude flag data (given in a numeric vector)
#' @param aucinfo The area under the concentration versus time curve from zero time to infinity (Observed) (numeric value)
#' @param auclast The area under the concentration versus time curve from zero time until the time (TLAST) of the last measurable concentration (CLASTi) during the ith dosing interval (numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item AUC_PER: percentage of area under the curve
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
#' #Data mentioned will be used for the following example
#'
#' #auc_XpctO()
#' #Error in auc_XpctO: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- c(2.89, 2.49, 2.47, 2.38, 2.32, 2.28)
#' time_vector <- c(0, 1, 2, 3, 4, 5)
#'
#' auc_XpctO(conc = conc_vector, time = time_vector)
#' #81.96408
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
#' auc_XpctO(conc = conc_vector, time = time_vector)
#' #0
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
auc_XpctO <- function(conc = NULL, time = NULL, method = 1, kelflag = NULL, aucflag = NULL, auc_info = NULL, auclast = NULL) {
  if(is.null(conc) && is.null(time)){
    stop("Error in auc_XpctO: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in auc_XpctO: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in auc_XpctO: 'time' vectors is NULL")
  } else if(all(is.na(time))) { # 2019-09-11/TGT/
      return(NA)
  } else if(all(is.na(conc))) { # 2019-09-11/TGT/
      return(NA)
  }

  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in auc_XpctO: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in auc_XpctO: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc)){
    stop("Error in auc_XpctO: length of 'time' and 'conc' vectors are not equal")
  }
  if(method != 1 && method != 2 && method != 3 && method != 4){
    stop("Error in auc_XpctO: the value provided for 'method' is not correct")
  }

  if(sum(conc, na.rm=T) == 0){
    auc_xpcto <- 0
    return(auc_xpcto)
  } else {
    if(is.null(auc_info)){
      auc_info <- auc_inf_o(conc = conc, time = time, method = method, kelflag = kelflag, aucflag = aucflag)
    }
    if(is.null(auclast)){
      auclast <- auc_last(conc = conc, time = time, method = method, exflag = aucflag)
    }

    if(is.na(auc_info) || auc_info == 0 || is.na(auclast)){
      auc_xpcto <- NA
      return(auc_xpcto)
    } else {
      auc_xpcto <- ((auc_info - auclast)/auc_info)*100
      return(auc_xpcto)
    }
  }
}
