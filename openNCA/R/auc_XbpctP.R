#' Percentage of AUCINFP obtained by back extrapolation from the first non-zero concentration time point
#'
#' Percentage of AUCINFP obtained by back extrapolation from the first non-zero concentration time point.\cr
#'
#' @details
#' \strong{Equation}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{auc_XpctP1.png} \cr
#'  }
#' }
#' @section Additional Details:
#'
#' @section Note:
#' \strong{auc_inf_p}: Refer to \code{\link{auc_inf_p}} for more details \cr
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
#' #data <- data.frame(
#' #    SID = ...,
#' #    TIME = ...,
#' #    RESULT = ...
#' #)
#' #Same data as above, just represented as a dataframe
#'
#' #auc_XpctP()
#' #Error in auc_all: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- data$CONC
#' time_vector <- data$TIME
#'
#' auc_XpctP(conc = conc_vector, time = time_vector)
#' #81.59327
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
#' #Same data as above, just represented as a dataframe
#'
#' conc_vector <- data2$CONC
#' time_vector <- data2$TIME
#'
#' auc_XpctP(conc = conc_vector, time = time_vector)
#' #0
#'
#' @author
#' \itemize{
#'  \item \strong{Thomas Tensfeldt, Pfizer}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{thomas.g.tensfeldt@pfizer.com}
#' }
#' @export
auc_XbpctP <- function(conc = NULL, time = NULL, method = 2, kelflag = NULL, aucflag = NULL){
  if(is.null(conc) && is.null(time)){
    stop("Error in auc_XpctP: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in auc_XpctP: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in auc_XpctP: 'time' vectors is NULL")
  } else if(all(is.na(time))) { # 2019-09-16/TGT/
      return(NA)
  } else if(all(is.na(conc))) { # 2019-09-16/TGT/
      return(NA)
  }

  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in auc_XpctP: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in auc_XpctP: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc)){
    stop("Error in auc_XpctP: length of 'time' and 'conc' vectors are not equal")
  }
  if(method != 1 && method != 2 && method != 3 && method != 4){
    stop("Error in auc_XpctP: the value provided for 'method' is not correct")
  }

### 2019-09-25/TGT/ Compute AUC from 0 to first non-zero concentration value
  auct0_t1 <- NA
  auct0_t1_name <- "AUCT0_T1"
  xdf <- data.frame(time=time, conc=conc)
  xdf <- xdf[!is.na(xdf$time),]
  if(xdf$time[order(xdf$time)][1]==0) {
    xdf <- xdf[order(xdf$time),]
    k <- (xdf$conc>0 | xdf$time==0)
    xdf <- xdf[k,][1:2,]
    auct0_t1 <- auc_t1_t2(conc = xdf$conc, time=xdf$time, t1=xdf$time[1], t2=xdf$time[2], method=2)
  }
  if(!is.na(auct0_t1)) {
      auc_infp <- auc_inf_p(conc = conc, time = time, method = method, kelflag = kelflag, aucflag = aucflag)
  }
  if(!is.na(auct0_t1) && !is.na(auc_infp) && auc_infp!=0) {
    auc_xbpctp <- 100*(auct0_t1)/auc_infp
  }

  return(auc_xbpctp)

### 2019-09-25/TGT/ All of the following is incorrect and removed  
###  if(sum(conc, na.rm=T) == 0){
###    auc_xpctp <- 0
###    return(auc_xpctp)
###  } else {
###    auc_infp <- auc_inf_p(conc = conc, time = time, method = method, kelflag = kelflag, aucflag = aucflag)
###    auclast <- auc_last(conc = conc, time = time, method = method, exflag = aucflag)
###
###    if(is.na(auc_infp) || auc_infp == 0 || is.na(auclast)){
###      auc_xpctp <- NA
###      return(auc_xpctp)
###    } else {
###      auc_xpctp <- ((auc_infp - auclast)/auc_infp)*100
###      return(auc_xpctp)
###    }
###  }
}
