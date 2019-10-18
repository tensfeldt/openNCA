#' Estimated C0 concentration estimated for IV bolus administration
#'
#' Estimated C0 concentration obtained from back-extrapolation of the first of several
#' concentration datapoints obtained immediately after an IV bolus administration.
#'
#' Note: consider option to scan time,conc data until a negative slope is obtained to avoid positive slopes
#' if no negative slope can be obtained with this option, return NA as est_c0. Not implemented as of 2019-09-24.
#' 
#' @param conc The concentration data (given in a vector form)
#' @param time The time data (given in a vector form)
#' @param npts The # of data points to utilize in back extrapolation (Default: 2)
#' @param verbose If TRUE print additional messages
#' @param returnall If TRUE return a list with the time, conc, and resulting est_c0, otherwise return est_c0
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item est_C0: estimated initial concentration
#' }
#'
#' @examples
#' time <- c(0.00,0.25,0.50,1.00,1.50,2.00,3.00,5.00,8.00,12.00,24.00,48.00,72.00,96.00,120.00,168.00,336.00,504.00)
#' conc <- c(0.0,125.0,122.0,126.0,127.0,130.0,136.0,118.0,99.0,85.4,63.2,36.2,23.5,17.4,11.5,0.0,0.0,0.0)
#' est_c0(conc=conc, time=time)
#' warning: <est_c0.R>: Following 0/zero/NA valued concentration data points were removed from regression:
#'
#'   time conc
#' 1     0    0
#' 16  168    0
#' 17  336    0
#' 18  504    0
#' (Intercept) 
#' 128.0738
#' 
#' @author
#' \itemize{
#'  \item \strong{Thomas Tensfeldt, Pfizer Inc}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{thomas.g.tensfeldt@pfizer.com}
#' }
#' @export
est_c0 <- function(conc = NULL, time = NULL, npts=2, verbose=FALSE, returnall=TRUE){
  if(is.null(conc) && is.null(time)){
    stop("Error in cmax: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in cmax: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in cmax: 'time' vectors is NULL")
  } else if(all(is.na(time))) { # 2019-09-13/TGT/
      return(NA)
  } else if(all(is.na(conc))) { # 2019-09-13/TGT/
      return(NA)
  }

  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in cmax: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in cmax: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc) ){
    stop("Error in cmax: length of 'time' and 'conc' vectors are not equal")
  }

  tmp <- data.frame(time, conc)
  tmp <- tmp[order(tmp$time), ]

### Remove NA points
  j <- !is.na(tmp$conc)

### Remove ZERO/NEGATIVE points
  k <- tmp$conc>0

  m <- k & j

  if(any(!m)) {
    if(verbose) {
      message("warning: <est_c0.R>: Following 0/zero/NA valued concentration data points were removed from regression:\n")
      print(tmp[!m,])
    }
    tmp <- tmp[m,]
  }

### retain npts  
  tmp <- tmp[1:npts, ]

  if(verbose) { cat("<est_c0.R>: c0 estimation data: \n"); print(tmp) }
  
### initialize est_c0
  x <- NA
  y <- NA
  slope <- NA
  intercept <- NA
  est_c0 <- NA
  if(nrow(tmp) > 1) {
    x <- tmp$time
    y <- tmp$conc
    m <- lm(log(y)~x)
    slope <- coef(m)["x"]
    intercept <- coef(m)["(Intercept)"]
    x <- range(x)
    y <- exp(intercept + slope*x)
    est_c0 <- exp(intercept)
  }
  if(verbose) { cat("est_c0: ", est_c0, " slope: ", slope, " intercept: ", intercept, " x: ", x, " y: ", y, "\n") }

  # Return est_c0 as well as the time and concentration data used to estimate est_c0 if returnall TRUE
  if(returnall) {
      results <- list(time=tmp$time, conc=tmp$conc, est_c0=est_c0)
  }
  else { results <- est_c0 }

  return(results)
}
