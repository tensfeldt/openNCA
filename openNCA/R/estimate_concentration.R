#! Estimate concentrations from results of linear regression of the time-conc data
#!
#' @param tine The vector of time point data
#' @param conc The vector of concentration data
#' @param slope The slope of the log-lin regression of the time-conc data generated following selection of the data points
#!
#! 2019-08-05/TGT/ estimate_concentration
#'
#' @author
#' \itemize{
#'  \item \strong{Tom Tensfeldt, Pfizer}
#'  \item website: \url{}
#'  \item email: \url{thomas.g.tensfeldt@pfizer.com}
#' }
#' 
#! @export
estimate_concentration <- function(time, conc, slope) {
    d <- data.frame(time=time, conc=conc)
    
    # Remove ZERO/NEGATIVE points
    k <- d$conc>0
    # Remove NA points
    j <- !is.na(d$conc)

    m <- k & j
    d <- d[m,]
    d$lconc <- log(d$conc)

     n <- length(d$conc)
     t2 <- d$time^2
     intercept <- (sum(d$lconc)*sum(t2)) - (sum(d$time)*sum(d$time*d$lconc))
     intercept <- intercept/(n*sum(t2)-(sum(d$time))^2)

     slope <- -1*slope
     est <- exp(intercept + (slope)*d$time)

     return(est)
}

