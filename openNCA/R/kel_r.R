#' Goodness of fit statistic for Terminal or Elimination phase rate constant
#'
#' This function gets the goodness of fit statistic (r^2) for the terminal phase rate constant (kel). 
#' This function also gets the goodness of fit statistic (r^2) for the terminal phase rate constant 
#' (kel) adjusted for the number of points used for the estimation of (kel). This function gets the 
#' root of goodness of fit statistic (r^2) for the terminal phase rate constant (kel). 
#' 
#' @aliases kelrsq kelrsqa
#' 
#' @details
#' \strong{Equation} \cr  
#' \figure{kel_rsq0.png} \cr
#' \figure{kel_rsq1.png} \cr
#' \figure{kel_rsq2.png} \cr
#' \figure{kel_rsq3.png} \cr
#' \eqn{S = Slope} \cr
#' \eqn{C_{i} = Concentration at each point for a profile}{Ci = Concentration at each point for a profile} \cr
#' \eqn{T_{i} = Time at each point for a profile}{Ti = Time at each point for a profile} \cr
#' \eqn{N = Number of provided times/concentrations data points in a profile} \cr \cr
#' \strong{Note:} If curve stripping is checked then follow curve stripping logic before outputting KELRSQ value \cr
#' 
#' @param conc The concentration data (given in a vector form) 
#' @param time The time data (given in a vector form)
#' 
#' @section Returns:
#' \strong{Vector} \cr 
#' \itemize{
#'  \item KELR: Root of Goodness of fit statistic for Terminal or Elimination phase rate constant
#'  \item KELRSQ: Goodness of fit statistic for Terminal or Elimination phase rate constant
#'  \item KELRSQA: Goodness of fit statistic for Terminal or Elimination phase rate constant adjusted
#' }
#' @section Additional Details for RSQ_A:
#' \strong{Equation} \cr  
#' If the user wanted the adjusted r squared value: \cr
#' \figure{kel_rsqa.png} \cr
#' \eqn{r^{2} = Another primary parameter}{r^2 = Another primary parameter} \cr
#' \eqn{N = Number of provided times/concentrations data points in a profile} \cr \cr
#' \strong{Note:} This function call on kel() in order to generate the goodness of fir statistic \cr
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
#' #################################
#' 
#' data <- data.frame(
#'     SID = ...,
#'     TIME = ...,
#'     RESULT = ...
#' )
#' #Same data as above, just represented as a dataframe
#' 
#' kel()   
#' #Error in kel_r: 'conc' and 'time' vectors are NULL
#' 
#' conc_vector <- data$CONC
#' time_vector <- data$TIME
#' 
#' kel_r(conc = conc_vector, time = time_vector)
#' #      KELR    KELRSQ   KELRSQA 
#' # 0.7884088 0.6215884 0.7707762 
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
#' data2 <- data.frame(
#'     SID = ...,
#'     TIME = ...,
#'     RESULT = ...
#' )
#' #Same data as above, just represented as a dataframe
#' 
#' conc_vector <- data2$CONC
#' time_vector <- data2$TIME
#' 
#' kel_r(conc = conc_vector, time = time_vector)
#' # KELR  KELRSQ KELRSQA 
#' #   NA      NA      NA 
#' 
#' ############
#' ## Data 3 ##
#' #################################
#' ##  SID  ##  TIME  ##   CONC   ## 
#' #################################
#' ##   32  ##    0   ##   2.19   ## 
#' ##   32  ##    1   ##   2.23   ##
#' ##   32  ##    2   ##   2.04   ##
#' ##   32  ##    3   ##   BLQ    ##
#' #################################
#' 
#' data3 <- data.frame(
#'     SID = ...,
#'     TIME = ...,
#'     RESULT = ...
#' )
#' #Same data as above, just represented as a dataframe
#' 
#' conc_vector <- data3$CONC
#' time_vector <- data3$TIME
#' 
#' kel_r(conc = conc_vector, time = time_vector)
#' #      KELR    KELRSQ   KELRSQA 
#' # 0.5681037 0.3227419 0.5321124 
#' 
#' @author
#' \itemize{
#'  \item Kevin McConnell
#' } 
#' @export
kel_r <- function(conc = NULL, time = NULL){
  if(is.null(conc) && is.null(time)){
    stop("Error in kel_r: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in kel_r: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in kel_r: 'time' vectors is NULL")
  }
  
  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in kel_r: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in kel_r: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc) ){
    stop("Error in kel_r: length of 'time' and 'conc' vectors are not equal")
  }
  
  kel_val <- kel(conc = conc, time = time)
  if(is.na(kel_val[["KEL"]])){
    kel_r_val <- c(NA, NA, NA)
    names(kel_r_val) <- c("KELR", "KELRSQ", "KELRSQA")
  } else {
    #Formatting data to remove any NA or less than 0 concentration values and corresponding time values 
    time <- time[!is.na(conc)]
    conc <- conc[!is.na(conc)]
    time <- time[conc > 0]
    conc <- conc[conc > 0]
    
    if(length(time) < 2 || length(conc) < 2){
      kel_r_val <- c(NA, NA, NA)
      names(kel_r_val) <- c("KELR", "KELRSQ", "KELRSQA")
    } else {
      conc_ln <- sapply(conc, function(x) {
        return(log(x))
      })
      #print(paste0("ln(Concentration):", conc_ln))
      ssct_p1 <- sum(sapply(time, function(x) { 
        index <- which(time %in% x) 
        if(length(index) > 1){
          stop("Error in kel_r: unable to calcualte due to duplicate entires in 'time' vector")
        } else if(length(index) < 1){
          stop("Error in kel_r: unable to calcualte due to value not present in 'time' vector")
        } else {
          return(x*conc_ln[index]) 
        }
      }))
      #print(paste0("ssct_p1:", ssct_p1))
      ssct_p2 <- ((sum(conc_ln) * sum(time))/length(time))
      #print(paste0("ssct_p2:", ssct_p2))
      ssct <- ssct_p1 - ssct_p2
      sst <- (sum(sapply(time, function(x){
        return(x*x)
      }))-((sum(time) * sum(time))/length(time)))
      #print(paste0("sst:", sst))
      ssc <- (sum(sapply(conc_ln, function(x) { 
        (x*x) 
      }))-((sum(conc_ln) * sum(conc_ln))/length(time)))
      #print(paste0("ssc:", ssc))
      r_sq <- ((ssct*ssct)/(sst*ssc))
      kel_r <- r_sq
      #print(paste0("r:", kel_r))
      kel_r_sq <- r_sq*r_sq
      #print(paste0("r_sq:", kel_r_sq))
      kel_r_sqa <- (1-(1-r_sq)*((length(conc)-1)/(length(conc)-2)))
      #print(paste0("r_sqa:", kel_r_sqa))
      
      kel_r_val <- c(kel_r, kel_r_sq, kel_r_sqa)
      names(kel_r_val) <- c("KELR", "KELRSQ", "KELRSQA") 
    } 
  }
  
  return(kel_r_val)
}
