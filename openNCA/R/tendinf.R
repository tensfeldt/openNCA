#' Observed time at the end of infusion
#'
#' This function gets the observed time at the end of infusion. In the case of multiple dosing, CENDINFi
#' is obtained by inspection of the data during each dosing interval i.
#'
#' @details The function will ensure that the concentration data and the time data provided are numeric vectors and
#' are also of the same length. \cr
#'
#' @param conc The concentration data (given in a vector form)
#' @param time The time data (given in a vector form)
#' @param dof The duration of infusion data (given as a numeric value)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item TENDINF: observed time at the end of infusion
#' }
#'
#' @examples
#' ##########
#' ## Data ##
#' ###################################
#' ##  SDEID  ##  TIME  ##   CONC   ##
#' ###################################
#' ##   30    ##    0   ##   2.89   ##
#' ##   30    ##    1   ##   2.49   ##
#' ##   30    ##    2   ##   2.47   ##
#' ###################################
#'
#' #Data mentioned will be used for the following example
#'
#' tendinf()
#' #Error in tendinf: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- c(2.89, 2.49, 2.47)
#' time_vector <- c(0, 1, 2)
#'
#' tendinf(conc = conc_vector, time = time_vector, dof = 1)
#' #1
#'
#' ############
#' ## Data 2 ##
#' ###################################
#' ##  SDEID  ##  TIME  ##   CONC   ##
#' ###################################
#' ##    31   ##    0   ##     0    ##
#' ##    31   ##    1   ##     0    ##
#' ##    31   ##    2   ##     0    ##
#' ###################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(0, 0, 0)
#' time_vector <- c(0, 1, 2)
#'
#' tendinf(conc = conc_vector, time = time_vector, dof = NA)
#' #NA
#'
#' ############
#' ## Data 3 ##
#' #################################
#' ##  SID  ##  TIME  ##   CONC   ##
#' #################################
#' ##   32  ##    0   ##   1.19   ##
#' ##   32  ##    1   ##   1.23   ##
#' ##   32  ##    2   ##   1.34   ##
#' ##   32  ## "None" ##   1.32   ##
#' #################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(1.19, 1.23, 1.34, 1.32)
#' time_vector <- c(0, 1, 2, "None")
#'
#' tendinf(conc = conc_vector, time = time_vector)
#' #Error in tendinf: 'time' is not a numeric vector
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
tendinf <- function(conc = NULL, time = NULL, dof = NULL){
  if(is.null(conc) && is.null(time) && is.null(dof)){
    stop("Error in tendinf: 'conc', 'time' and 'dof' vectors are NULL")
  } else if(is.null(conc) && is.null(time)){
    stop("Error in tendinf: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc) && is.null(dof)){
    stop("Error in tendinf: 'conc' and 'dof' vectors are NULL")
  } else if(is.null(time) && is.null(dof)){
    stop("Error in tendinf: 'time' and 'dof' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in tendinf: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in tendinf: 'time' vectors is NULL")
  } else if(is.null(dof)) {
    stop("Error in tendinf: 'dof' vectors is NULL")
  } else if(all(is.na(time))) {
    return(NA)
  } else if(all(is.na(conc))) {
    return(NA)
  } else if(is.na(dof)) {
    return(NA)
  }

  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in tendinf: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in tendinf: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc) ){
    stop("Error in tendinf: length of 'time' and 'conc' vectors are not equal")
  }

  tmp <- data.frame(time, conc)
  t_endinf <- NA
  
  if(!is.na(dof)){
    tmp_end_dof <- tmp$time[1] + dof 
    if(tmp_end_dof %in% tmp$time){
      t_endinf <- tmp[tmp$time == tmp_end_dof,]$time[1]
    }
  } 
  
  return(t_endinf)
}
