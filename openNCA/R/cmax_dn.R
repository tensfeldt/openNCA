#' Dose normalized CMAXi
#'
#' @details
#' \strong{Equation} \cr
#' \figure{cmax_dn.png} \cr
#' 
#' @param cmax The cmax data (given in a vector form) 
#' @param dose The dose data (given in a vector form)
#' 
#' @section Returns:
#' \strong{Value or Vector} \cr 
#' \itemize{
#'  \item CMAX: dose normalized CMAXi
#' }
#' 
#' @examples 
#' Will add some soon!!
#' 
#' @author
#' \itemize{
#'  \item Kevin McConnell
#' }
#' @export
cmax_dn <- function(cmax = NULL, dose = NULL){
  if(is.null(cmax) && is.null(dose)){
    stop("Error in cmax_dn: 'cmax' and 'dose' vectors are NULL")
  } else if(is.null(cmax)) {
    stop("Error in cmax_dn: 'cmax' vector is NULL")
  } else if(is.null(dose)) {
    stop("Error in cmax_dn: 'dose' vectors is NULL")
  }
  
  if(length(dose) != length(cmax) ){
    stop("Error in auc_dn: length of vector arguments do not match")
  }
 
  if(is.na(dose) || (0 %in% dose) || is.na(cmax)) {
    cmaxdn <- NA
  } else {
    cmaxdn <- cmax/dose
    cmaxdn <- replace(cmaxdn, is.infinite(cmaxdn), NA)
  }
  
  return(cmaxdn)
}