#' Initial concentration resulting immediately after an IV bolus administration.
#'
#' @param conc The concentration data (given in a vector form)
#' @param time The time data (given in a vector form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item C0: initial concentration
#' }
#'
#' @examples
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
c0 <- function(conc = NULL, time = NULL){
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
  c_0 <- NA
  if(nrow(tmp) < 1){
    return(c_0)
  }
  if(sum(tmp$conc, na.rm=T) == 0){
    return(0)
  }
  # Find the initial concentration value and ignore NA values (SD only at the moment).
  c_0 <- na.omit(conc)[1]
  return(c_0)
}
