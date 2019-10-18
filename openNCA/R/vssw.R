#' The volume of distribution normalized by weight
#'
#' The volume of distribution at steady state based on the observed or predicted last measurable plasma
#' concentration for a substance administered by intravascular dosing normalized by the body weight of the subject.
#'
#' @details
#' \strong{Model M2 and M3 (SD/SS) Derived}
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{vssw.png} \cr
#'  }
#' }
#' \eqn{VSS = Steady-state (observed/predicted) volume of distribution for non-steady state or steady state data} \cr
#' \eqn{NORMBS = BW = Body weight} \cr
#'
#' @param vss The volume of distribution at steady-state (given in a vector form)
#' @param normbs The body weight of the subject/experimental unit (given in a vector form)
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item VSS: the volume of distribution at steady state normalized by body weight
#' }
#'
#' @examples
#'
#' @author
#' \itemize{
#'  \item \strong{Thomas Tensfeldt, Pfizer}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{thomas.g.tensfeldt@pfizer.com}
#' }
#' @export
vssw <- function(vss = NULL, normbs = NULL){
  if(is.null(vss) && is.null(normbs)){
    stop("Error in vssw: 'vss' and 'normbs' vectors are NULL")
  } else if(is.null(vss)) {
    stop("Error in vssw: 'vss' vector is NULL")
  } else if(is.null(normbs)) {
    normbs <- rep(1.0, length(vss))
    warning("Warning in vssw: supplied 'normbs' vector is NULL, assuming unit normbs==1")
###    stop("Error in vssw: 'normbs' vectors is NULL")
  }

  normbs <- type.convert(normbs, as.is=TRUE)

  if(length(vss) != length(normbs) ){
    stop("Error in vssw: length of vector arguments do not match")
  }

  if(is.na(vss) || is.na(normbs) || mode(normbs)!="numeric") {
    vss_w <- NA
  } else {
    vss_w <- vss/normbs
  }

  return(vss_w)
}
