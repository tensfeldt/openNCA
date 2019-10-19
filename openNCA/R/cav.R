#' Average concentration at steady state
#'
#' @details
#' Single Dose Equation:
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{cav.png} \cr
#'  }
#' }
#' \eqn{AUCTAUi = Area under the concentration versus time curve from zero time until the end of the ith dosing interval} \cr
#' \eqn{TAUi = Duration of the ith Dosing interval} \cr
#'
#' @section Note:
#' \strong{auc_tau}: Refer to \code{\link{auc_tau}} for more details
#'
#' @param auctau The area under the concentration versus time curve (numeric value)
#' @param tau The time duration of dosing interval (numeric value)
#'
#' @section Returns:
#' \strong{Dataset} \cr
#' \itemize{
#'  \item CAV: average concentration at steady state
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
cav <- function(auctau = NULL, tau = NULL){
  if(is.null(auctau) && is.null(tau)){
    stop("Error in vzo: 'auctau' and 'tau' vectors are NULL")
  } else if(is.null(auctau)) {
    stop("Error in vzo: 'auctau' vector is NULL")
  } else if(is.null(tau)) {
    stop("Error in vzo: 'tau' vectors is NULL")
  }

  if(is.na(auctau) || is.na(tau) || (0 %in% tau)){
    ca_v <- NA
  } else {
    ca_v <- auctau/tau
    ca_v <- replace(ca_v, is.infinite(ca_v), NA)
  }

  return(ca_v)
}
