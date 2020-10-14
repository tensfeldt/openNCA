#' parameter_required
#'
#' This function determines whether one or more parameter names have been specified
#' in the PARAMETERLIST from the Model Configuration Template.
#'
#' @param parameter_names The vector of character values of the regular expression to search in the parameter_list
#' @param parameter_list  The character vector of parameter names from the PARAMETERLIST of the Model Configuration Template
#' @param simplify        If simplify is TRUE, default = TRUE, then return a single boolean of whether ALL of the required parameter_names are present in parameter_list. If FALSE, return a list
#' 
#' @section Returns:
#' \strong{TRUE} \cr
#' OR \cr
#' \strong{FALSE} \cr
#'
#' @examples
#' plist <- c("AUC", "RATEN", "RATE1", "abc", "TMAX", "RATE3", "RATEa", "defg")
#'
#' parameter_required("^RATEN$", plist, simplify=FALSE)
#' #$RATEN
#' #[1] TRUE
#'
#' parameter_required("^RATEN$", plist, simplify=TRUE)
#' #[1] TRUE
#'
#' parameter_required("^(RATE)([0-9]*?|A|N)$", plist, simplify=FALSE)
#' #$RATEN
#' #[1] TRUE
#' #
#' #$RATE1
#' #[1] TRUE
#' #
#' #$RATE3
#' #[1] TRUE
#' #
#' #$RATEa
#' #[1] TRUE
#' #
#' parameter_required("^(RATE)([0-9]*?|A|N)$", plist, simplify=TRUE)
#' #[1] TRUE
#'
#' parameter_required(c("^(RATE)([0-9]*?|A|N)$", "TMAX", "AUC"), plist, simplify=FALSE)
#' #$RATEN
#' #[1] TRUE
#' #
#' #$RATE1
#' #[1] TRUE
#' #
#' #$RATE3
#' #[1] TRUE
#' #
#' #$RATEa
#' #[1] TRUE
#' #
#' #$TMAX
#' #[1] TRUE
#' #
#' #$AUC
#' #[1] TRUE
#' #
#' parameter_required(c("^(RATE)([0-9]*?|A|N)$", "TMAX", "AUC"), plist, simplify=TRUE)
#' #[1] TRUE
#'
#' parameter_required("qrx", plist, simplify=FALSE)
#' #[1] FALSE
#' 
#' @author
#' \itemize{
#'  \item \strong{Thomas Tensfeldt, Pfizer}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{thomas.g.tensfeldt@pfizer.com}
#' }
#'
#' @export
parameter_required <- function(parameter_names, parameter_list, simplify=TRUE) {
  k <- parameter_indices(parameter_names, parameter_list, simplify = simplify)
  k <- lapply(k, FUN=function(x) { length(x)>0 & !is.na(x) } )
  if(length(k)==0) { return(FALSE) }

  if(simplify) { result <- all(unlist(k)) }
  else { result <- k }
  return(result)
}
