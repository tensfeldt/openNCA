#' parameter_indices
#'
#' This function returns the locations of parameter name values in the provided parameter_list character vector
#'
#' @section Note:
#'
#' @param parameter_names The character value, optionally vector of character values, of the regular expression(s) to search in the parameter_list
#' @param parameter_list  The character vector of parameter names from the PARAMETERLIST of the Model Configuration Template
#' @param simplify	  If simplify is TRUE, default = TRUE, then return a single vector of indices. If FALSE, return a list of indices, named by entries in parameter_name
#' 
#' @section Returns:
#' \strong{TRUE} \cr
#' OR \cr
#' \strong{FALSE} \cr
#'
#' @examples
#' plist <- c("AUC", "RATEN", "RATE1", "abc", "TMAX", "RATE3", "RATEa", "defg")
#' 
#' parameter_indices("^RATEN$", display_list, simplify=FALSE)
#' #$RATEN
#' #[1] 2
#' #
#' pi("^RATEN$", display_list, simplify=TRUE)
#' #RATEN 
#' #2
#' 
#' parameter_indices("^(RATE)([0-9]*?|A|N)$", plist, simplify=FALSE)
#' #$RATEN
#' #[1] 2
#' #
#' #$RATE1
#' #[1] 3
#' #
#' #$RATE3
#' #[1] 6
#' #
#' #$RATEa
#' #[1] 7
#' 
#' parameter_indices("^(RATE)([0-9]*?|A|N)$", plist, simplify=TRUE)
#' #RATEN RATE1 RATE3 RATEa 
#' #    2     3     6     7 
#'
#' parameter_indices(c("^(RATE)([0-9]*?|A|N)$", "TMAX", "AUC"), display_list, simplify=FALSE)
#' #$RATEN
#' #[1] 2
#' #
#' #$RATE1
#' #[1] 3
#' #
#' #$RATE3
#' #[1] 6
#' #
#' #$RATEa
#' #[1] 7
#' #
#' #$TMAX
#' #[1] 5
#' #
#' #$AUC
#' #[1] 1
#' #
#' #parameter_indices(c("^(RATE)([0-9]*?|A|N)$", "TMAX", "AUC"), display_list, simplify=TRUE)
#' #RATEN RATE1 RATE3 RATEa  TMAX   AUC 
#' #    2     3     6     7     5     1 
#' 
#' parameter_indices(c("qrx"), display_list, simplify=FALSE)
#' #named list()
#' 
#' parameter_indices("qrx", display_list, simplify=FALSE)
#' #named list()
#' 
#' parameter_indices("qrx", display_list, simplify=TRUE)
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
parameter_indices <- function(parameter_names, parameter_list, simplify=FALSE) {
  result <- lapply(parameter_names, FUN=function(x) { grep(x, parameter_list, ignore.case=TRUE, perl=TRUE) } )
  result <- unlist(lapply(result, FUN=unlist, recursive=TRUE))
  if(!is.null(result)) {
    names(result) <- parameter_list[result]
    result <- as.list(result)
  }
  else { result <- integer(0) } 
  if(simplify) { result <- unlist(result) }
  return(result)
}
