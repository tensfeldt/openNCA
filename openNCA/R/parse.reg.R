#' parse.reg
#'
#' for a single un-named capture field from a regular expression
#'  return the character string representing the captured value.
#' parse regular expression result
#'
#' @param res character string vector
#' @param result value from regexpr parse eval
#' 
#' @return dataframe with results of the captures, with named fields if possible
#'
#' @examples
#' parameters <- c("AUC", "THALF", "CMAX", "CMAXi", "TAU1", "TOLD1", "AUCINFP", "TAU2", "TAU", "TOLD2")
#' k <- regexpr("(?<basename>.+?)(?<index>i|[0-9]+)?$", parameters, ignore.case=FALSE, perl=TRUE)
#' terms <- parse.reg(parameters, k)
#' #> print(terms)
#' #   basename index
#' #1       AUC      
#' #2     THALF      
#' #3      CMAX      
#' #4      CMAX     i
#' #5       TAU     1
#' #6      TOLD     1
#' #7   AUCINFP      
#' #8       TAU     2
#' #9       TAU      
#' #10     TOLD     2
#'
#' @export
parse.reg <- function(res, result) {
  m <- do.call(rbind, lapply(seq_along(res), function(i) {
    if(result[i] == -1) return("")
    st <- attr(result, "capture.start")[i, ] - 1
    substring(res[i], st, st + attr(result, "capture.length")[i, ])
  }))
  x <- attr(result, "capture.names")
  colnames(m) <- attr(result, "capture.names")
  m <- as.data.frame(m)
  return(m)
}
