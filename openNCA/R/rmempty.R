#' rmempty will remove all empty columns from a dataframe
#' 
#' Remove columns that are completely empty, i.e. either all values are NA or "",
#' from a dataframe
#'
#' @param d dataframe to remove empty columns
#' @param pattern only remove columns that match pattern
#' @param quiet FALSE=display messages, TRUE=quiet operation
#' @return dataframe reduced with empty columns removed
#'
#' #' @author
#' \itemize{
#'  \item \strong{Thomas Tensfeldt, Pfizer}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{thomas.g.tensfeldt@pfizer.com}
#' }
#'
#' @export
# ------------------------------------------------
rmempty <- function(d, pattern, quiet=FALSE) {
  nlist <- names(d)
  # if pattern set, only remove fields that match pattern
  if(!missing(pattern)) { 
    nlist <- names(d)[grep(pattern, nlist, ignore.case = TRUE, perl = TRUE)]
  }
  rlist <- unlist(lapply(d[,nlist], FUN=function(x) { all(is.na(x)) | all(x=="") } ))
  rlist <- names(rlist[rlist])
  rlist <- rlist[!is.na(rlist)]  
  rmd <- d
  if(length(rlist)>0) {
    rmd <- rmd[, -match(rlist, names(rmd))]
    if(!quiet) { 
      message(paste('The following columns are completely empty/NA and are removed:\n'))
      pr <- paste0(' ', sort(setdiff(names(d), names(rmd))))
      message(pr)
    }
  }
  return(rmd)
}
