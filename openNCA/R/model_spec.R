#' model_spec generates a brief description of the Model Configuration Template provided
#'
#' Generates a description string containing the MODEL and DOSINGTYPE from the
#' Model Configuration Template (MCT)
#'
#' @param map Model Configuration Template (MCT)
#' 
#' @section Returns:
#' \strong{TRUE} \cr
#' OR \cr
#' \strong{FALSE} \cr
#'
#' @examples
#'
#' @author
#' \itemize{
#'  \item \strong{Thomas Tensfeldt, Pfizer}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{thomas.g.tensfeldt@pfizer.com}
#' }
#'
#' @export
model_spec <- function(map) {
    result <- ""
    j <- parameter_indices(c("MODEL", "DOSINGTYPE", "AUCMETHOD"), names(map))
    if(length(j)>0) {
        result <- paste("openNCA:", "Model:", map[j$MODEL], "DosingType:", map[j$DOSINGTYPE], "AUCMETHOD:", map[j$AUCMETHOD], sep=" ")
    }
    return(result)
}
