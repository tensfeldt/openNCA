#' Version of openNCA
#'
#' This function prints and returns the version of the openNCA R Package
#'
#' @param print_version Logical value that determines if the version numbers prints to the console
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item openNCA Version: X.X.X
#' }
#'
#' @examples
#' opennca_version()
#' #sopenNCA computation engine version <X.X.X>
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
opennca_version <- function(print_version = TRUE){
  version <- packageVersion("openNCA")
  if(isTRUE(print_version)){
    cat(paste0("openNCA computation engine version ", packageVersion("openNCA"), "\n"))
  }
  
  return(version)
}
