valid_models <- function(dl=create_dependency_list()) {
  y <- lapply(dl, FUN=function(x) { x$valid_models } )
  y <- unlist(y)
  y <- as.vector(y[!duplicated(y)])
  return(y)
} 
