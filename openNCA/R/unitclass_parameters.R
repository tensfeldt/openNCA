unitclass_parameters <- function(parameter, dl=create_dependency_list()) {
  y <- lapply(dl, FUN=function(x) { if(parameter_required(parameter, x$unit_class)) { return(TRUE) } else { return(FALSE) }  })
  k <- y[y==TRUE]
  k <- names(k)
  return(k)
}
