model_parameters <- function(model, dl=create_dependency_list()) {
  if(missing(model)) { model <- valid_models() }
  y <- lapply(dl, FUN=function(x) { if(parameter_required(model, x$valid_models)) { return(TRUE) } else { return(FALSE) }  })
  k <- y[y==TRUE]
  k <- names(k)
  return(k)
}
