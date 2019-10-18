model_display_parameters <- function(model, dl=create_dependency_list()) {
  y <- lapply(dl, FUN=function(x) { if(parameter_required(model, x$display_list_models)) { return(TRUE) } else { return(FALSE) }  })
  k <- y[y==TRUE]
  k <- names(k)
  return(k)
}
