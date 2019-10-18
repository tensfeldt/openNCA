parameter_regex <- function(parameter, dl=create_dependency_list()) {
    regex <- dl[[parameter]]$regex
  return(regex)
}
