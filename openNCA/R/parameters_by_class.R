parameters_by_class <- function(unitclass, checklist, dl=create_dependency_list()) {
    parameters <- unitclass_parameters(unitclass)
    parameterregex <- unlist(lapply(parameters, FUN=function(x) { return(parameter_regex(x)) }))
    mp <- unlist(lapply(parameterregex, FUN=function(x) { parameter_required(x, checklist, simplify = FALSE) } ))
    mp <- names(mp[mp])
    mp <- mp[!duplicated(mp)]
  return(mp)
}
