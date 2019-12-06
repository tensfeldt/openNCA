dependent_parameters <- function(parameter, dl=create_dependency_list(), depth=TRUE) {
  y <- lapply(dl, FUN=function(x) { if(parameter_required(parameter, x$predecessors)) { return(TRUE) } else { return(FALSE) }  })
  k <- y[y==TRUE]
  k <- names(k)
  if(depth){
    if(length(k) > 0){
      m <- k
      while(length(m) > 0){
        n <- c()
        for(i in 1:length(m)){
          z <- lapply(dl, FUN=function(x) { if(parameter_required(m[i], x$predecessors)){ return(TRUE) } else { return(FALSE) }  })
          l <- z[z==TRUE]
          l <- names(l)
          if(length(l) > 0  && !(l %in% k)){
            n <- c(n, l)
            k <- c(k, l)
          }
        }
        m <- n
      }
    }
  }
  return(k)
}
