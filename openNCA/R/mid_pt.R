midpt <- function(start_time = NULL, end_time = NULL, type = "INTERVAL"){
  if(is.null(start_time) && is.null(end_time)) {
    stop("Error in midpt: 'start_time' and 'end_time' vectors are NULL")
  } else if(is.null(start_time)) {
    stop("Error in midpt: 'start_time' vector is NULL")
  } else if(is.null(end_time)) {
    stop("Error in midpt: 'end_time' vector is NULL")
  } 
  
  if(!(is.numeric(start_time) && is.vector(start_time))){
    stop("Error in midpt: 'start_time' is not a numeric vector")
  }
  if(!(is.numeric(end_time) && is.vector(end_time))){
    stop("Error in midpt: 'end_time' is not a numeric vector")
  }
  if(length(start_time) != length(end_time)){
    stop("Error in midpt: length of 'start_time' and 'end_time' vectors are not equal")
  }
  if(!is.character(type) || is.null(type)){
    stop("Error in midpt: 'type' is not a character value")
  }
  
  if(toupper(type) != "INTERVAL") {
    mid_pt <- NA
  } else {
    mid_pt <- ((end_time - start_time)/2) + start_time
  }
  
  return(mid_pt)
}
