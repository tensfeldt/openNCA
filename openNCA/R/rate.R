#' Excretion Rate for each collection interval
#'
#' @export
### 2019-10-03/TGT/
###rate <- function(start_time = NULL, end_time = NULL, conc = NULL, vol = NULL, type = "INTERVAL"){
rate <- function(start_time = NULL, end_time = NULL, conc = NULL, vol = NULL, volu = NULL, type = "INTERVAL", map = NULL){
  if(is.null(start_time) || is.null(end_time) || is.null(conc) || is.null(vol)){
    count <- 0
    err <- ""
    if(is.null(start_time)){
      err <- paste0(err, "'start_time'")
      count <- count + 1
    } 
    if(is.null(end_time)){
      err <- paste0(err, ifelse(is.null(start_time), ", ", ""), "'end_time'")
      count <- count + 1
    } 
    if(is.null(conc)){
      err <- paste0(err, ifelse((is.null(start_time) || is.null(end_time)), ", ", ""), "'conc'")
      count <- count + 1
    } 
    if(is.null(vol)){
      err <- paste0(err, ifelse((is.null(start_time) || is.null(end_time) || is.null(conc)), ", ", ""), "'vol'")
      count <- count + 1
    }
    if(count > 1){
      stop(paste0("Error in rate: ", err, " vectors are NULL")) 
    } else {
      stop(paste0("Error in rate: ", err, " vector is NULL"))
    }
  } else if(all(is.na(start_time))) { # 2019-11-24/RD/
    return(NA)
  } else if(all(is.na(end_time))) { # 2019-11-24/RD/
    return(NA)
  } else if(all(is.na(conc))) { # 2019-11-24/RD/
    return(NA)
  } else if(all(is.na(vol))) { # 2019-11-24/RD/
    return(NA)
  }
  
  if(!(is.numeric(start_time) && is.vector(start_time))){
    stop("Error in rate: 'start_time' is not a numeric vector")
  }
  if(!(is.numeric(end_time) && is.vector(end_time))){
    stop("Error in rate: 'end_time' is not a numeric vector")
  }
  if(!(is.numeric(conc) && is.vector(conc))){
    stop("Error in rate: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(vol) && is.vector(vol))){
    stop("Error in rate: 'vol' is not a numeric vector")
  }
  if(length(start_time) != length(end_time) && length(end_time) != length(conc) && length(conc) != length(vol)){
    stop("Error in rate: length of 'start_time', 'end_time', 'conc' and 'vol' vectors are not equal")
  }
  if(!is.character(type) && !is.null(type)){
    stop("Error in rate: 'type' is not a character value")
  }
  
  r <- NA
  if(!is.null(type)) {
    if(toupper(type) == "INTERVAL") {
  ### 2019-10-03/TGT/ update to include specific gravity adjustment for the rates
  ###    r <- ((conc * vol)/(end_time - start_time))
      r <- ((conc * vol)/(end_time - start_time))/specific_gravity_adjustment(amt_units=volu, map=map)
      if(any(is.infinite(r))){
        r[is.infinite(r)] <- 0
      }
      if(any(is.nan(r))){
        r[is.nan(r)] <- NA
      }
    }
  }
  
  return(r)
}
