#' DOSEC - DOSEADJ - Dose Scaled to Concentration Units
#'
#' @details This function scales the DOSE to DOSEC to the same amount unit for concentration. \cr
#' This scaling is required to place DOSE on the same unit basis for concentration for computation of
#' CLFO, CLO, CLFP, CLFTAU, CLP, CLTAU, CLTAUi, VZFO, VZFP, VZFTAU, VZTAUi, VZO, VZP. \cr
#' Assumptions are that input Model Configuration Template (MCT/map) has been updated by update_mct_data and
#' unit_conversion is used for DOSE unit scaling. \cr
#'
#' @param data The dataframe that contains the raw data
#' @param map The dataframe that contains the map data
#'
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item DOSEC: DOSE adjusted for concentration units
#' }
#'
#' @examples
#'
#' @author
#' \itemize{
#'  \item \strong{Thomas Tensfeldt, Pfizer}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{Thomas.G.Tensfeldt@pfizer.com}
#' }
#' @export
dosec <- function(data = NULL, map = NULL, idose = NULL){
  function_name <- as.list(sys.call())[[1]]
        
  if(is.null(data)){
    stop("Please provide a valid path for the 'data' parameter")
  } else {
    if(is.data.frame(data)){
      data_data <- data
    } else {
      stop("Invalid data frame provided for 'data'! Please provide a valid data frame")
    }
  }
  if(is.null(map)){
    stop("Please provide a valid path for the 'map' parameter")
  } else {
    if(is.data.frame(map)){
      map_data <- as.data.frame(lapply(map, as.character), stringsAsFactors = FALSE)
    } else {
      stop("Invalid data frame provided for 'map'! Please provide a valid data frame")
    }
  }
  if(is.null(idose)) { idose = 1 }
  
  ### Concentration Units from data_data
  xconcu <- unique(data_data[,map_data$CONCU])
  ### Determine amount unit for concentration, i.e. if CONCU is "NG/ML" return "NG"
  k <- regexpr("(?<concamtu>[a-zA-Z]{1,2})/[a-zA-Z]{1,2}", xconcu, ignore.case=TRUE, perl=TRUE)

  concamtu <- as.character(unlist(unique(parse.reg(xconcu, k))))
  
  ### Update map_data$DOSEOUTPUTUNIT to concamtu
  map_data$DOSEOUTPUTUNIT <- concamtu

  ### Determine First DOSE and DOSE unit from map
  xdoseu <- map_data[,unlist(strsplit(map_data$DOSEULIST, ";"))[idose]]
  xdose <- map_data[,unlist(strsplit(map_data$DOSELIST, ";"))[idose]]
  
  ### Create a pseudo results (resulting parameters) dataset to drive unit_conversion
  vlist <- c(map_data$CONCU, xdoseu, xdose)
  vlist <- unlist(vlist)

  data_data <- data_data[!duplicated(data_data[,xdose]), vlist]
  
  df <- unit_conversion(data = data_data, map = map_data, result = data_data, unit_class = "DOSEU", verbose=FALSE)
  dose_c <- df[,xdose]

  return(dose_c)
}
