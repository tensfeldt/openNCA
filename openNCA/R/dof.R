#' Duration of infusion for constant infusion model M3
#'
#' This function returns the duration of infusion 
#'
#' @param data The dataframe that contains the raw data
#' @param map The dataframe that contains the map data
#' @param dof_name The name of the DOF column in the map data 
#' 
#' @section Returns:
#' \strong{List} \cr
#' \itemize{
#'  \item DOF: duration of infusion
#' }
#' 
#' @examples
#' 
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
dof <- function(data, map, dof_name = "DOF1") {
  dose_inf <- ifelse(dof_name %in% names(map), ifelse(map[[dof_name]] %in% names(data), unique(data[,map[[dof_name]]])[1], NA), ifelse("DOF" %in% names(map), ifelse(map$DOF %in% names(data), unique(data[,map$DOF])[1], NA), NA))
  
  if(is.na(dose_inf)){
    date_str <- ifelse("DRGDATE" %in% names(map), ifelse(map$DRGDATE %in% names(data), unique(data[,map$DRGDATE])[1], NA), NA)
    time_str <- ifelse("DOSETIM" %in% names(map), ifelse(map$DOSETIM %in% names(data), unique(data[,map$DOSETIM])[1], NA), NA)
    date_end <- ifelse("DRGDATESTOP" %in% names(map), ifelse(map$DRGDATESTOP %in% names(data), unique(data[,map$DRGDATESTOP])[1], NA), NA)
    time_end <- ifelse("DOSETIMSTOP" %in% names(map), ifelse(map$DOSETIMSTOP %in% names(data), unique(data[,map$DOSETIMSTOP])[1], NA), NA)
    
    if(!is.na(date_str) && !is.na(time_str) && !is.na(date_end) && !is.na(time_end)){
      if(!grepl(":", time_str) & nchar(time_str) < 4) {
        time_str <- paste(strrep("0", (4 - nchar(time_str))), time_str, sep="")
      } else {
        if(!grepl(":", time_str) & nchar(time_str) > 4) {return (NA)}
      }
      
      if(!grepl(":", time_end) & nchar(time_end) < 4) {
        time_end <- paste(strrep("0", (4 - nchar(time_end))), time_end, sep="")
      } else {
        if(!grepl(":", time_end) & nchar(time_end) > 4) {return (NA)}
      }
      
      date_time_str <- paste(date_str, time_str, sep=" ")
      date_time_end <- paste(date_end, time_end, sep=" ")
      
      dtstime <- as.POSIXct(date_time_str, format="%Y-%m-%d %H:%M")
      if(is.na(dtstime)) {
        dtstime <- as.POSIXct(date_time_str, format="%Y%m%d %H:%M")
      } 
      dtstime <- as.POSIXct(date_time_str, format="%Y %m %d %H:%M")
      if(is.na(dtstime)) {
        dtstime <- as.POSIXct(date_time_str, format="%Y%m%d %H%M")
      } 
      dtstime <- as.POSIXct(date_time_str, format="%Y %m %d %H%M")
      if(is.na(dtstime)) {
        dtstime <- as.POSIXct(date_time_str, format="%Y-%m-%d %H%M")
      }
      
      dtetime <- as.POSIXct(date_time_end, format="%Y-%m-%d %H:%M")
      if(is.na(dtetime)) {
        dtetime <- as.POSIXct(date_time_end, format="%Y%m%d %H:%M")
      } 
      dtetime <- as.POSIXct(date_time_end, format="%Y %m %d %H:%M")
      if(is.na(dtetime)) {
        dtetime <- as.POSIXct(date_time_end, format="%Y%m%d %H%M")
      } 
      dtetime <- as.POSIXct(date_time_end, format="%Y %m %d %H%M")
      if(is.na(dtetime)) {
        dtetime <- as.POSIXct(date_time_end, format="%Y-%m-%d %H%M")
      }
      
      if(!is.na(dtstime) && !is.na(dtetime)) {
        dose_inf <- round(as.numeric(difftime(datetime1, datetime2, units = "hours"), units="hours"), digits = 2)
      }
    }
  }
    
  return(dose_inf)
} 
