#' Run Computation 
#'
#' This function will compute all the relevant parameters for a specified model.\cr
#' 
#' @details
#' \strong{Linear Method} \cr  
#' \figure{auc_1.png} \cr
#' \strong{Log Method} \cr  
#' \figure{auc_2.png} \cr
#' \eqn{AUC = Area under the cruve} \cr
#' \eqn{C_{i} = Concentration 1}{Ci = Concentration 1} \cr
#' \eqn{C_{i+1} = Concentration 2}{Ci+1 = Concentration 2} \cr
#' \eqn{T_{i} = Time 1}{Ti = Time 1} \cr
#' \eqn{T_{i+1} = Time 2}{Ti+1 = Time 2} \cr
#' \eqn{ln = Natural Logarithm} \cr \cr
#' \strong{Methods:} You can use the following methods to calculate AUC: \cr
#' \enumerate{
#'  \item \strong{Linear-Log Trapazoidal Rule}(default method): The linear method is used up to Tmax (the 
#'  first occurance of Cmax) and the log trapezoidal method is used for the remainder of the profile. If
#'  Ci or Ci+1 is 0 then the linear trapezoidal rule is used.
#'  \item \strong{Linear Trapazoidal Rule}: The linear method is used for the entire profile.
#'  \item \strong{Log Trapazoidal Rule}: The log trapezoidal method is used for the entire profile. If
#'  Ci or Ci+1 is 0 then the linear trapezoidal rule is used.
#'  \item \strong{Linear Up - Log Down Trapazoidal Rule}: Linear trapezoidal while the concentrations
#'  are increasing and log trapezoidal while the concentration are decreasing, the assessment is made on
#'  a step basis for each portion of the profile i.e. t1 to t2. If Ci or Ci+1 is 0 then the linear 
#'  trapezoidal rule is used.
#' }
#' You can specify the options to subset the list of parameters that are returned: \cr
#' \strong{Return List options} \cr  
#' \enumerate{
#'  \item \strong{cmax}: Refer to \code{\link{cmax}} for more details
#'  \item \strong{cmax_c}: Refer to \code{\link{cmaxc}} for more details
#'  \item \strong{cmax_dn}: Refer to \code{\link{cmax_dn}} for more details
#'  \item \strong{clast}: Refer to \code{\link{clast}} for more details
#'  \item \strong{tmax}: Refer to \code{\link{tmax}} for more details
#'  \item \strong{tlast}: Refer to \code{\link{tlast}} for more details
#'  \item \strong{kel}: Refer to \code{\link{kel}} for more details
#'  \item \strong{kelr}: Refer to \code{\link{kel_r}} for more details
#'  \item \strong{lasttime}: Refer to \code{\link{lasttime}} for more details
#'  \item \strong{auc_dn}: Refer to \code{\link{auc_dn}} for more details
#'  \item \strong{auc_all}: Refer to \code{\link{auc_all}} for more details
#'  \item \strong{auc_last}: Refer to \code{\link{auc_last}} for more details
#'  \item \strong{auc_last_c}: Refer to \code{\link{auc_lastc}} for more details
#'  \item \strong{auc_last_dn}: Refer to \code{\link{auc_dn}} for more details
#'  \item \strong{aumc_last}: Refer to \code{\link{aumc_last}} for more details
#'  \item \strong{auc_t1_t2}: Refer to \code{\link{auc_t1_t2}} for more details
#'  \item \strong{auc_inf_o}: Refer to \code{\link{auc_inf_o}} for more details
#'  \item \strong{auc_inf_p}: Refer to \code{\link{auc_inf_p}} for more details
#'  \item \strong{aumc_inf_o}: Refer to \code{\link{aumc_inf_o}} for more details
#'  \item \strong{auc_inf_o_c}: Refer to \code{\link{auc_inf_oc}} for more details
#'  \item \strong{auc_inf_p}: Refer to \code{\link{auc_inf_p}} for more details
#'  \item \strong{auc_inf_p_c}: Refer to \code{\link{auc_inf_pc}} for more details
#'  \item \strong{auc_inf_o_dn}: Refer to \code{\link{auc_dn}} for more details
#'  \item \strong{auc_inf_p_dn}: Refer to \code{\link{auc_dn}} for more details
#'  \item \strong{mrt_last}: Refer to \code{\link{mrt_last}} for more details
#'  \item \strong{mrto}: Refer to \code{\link{mrt_evif_o}} for more details
#'  \item \strong{mrtp}: Refer to \code{\link{mrt_evif_p}} for more details
#'  \item \strong{auc_xpct_o}: Refer to \code{\link{auc_XpctO}} for more details
#'  \item \strong{auc_xpct_p}: Refer to \code{\link{auc_XpctP}} for more details
#'  \item \strong{aumc_xpct_o}: Refer to \code{\link{aumc_XpctO}} for more details
#'  \item \strong{aumc_xpct_p}: Refer to \code{\link{aumc_XpctP}} for more details
#'  \item \strong{clo}: Refer to \code{\link{clo}} for more details
#'  \item \strong{clfo}: Refer to \code{\link{clfo}} for more details
#'  \item \strong{clfow}: Refer to \code{\link{clfow}} for more details
#'  \item \strong{clfp}: Refer to \code{\link{clfp}} for more details
#'  \item \strong{clfpw}: Refer to \code{\link{clfpw}} for more details
#'  \item \strong{vzfo}: Refer to \code{\link{vzfo}} for more details
#'  \item \strong{vzfow}: Refer to \code{\link{vzfow}} for more details
#'  \item \strong{vzfp}: Refer to \code{\link{vzfp}} for more details
#'  \item \strong{vzfpw}: Refer to \code{\link{vzfpw}} for more details
#' }
#' 
#' @section Note:
#' By default all the return list options are selected and calculated
#' 
#' @param data The dataframe that contians the raw data and flag data
#' @param map The dataframe that contians the map data and flag data
#' @param flag The dataframe that contians the flag data (optional)
#' @param return The list of parameters to return (by defualt it is empty, which means it will retunr all parameters)
#' 
#' @section Returns:
#' \strong{Dataset} \cr 
#' 
#' @examples 
#' ##########
#' ## Data ##
#' #################################
#' ##  SID  ##  TIME  ##  RESULT  ##
#' #################################
#' ##   30  ##    0   ##   2.89   ##
#' ##   30  ##    1   ##   2.49   ##
#' ##   30  ##    2   ##   2.47   ##
#' ##   31  ##    0   ##      0   ##
#' ##   31  ##    1   ##   1.00   ##
#' ##   31  ##    2   ##      0   ##
#' ##   32  ##    0   ##   1.19   ##
#' ##   32  ##    1   ##   1.23   ##
#' ##   32  ##    2   ##   1.34   ##
#' ##   32  ##    4   ##   1.32   ##
#' #################################
#' 
#' data <- data.frame(
#'     SID = ...,
#'     TIME = ...,
#'     RESULT = ...
#' )
#' #Same data as above, just represented as a dataframe
#' 
#' kel()   
#' #No data found!
#' 
#' kel(data)  
#' #Object not of class NCA
#' 
#' mod <- model("~/data.csv")  
#' #Creates an NCA object with data represented in 'data' above
#' kel(mod)  
#' #Please specify for which subject you want to get the KEL for!
#' 
#' kel(mod, sid = "all")  
#' # SID     KEL TMLO  TMHI  NOPT
#' #  30  0.0785    0     2     3
#' #  31      ND   ND    ND     1
#' #  32      ND   ND    ND     4
#' 
#' kel(mod, sid = 31)  
#' # SID KEL TMLO TMHI NOPT
#' #  31  ND  ND    ND   1
#' 
#' kel(mod, sid = 10)  
#' #Invaild subject ID! Subject ID not found in the data provided!
#' 
#' @author
#' \itemize{
#'  \item Kevin McConnell
#' }
#' @export
run_computation <- function(data = NULL, map = NULL, flag = NULL, return = list()){
  if(is.null(data)){
    stop("Please provide a valid path for the 'data' parameter")
  } else {
    if(is.data.frame(data)){
      data_data <- data
    } else {
      if(file.exists(data)){
        data_data <- read.csv(file = data)
      } else {
        stop("Invalid path provided for 'data'! Please provide a valid path for the 'data' parameter")
      } 
    }
  }
  if(is.null(map)){
    stop("Please provide a valid path for the 'map' parameter")
  } else {
    if(is.data.frame(map)){
      map_data <- as.data.frame(lapply(map, as.character), stringsAsFactors = FALSE)
    } else {
      if(file.exists(map)){
        map_data <- read.csv(file = map, stringsAsFactors = FALSE)
      } else {
        stop("Invalid path provided for 'map'! Please provide a valid path for the 'map' parameter")
      }
    }
  }
  if(!is.null(flag)){
    if(is.data.frame(flag)){
      flag_data <- as.data.frame(lapply(flag, as.character), stringsAsFactors = FALSE)
    } else {
      if(file.exists(flag)){
        flag_data <- read.csv(file = flag, stringsAsFactors = FALSE)
      } else {
        stop("Invalid path provided for 'flag'! Please provide a valid path for the 'flag' parameter")
      }
    }
  }
  if(!("SDEID" %in% names(map_data) && "NOMTIME" %in% names(map_data) && "CONC" %in% names(map_data))){
    stop("Dataset provided via 'map' does not contain the required columns")
  }
  if(!(map_data$SDEID %in% names(data_data) && map_data$NOMTIME %in% names(data_data) && map_data$CONC %in% names(data_data))){
    stop("Values provided via 'map' are not present in the dataset provided via 'data'")
  }
  
  merged_data <- merge(x = data_data, y = flag_data, by = map_data$FLGMERGE)
  colnames(merged_data) <- gsub('.x','.dataset',names(merged_data))
  colnames(merged_data) <- gsub('.y','',names(merged_data))
  merged_data[,map_data$NOMTIME] <- as.numeric(merged_data[,map_data$NOMTIME])
  
  if(toupper(map_data$AUCMETHOD) == "LINLOG"){
    method <- 1
  } else if(toupper(map_data$AUCMETHOD) == "LIN"){
    method <- 2
  } else if(toupper(map_data$AUCMETHOD) == "LOG"){
    method <- 3
  } else if(toupper(map_data$AUCMETHOD) == "LINUPLOGDOWN"){
    method <- 4
  }
  
  data_out <- NULL
  if(toupper(map_data$MODEL) == 'M1' && toupper(map_data$DOSINGTYPE) == 'SD'){
    data_out <- run_M1_SD_computation(data = merged_data, map = map_data, method = method, model = toupper(map_data$MODEL), 
                          parameter = toupper(map_data$DOSINGTYPE), return = return) 
  } else if (toupper(map_data$MODEL) == 'M2' && toupper(map_data$DOSINGTYPE) == 'SD'){
    data_out <- run_M2_SD_computation(data = merged_data, map = map_data, method = method, model = toupper(map_data$MODEL), 
                          parameter = toupper(map_data$DOSINGTYPE), return = return) 
  } else if (toupper(map_data$MODEL) == 'M3' && toupper(map_data$DOSINGTYPE) == 'SD'){
    data_out <- run_M3_SD_computation(data = merged_data, map = map_data, method = method, model = toupper(map_data$MODEL), 
                          parameter = toupper(map_data$DOSINGTYPE), return = return) 
  } else if (toupper(map_data$MODEL) == 'M4' && toupper(map_data$DOSINGTYPE) == 'SD'){
    data_out <- run_M4_SD_computation(data = merged_data, map = map_data, method = method, model = toupper(map_data$MODEL), 
                          parameter = toupper(map_data$DOSINGTYPE), return = return) 
  }
  return(data_out)
}

