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
#' You can specify the options to subset the list of parameters that are return_listed: \cr
#' \strong{return_list List options} \cr  
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
#' By default all the return_list list options are selected and calculated
#' 
#' @param data The dataframe that contians the raw data and flag data
#' @param map The dataframe that contians the map data and flag data
#' @param flag The dataframe that contians the flag data (optional)
#' @param return_list The list of parameters to return_list (by defualt it is empty, which means it will retunr all parameters)
#' 
#' @section return_lists:
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
run_computation <- function(data = NULL, map = NULL, flag = NULL, retvirt=FALSE){ # 2018-10-31/TGT/ Added retvirt
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
  
  if(toupper(map_data$DOSINGTYPE) == 'SS'){
    ss_dose <- c("DOSE1", "DOSE2", "DOSE3", "DOSE4", "DOSE5")
    ss_tau <- c("TAU1", "TAU2","TAU3", "TAU4","TAU5")
    ss_told <- c("TOLD1", "TOLD2", "TOLD3","TOLD4", "TOLD5")
    
    if(any(ss_dose %in% names(map_data) || ss_tau %in% names(map_data) || ss_told %in% names(map_data))){
      for(i in 1:length(ss_dose)){
        if(ss_dose[i] %in% names(map_data) & ss_tau[i] %in% names(map_data) & ss_told[i] %in% names(map_data)) {
          merged_data[c(paste0("DI", i, "F"))] <- NA
          
          for(j in 1:length(unique(merged_data[,map_data$SDEID]))){
            tmp_df <- merged_data[merged_data[,map_data$SDEID] == unique(merged_data[,map_data$SDEID])[j],]
            s_time <- tmp_df[, map_data$NOMTIME]
            e_time <- tmp_df[, map_data$NOMENDTIME]
            
            told <- unique(tmp_df[,as.character(map_data[,ss_told[i]])])[[1]]
            tau <- unique(tmp_df[,as.character(map_data[,ss_tau[i]])])[[1]]
            #print(paste("SDEID", i, unique(merged_data[,map_data$SDEID])[j]))
            #print(s_time)
            #print(e_time)
            #print(paste("TOLD:", told))
            #print(paste("TAU:", tau))
            if(is.na(told) || is.na(tau)){
              merged_data[merged_data[,map_data$SDEID] == unique(merged_data[,map_data$SDEID])[j],][c(paste0("DI", i, "F"))][1:nrow(tmp_df),] <- rep(0, nrow(tmp_df))
            } else {
              told_i <- match(told, s_time)
              tau_i <- match(tau, s_time)
              tend_i <- match(tau+told, s_time)
              told_i <- ifelse(is.na(told_i), match(told, e_time), told_i)
              tau_i <- ifelse(is.na(tau_i), match(tau, e_time), tau_i)
              tend_i <- ifelse(is.na(tend_i), match(tau+told, e_time), tend_i)
              #print(nrow(tmp_df))
              #print(paste("TOLD index:", told_i))
              #print(paste("TAU index:", tau_i))
              #print(paste("TEND index:", tend_i))
              start_i <-  1
              end_i <- ifelse(tau_i < nrow(tmp_df), nrow(tmp_df), tau_i)
              #print(paste("Start:", start_i))
              #print(paste("End:", end_i))
              #print(c(rep(0, told_i-1), rep(1, tau_i), rep(0, (nrow(tmp_df) - tend_i))))
              
              if(is.numeric(told_i) & is.numeric(tau_i)) {
                merged_data[merged_data[,map_data$SDEID] == unique(merged_data[,map_data$SDEID])[j],][c(paste0("DI", i, "F"))][start_i:end_i,] <- c(rep(0, told_i-1), rep(1, tau_i), rep(0, (nrow(tmp_df) - tend_i)))
              }
            }
          }   
        }
      }
    } else {
      stop("Unable to generate dosing interval for Stedy State data! Please provide a valid data parameters")
    }
  }
  
  if(toupper(map_data$AUCMETHOD) == "LINLOG"){
    method <- 1
  } else if(toupper(map_data$AUCMETHOD) == "LIN"){
    method <- 2
  } else if(toupper(map_data$AUCMETHOD) == "LOG"){
    method <- 3
  } else if(toupper(map_data$AUCMETHOD) == "LINUPLOGDOWN"){
    method <- 4
  }
  #return_list(merged_data)
  return_list <- NULL

#  cat('map_data$RETURNCOLS:', map_data$RETURNCOLS, '\n')
#  cat('map_data$PARAMETERLIST:', map_data$PARAMETERLIST, '\n')
#  cat('map_data$PARAMETERDISPLAYLIST:', map_data$PARAMETERDISPLAYLIST, '\n')

#  if(retvirt) { return_list <- unlist(strsplit(map_data$PARAMETERDISPLAYLIST, ';')) }
#  else { return_list <- c(unlist(strsplit(map_data$RETURNCOLS, ';')), unlist(strsplit(map_data$PARAMETERLIST,';'))) }
#  return_list <- as.list(return_list);
  
#  cat('return_list:\n')
#  print(return_list)

### 
  base_parameter_names <-
      c("CMAX", "TMAX", "CLAST", "TLAST", "AUC", "AUCLAST", "AUMC", "AUCINF",
        "CL", "VZ", "CAV", "CLFTAU", "THALF", "THALFF", "TLAG", "TAU", "TOLD",
        "TMIN", "MRTLAST", "KEL", "KELRSQ", "KELRSQA", "KELNOPT", "KELTMHI", "KELTMLO",
        "LASTTIME", "F", "FREL", "FRELLAST", "DI",
        "MRAUC", "MRAUCLAST", "MRCMAX", "MRTLAST", "MRTEVIF", )
  suffix_patterns <- c("T", "i", "DN", "O", "P", "TAU", "W", "C", "XPCT", "XPT" )
  suf <- paste0(suffix_patterns, collapse="|")
  #regxpr(base_parameter_names, map_data$PARAMETERLIST)
  
#  if(!is.null(return_list) && !is.na(return_list)){
  if(!is.null(map_data$PARAMETERLIST) && !is.na(map_data$PARAMETERLIST)){
    return_list <- as.list(strsplit(map_data$PARAMETERLIST, ";")[[1]])
    if(toupper(map_data$MODEL) == 'M1' && toupper(map_data$DOSINGTYPE) == 'SD'){
      if("CMAXCi" %in% return_list && !"CMAXi" %in% return_list && !"KEL" %in% return_list) {
        return_list[[length(return_list)+1]] <- "CMAXi"
        return_list[[length(return_list)+1]] <- "KEL"
      }
      if("CMAXiDN" %in% return_list && !"CMAXi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "CMAXi"
      }
      if("AUCDN" %in% return_list && !"AUCALL" %in% return_list){
        return_list[[length(return_list)+1]] <- "AUCALL"
      }
      if("AUCLASTCi" %in% return_list && !"AUCLASTi" %in% return_list && !"KEL" %in% return_list && !"TLAST" %in% return_list){
        return_list[[length(return_list)+1]] <- "AUCLASTi"
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "TLAST"
      }
      if("AUCLASTiDN" %in% return_list && !"AUCLASTi" %in% return_list){
        return_list[[length(return_list)+1]] <- "AUCLASTi"
      }
      if("AUCINFOC" %in% return_list && !"KEL" %in% return_list && !"AUCINFO" %in% return_list){
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("AUCINFODN" %in% return_list && !"AUCINFO" %in% return_list){
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("AUCINFPC" %in% return_list && !"KEL" %in% return_list && !"AUCINFP" %in% return_list){
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
      if("AUCINFPDN" %in% return_list && !"AUCINFP" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
      if("CLO" %in% return_list && !"AUCINFO" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("CLFOW" %in% return_list && !"CLFO" %in% return_list) {
        return_list[[length(return_list)+1]] <- "CLFO"
      }
      if("CLFO" %in% return_list && !"AUCINFO" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("CLFPW" %in% return_list && !"CLFP" %in% return_list) {
        return_list[[length(return_list)+1]] <- "CLFP"
      }
      if("CLFP" %in% return_list && !"AUCINFP" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
      if("VZFOW" %in% return_list && !"VZFO" %in% return_list) {
        return_list[[length(return_list)+1]] <- "VZFO"
      }
      if("VZFO" %in% return_list && !"AUCINFO" %in% return_list && !"KEL" %in% return_list) {
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("VZFPW" %in% return_list && !"VZFP" %in% return_list) {
        return_list[[length(return_list)+1]] <- "VZFP"
      }
      if("VZFP" %in% return_list && !"AUCINFP" %in% return_list && !"KEL" %in% return_list) {
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
    } else if(toupper(map_data$MODEL) == 'M1' && toupper(map_data$DOSINGTYPE) == 'SS'){
      if("CMAXiDN" %in% return_list && !"CMAXi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "CMAXi"
      }
      if("AUCDN" %in% return_list && !"AUCALL" %in% return_list){
        return_list[[length(return_list)+1]] <- "AUCALL"
      }
      if("AUCLASTiDN" %in% return_list && !"AUCLASTi" %in% return_list){
        return_list[[length(return_list)+1]] <- "AUCLASTi"
      }
      if("AUCINFOC" %in% return_list && !"KEL" %in% return_list && !"AUCINFO" %in% return_list){
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("AUCINFODN" %in% return_list && !"AUCINFO" %in% return_list){
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("AUCINFPC" %in% return_list && !"KEL" %in% return_list && !"AUCINFP" %in% return_list){
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
      if("AUCINFPDN" %in% return_list && !"AUCINFP" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
      if("CLO" %in% return_list && !"AUCINFO" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("CLFO" %in% return_list && !"AUCINFO" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("VZFP" %in% return_list && !"AUCINFP" %in% return_list && !"KEL" %in% return_list) {
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
      if("CAVi" %in% return_list && !"AUCTAUi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
      if("CLFTAUi" %in% return_list && !"AUCTAUi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
      if("CLFTAUWi" %in% return_list && !"CLFTAUi" %in% return_list && !"AUCTAUi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "CLFTAUi"
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
      if("PTFi" %in% return_list && !"CMAXi" %in% return_list && !"CMINi" %in% return_list && !"CAVi" %in% return_list && "AUCTAUi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "CMAXi"
        return_list[[length(return_list)+1]] <- "CMINi"
        return_list[[length(return_list)+1]] <- "CAVi"
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
      if("PTRi" %in% return_list && !"CMAXi" %in% return_list && !"CMINi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "CMAXi"
        return_list[[length(return_list)+1]] <- "CMINi"
      }
      if("VZFTAUi" %in% return_list && !"KEL" %in% return_list && !"AUCTAUi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
      if("VZFTAUWi" %in% return_list && !"VZFTAUi" %in% return_list && !"KEL" %in% return_list && !"AUCTAUi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "VZFTAUi"
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
    } else if (toupper(map_data$MODEL) == 'M2' && toupper(map_data$DOSINGTYPE) == 'SD'){
      if("CMAXCi" %in% return_list && !"CMAXi" %in% return_list && !"KEL" %in% return_list) {
        return_list[[length(return_list)+1]] <- "CMAXi"
        return_list[[length(return_list)+1]] <- "KEL"
      }
      if("CMAXiDN" %in% return_list && !"CMAXi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "CMAXi"
      }
      if("AUCDN" %in% return_list && !"AUCALL" %in% return_list){
        return_list[[length(return_list)+1]] <- "AUCALL"
      }
      if("AUCLASTCi" %in% return_list && !"AUCLASTi" %in% return_list && !"KEL" %in% return_list && !"TLAST" %in% return_list){
        return_list[[length(return_list)+1]] <- "AUCLASTi"
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "TLAST"
      }
      if("AUCLASTiDN" %in% return_list && !"AUCLASTi" %in% return_list){
        return_list[[length(return_list)+1]] <- "AUCLASTi"
      }
      if("AUCINFOC" %in% return_list && !"KEL" %in% return_list && !"AUCINFO" %in% return_list){
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("AUCINFODN" %in% return_list && !"AUCINFO" %in% return_list){
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("AUCINFPC" %in% return_list && !"KEL" %in% return_list && !"AUCINFP" %in% return_list){
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
      if("AUCINFPDN" %in% return_list && !"AUCINFP" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
      if("VZFP" %in% return_list && !"AUCINFP" %in% return_list && !"KEL" %in% return_list) {
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
      if("CAVi" %in% return_list_list && !"AUCTAUi" %in% return_list_list) {
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
      if("CLFTAUi" %in% return_list_list && !"AUCTAUi" %in% return_list_list) {
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
      if("CLFTAUWi" %in% return_list_list && !"CLFTAUi" %in% return_list_list && !"AUCTAUi" %in% return_list_list) {
        return_list[[length(return_list)+1]] <- "CLFTAUi"
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
      if("PTFi" %in% return_list_list && !"CMAXi" %in% return_list_list && !"CMINi" %in% return_list_list && !"CAVi" %in% return_list_list && "AUCTAUi" %in% return_list_list) {
        return_list[[length(return_list)+1]] <- "CMAXi"
        return_list[[length(return_list)+1]] <- "CMINi"
        return_list[[length(return_list)+1]] <- "CAVi"
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
      if("PTRi" %in% return_list_list && !"CMAXi" %in% return_list_list && !"CMINi" %in% return_list_list) {
        return_list[[length(return_list)+1]] <- "CMAXi"
        return_list[[length(return_list)+1]] <- "CMINi"
      }
      if("VZFTAUi" %in% return_list_list && !"KEL" %in% return_list_list && !"AUCTAUi" %in% return_list_list) {
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
      if("VZFTAUWi" %in% return_list_list && !"VZFTAUi" %in% return_list_list && !"KEL" %in% return_list_list && !"AUCTAUi" %in% return_list_list) {
        return_list[[length(return_list)+1]] <- "VZFTAUi"
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
    } else if (toupper(map_data$MODEL) == 'M2' && toupper(map_data$DOSINGTYPE) == 'SS'){
      if("CMAXiDN" %in% return_list && !"CMAXi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "CMAXi"
      }
      if("AUCDN" %in% return_list && !"AUCALL" %in% return_list){
        return_list[[length(return_list)+1]] <- "AUCALL"
      }
      if("AUCLASTiDN" %in% return_list && !"AUCLASTi" %in% return_list){
        return_list[[length(return_list)+1]] <- "AUCLASTi"
      }
      if("AUCINFOC" %in% return_list && !"KEL" %in% return_list && !"AUCINFO" %in% return_list){
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("AUCINFODN" %in% return_list && !"AUCINFO" %in% return_list){
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("AUCINFPC" %in% return_list && !"KEL" %in% return_list && !"AUCINFP" %in% return_list){
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
      if("AUCINFPDN" %in% return_list && !"AUCINFP" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
      if("AUCTAUiDN" %in% return_list && !"AUCTAUi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
      if("VZFP" %in% return_list && !"AUCINFP" %in% return_list && !"KEL" %in% return_list) {
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
      if("CAVi" %in% return_list && !"AUCTAUi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
      if("CLFTAUi" %in% return_list && !"AUCTAUi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
      if("CLFTAUWi" %in% return_list && !"CLFTAUi" %in% return_list && !"AUCTAUi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "CLFTAUi"
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
      if("PTFi" %in% return_list && !"CMAXi" %in% return_list && !"CMINi" %in% return_list && !"CAVi" %in% return_list && "AUCTAUi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "CMAXi"
        return_list[[length(return_list)+1]] <- "CMINi"
        return_list[[length(return_list)+1]] <- "CAVi"
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
      if("PTRi" %in% return_list && !"CMAXi" %in% return_list && !"CMINi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "CMAXi"
        return_list[[length(return_list)+1]] <- "CMINi"
      }
      if("VZO" %in% return_list && !"AUCINFO" %in% return_list && !"KEL" %in% return_list) {
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("VZP" %in% return_list && !"AUCINFP" %in% return_list && !"KEL" %in% return_list) {
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
    } else if (toupper(map_data$MODEL) == 'M3' && toupper(map_data$DOSINGTYPE) == 'SD'){
      if("CMAXCi" %in% return_list && !"CMAXi" %in% return_list && !"KEL" %in% return_list) {
        return_list[[length(return_list)+1]] <- "CMAXi"
        return_list[[length(return_list)+1]] <- "KEL"
      }
      if("CMAXiDN" %in% return_list && !"CMAXi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "CMAXi"
      }
      if("AUCDN" %in% return_list && !"AUCALL" %in% return_list){
        return_list[[length(return_list)+1]] <- "AUCALL"
      }
      if("AUCLASTCi" %in% return_list && !"AUCLASTi" %in% return_list && !"KEL" %in% return_list && !"TLAST" %in% return_list){
        return_list[[length(return_list)+1]] <- "AUCLASTi"
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "TLAST"
      }
      if("AUCLASTiDN" %in% return_list && !"AUCLASTi" %in% return_list){
        return_list[[length(return_list)+1]] <- "AUCLASTi"
      }
      if("AUCINFOC" %in% return_list && !"KEL" %in% return_list && !"AUCINFO" %in% return_list){
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("AUCINFODN" %in% return_list && !"AUCINFO" %in% return_list){
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("AUCINFPC" %in% return_list && !"KEL" %in% return_list && !"AUCINFP" %in% return_list){
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
      if("AUCINFPDN" %in% return_list && !"AUCINFP" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
      if("CLOW" %in% return_list && !"CLO" %in% return_list) {
        return_list[[length(return_list)+1]] <- "CLO"
      }
      if("CLO" %in% return_list && !"AUCINFO" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("CLPW" %in% return_list && !"CLP" %in% return_list) {
        return_list[[length(return_list)+1]] <- "CLP"
      }
      if("CLP" %in% return_list && !"AUCINFP" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
      if("VZOW" %in% return_list && !"VZO" %in% return_list) {
        return_list[[length(return_list)+1]] <- "VZO"
      }
      if("VZO" %in% return_list && !"AUCINFO" %in% return_list && !"KEL" %in% return_list) {
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("VZPW" %in% return_list && !"VZP" %in% return_list) {
        return_list[[length(return_list)+1]] <- "VZP"
      }
      if("VZP" %in% return_list && !"AUCINFP" %in% return_list && !"KEL" %in% return_list) {
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
    } else if(toupper(map_data$MODEL) == 'M3' && toupper(map_data$DOSINGTYPE) == 'SS'){
      if("CMAXiDN" %in% return_list && !"CMAXi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "CMAXi"
      }
      if("AUCDN" %in% return_list && !"AUCALL" %in% return_list){
        return_list[[length(return_list)+1]] <- "AUCALL"
      }
      if("AUCLASTiDN" %in% return_list && !"AUCLASTi" %in% return_list){
        return_list[[length(return_list)+1]] <- "AUCLASTi"
      }
      if("AUCINFOC" %in% return_list && !"KEL" %in% return_list && !"AUCINFO" %in% return_list){
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("AUCINFODN" %in% return_list && !"AUCINFO" %in% return_list){
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("AUCINFPC" %in% return_list && !"KEL" %in% return_list && !"AUCINFP" %in% return_list){
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
      if("AUCINFPDN" %in% return_list && !"AUCINFP" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
      if("AUCTAUiDN" %in% return_list && !"AUCTAUi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
      if("VZFP" %in% return_list && !"AUCINFP" %in% return_list && !"KEL" %in% return_list) {
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
      if("CAVi" %in% return_list && !"AUCTAUi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
      if("CLFTAUi" %in% return_list && !"AUCTAUi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
      if("CLFTAUWi" %in% return_list && !"CLFTAUi" %in% return_list && !"AUCTAUi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "CLFTAUi"
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
      if("PTFi" %in% return_list && !"CMAXi" %in% return_list && !"CMINi" %in% return_list && !"CAVi" %in% return_list && "AUCTAUi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "CMAXi"
        return_list[[length(return_list)+1]] <- "CMINi"
        return_list[[length(return_list)+1]] <- "CAVi"
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
      if("PTRi" %in% return_list && !"CMAXi" %in% return_list && !"CMINi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "CMAXi"
        return_list[[length(return_list)+1]] <- "CMINi"
      }
      if("VZO" %in% return && !"AUCINFO" %in% return && !"KEL" %in% return) {
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("VZP" %in% return && !"AUCINFP" %in% return && !"KEL" %in% return) {
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
    } else if (toupper(map_data$MODEL) == 'M3' && toupper(map_data$DOSINGTYPE) == 'SD'){
      if("CMAXCi" %in% return && !"CMAXi" %in% return && !"KEL" %in% return) {
        return_list[[length(return_list)+1]] <- "CMAXi"
        return_list[[length(return_list)+1]] <- "KEL"
      }
      if("CMAXiDN" %in% return && !"CMAXi" %in% return) {
        return_list[[length(return_list)+1]] <- "CMAXi"
      }
      if("AUCDN" %in% return && !"AUCALL" %in% return){
        return_list[[length(return_list)+1]] <- "AUCALL"
      }
      if("AUCLASTCi" %in% return && !"AUCLASTi" %in% return && !"KEL" %in% return && !"TLAST" %in% return){
        return_list[[length(return_list)+1]] <- "AUCLASTi"
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "TLAST"
      }
      if("AUCLASTiDN" %in% return && !"AUCLASTi" %in% return){
        return_list[[length(return_list)+1]] <- "AUCLASTi"
      }
      if("AUCINFOC" %in% return && !"KEL" %in% return && !"AUCINFO" %in% return){
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("AUCINFODN" %in% return && !"AUCINFO" %in% return){
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("AUCINFPC" %in% return && !"KEL" %in% return && !"AUCINFP" %in% return){
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
      if("AUCINFPDN" %in% return && !"AUCINFP" %in% return) {
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
      if("CLOW" %in% return && !"CLO" %in% return) {
        return_list[[length(return_list)+1]] <- "CLO"
      }
      if("CLO" %in% return && !"AUCINFO" %in% return) {
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("CLPW" %in% return && !"CLP" %in% return) {
        return_list[[length(return_list)+1]] <- "CLP"
      }
      if("CLP" %in% return && !"AUCINFP" %in% return) {
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
      if("VZOW" %in% return && !"VZO" %in% return) {
        return_list[[length(return_list)+1]] <- "VZO"
      }
      if("VZO" %in% return && !"AUCINFO" %in% return && !"KEL" %in% return) {
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("VZPW" %in% return && !"VZP" %in% return) {
        return_list[[length(return_list)+1]] <- "VZP"
      }
      if("VZP" %in% return && !"AUCINFP" %in% return && !"KEL" %in% return) {
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
    } else if(toupper(map_data$MODEL) == 'M3' && toupper(map_data$DOSINGTYPE) == 'SS'){
      if("CMAXiDN" %in% return && !"CMAXi" %in% return) {
        return_list[[length(return_list)+1]] <- "CMAXi"
      }
      if("AUCDN" %in% return && !"AUCALL" %in% return){
        return_list[[length(return_list)+1]] <- "AUCALL"
      }
      if("AUCLASTiDN" %in% return && !"AUCLASTi" %in% return){
        return_list[[length(return_list)+1]] <- "AUCLASTi"
      }
      if("AUCINFOC" %in% return && !"KEL" %in% return && !"AUCINFO" %in% return){
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("AUCINFODN" %in% return && !"AUCINFO" %in% return){
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("AUCINFPC" %in% return && !"KEL" %in% return && !"AUCINFP" %in% return){
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
      if("AUCINFPDN" %in% return && !"AUCINFP" %in% return) {
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
      if("AUCTAUiDN" %in% return_list && !"AUCTAUi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
      if("VZFP" %in% return && !"AUCINFP" %in% return && !"KEL" %in% return) {
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
      if("CAVi" %in% return_list && !"AUCTAUi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
      if("CLFTAUi" %in% return_list && !"AUCTAUi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
      if("CLFTAUWi" %in% return_list && !"CLFTAUi" %in% return_list && !"AUCTAUi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "CLFTAUi"
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
      if("PTFi" %in% return_list && !"CMAXi" %in% return_list && !"CMINi" %in% return_list && !"CAVi" %in% return_list && "AUCTAUi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "CMAXi"
        return_list[[length(return_list)+1]] <- "CMINi"
        return_list[[length(return_list)+1]] <- "CAVi"
        return_list[[length(return_list)+1]] <- "AUCTAUi"
      }
      if("PTRi" %in% return_list && !"CMAXi" %in% return_list && !"CMINi" %in% return_list) {
        return_list[[length(return_list)+1]] <- "CMAXi"
        return_list[[length(return_list)+1]] <- "CMINi"
      }
      if("VZO" %in% return && !"AUCINFO" %in% return && !"KEL" %in% return) {
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFO"
      }
      if("VZP" %in% return && !"AUCINFP" %in% return && !"KEL" %in% return) {
        return_list[[length(return_list)+1]] <- "KEL"
        return_list[[length(return_list)+1]] <- "AUCINFP"
      }
    } else if (toupper(map_data$MODEL) == 'M4' && toupper(map_data$DOSINGTYPE) == 'SD'){
      if("AEPCT" %in% return_list && !"AE" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AE"
      }
      if("AETPCT" %in% return_list && !"AET" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AET"
      }
    } else if(toupper(map_data$MODEL) == 'M4' && toupper(map_data$DOSINGTYPE) == 'SS'){
      if("AEPCT" %in% return_list && !"AE" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AE"
      }
      if("AETPCT" %in% return_list && !"AET" %in% return_list) {
        return_list[[length(return_list)+1]] <- "AET"
      }
    }
  } else { # TGT/ARE THESE DEFAULTS IF NOT MAP PROVIDED????
    if(toupper(map_data$MODEL) == 'M1' && toupper(map_data$DOSINGTYPE) == 'SD'){
      return_list <- as.list(c("CMAXi", "CMINi", "CLASTi", "CMAXCi", "CMAXiDN", "TMAXi", "TMINi", "TLAST", "TLAG", "KEL", 
                          "KELTMLO", "KELTMHI", "KELNOPT", "KELRSQ", "KELRSQA", "THALF", "AUCALL", "AUCDN", "AUCLASTi",
                          "AUCLASTCi", "AUCLASTiDN", "AUMCLAST", "AUCT1_T2", "AUCINFO", "AUCINFP", "AUCINFOC", "AUCINFPC", "AUCINFODN", 
                          "AUCINFPDN", "AUMCINFO", "AUMCINFP", "MRTLAST", "MRTEVIFOi", "MRTEVIFPi", "AUCXPCTO", "AUCXPCTP", 
                          "AUMCXPCTO", "AUMCXPCTP", "CLO", "CLFO", "CLFOW", "CLFP", "CLFPW", "VZFO", "VZFOW", "VZFP", "VZFPW"))
    } else if(toupper(map_data$MODEL) == 'M1' && toupper(map_data$DOSINGTYPE) == 'SS'){
      return_list <- as.list(c("CMAXi", "CMINi", "CLASTi", "CMAXiDN", "TMAXi", "TMINi", "TLAST", "DIi", "TAUi", "TOLD",
                          "DOSEi", "AUCDN", "AUCLASTi", "AUCLASTiDN", "AUCINFODN", "AUCINFPDN", "AUCTAUi", "AUCTAUiDN",
                          "VZFP", "CLO", "CLFO", "CAVi", "CLFTAUi", "CLFTAUWi", "PTFi", "PTRi", "VZFTAUi", "VZFTAUWi",
                          "TLAG", "KEL", "KELTMLO", "KELTMHI", "KELNOPT", "KELRSQ", "KELRSQA", "THALF", "LASTTIME", "AUCALL",
                          "AUCINFO", "AUCINFP", "AUCINFOC", "AUCINFPC", "MRTEVIFOi", "MRTEVIFPi", "AUCXPCTO", "AUCXPCTP",
                          "AUCT1_T2"))
    } else if (toupper(map_data$MODEL) == 'M2' && toupper(map_data$DOSINGTYPE) == 'SD'){
      return_list <- as.list(c("C0", "V0", "CMAXi", "CLASTi", "CMAXCi", "CMAXiDN", "TMAXi", "TLAST", "KEL", 
                          "KELTMLO", "KELTMHI", "KELNOPT", "KELRSQ", "KELRSQA", "THALF", "AUCALL", "AUCDN", "AUCLASTi",
                          "AUCLASTCi", "AUCLASTiDN", "AUMCLAST", "AUCT1_T2", "AUCINFO", "AUCINFP", "AUCINFOC", "AUCINFPC", "AUCINFODN", 
                          "AUCINFPDN", "AUMCINFO", "AUMCINFP", "MRTLAST", "MRTIVIFOi", "MRTIVIFPi", "AUCXPCTO", "AUCXPCTP", 
                          "AUMCXPCTO", "AUMCXPCTP", "CLO", "CLOW", "CLP", "CLPW", "VZO", "VZOW", "VZP", "VZPW"))
    } else if (toupper(map_data$MODEL) == 'M2' && toupper(map_data$DOSINGTYPE) == 'SS'){
      return_list <- as.list(c("V0","CMAXi", "CMINi", "CLASTi", "CMAXiDN", "TMAXi", "TMINi", "TLAST", "DIi", "TAUi", "TOLD",
                          "DOSEi", "AUCDN", "AUCLASTi", "AUCLASTiDN", "AUCINFODN", "AUCINFPDN", "AUCTAUi", "AUCTAUiDN",
                          "VZFP", "CAVi", "CLTAUi", "CLTAUWi", "PTFi", "PTRi", "VZO", "VZP", "C0", "KEL", "KELTMLO", "KELTMHI", "KELNOPT", "KELRSQ", 
                          "KELRSQA", "THALF", "LASTTIME", "AUCALL", "AUCINFO", "AUCINFP", "AUCINFOC", "AUCINFPC", "MRTEVIFOi", 
                          "MRTEVIFPi", "AUCXPCTO", "AUCXPCTP", "AUCT1_T2"))
    } else if (toupper(map_data$MODEL) == 'M3' && toupper(map_data$DOSINGTYPE) == 'SD'){
      return_list <- as.list(c("CMAXi", "CLASTi", "CMAXCi", "CMAXiDN", "TMAXi", "TLAST", "KEL", 
                          "KELTMLO", "KELTMHI", "KELNOPT", "KELRSQ", "KELRSQA", "THALF", "AUCALL", "AUCDN", "AUCLASTi",
                          "AUCLASTCi", "AUCLASTiDN", "AUMCLAST", "AUCT1_T2", "AUCINFO", "AUCINFP", "AUCINFOC", "AUCINFPC", "AUCINFODN", 
                          "AUCINFPDN", "AUMCINFO", "AUMCINFP", "MRTLAST", "MRTIVIFOi", "MRTIVIFPi", "AUCXPCTO", "AUCXPCTP", 
                          "AUMCXPCTO", "AUMCXPCTP", "CLO", "CLOW", "CLP", "CLPW", "VZO", "VZOW", "VZP", "VZPW"))
    } else if(toupper(map_data$MODEL) == 'M3' && toupper(map_data$DOSINGTYPE) == 'SS'){
      return_list <- as.list(c("CMAXi", "CMINi", "CLASTi", "CMAXiDN", "TMAXi", "TMINi", "TLAST", "DIi", "TAUi", "TOLD",
                          "DOSEi", "AUCDN", "AUCLASTi", "AUCLASTiDN", "AUCINFODN", "AUCINFPDN", "AUCTAUi", "AUCTAUiDN",
                          "VZFP", "CAVi", "CLTAUi", "CLTAUWi", "PTFi", "PTRi", "VZO", "VZP", "KEL", "KELTMLO", "KELTMHI", "KELNOPT", "KELRSQ", 
                          "KELRSQA", "THALF", "LASTTIME", "AUCALL", "AUCINFO", "AUCINFP", "AUCINFOC", "AUCINFPC", "MRTEVIFOi", 
                          "MRTEVIFPi", "AUCXPCTO", "AUCXPCTP", "AUCT1_T2"))
    } else if (toupper(map_data$MODEL) == 'M4' && toupper(map_data$DOSINGTYPE) == 'SD'){
      return_list <- as.list(c("AET", "AETPCT", "AE", "AEPCT", "MAXRATEi", "TMAXRATEi", "RATELASTi", "MIDPTLASTi", "TLAG", "KEL", "KELTMLO", "KELTMHI", 
                          "KELNOPT", "KELRSQ", "KELRSQA", "THALF", "AUCDN", "AURCALL", "AURCLAST", "AURCINFO", "AURCINFP", "AURCXPCTO", "AURCXPCTP",
                          "AURCT1_T2"))
    } else if(toupper(map_data$MODEL) == 'M4' && toupper(map_data$DOSINGTYPE) == 'SS'){
      return_list <- as.list(c("AET", "AETPCT", "MAXRATEi", "TMAXRATEi", "RATELASTi", "MIDPTLASTi", "DIi", "TAUi", "TOLD", "DOSEi",
                          "AE", "AEPCT", "TLAG", "KEL", "KELTMLO", "KELTMHI", "KELNOPT", "KELRSQ", "KELRSQA", "THALF", 
                          "AUCDN", "AURCALL", "AURCLAST", "AURCINFO", "AURCINFP", "AURCXPCTO", "AURCXPCTP", "AURCT1_T2")) 
    }
  }
  ##cat('return_list2:\n')
  ##print(return_list)
  
  data_out <- NULL
  if(toupper(map_data$MODEL) == 'M1' && toupper(map_data$DOSINGTYPE) == 'SD'){
    data_out <- run_M1_SD_computation(data = merged_data, map = map_data, method = method, model = toupper(map_data$MODEL), 
                                      parameter = toupper(map_data$DOSINGTYPE), return_list = return_list) 
  } else if(toupper(map_data$MODEL) == 'M1' && toupper(map_data$DOSINGTYPE) == 'SS'){
    data_out <- run_M1_SS_computation(data = merged_data, map = map_data, method = method, model = toupper(map_data$MODEL), 
                                      parameter = toupper(map_data$DOSINGTYPE), return_list = return_list) 
  } else if (toupper(map_data$MODEL) == 'M2' && toupper(map_data$DOSINGTYPE) == 'SD'){
    data_out <- run_M2_SD_computation(data = merged_data, map = map_data, method = method, model = toupper(map_data$MODEL), 
                                      parameter = toupper(map_data$DOSINGTYPE), return_list = return_list) 
  } else if (toupper(map_data$MODEL) == 'M2' && toupper(map_data$DOSINGTYPE) == 'SS'){
    data_out <- run_M2_SS_computation(data = merged_data, map = map_data, method = method, model = toupper(map_data$MODEL), 
                                      parameter = toupper(map_data$DOSINGTYPE), return_list = return_list) 
  } else if (toupper(map_data$MODEL) == 'M3' && toupper(map_data$DOSINGTYPE) == 'SD'){
    data_out <- run_M3_SD_computation(data = merged_data, map = map_data, method = method, model = toupper(map_data$MODEL), 
                                      parameter = toupper(map_data$DOSINGTYPE), return_list = return_list) 
  } else if(toupper(map_data$MODEL) == 'M3' && toupper(map_data$DOSINGTYPE) == 'SS'){
    data_out <- run_M3_SS_computation(data = merged_data, map = map_data, method = method, model = toupper(map_data$MODEL), 
                                      parameter = toupper(map_data$DOSINGTYPE), return_list = return_list) 
  } else if (toupper(map_data$MODEL) == 'M4' && toupper(map_data$DOSINGTYPE) == 'SD'){
    data_out <- run_M4_SD_computation(data = merged_data, map = map_data, method = method, model = toupper(map_data$MODEL), 
                                      parameter = toupper(map_data$DOSINGTYPE), return_list = return_list) 
  } else if(toupper(map_data$MODEL) == 'M4' && toupper(map_data$DOSINGTYPE) == 'SS'){
    data_out <- run_M4_SS_computation(data = merged_data, map = map_data, method = method, model = toupper(map_data$MODEL), 
                                      parameter = toupper(map_data$DOSINGTYPE), return_list = return_list) 
  }

  return(data_out)
}


