#' Run M3 SS Computation
#'
#' This function will compute all the relevant parameters for a M3 model Stedy State (SS).\cr
#'
#' @details
#' \strong{Methods:} You can use the following methods to calculate AUC: \cr
#' \enumerate{
#'  \item \strong{1: Linear-Log Trapazoidal Rule}(default method): The linear method is used up to Tmax (the
#'  first occurance of Cmax) and the log trapezoidal method is used for the remainder of the profile. If
#'  Ci or Ci+1 is 0 then the linear trapezoidal rule is used.
#'  \item \strong{2: Linear Trapazoidal Rule}: The linear method is used for the entire profile.
#'  \item \strong{3: Log Trapazoidal Rule}: The log trapezoidal method is used for the entire profile. If
#'  Ci or Ci+1 is 0 then the linear trapezoidal rule is used.
#'  \item \strong{4: Linear Up - Log Down Trapazoidal Rule}: Linear trapezoidal while the concentrations
#'  are increasing and log trapezoidal while the concentration are decreasing, the assessment is made on
#'  a step basis for each portion of the profile i.e. t1 to t2. If Ci or Ci+1 is 0 then the linear
#'  trapezoidal rule is used.
#' }
#' The following are the functions that this function uses to generate parameters: \cr
#' \strong{Additional Functions Used} \cr
#' \enumerate{
#'  \item \strong{cmax}: Refer to \code{\link{cmax}} for more details
#'  \item \strong{cmax_c}: Refer to \code{\link{cmaxc}} for more details
#'  \item \strong{cmax_dn}: Refer to \code{\link{cmax_dn}} for more details
#'  \item \strong{cmin}: Refer to \code{\link{cmin}} for more details
#'  \item \strong{clast}: Refer to \code{\link{clast}} for more details
#'  \item \strong{tmax}: Refer to \code{\link{tmax}} for more details
#'  \item \strong{tmin}: Refer to \code{\link{tmin}} for more details
#'  \item \strong{tlast}: Refer to \code{\link{tlast}} for more details
#'  \item \strong{kel}: Refer to \code{\link{kel}} for more details
#'  \item \strong{kelr}: Refer to \code{\link{kel_r}} for more details
#'  \item \strong{lasttime}: Refer to \code{\link{lasttime}} for more details
#'  \item \strong{auc_all}: Refer to \code{\link{auc_all}} for more details
#'  \item \strong{auc_dn}: Refer to \code{\link{auc_dn}} for more details
#'  \item \strong{auc_last}: Refer to \code{\link{auc_last}} for more details
#'  \item \strong{auc_last_c}: Refer to \code{\link{auc_lastc}} for more details
#'  \item \strong{auc_last_dn}: Refer to \code{\link{auc_dn}} for more details
#'  \item \strong{aumc_last}: Refer to \code{\link{aumc_last}} for more details
#'  \item \strong{auc_t1_t2}: Refer to \code{\link{auc_t1_t2}} for more details
#'  \item \strong{auc_inf_o}: Refer to \code{\link{auc_inf_o}} for more details
#'  \item \strong{auc_inf_o_c}: Refer to \code{\link{auc_inf_oc}} for more details
#'  \item \strong{auc_inf_o_dn}: Refer to \code{\link{auc_dn}} for more details
#'  \item \strong{cest}: Refer to \code{\link{cest}} for more details
#'  \item \strong{auc_inf_p}: Refer to \code{\link{auc_inf_p}} for more details
#'  \item \strong{auc_inf_p_c}: Refer to \code{\link{auc_inf_pc}} for more detail
#'  \item \strong{auc_inf_p_dn}: Refer to \code{\link{auc_dn}} for more details
#'  \item \strong{aumc_inf_o}: Refer to \code{\link{aumc_inf_o}} for more details
#'  \item \strong{aumc_inf_p}: Refer to \code{\link{aumc_inf_p}} for more details
#'  \item \strong{auc_tau}: Refer to \code{\link{auc_tau}} for more details
#'  \item \strong{aumc_tau}: Refer to \code{\link{aumc_tau}} for more details
#'  \item \strong{mrt_last}: Refer to \code{\link{mrt_last}} for more details
#'  \item \strong{mrt_ivif_o}: Refer to \code{\link{mrt_ivif_o}} for more details
#'  \item \strong{mrt_ivif_p}: Refer to \code{\link{mrt_ivif_p}} for more detail
#'  \item \strong{auc_XpctO}: Refer to \code{\link{auc_XpctO}} for more details
#'  \item \strong{auc_XpctP}: Refer to \code{\link{auc_XpctP}} for more details
#'  \item \strong{aumc_XpctO}: Refer to \code{\link{aumc_XpctO}} for more details
#'  \item \strong{aumc_XpctP}: Refer to \code{\link{aumc_XpctP}} for more details
#'  \item \strong{cav}: Refer to \code{\link{cav}} for more details
#'  \item \strong{cltau}: Refer to \code{\link{cltau}} for more details
#'  \item \strong{cltauw}: Refer to \code{\link{cltauw}} for more details
#'  \item \strong{ptr}: Refer to \code{\link{ptr}} for more details
#'  \item \strong{ptf}: Refer to \code{\link{ptf}} for more details
#'  \item \strong{vzo}: Refer to \code{\link{vzo}} for more details
#'  \item \strong{vzp}: Refer to \code{\link{vzp}} for more details
#' }
#'
#' @section Note:
#' By default all the return list options are selected and calculated if 'parameter_list' is not specified. Please refer to \code{\link{mct_template}}
#' to get more calrification on how to specify which parameters to calculate to this function if you wish to subset the default caculated parameters. \cr
#' By default 'display_list' argument is empty, which means that this function will return all caculated parameters specifed by the 'parameter_list' argument.
#' Only specfiy a list of parameters to the 'display_list' if you want to subset the calculated parameters returned as a result of this function. \cr
#' By default 'return_list' argument is empty, which means that this function will not append parameters passed from 'data' argument.
#' Only specfiy a list of parameters to the 'return_list' if you want to return them as a result of this function. \cr
#' If 'optimize_kel' is FALSE AND KEL is not one of the parameters (default or specified by 'parameter_list' argument) then
#' this functions will return a datafram. If 'optimize_kel' is FALSE AND KEL is one of the parameters (default or specified by 'parameter_list' argument) then
#' this functions will return a list with 'data_out' and 'est_data'. If 'optimize_kel' is TRUE AND KEL is one of the parameters (default or specified
#' by 'parameter_list' argument) then this functions will return a list with 'data_out', 'optimized_kel_flag' and 'est_data'.
#' If 'optimize_kel' is TRUE AND KEL is not one of the parameters (default or specified by 'parameter_list' argument) then
#' this functions will return a list with 'data_out' and 'optimized_kel_flag'. \cr
#' Please note that this function does not contain all the features of a M3 SS computation, so it is recommended that you
#' use the parent function \code{\link{run_computation}}
#'
#' @param data The dataframe that contains the raw data
#' @param map The dataframe that contains the map data
#' @param method The methos of calculation for Area Under the Curve (see Details below)
#' @param parameter_list The list of parameters to calculate (empty by default)
#' @param display_list The list of parameters to return (empty by default)
#' @param return_list The list of parameters to return from the original data (empty by default)
#'
#' @section Returns:
#' \strong{Dataframe} \cr
#' \itemize{
#'  \item M3SS_Parameters: Caculated default/specified parameters
#' }
#' OR \cr
#' \strong{List} \cr
#' \itemize{
#'  \item data_out: Calculated default/specified M3SS Parameters
#'  \item optimized_kel_flag: Optimized KEL flag data used to calulate KEL based parameters
#'  \item est_data: Calculated Estimated Parameters
#' }
#'
#' @examples
#'
#' @author
#' \itemize{
#'  \item \strong{Thomas Tensfeldt, Pfizer, & Rudraya Technical Team}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{thomas.g.tensfeldt@.com}
#' }
#' @export
### 2019-10-10/TGT/ Remove display_list argument and incorporate model_regex argument
###run_M3_SS_computation <- function(data = NULL, map = NULL, method = 1, parameter_list = list(), display_list = list(), return_list = list()){
run_M3_SS_computation <- function(data = NULL, map = NULL, method = 1, model_regex = "^M3(SS)*?$", parameter_list = list(), return_list = list(), optimize_kel_debug = FALSE){
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
  if(!is.list(parameter_list)){
    stop("Invalid list provided for 'parameter_list'! Please provide a valid list")
  }
###  if(!is.list(display_list)){
###    stop("Invalid list provided for 'display_list'! Please provide a valid list")
###  }
  if(!is.list(return_list)){
    stop("Invalid list provided for 'return_list'! Please provide a valid list")
  }
  #if(!(is.logical(generate_nominal_conc) && !is.na(generate_nominal_conc) && !is.null(generate_nominal_conc))){
  #  stop("Values provided via 'generate_nominal_conc' is not valid! Please provide either 'TRUE' or 'FALSE'")
  #} else {
  #  if(generate_nominal_conc){
  #    if(!("CONCMOD" %in% names(map_data) && "CONCMODU" %in% names(map_data))){
  #      stop("Dataset provided via 'map' does not contain the required columns needed for generating nominal concentration")
  #    }
  #  }
  #}

### 2019-09-29/TGT/ added standardized "validate_timeconc_data" routine to resolve issue
###                 that was not handled if TIME is set to a value in the input dataset directly
### 2019-09-11/TGT/ all map updates complete in run_computation now
###  timeconcvalues <- validate_timeconc_data(map_data, data_data)
###  map_data$TIME  <- timeconcvalues$time
###  map_data$TIMEU <- timeconcvalues$timeu
###  map_data$CONC  <- timeconcvalues$conc
###  map_data$CONCU <- timeconcvalues$concu

  if(!(parameter_required("^SDEID$",names(data_data)))) {
    stop("Value for 'SDEID' provided via 'map' is not present in the dataset provided via 'data'")
  }

###  if(!("SDEID" %in% names(map_data) && "TIME" %in% names(map_data) && "CONC" %in% names(map_data))){
###    stop("Dataset provided via 'map' does not contain the required columns 'SDEID', 'TIME' and 'CONC'")
###  } else {
###    if(!(map_data$TIME %in% c("Nominal", "Actual"))){
###      stop("'TIME' value provided via 'map' does not have valid value! Please provide either 'Nominal' or 'Actual'")
###    } else {
###      if(casefold(map_data$TIME) == 'nominal'){
###        map_data$TIME <- "NOMTIME"
###        map_data$TIMEU <- "NOMTIMEU"
###      } else if(casefold(map_data$TIME) == 'actual'){
###        map_data$TIME <- "ACTTIME"
###        map_data$TIMEU <- "ACTTIMEU"
###      }
###      if(!(map_data$TIME %in% names(map_data))){
###        stop("'TIME' value provided via 'map' is not present in 'map' dataset")
###      }
###    }
###  }
###  if(!(map_data$SDEID %in% names(data_data) && map_data[[map_data$TIME]] %in% names(data_data) && map_data$CONC %in% names(data_data))){
###    stop("Values for 'SDEID', 'TIME' and 'CONC' provided via 'map' are not present in the dataset provided via 'data'")
###  }

###  model_regex <- "^M3(SS)*?$"

  ss_dose <- c("DI1F", "DI2F", "DI3F", "DI4F", "DI5F")

  if(any(ss_dose %in% names(data_data))){
    di_col <- sum(ss_dose %in% names(data_data))
  } else {
    stop("Unable to find dosing interval for Stedy State data! Please provide a valid 'data' parameter")
  }

  auc_list <- c("AUCT", "AUCTDN")
  auc_par <- c("AUCT1_T2")

### 2019-08-22/TGT/ Modify
###  CMAXiDN -> CMAXDNi
###  AUCLASTiDN -> AUCLASTDNi
###  AUCTAUiDN -> AUCTAUDNi
       
### 2019-08-30/TGT/ Remove "CMAXC"/"CMAXCi" since only valid for M2 Bolus administration applications
###  interval_list <- c("CMAXi", "CMAXCi", "CMAXDNi", "CMINi", "CLASTi", "TMAXi", "TMINi", "TLASTi", "LASTTIMEi", "AUCDN",
  interval_list <- c("CMAXi", "CMAXDNi", "CMINi", "CLASTi", "TMAXi", "TMINi", "TLASTi", "LASTTIMEi", "AUCDN",
                     "AUCLASTi", "AUCLASTCi", "AUCLASTDNi", "AUMCLASTi", "AUCINFOi", "AUCINFPi", "AUMCINFOi", "AUMCINFPi",
                     "AUCTAUi", "AUCTAUDNi", "AUMCTAUi", "MRTLASTi", "MRTIVIFOi", "MRTIVIFPi", "AUCXPCTOi", "AUCXPCTPi",
                     "AUMCXPTOi", "AUMCXPTPi", "CAVi", "CLTAUi", "CLTAUWi", "PTFi", "PTRi", "VZO", "VZP", "DIi", "TAUi",
                     "TOLD", "DOSEi")
### 2019-08-30/TGT/ Remove "CMAXC"/"CMAXCi" since only valid for M2 Bolus administration applications
###  regular_list <- c("CMAX", "CMAXC", "CMAXDN", "CMIN", "CLAST", "TMAX", "TMIN", "TLAST", "KEL", "KELC0", "KELTMLO",
  regular_list <- c("CMAX", "CMAXDN", "CMIN", "CLAST", "TMAX", "TMIN", "TLAST", "KEL", "KELC0", "KELTMLO",
                    "KELTMHI", "KELNOPT", "KELRSQ", "KELRSQA", "THALF", "THALFF", "LASTTIME", "AUCALL", "AUCLAST", "AUCLASTC",
                    "AUCLASTDN", "AUCINFO", "AUCINFOC", "AUCINFODN", "CEST", "AUCINFP", "AUCINFPC", "AUCINFPDN", "MRTLAST", "AUCXPCTO",
                    "AUCXPCTP", "AUMCXPTO", "AUMCXPTP")
  regular_int_type <- NULL
  auc_pair_check <- FALSE

  #auc_col <- length(unique(data_data[,map_data[[map_data$TIME]]]))-1
  #col <- 21 + 2*auc_col + 24*di_col + 1

### 2019-08-08/TGT/ auc_len is intended to represent the maximum # of AUC intervals that could be generated from the # of
###                 unique time points available for any of the profiles in the dataset. This assumes that data removed
###                 (FLGEXSDE for example) from the dataset do not contribute to these time-intervals.
  index1 <- data_data[,map_data$SDEID]
  auc_len <- max(tapply(index1, index1, length))-1
  reg_col <- sum(regular_list %in% parameter_list) + ifelse(any(c("KELRSQ","KELRSQA") %in% parameter_list), 1, 0)
  auc_col <- ifelse(sum(auc_list %in% parameter_list) > 0, sum(auc_list %in% parameter_list)+1, 0)
  interval_col <- sum(interval_list %in% parameter_list)
### 2019-08-29/TGT/ Validate # of Partial AUCs not from AUCNPAIR (remove from Computation Engine Specification)
###                  but rather directly from the entries of the AUC.#.T1,AUC.#.T2 values themselves. 
###    auc_par_len <- ifelse(auc_par %in% parameter_list && 'AUCNPAIR' %in% names(map_data), ifelse(!(is.null(map_data$AUCNPAIR) || is.na(suppressWarnings(as.numeric(map_data$AUCNPAIR)))), suppressWarnings(as.numeric(map_data$AUCNPAIR)), 0), 0)
###
  aucpari <- grep('^AUC.([0-9]+?).T[1-2]$', names(map_data), ignore.case=TRUE, perl=TRUE)
  if(length(aucpari)>0) {
      auc_par_len <- floor(length(aucpari)/2)
      g <- names(map_data)[aucpari]
      ### Ensure pairs are coherent
      aucpar1 <- grep('^AUC.([0-9]+?).T[1]$', names(map_data), ignore.case=TRUE, perl=TRUE)
      aucpar2 <- grep('^AUC.([0-9]+?).T[2]$', names(map_data), ignore.case=TRUE, perl=TRUE)
      if(length(aucpar1)!=length(aucpar2)) {
          msg <- paste0(function_name, ': unequal Partial AUCs specified: ', names(map_data)[aucpar1], ' vs ', names(map_data)[aucpar2])
          warning(msg)
          msg <- paste0(function_name, ': no Partial AUCs will be generated')
          warning(msg)
          auc_par_len <- 0
      }
  } else { auc_par_len <- 0 }

  col <- reg_col + (auc_col * auc_len) + (interval_col * di_col) + 1 + (2 * (auc_len+1))

  ### Determine DOSEs in dosevar, a vector of dose names pointing into map_data
  doselist <- names(parameter_indices("^DOSELIST$", names(map_data), simplify=FALSE))
  dosenames <- unlist(strsplit(map_data[,doselist], ";"))
  dosevar <- as.character(map[,dosenames])
  if(!any(duplicated(as.character(unlist(dosevar))))){
    dosenames <- dosenames[!duplicated(as.character(unlist(dosevar)))]
  }

  ### 2019-09-17/TGT/ Precompute list of required parameters for col_names, parameter function evaluation and row_data generation  
  comp_required <- list()
  disp_required <- list()
  plist <- parameter_list
  for(i in model_parameters()) {
    rg <- parameter_regex(i)
###      cat('i: ', i, ' is in parameter_list: ', i %in% unlist(plist), ' parameters dependent upon ', i, ': \n')
###      print(dependent_parameters(rg))
    pr <- parameter_required(rg, parameter_list=plist)
    dp <- parameter_required(dependent_parameters(rg), plist)
    comp_required[[i]] <- pr || dp
    disp_required[[i]] <- pr
###    cat('i: ', i, ' pr: ', pr, ' dp: ', dp, ' comp_required[[', i, ']]: ', comp_required[[i]], ' disp_required[[', i, ']]: ', disp_required[[i]], '\n')
    
###    if(comp_required[[i]]) { cat('parameter: ', i, ' regex: ', rg, ' required: ', comp_required[[i]], '\n') }
  }
###cat('comp_required: \n')
###print(names(comp_required))
###cat('disp_required: \n')
###print(names(disp_required))
  
  if("FLGACCEPTKELCRIT" %in% names(map_data) && (("KEL" %in% parameter_list && "KELNOPT" %in% parameter_list) || "KELRSQ" %in% parameter_list)) {
    if(length(unlist(strsplit(as.character(map_data$FLGACCEPTKELCRIT), ","))) > 0){
      col <- col + 1
    }
  }
  if("FLGEMESIS" %in% names(map_data) && ("TMAXi" %in% parameter_list)){
    col <- col + 1
  }
  if("LASTTIMEACCEPTCRIT" %in% names(map_data) && "LASTTIME" %in% parameter_list) {
    col <- col + 1
  }
  if("FLGACCEPTPREDOSECRIT" %in% names(map_data) && ("CMAXi" %in% parameter_list)){
    col <- col + 1
  }
  if(auc_par_len > 0){
    for(t in 1:auc_par_len){
      val_t1 <- paste0("AUC.", t, ".T1")
      val_t2 <- paste0("AUC.", t, ".T2")

      if(val_t1 %in% names(map_data) && val_t2 %in% names(map_data)){
        if((!is.numeric(suppressWarnings(as.numeric(map_data[, val_t1]))) || is.na(suppressWarnings(as.numeric(map_data[, val_t1])))) && (!is.numeric(suppressWarnings(as.numeric(map_data[, val_t2]))) || is.na(suppressWarnings(as.numeric(map_data[, val_t2]))))){
          warning(paste0("'", val_t1, "' and '", val_t2, "' value provided via 'map' is not valid! Please provide a numeric value!"))
        } else if(!is.numeric(suppressWarnings(as.numeric(map_data[, val_t1]))) || is.na(suppressWarnings(as.numeric(map_data[, val_t1])))){
          warning(paste0("'", val_t1, "' value provided via 'map' is not valid! Please provide a numeric value!"))
        } else if(!is.numeric(suppressWarnings(as.numeric(map_data[, val_t2]))) || is.na(suppressWarnings(as.numeric(map_data[, val_t2])))){
          warning(paste0("'", val_t2, "' value provided via 'map' is not valid! Please provide a numeric value!"))
        } else {
          col <- col + 1
          auc_pair_check <- TRUE
        }
      } else {
        if(!(val_t1 %in% names(map_data)) && !(val_t2 %in% names(map_data))){
          warning(paste0("'", val_t1, "' and '", val_t2, "' are not present in the dataset provided via 'map'"))
        } else if(!(val_t1 %in% names(map_data))){
          warning(paste0("'", val_t1, "' is not present in the dataset provided via 'map'"))
        } else if(!(val_t2 %in% names(map_data))){
          warning(paste0("'", val_t2, "' is not present in the dataset provided via 'map'"))
        }
      }
    }
  }

### 2019-09-17/TGT/
###  if("KEL" %in% parameter_list){
    elist <- c("PKDATAROWID", "SDEID","TIME","CEST_KEL","CEST_INT","CEST_EXT","CEST_C0","CEST_TLAST")
    est_data <- data.frame(matrix(ncol = length(elist), nrow = 0))
    names(est_data) <- elist
###  }
  
### 2018-08-09/TGT/ Re-position timing of creation of template computation_df
###                 and base it on "length(col_names)" rather than "col"
###  computation_df <- data.frame(matrix(ncol = col, nrow = 0))
###

  col_names <- c("SDEID")
  
  if(disp_required[["DOSEi"]]) {
###    col_names <- c(col_names, rep(paste0("DOSE",1:di_col)))
    col_names <- c(col_names, dosenames)
###    regular_int_type <- c(regular_int_type, rep(paste0("DOSE",1:di_col)))
    regular_int_type <- c(regular_int_type, dosenames)
  }
  
  if(disp_required[["DOSEC"]]) {
    col_names <- c(col_names, "DOSEC")
    regular_int_type <- c(regular_int_type, "DOSEC")
  }
  if(disp_required[["DOSECi"]]) {
    col_names <- c(col_names, rep(paste0("DOSEC",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("DOSEC",1:di_col)))
  }
  if(disp_required[["DOFi"]]){
    col_names <- c(col_names, rep(paste0("DOF",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("DOF",1:di_col)))
  }
  if(disp_required[["CENDINFi"]]){
    col_names <- c(col_names, rep(paste0("CENDINF",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("CENDINF",1:di_col)))
  }
  if(disp_required[["CENDINFDNi"]]){
    col_names <- c(col_names, rep(paste0("CENDINFDN",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("CENDINFDN",1:di_col)))
  }
  if(disp_required[["TENDINFi"]]){
    col_names <- c(col_names, rep(paste0("TENDINF",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("TENDINF",1:di_col)))
  }
###  if("CMAX" %in% parameter_list) {
  if(disp_required[["CMAX"]]) {
    col_names <- c(col_names, "CMAX")
    regular_int_type <- c(regular_int_type, "CMAX")
  }
###  if("CMAXi" %in% parameter_list) {
  if(disp_required[["CMAXi"]]) {
    col_names <- c(col_names, rep(paste0("CMAX",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("CMAX",1:di_col)))
  }
### 2019-08-30/TGT/ Remove "CMAXC"/"CMAXCi" since only valid for M2 Bolus administration applications
###  if("CMAXC" %in% parameter_list && "CMAX" %in% parameter_list && "TMAX" %in% parameter_list && "KEL" %in% parameter_list) {
###    col_names <- c(col_names, "CMAXC")
###    regular_int_type <- c(regular_int_type, "CMAXC")
###  }
### 2019-08-30/TGT/ Remove "CMAXC"/"CMAXCi" since only valid for M2 Bolus administration applications
###  if("CMAXCi" %in% parameter_list && "CMAXi" %in% parameter_list && "TMAXi" %in% parameter_list && "KEL" %in% parameter_list) {
###    col_names <- c(col_names, rep(paste0("CMAXC",1:di_col)))
###    regular_int_type <- c(regular_int_type, rep(paste0("CMAXC",1:di_col)))
###  }
###  if("CMAXDN" %in% parameter_list && "CMAX" %in% parameter_list) {
  if(disp_required[["CMAXDN"]]) {
    col_names <- c(col_names, "CMAXDN")
    regular_int_type <- c(regular_int_type, "CMAXDN")
  }
###  if("CMAXDNi" %in% parameter_list && "CMAXi" %in% parameter_list) {
  if(disp_required[["CMAXDNi"]]) {
    col_names <- c(col_names, rep(paste0("CMAXDN",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("CMAXDN",1:di_col)))
  }
  if(disp_required[["FLGACCEPTPREDOSE"]] && "FLGACCEPTPREDOSECRIT" %in% names(map_data)){
    col_names <- c(col_names, "FLGACCEPTPREDOSE")
  }
###  if("CMIN" %in% parameter_list) {
  if(disp_required[["CMIN"]]) {
    col_names <- c(col_names, "CMIN")
    regular_int_type <- c(regular_int_type, "CMIN")
  }
###  if("CMINi" %in% parameter_list) {
  if(disp_required[["CMINi"]]) {
    col_names <- c(col_names, rep(paste0("CMIN",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("CMIN",1:di_col)))
  }
  if(disp_required[["CMINDN"]]){
    col_names <- c(col_names, "CMINDN")
    regular_int_type <- c(regular_int_type, "CMINDN")
  }
  if(disp_required[["CMINDNi"]]){
    col_names <- c(col_names, rep(paste0("CMINDN",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("CMINDN",1:di_col)))
  }
###  if("CLAST" %in% parameter_list) {
  if(disp_required[["CLAST"]]) {
    col_names <- c(col_names, "CLAST")
    regular_int_type <- c(regular_int_type, "CLAST")
  }
###  if("CLASTi" %in% parameter_list) {
  if(disp_required[["CLASTi"]]) {
    col_names <- c(col_names, rep(paste0("CLAST",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("CLAST",1:di_col)))
  }
###  if("TMAX" %in% parameter_list) {
  if(disp_required[["TMAX"]]) {
    col_names <- c(col_names, "TMAX")
    regular_int_type <- c(regular_int_type, "TMAX")
  }
###  if("TMAXi" %in% parameter_list) {
  if(disp_required[["TMAXi"]]) {
    col_names <- c(col_names, rep(paste0("TMAX",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("TMAX",1:di_col)))
  }
  if(disp_required[["FLGACCEPTTMAX"]] && "FLGEMESIS" %in% names(map_data)){
    col_names <- c(col_names, "FLGACCEPTTMAX")
  }
###  if("TMIN" %in% parameter_list) {
  if(disp_required[["TMIN"]]) {
    col_names <- c(col_names, "TMIN")
    regular_int_type <- c(regular_int_type, "TMIN")
  }
###  if("TMINi" %in% parameter_list) {
  if(disp_required[["TMINi"]]) {
    col_names <- c(col_names, rep(paste0("TMIN",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("TMIN",1:di_col)))
  }
###  if("TLAST" %in% parameter_list) {
  if(disp_required[["TLAST"]]) {
    col_names <- c(col_names, "TLAST")
    regular_int_type <- c(regular_int_type, "TLAST")
  }
###  if("TLASTi" %in% parameter_list) {
  if(disp_required[["TLASTi"]]) {
    col_names <- c(col_names, rep(paste0("TLAST",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("TLAST",1:di_col)))
  }
  if(disp_required[["CTROUGHi"]]){
    col_names <- c(col_names, rep(paste0("CTROUGH",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("CTROUGH",1:di_col)))
  }
  if(disp_required[["CTROUGHENDi"]]){
    col_names <- c(col_names, rep(paste0("CTROUGHEND",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("CTROUGHEND",1:di_col)))
  }
  if(disp_required[["PTROUGHRi"]]){
    col_names <- c(col_names, rep(paste0("PTROUGHR",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("PTROUGHR",1:di_col)))
  }
  if(disp_required[["PTROUGHRENDi"]]){
    col_names <- c(col_names, rep(paste0("PTROUGHREND",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("PTROUGHREND",1:di_col)))
  }
###  if("KEL" %in% parameter_list) {
  if(disp_required[["KEL"]]) {
    col_names <- c(col_names, "KEL")
    regular_int_type <- c(regular_int_type, "KEL")
  }
###  if("KELC0" %in% parameter_list) {
  if(disp_required[["KELC0"]]) {
    col_names <- c(col_names, "KELC0")
    regular_int_type <- c(regular_int_type, "KELC0")
  }
###  if("KELTMLO" %in% parameter_list) {
  if(disp_required[["KELTMLO"]]) {
    col_names <- c(col_names, "KELTMLO")
    regular_int_type <- c(regular_int_type, "KELTMLO")
  }
###  if("KELTMHI" %in% parameter_list) {
  if(disp_required[["KELTMHI"]]) {
    col_names <- c(col_names, "KELTMHI")
    regular_int_type <- c(regular_int_type, "KELTMHI")
  }
###  if("KELNOPT" %in% parameter_list) {
  if(disp_required[["KELNOPT"]]) {
    col_names <- c(col_names, "KELNOPT")
    regular_int_type <- c(regular_int_type, "KELNOPT")
  }
### 2019-08-12/TGT/ Modify this to explicitly refer to KELR rather than impute it
###  if("KELRSQ" %in% parameter_list || "KELRSQA" %in% parameter_list){
###    col_names <- c(col_names, "KELR")
###    regular_int_type <- c(regular_int_type, "KELR")
###  }
###  if(parameter_required("^KELR$", parameter_list)){
  if(disp_required[["KELR"]]){
    col_names <- c(col_names, "KELR")
    regular_int_type <- c(regular_int_type, "KELR")
  }
###  if("KELRSQ" %in% parameter_list){
  if(disp_required[["KELRSQ"]]){
    col_names <- c(col_names, "KELRSQ")
    regular_int_type <- c(regular_int_type, "KELRSQ")
  }
###  if("KELRSQA" %in% parameter_list){
  if(disp_required[["KELRSQA"]]){
    col_names <- c(col_names, "KELRSQA")
    regular_int_type <- c(regular_int_type, "KELRSQA")
  }
  if(disp_required[["FLGACCEPTKEL"]] && "FLGACCEPTKELCRIT" %in% names(map_data)) {
    if(length(unlist(strsplit(as.character(map_data$FLGACCEPTKELCRIT), ","))) > 0){
      col_names <- c(col_names, "FLGACCEPTKEL")
    }
  }
###  if("THALF" %in% parameter_list) {
  if(disp_required[["THALF"]]) {
    col_names <- c(col_names, "THALF")
    regular_int_type <- c(regular_int_type, "THALF")
  }
###  if("THALFF" %in% parameter_list) {
  if(disp_required[["THALFF"]]) {
    col_names <- c(col_names, "THALFF")
  }
###  if("LASTTIME" %in% parameter_list) {
  if(disp_required[["LASTTIME"]]) {
    col_names <- c(col_names, "LASTTIME")
    regular_int_type <- c(regular_int_type, "LASTTIME")
  }
###  if("LASTTIMEi" %in% parameter_list) {
  if(disp_required[["LASTTIMEi"]]) {
    col_names <- c(col_names, rep(paste0("LASTTIME",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("LASTTIME",1:di_col)))
  }
  if(disp_required[["FLGACCEPTTAU"]] && "LASTTIMEACCEPTCRIT" %in% names(map_data)) {
    col_names <- c(col_names, "FLGACCEPTTAU")
  }
###  if("AUCALL" %in% parameter_list && 'TMAX' %in% parameter_list) {
  if(disp_required[["AUCALL"]]) {
    col_names <- c(col_names, "AUCALL")
    regular_int_type <- c(regular_int_type, "AUCALL")
  }
###  if("AUCDN" %in% parameter_list && "AUCALL" %in% parameter_list) {
### 2019-10-10/TGT/ replace AUCDN with AUCALLDN
###  if(disp_required[["AUCDN"]]) {
###    col_names <- c(col_names, rep(paste0("AUCDN",1:di_col)))
###    regular_int_type <- c(regular_int_type, rep(paste0("AUCDN",1:di_col)))
  if(disp_required[["AUCALLDN"]]) {
    col_names <- c(col_names, rep(paste0("AUCALLDN",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUCALLDN",1:di_col)))
  }
###  if("AUCLAST" %in% parameter_list && 'TMAX' %in% parameter_list && 'TLAST' %in% parameter_list) {
  if(disp_required[["AUCLAST"]]) {
    col_names <- c(col_names, "AUCLAST")
    regular_int_type <- c(regular_int_type, "AUCLAST")
  }
###  if("AUCLASTC" %in% parameter_list && "AUCLAST" %in% parameter_list && "TLAST" %in% parameter_list && "KEL" %in% parameter_list){
  if(disp_required[["AUCLASTC"]]){
    col_names <- c(col_names, "AUCLASTC")
    regular_int_type <- c(regular_int_type, "AUCLASTC")
  }
###  if("AUCLASTDN" %in% parameter_list && "AUCLAST" %in% parameter_list) {
  if(disp_required[["AUCLASTDN"]]) {
    col_names <- c(col_names, "AUCLASTDN")
    regular_int_type <- c(regular_int_type, "AUCLASTDN")
  }
###  if("AUCLASTi" %in% parameter_list && 'TMAXi' %in% parameter_list && 'TLASTi' %in% parameter_list) {
  if(disp_required[["AUCLASTi"]]) {
    col_names <- c(col_names, rep(paste0("AUCLAST",1:di_col)))
    regular_int_type <-  c(regular_int_type, rep(paste0("AUCLAST",1:di_col)))
  }
###  if("AUCLASTCi" %in% parameter_list && "AUCLASTi" %in% parameter_list && "TLASTi" %in% parameter_list && "KEL" %in% parameter_list){
  if(disp_required[["AUCLASTCi"]]){
    col_names <- c(col_names, rep(paste0("AUCLASTC",1:di_col)))
    regular_int_type <-  c(regular_int_type, rep(paste0("AUCLASTC",1:di_col)))
  }
###  if("AUCLASTDNi" %in% parameter_list && "AUCLASTi" %in% parameter_list) {
  if(disp_required[["AUCLASTDNi"]]) {
    col_names <- c(col_names, rep(paste0("AUCLASTDN",1:di_col)))
    regular_int_type <-  c(regular_int_type, rep(paste0("AUCLASTDN",1:di_col)))
  }
###  if("AUMCLASTi" %in% parameter_list && 'TMAXi' %in% parameter_list && 'TLASTi' %in% parameter_list) {
  if(disp_required[["AUMCLASTi"]]) {
    col_names <- c(col_names, rep(paste0("AUMCLAST",1:di_col)))
    regular_int_type <-  c(regular_int_type, rep(paste0("AUMCLAST",1:di_col)))
  }
###  if("AUCT" %in% parameter_list && "TMAXi" %in% parameter_list) {
  if(disp_required[["AUCT"]] && auc_len > 1) {
    col_names <- c(col_names, rep(paste0("AUC",1:auc_len)))
    regular_int_type <- c(regular_int_type, paste0("AUC",1:auc_len))
  }
###  if("AUCTDN" %in% parameter_list && "TMAXi" %in% parameter_list) {
  if(disp_required[["AUCTDN"]] && auc_len > 1) {
    col_names <- c(col_names, rep(paste0("AUC",1:auc_len,"DN")))
    regular_int_type <- c(regular_int_type, paste0("AUC",1:auc_len,"DN"))
  }
###  if("AUCT" %in% parameter_list || "AUCTDN" %in% parameter_list){
  if((disp_required[["AUCT"]] || disp_required[["AUCTDN"]]) && auc_len > 1){
    col_names <- c(col_names, rep(paste0("AUCINT",1:auc_len)))
  }
###  if("AUCT1_T2" %in% parameter_list && "TMAXi" %in% parameter_list && auc_pair_check) {
  if(disp_required[["AUCT1_T2"]] && auc_pair_check) {
    col_names <- c(col_names, rep(paste0("AUC", map_data[,rep(paste0("AUC.", 1:auc_par_len, ".T1"))], "_", map_data[,rep(paste0("AUC.", 1:auc_par_len, ".T2"))])))
    regular_int_type <- c(regular_int_type, rep(paste0("AUC", map_data[,rep(paste0("AUC.", 1:auc_par_len, ".T1"))], "_", map_data[,rep(paste0("AUC.", 1:auc_par_len, ".T2"))])))
  }
###  if("AUCINFO" %in% parameter_list && "AUCLAST" %in% parameter_list && "CLAST" %in% parameter_list && "KEL" %in% parameter_list) {
  if(disp_required[["AUCINFO"]]) {
    col_names <- c(col_names, "AUCINFO")
    regular_int_type <- c(regular_int_type, "AUCINFO")
  }
###  if("AUCINFOi" %in% parameter_list && "AUCLASTi" %in% parameter_list && "CLASTi" %in% parameter_list && "KEL" %in% parameter_list) {
  if(disp_required[["AUCINFOi"]]) {
    col_names <- c(col_names, rep(paste0("AUCINFO",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUCINFO",1:di_col)))
  }
###  if("AUCINFOC" %in% parameter_list && "KEL" %in% parameter_list && "AUCINFO" %in% parameter_list) {
  if(disp_required[["AUCINFOC"]]) {
    col_names <- c(col_names, "AUCINFOC")
    regular_int_type <- c(regular_int_type, "AUCINFOC")
  }
  if(disp_required[["AUCINFOCi"]]){
    col_names <- c(col_names, rep(paste0("AUCINFOC",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUCINFOC",1:di_col)))
  }
###  if("AUCINFODN" %in% parameter_list && "AUCINFO" %in% parameter_list) {
  if(disp_required[["AUCINFODN"]]) {
    col_names <- c(col_names, "AUCINFODN")
    regular_int_type <- c(regular_int_type, "AUCINFODN")
  }
  if(disp_required[["AUCINFODNi"]]){
    col_names <- c(col_names, rep(paste0("AUCINFODN",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUCINFODN",1:di_col)))
  }
###  if("CEST" %in% parameter_list && "TLAST" %in% parameter_list && "KEL" %in% parameter_list) {
  if(disp_required[["CEST"]]) {
    col_names <- c(col_names, "CEST")
    regular_int_type <- c(regular_int_type, "CEST")
  }
###  if("AUCINFP" %in% parameter_list && "CEST" %in% parameter_list && "AUCLAST" %in% parameter_list && "TLAST" %in% parameter_list && "KEL" %in% parameter_list) {
  if(disp_required[["AUCINFP"]]) {
    col_names <- c(col_names, "AUCINFP")
    regular_int_type <- c(regular_int_type, "AUCINFP")
  }
###  if("AUCINFPi" %in% parameter_list && "CEST" %in% parameter_list && "AUCLASTi" %in% parameter_list && "TLASTi" %in% parameter_list && "KEL" %in% parameter_list) {
  if(disp_required[["AUCINFPi"]]) {
    col_names <- c(col_names, rep(paste0("AUCINFP",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUCINFP",1:di_col)))
  }
###  if("AUCINFPC" %in% parameter_list && "KEL" %in% parameter_list && "AUCINFP" %in% parameter_list) {
  if(disp_required[["AUCINFPC"]]) {
    col_names <- c(col_names, "AUCINFPC")
    regular_int_type <- c(regular_int_type, "AUCINFPC")
  }
  if(disp_required[["AUCINFPCi"]]){
    col_names <- c(col_names, rep(paste0("AUCINFPC",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUCINFPC",1:di_col)))
  }
###  if("AUCINFPDN" %in% parameter_list && "AUCINFP" %in% parameter_list) {
  if(disp_required[["AUCINFPDN"]]) {
    col_names <- c(col_names, "AUCINFPDN")
    regular_int_type <- c(regular_int_type, "AUCINFPDN")
  }
  if(disp_required[["AUCINFPDNi"]]){
    col_names <- c(col_names, rep(paste0("AUCINFPDN",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUCINFPDN",1:di_col)))
  }
###  if("AUMCINFOi" %in% parameter_list && "AUMCLASTi" %in% parameter_list && "CLASTi" %in% parameter_list && "KEL" %in% parameter_list) {
  if(disp_required[["AUMCINFOi"]]) {
    col_names <- c(col_names, rep(paste0("AUMCINFO",1:di_col)))
    regular_int_type <-  c(regular_int_type, rep(paste0("AUMCINFO",1:di_col)))
  }
###  if("AUMCINFPi" %in% parameter_list && "CEST" %in% parameter_list && "AUMCLASTi" %in% parameter_list && "TLASTi" %in% parameter_list && "KEL" %in% parameter_list) {
  if(disp_required[["AUMCINFPi"]]) {
    col_names <- c(col_names, rep(paste0("AUMCINFP",1:di_col)))
    regular_int_type <-  c(regular_int_type, rep(paste0("AUMCINFP",1:di_col)))
  }
###  if("AUCTAUi" %in% parameter_list) {
  if(disp_required[["AUCTAUi"]]) {
    col_names <- c(col_names, rep(paste0("AUCTAU",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUCTAU",1:di_col)))
  }
###  if("AUCTAUDNi" %in% parameter_list && "AUCTAUi" %in% parameter_list) {
  if(disp_required[["AUCTAUDNi"]]) {
    col_names <- c(col_names, rep(paste0("AUCTAUDN",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUCTAUDN",1:di_col)))
  }
###  if("AUMCTAUi" %in% parameter_list && "TMAXi" %in% parameter_list) {
  if(disp_required[["AUMCTAUi"]]) {
    col_names <- c(col_names, rep(paste0("AUMCTAU",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUMCTAU",1:di_col)))
  }
###  if("MRTLAST" %in% parameter_list && "AUCLAST" %in% parameter_list && "AUMCLAST" %in% parameter_list){
  if(disp_required[["MRTLAST"]]){
    col_names <- c(col_names, "MRTLAST")
    regular_int_type <- c(regular_int_type, "MRTLAST")
  }
###  if("MRTLASTi" %in% parameter_list && "AUCLASTi" %in% parameter_list && "AUMCLASTi" %in% parameter_list){
  if(disp_required[["MRTLASTi"]]){
    col_names <- c(col_names, rep(paste0("MRTLAST",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("MRTLAST",1:di_col)))
  }
###  if("MRTIVIFOi" %in% parameter_list && "AUCINFOi" %in% parameter_list && "AUCTAUi" %in% parameter_list && "AUMCTAUi" %in% parameter_list){
  if(disp_required[["MRTIVIFOi"]]){
    col_names <- c(col_names, rep(paste0("MRTIVIFO",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("MRTIVIFO",1:di_col)))
  }
###  if("MRTIVIFPi" %in% parameter_list && "AUCINFPi" %in% parameter_list && "AUCTAUi" %in% parameter_list && "AUMCTAUi" %in% parameter_list){
  if(disp_required[["MRTIVIFPi"]]){
    col_names <- c(col_names, rep(paste0("MRTIVIFP",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("MRTIVIFP",1:di_col)))
  }
###  if("AUCXPCTO" %in% parameter_list && "AUCINFO" %in% parameter_list && "AUCLAST" %in% parameter_list){
  if(disp_required[["AUCXPCTO"]]){
    col_names <- c(col_names, "AUCXPCTO")
    regular_int_type <- c(regular_int_type, "AUCXPCTO")
  }
###  if("AUCXPCTOi" %in% parameter_list && "AUCINFOi" %in% parameter_list && "AUCLASTi" %in% parameter_list){
  if(disp_required[["AUCXPCTOi"]]){
    col_names <- c(col_names, rep(paste0("AUCXPCTO",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUCXPCTO",1:di_col)))
  }
###  if("AUCXPCTP" %in% parameter_list && "AUCINFP" %in% parameter_list && "AUCLAST" %in% parameter_list){
  if(disp_required[["AUCXPCTP"]]){
    col_names <- c(col_names, "AUCXPCTP")
    regular_int_type <- c(regular_int_type, "AUCXPCTP")
  }
###  if("AUCXPCTPi" %in% parameter_list && "AUCINFPi" %in% parameter_list && "AUCLASTi" %in% parameter_list){
  if(disp_required[["AUCXPCTPi"]]){
    col_names <- c(col_names, rep(paste0("AUCXPCTP",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUCXPCTP",1:di_col)))
  }
###  if("AUMCXPTO" %in% parameter_list && "AUMCINFO" %in% parameter_list && "AUMCLAST" %in% parameter_list){
  if(disp_required[["AUMCXPTO"]]){
    col_names <- c(col_names, "AUMCXPTO")
    regular_int_type <- c(regular_int_type, "AUMCXPTO")
  }
###  if("AUMCXPTOi" %in% parameter_list && "AUMCINFOi" %in% parameter_list && "AUMCLAST" %in% parameter_list){
  if(disp_required[["AUMCXPTOi"]]){
    col_names <- c(col_names, rep(paste0("AUMCXPTO",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUMCXPTO",1:di_col)))
  }
###  if("AUMCXPTP" %in% parameter_list && "AUMCINFP" %in% parameter_list && "AUMCLAST" %in% parameter_list){
  if(disp_required[["AUMCXPTP"]]){
    col_names <- c(col_names, "AUMCXPTP")
    regular_int_type <- c(regular_int_type, "AUMCXPTP")
  }
###  if("AUMCXPTPi" %in% parameter_list && "AUMCINFPi" %in% parameter_list && "AUMCLAST" %in% parameter_list){
  if(disp_required[["AUMCXPTPi"]]){
    col_names <- c(col_names, rep(paste0("AUMCXPTP",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUMCXPTP",1:di_col)))
  }
###  if("CAVi" %in% parameter_list && "AUCTAUi" %in% parameter_list) {
  if(disp_required[["CAVi"]]) {
    col_names <- c(col_names, rep(paste0("CAV",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("CAV",1:di_col)))
  }
###  if("CLTAUi" %in% parameter_list && "AUCTAUi" %in% parameter_list) {
  if(disp_required[["CLTAUi"]]) {
    col_names <- c(col_names, rep(paste0("CLTAU",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("CLTAU",1:di_col)))
  }
###  if("CLTAUWi" %in% parameter_list && "CLTAUi" %in% parameter_list && "AUCTAUi" %in% parameter_list) {
  if(disp_required[["CLTAUWi"]]) {
    col_names <- c(col_names, rep(paste0("CLTAUW",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("CLTAUW",1:di_col)))
  }
###  if("PTFi" %in% parameter_list && "CMAXi" %in% parameter_list && "CMINi" %in% parameter_list && "CAVi" %in% parameter_list && "AUCTAUi" %in% parameter_list) {
  if(disp_required[["PTFi"]]) {
    col_names <- c(col_names, rep(paste0("PTF",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("PTF",1:di_col)))
  }
###  if("PTRi" %in% parameter_list && "CMAXi" %in% parameter_list && "CMINi" %in% parameter_list) {
  if(disp_required[["PTRi"]]) {
    col_names <- c(col_names, rep(paste0("PTR",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("PTR",1:di_col)))
  }
  if(disp_required[["VSSOi"]]) {
    col_names <- c(col_names, rep(paste0("VSSO",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("VSSO",1:di_col)))
  }
  if(disp_required[["VSSPi"]]) {
    col_names <- c(col_names, rep(paste0("VSSP",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("VSSP",1:di_col)))
  }
  if(disp_required[["VSSOWi"]]) {
    col_names <- c(col_names, rep(paste0("VSSOW",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("VSSOW",1:di_col)))
  }
  if(disp_required[["VSSPWi"]]) {
    col_names <- c(col_names, rep(paste0("VSSPW",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("VSSPW",1:di_col)))
  }
  if(disp_required[["VZTAUi"]]) {
    col_names <- c(col_names, rep(paste0("VZTAU",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("VZTAU",1:di_col)))
  }
  if(disp_required[["VZTAUWi"]]) {
    col_names <- c(col_names, rep(paste0("VZTAUW",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("VZTAUW",1:di_col)))
  }
  col_names <- c(col_names, rep(paste0("CONC",1:(auc_len+1))), rep(paste0("CONCTIME",1:(auc_len+1))))
  regular_int_type <- c(regular_int_type, rep(paste0("CONC",1:(auc_len+1))), rep(paste0("CONCTIME",1:(auc_len+1))))
###  if("DIi" %in% parameter_list) {
  if(disp_required[["DIi"]]) {
    col_names <- c(col_names, rep(paste0("DI",1:di_col)))
  }
###  if("TAUi" %in% parameter_list) {
  if(disp_required[["TAUi"]]) {
    col_names <- c(col_names, rep(paste0("TAU",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("TAU",1:di_col)))
  }
###  if("TOLDi" %in% parameter_list) {
  if(disp_required[["TOLDi"]]) {
    col_names <- c(col_names, rep(paste0("TOLD",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("TOLD",1:di_col)))
  }
### 2019-10-20/TGT/ Reposition
###  if("DOSEi" %in% parameter_list) {
###  if(disp_required[["DOSEi"]]) {
###    col_names <- c(col_names, rep(paste0("DOSE",1:di_col)))
###    regular_int_type <- c(regular_int_type, rep(paste0("DOSE",1:di_col)))
###  }

### 2019-08-09/TGT/ Reposition location of template computaton_df creation to following
###                 the generation of "col_names" and use planned "length(col_names)"
###                 instead of "col"
###  computation_df <- data.frame(matrix(ncol = col, nrow = 0))

  computation_df <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  names(computation_df) <- col_names
  
  #names(computation_df) <- c("SDEID", rep(paste0("CMAX",1:di_col)), rep(paste0("CMIN",1:di_col)), rep(paste0("CLAST",1:di_col)),
  #                           rep(paste0("CMAXDN",1:di_col)), rep(paste0("TMAX",1:di_col)), rep(paste0("TMIN",1:di_col)), rep(paste0("TLAST",1:di_col)),
  #                           "KEL", "KELTMLO", "KELTHMI", "KELNOPT", "KELR", "KELRSQ", "KELRSQA", "THALF", "LASTTIME", rep(paste0("DI",1:di_col)), rep(paste0("TAU",1:di_col)),
  #                           rep(paste0("TOLD",1:di_col)), rep(paste0("DOSE",1:di_col)), "AUCALL", rep(paste0("AUCDN",1:di_col)), rep(paste0("AUCLAST",1:di_col)), rep(paste0("AUCLASTDN",1:di_col)),
  #                           rep(paste0("AUC",1:auc_col)), rep(paste0("AUCINT",1:auc_col)), "AUCINFO", "AUCINFP", "AUCINFOC", "AUCINFPC", rep(paste0("AUCINFODN",1:di_col)),
  #                           rep(paste0("AUCINFPDN",1:di_col)), rep(paste0("AUCTAU",1:di_col)), rep(paste0("AUCTAUDN",1:di_col)), "MRTO", "MRTP", "AUCXPCTO",
  #                           "AUCXPCTP", rep(paste0("VZFP",1:di_col)), rep(paste0("CAV",1:di_col)), rep(paste0("PTF",1:di_col)), rep(paste0("PTR",1:di_col)),
  #                           rep(paste0("VZO",1:di_col)), rep(paste0("VZP",1:di_col)))

  if("FLGACCEPTKELCRIT" %in% names(map_data)) {
    kel_crit <- unlist(strsplit(as.character(map_data$FLGACCEPTKELCRIT), ","))

    if(length(kel_crit) > 0){
      x <- regexpr("^[[:space:]]*?([[:alnum:]_.+]+?)([<>=!]+?)([[:digit:].]+?)$", kel_crit, ignore.case=TRUE, perl=TRUE)
      flag_df <- data.frame(matrix(ncol = 3, nrow = 0))
      names(flag_df) <- c("VAR", "OPR", "CRIT")
      flag_subset <- NA

      for(j in 1:length(kel_crit)){
        index <-  as.numeric(attributes(x)$capture.start[j,])

        if(-1 %in% index){
          warning("Flag 'FLGACCEPTKELCRIT' values provided via 'map' does not have a proper format! Please make sure the value has <Parameter><Operator><Numeric> format!")
        } else {
          flag_df[j,] <- c(substr(kel_crit[j], index[1], index[2]-1), substr(kel_crit[j], index[2], index[3]-1), substr(kel_crit[j], index[3], nchar(kel_crit[j])))

          if(as.character(flag_df$VAR[j]) %in% parameter_list){
            if(j == 1 || is.na(flag_subset)) {
              flag_subset <-  paste(paste0("!is.na(computation_df", "$", flag_df$VAR[j],")"))
              flag_subset <-  paste(flag_subset, "&", paste0("computation_df", "$", flag_df$VAR[j]), flag_df$OPR[j], flag_df$CRIT[j])
            } else {
              flag_subset <-  paste(flag_subset, "&", paste(paste0("!is.na(computation_df", "$", flag_df$VAR[j]),")"))
              flag_subset <-  paste(flag_subset, "&", paste0("computation_df", "$", flag_df$VAR[j]), flag_df$OPR[j], flag_df$CRIT[j])
            }
          } else {
            warning(paste0("Flag 'FLGACCEPTKELCRIT' values provided via 'map' does not have a valid parameter name '", flag_df$VAR[j], "'"))
          }
        }
      }
    } else {
      warning("Flag 'FLGACCEPTKELCRIT' is not present in the dataset")
    }
  } else {
    warning("Flag 'FLGACCEPTKELCRIT' is not present in the dataset")
  }
  if(disp_required[["FLGACCEPTTAU"]] && "LASTTIMEACCEPTCRIT" %in% names(map_data)){
    if(length(unlist(strsplit(as.character(map_data$LASTTIMEACCEPTCRIT), "[*]"))) == 2){
      last_crit <- unlist(strsplit(as.character(map_data$LASTTIMEACCEPTCRIT), "[*]"))
      if(as.character(gsub(" ", "", last_crit[2])) == "TAUi"){
        last_crit_factor <- as.numeric(gsub(" ", "", last_crit[1]))
      } else {
        last_crit_factor <- NA
        warning("Flag 'LASTTIMEACCEPTCRIT' does not have a valid column name")
      }
    } else {
      last_crit_factor <- NA
      warning("Flag 'LASTTIMEACCEPTCRIT' is not in a valid form! Please make sure it contains '*'")
    }
    if(paste0("TAU", di_col) %in% names(map_data)){
      if(!map_data[, paste0("TAU", di_col)] %in% names(data_data)) {
        warning("Flag 'FLGACCEPTTAU' cannot be computed if 'TAUi' is not provided")
      }
    } else {
      warning("Flag 'FLGACCEPTTAU' cannot be computed if 'TAUi' is not provided")
    }
  } else {
    if(disp_required[["FLGACCEPTTAU"]] && !("LASTTIMEACCEPTCRIT" %in% names(map_data))){
      warning("Flag 'FLGACCEPTTAU' cannot be computed if 'LASTTIMEACCEPTCRIT' is not provided")
    }
  }

  if(!("FLGEXKEL" %in% names(map_data) && map_data$FLGEXKEL %in% names(data_data))){
    warning("Flag 'FLGEXKEL' is not present in the dataset")
  }
  if(!("FLGEXAUC" %in% names(map_data) && map_data$FLGEXAUC %in% names(data_data))){
    warning("Flag 'FLGEXAUC' is not present in the dataset")
  }
  if(!("FLGEXSDE" %in% names(map_data) && map_data$FLGEXSDE %in% names(data_data))){
    warning("Flag 'FLGEXSDE' is not present in the dataset")
  }
  if(disp_required[["FLGACCEPTTMAX"]] && !("FLGEMESIS" %in% names(map_data) && map_data$FLGEMESIS %in% names(data_data))){
    warning("Flag 'FLGEMESIS' is not present in the dataset")
  }
  if(disp_required[["FLGACCEPTPREDOSE"]] && !("FLGACCEPTPREDOSECRIT" %in% names(map_data))){
    warning("Flag 'FLGACCEPTPREDOSECRIT' is not present in the dataset")
  } else if("FLGACCEPTPREDOSECRIT" %in% names(map_data)){
    if(!("CMAXi" %in% parameter_list)){
      warning("Flag 'FLGACCEPTPREDOSECRIT' cannot be computed if 'CMAXi' is not part of the calculated parameters")
    }
    if(!(is.numeric(suppressWarnings(as.numeric(map_data$FLGACCEPTPREDOSECRIT)))) || (is.na(suppressWarnings(as.numeric(map_data$FLGACCEPTPREDOSECRIT))))){
      warning("Flag 'FLGACCEPTPREDOSECRIT' does not have valid form! Please try again with numeric value")
    }
  }
  if(disp_required[["FLGACCEPTTAU"]] && !("LASTTIMEACCEPTCRIT" %in% names(map_data))){
    warning("Flag 'FLGACCEPTTAU' is not present in the dataset")
  }
  if(!("SPANRATIOCRIT" %in% names(map_data) && "THALFF" %in% parameter_list)){
    warning("Flag 'SPANRATIOCRIT' is not present in the dataset")
  } else if("SPANRATIOCRIT" %in% names(map_data)){
    if(!("THALFF" %in% parameter_list)){
      warning("Flag 'SPANRATIOCRIT' cannot be used if 'THALFF' is not part of the calculated parameters")
    }
    if(!(is.numeric(suppressWarnings(as.numeric(map_data$SPANRATIOCRIT)))) || (is.na(suppressWarnings(as.numeric(map_data$SPANRATIOCRIT))))){
      warning("Flag 'SPANRATIOCRIT' does not have valid form! Please try again with numeric value")
    }
  }
  if("OPTIMIZEKEL" %in% names(map_data)){
    if(!(is.na(map_data[,"OPTIMIZEKEL"]))){
      if(map_data[,"OPTIMIZEKEL"] != 1 && map_data[,"OPTIMIZEKEL"] != 0){
        warning("Map 'OPTIMIZEKEL' does not have a valid value! Not using KEL optimization for this computation")
        optimize_kel <- FALSE
      } else {
        optimize_kel <- as.logical(as.numeric(map_data[,"OPTIMIZEKEL"]))
      }
    } else {
      optimize_kel <- FALSE
    }
  } else {
    optimize_kel <- FALSE
  }
  if(isTRUE(optimize_kel)){
    comp_required[["KEL"]] <- TRUE
    comp_required[["TMAXi"]] <- TRUE
    comp_required[["TLASTi"]] <- TRUE
    comp_required[["CMAXi"]] <- TRUE
    comp_required[["CLASTi"]] <- TRUE 
    comp_required[["AUCLASTi"]] <- TRUE
    if(isTRUE(optimize_kel_debug)){
      debug_idx <- 1
      if("AUCXPCTO" %in% flag_df$VAR){
        kel_debug <- data.frame("SDEID" = numeric(), "TIME" = numeric(), "CONC" = numeric(), "KEL" = numeric(), "KELNOPT" = numeric(), "KELRSQ" = numeric(), "AUCXPCTO" = numeric(), "KELNOPT_W" = numeric(), "KELRSQ_W" = numeric(), "AUCXPCTO_W" = numeric(), "TOTAL_W" = numeric())
      } else if("AUCXPCTP" %in% flag_df$VAR){
        kel_debug <- data.frame("SDEID" = numeric(), "TIME" = numeric(), "CONC" = numeric(), "KEL" = numeric(), "KELNOPT" = numeric(), "KELRSQ" = numeric(), "AUCXPCTP" = numeric(), "KELNOPT_W" = numeric(), "KELRSQ_W" = numeric(), "AUCXPCTP_W" = numeric(), "TOTAL_W" = numeric())
      }
    }
  }
##  2019-11-08/RD Added for Interpolation to account for error handling
##
  if("INCLUDEINTERPOLATION" %in% names(map_data)){
    if(isTRUE(map_data[, "INCLUDEINTERPOLATION"] != 0 && map_data[, "INCLUDEINTERPOLATION"] != 1)){
      warning("Flag 'INCLUDEINTERPOLATION' does not have a valid value! Please try again with numeric value (either 0 or 1)")
    }
  }
  if("INCLUDEEXTRAPOLATION" %in% names(map_data)){
    if(isTRUE(map_data[, "INCLUDEEXTRAPOLATION"] != 0 && map_data[, "INCLUDEEXTRAPOLATION"] != 1)){
      warning("Flag 'INCLUDEEXTRAPOLATION' does not have a valid value! Please try again with numeric value (either 0 or 1)")
    }
  }
  #if((!"LLOQPATTERNS" %in% names(map_data)) && generate_nominal_conc){
  #  warning("Flag 'LLOQPATTERNS' is not present in the map dataset")
  #  if("CONCRAW" %in% names(map_data) && "CONCRAW" %in% names(map_data)){
  #    if(map_data$CONCRAW %in% data_data && map_data$CONC %in% names(data_data)){
  #      tmp_conc <- suppressWarnings(as.numeric(data_data[,map_data$CONCRAW]))
  #    }
  #  }
  #}
  if(isTRUE(optimize_kel) && (!comp_required[["TMAXi"]] || !comp_required[["TLASTi"]] || !comp_required[["CMAXi"]] || !comp_required[["CLASTi"]] || !comp_required[["AUCLASTi"]] ||
     !"FLGACCEPTKELCRIT" %in% names(map_data) || !"FLGEXKEL" %in% names(map_data) || !map_data$FLGEXKEL %in% names(data_data))){
    warning("Kel optimization cannot be performed because 'TMAXi', 'TLASTi', 'CMAXi', 'CLASTi', 'AUCLASTi' are not part of the calculated parameters AND Flag 'FLGACCEPTKELCRIT' and Flag 'FLGEXKEL' are not present in the dataset")
  }
  
  if(isTRUE(optimize_kel) && comp_required[["TMAXi"]] && comp_required[["TLASTi"]] && comp_required[["CMAXi"]] && comp_required[["CLASTi"]] && comp_required[["AUCLASTi"]] &&
     "FLGACCEPTKELCRIT" %in% names(map_data) && "FLGEXKEL" %in% names(map_data) && map_data$FLGEXKEL %in% names(data_data)){
    kel_flag_optimized <- integer()
    kel_opt_warning <- FALSE
  }
  
  if(disp_required[["FLGACCEPTTMAX"]] && "FLGEMESIS" %in% names(map_data) && map_data$FLGEMESIS %in% names(data_data)){
    comp_required[["TMAXi"]] <- TRUE
    comp_required[["DOSEi"]] <- TRUE
  }

  for(i in 1:length(unique(data_data[,map_data$SDEID]))){
    #print(unique(data_data[,map_data$SDEID])[i])
    tryCatch({
      dof <- list()
      
      if(comp_required[["DOSECi"]] || comp_required[["DOSEC"]]){
        dose_c_i <- list()
      }
      if(comp_required[["CENDINFi"]]){
        cend_inf <- list()
      }
      if(comp_required[["CENDINFDNi"]]){
        cend_infdn <- list()
      }
      if(comp_required[["TENDINFi"]]){
        tend_inf <- list()
      }
###      if("CMAXi" %in% parameter_list) {
      if(comp_required[["CMAXi"]]) {
        c_maxi <- list()
      }
### 2019-08-30/TGT/ Remove "CMAXC"/"CMAXCi" since only valid for M2 Bolus administration applications
###      if("CMAXCi" %in% parameter_list && "CMAXi" %in% parameter_list && "TMAXi" %in% parameter_list && "KEL" %in% parameter_list) {
###        c_maxci <- list()
###      }
###      if("CMAXDNi" %in% parameter_list && "CMAXi" %in% parameter_list) {
      if(comp_required[["CMAXDNi"]]) {
        c_maxdni <- list()
      }
###      if("CMINi" %in% parameter_list) {
      if(comp_required[["CMINi"]]) {
        c_mini <- list()
      }
      if(comp_required[["CMINDNi"]]){
        c_mindni <- list()
      }
###      if("CLASTi" %in% parameter_list) {
      if(comp_required[["CLASTi"]]) {
        c_lasti <- list()
      }
###      if("TMAXi" %in% parameter_list) {
      if(comp_required[["TMAXi"]]) {
        t_maxi <- list()
      }
###      if("TMINi" %in% parameter_list) {
      if(comp_required[["TMINi"]]) {
        t_mini <- list()
      }
###      if("TLASTi" %in% parameter_list) {
      if(comp_required[["TLASTi"]]) {
        t_lasti <- list()
      }
###      if("LASTTIMEi" %in% parameter_list) {
      if(comp_required[["LASTTIMEi"]]) {
        last_timei <- list()
      }
      if(comp_required[["CTROUGHi"]]){
        c_troughi <- list()
      }
      if(comp_required[["CTROUGHENDi"]]){
        c_troughendi <- list()
      }
      if(comp_required[["PTROUGHRi"]]){
        p_troughri <- list()
      }
      if(comp_required[["PTROUGHRENDi"]]){
        p_troughrendi <- list()
      }
###      if("AUCINFOi" %in% parameter_list && "AUCLASTi" %in% parameter_list && "CLASTi" %in% parameter_list && "KEL" %in% parameter_list) {
      if(comp_required[["AUCINFOi"]]) {
        aucinfoi <- list()
      }
      if(comp_required[["AUCINFOCi"]]){
        aucinfoi_c <- list()
      }
      if(comp_required[["AUCINFODNi"]]){
        aucinfoi_dn <- list()
      }
###      if("AUCINFPi" %in% parameter_list && "CEST" %in% parameter_list && "AUCLASTi" %in% parameter_list && "TLASTi" %in% parameter_list && "KEL" %in% parameter_list) {
      if(comp_required[["AUCINFPi"]]) {
        aucinfpi <- list()
      }
      if(comp_required[["AUCINFPCi"]]){
        aucinfpi_c <- list()
      }
      if(comp_required[["AUCINFPDNi"]]){
        aucinfpi_dn <- list()
      }
###      if("AUMCINFOi" %in% parameter_list && "AUCLASTi" %in% parameter_list && "CLASTi" %in% parameter_list && "KEL" %in% parameter_list) {
      if(comp_required[["AUMCINFOi"]]) {
        aumcinfoi <- list()
      }
###      if("AUMCINFPi" %in% parameter_list && "CEST" %in% parameter_list && "AUCLASTi" %in% parameter_list && "TLASTi" %in% parameter_list && "KEL" %in% parameter_list) {
      if(comp_required[["AUMCINFPi"]]) {
        aumcinfpi <- list()
      }
###      if("AUCDN" %in% parameter_list && "AUCALL" %in% parameter_list) {
### 2019-10-10/TGT/ replace AUCDN with AUCALLDN
###      if(comp_required[["AUCDN"]]) {
###        aucdn <- list()
      if(comp_required[["AUCALLDN"]]) {
        aucalldn <- list()
      }
###      if("AUCLASTi" %in% parameter_list  && 'TMAXi' %in% parameter_list && 'TLASTi' %in% parameter_list) {
      if(comp_required[["AUCLASTi"]]) {
        auclasti <- list()
      }
###      if("AUCLASTCi" %in% parameter_list  && 'AUCLASTi' %in% parameter_list) {
      if(comp_required[["AUCLASTCi"]]) {
        auclasti_c <- list()
      }
###      if("AUCLASTDNi" %in% parameter_list  && 'AUCLASTi' %in% parameter_list) {
      if(comp_required[["AUCLASTDNi"]]) {
        auclasti_dn <- list()
      }
###      if("AUMCLASTi" %in% parameter_list && 'TMAXi' %in% parameter_list && 'TLASTi' %in% parameter_list) {
      if(comp_required[["AUMCLASTi"]]) {
        aumclasti <- list()
      }
###      if("AUCXPCTOi" %in% parameter_list && "AUCINFOi" %in% parameter_list && "AUCLASTi" %in% parameter_list){
      if(comp_required[["AUCXPCTOi"]]){
        aucxpctoi <- list()
      }
###      if("AUCXPCTPi" %in% parameter_list && "AUCINFPi" %in% parameter_list && "AUCLASTi" %in% parameter_list){
      if(comp_required[["AUCXPCTPi"]]){
        aucxpctpi <- list()
      }
###      if("TAUi" %in% parameter_list) {
      if(comp_required[["TAUi"]] || comp_required[["TAU"]]) {
        tau <- list()
      }
###      if("TOLDi" %in% parameter_list) {
      if(comp_required[["TOLDi"]] || comp_required[["TOLD"]]) {
        told <- list()
      }
###      if("AUCTAUi" %in% parameter_list) {
      if(comp_required[["AUCTAUi"]]) {
        auctau <- list()
      }
###      if("AUCTAUDNi" %in% parameter_list && "AUCTAUi" %in% parameter_list) {
      if(comp_required[["AUCTAUDNi"]]) {
        auctaudn <- list()
      }
###      if("AUMCTAUi" %in% parameter_list && "TMAXi" %in% parameter_list) {
      if(comp_required[["AUMCTAUi"]]) {
        aumctaui <- list()
      }
###      if("MRTLASTi" %in% parameter_list && "AUCLASTi" %in% parameter_list && "AUMCLASTi" %in% parameter_list){
      if(comp_required[["MRTLASTi"]]){
        mrtlasti <- list()
      }
###      if("MRTIVIFOi" %in% parameter_list && "AUCINFOi" %in% parameter_list && "AUCTAUi" %in% parameter_list && "AUMCTAUi" %in% parameter_list){
      if(comp_required[["MRTIVIFOi"]]){
        mrtivifoi <- list()
      }
###      if("MRTIVIFPi" %in% parameter_list && "AUCINFPi" %in% parameter_list && "AUCTAUi" %in% parameter_list && "AUMCTAUi" %in% parameter_list){
      if(comp_required[["MRTIVIFPi"]]){
        mrtivifpi <- list()
      }
###      if("AUMCXPTOi" %in% parameter_list && "AUMCINFOi" %in% parameter_list && "AUMCLAST" %in% parameter_list){
      if(comp_required[["AUMCXPTOi"]]){
        aumcxptoi <- list()
      }
###      if("AUMCXPTPi" %in% parameter_list && "AUMCINFPi" %in% parameter_list && "AUMCLAST" %in% parameter_list){
      if(comp_required[["AUMCXPTPi"]]){
        aumcxptpi <- list()
      }
###      if("CAVi" %in% parameter_list && "AUCTAUi" %in% parameter_list) {
      if(comp_required[["CAVi"]]) {
        ca_v <- list()
      }
###      if("DIi" %in% parameter_list) {
      if(comp_required[["DIi"]]) {
        di <- list()
      }
###      if("DOSEi" %in% parameter_list) {
      if(comp_required[["DOSEi"]]) {
        dose <- list()
      }
###      if("CLTAUi" %in% parameter_list && "AUCTAUi" %in% parameter_list) {
      if(comp_required[["CLTAUi"]]) {
        cl_tau <- list()
      }
###      if("CLTAUWi" %in% parameter_list && "CLTAUi" %in% parameter_list && "AUCTAUi" %in% parameter_list) {
      if(comp_required[["CLTAUWi"]]) {
        cl_tauw <- list()
      }
###      if("PTFi" %in% parameter_list && "CMAXi" %in% parameter_list && "CMINi" %in% parameter_list && "CAVi" %in% parameter_list && "AUCTAUi" %in% parameter_list) {
      if(comp_required[["PTFi"]]) {
        pt_f <- list()
      }
###      if("PTRi" %in% parameter_list && "CMAXi" %in% parameter_list && "CMINi" %in% parameter_list) {
      if(comp_required[["PTRi"]]) {
        pt_r <- list()
      }
      if(comp_required[["VSSOi"]]) {
        vsso <- list()
      }
      if(comp_required[["VSSPi"]]) {
        vssp <- list()
      }
      if(comp_required[["VSSOWi"]]) {
        vssow <- list()
      }
      if(comp_required[["VSSPWi"]]) {
        vsspw <- list()
      }
      if(comp_required[["VZTAUi"]]) {
        vz_tau <- list()
      }
      if(comp_required[["VZTAUWi"]]) {
        vz_tauw <- list()
      }

      tmp_df <- data_data[data_data[,map_data$SDEID] == unique(data_data[,map_data$SDEID])[i],]
      tmp_df <- tmp_df[order(tmp_df[,map_data$TIME]),]
      tmp_df[,map_data$CONC] <- as.numeric(tmp_df[,map_data$CONC])
      tmp_df[,map_data$TIME] <- as.numeric(tmp_df[,map_data$TIME])
      
      if("FLGEXSDE" %in% names(map_data)) {
        if(map_data$FLGEXSDE %in% names(data_data)){
          ex_flag <- as.numeric(tmp_df[,map_data$FLGEXSDE])
          if(all(is.na(ex_flag))){
            ex_flag[is.na(ex_flag)] <- 0
          }
          tmp_df <- tmp_df[!as.logical(ex_flag),]
        } else {
          ex_flag <- NULL
        }
      } else {
        ex_flag <- NULL
      }
      if("FLGEXKEL" %in% names(map_data)) {
        if(map_data$FLGEXKEL %in% names(data_data)){
          tmp_kel_flg <- as.numeric(tmp_df[,map_data$FLGEXKEL])
          kel_flag <- as.numeric(tmp_df[,map_data$FLGEXKEL])
          if(all(is.na(kel_flag))){
            kel_flag[is.na(kel_flag)] <- 0
          }
          if(isTRUE(optimize_kel)){
            kel_flag <- rep(1, length(tmp_kel_flg))
          }
        } else {
          kel_flag <- NULL
        }
      } else {
        kel_flag <- NULL
      }
      if("FLGEXAUC" %in% names(map_data)) {
        if(map_data$FLGEXAUC %in% names(data_data)){
          auc_flag <- as.numeric(tmp_df[,map_data$FLGEXAUC])
          if(all(is.na(auc_flag))){
            auc_flag[is.na(auc_flag)] <- 0
          }
        } else {
          auc_flag <- NULL
        }
      } else {
        auc_flag <- NULL
      }
      if("FLGEMESIS" %in% names(map_data)) {
        if(map_data$FLGEMESIS %in% names(data_data)){
          emesis_flag <- as.numeric(tmp_df[,map_data$FLGEMESIS])
          if(all(is.na(emesis_flag))){
            emesis_flag[is.na(emesis_flag)] <- 0
          }
        } else {
          emesis_flag <- NULL
        }
      } else {
        emesis_flag <- NULL
      }
      test_df <- tmp_df[,c(map_data$CONC, map_data$TIME)]
      if(any(duplicated(test_df))){
        tmp_df <- tmp_df[!duplicated(test_df),]
        if(!is.null(ex_flag)){
          ex_flag <- ex_flag[!duplicated(test_df)]
        }
        if(!is.null(kel_flag)){
          kel_flag <- kel_flag[!duplicated(test_df)]
        }
        if(!is.null(auc_flag)){
          auc_flag <- auc_flag[!duplicated(test_df)]
        }
        if(!is.null(emesis_flag)){
          emesis_flag <- emesis_flag[!duplicated(test_df)]
        }
        warning(paste0("Removing duplicate CONC and TIME values for SDEID: '", unique(data_data[,map_data$SDEID])[i], "'"))
      }
      test_df_2 <- tmp_df[,c(map_data$TIME)]
      if(any(duplicated(test_df_2))){
        tmp_df <- tmp_df[!duplicated(test_df_2),]
        if(!is.null(ex_flag)){
          ex_flag <- ex_flag[!duplicated(test_df_2)]
        }
        if(!is.null(kel_flag)){
          kel_flag <- kel_flag[!duplicated(test_df_2)]
        }
        if(!is.null(auc_flag)){
          auc_flag <- auc_flag[!duplicated(test_df_2)]
        }
        if(!is.null(emesis_flag)){
          emesis_flag <- emesis_flag[!duplicated(test_df_2)]
        }
        warning(paste0("Removing SDEID: '", unique(data_data[,map_data$SDEID])[i], "' due to duplicate TIME but different CONC values"))
      }
      cest_tmp <- data.frame("CONC" = numeric(), "TIME" = numeric(), "INT_EXT" = character())
      
##      2019-11-08/RD Added for Interpolation to account for INCLUDEINTERPOLATION Flag
##
      if("INCLUDEINTERPOLATION" %in% names(map_data)){
        map_data[,"INCLUDEINTERPOLATION"] <- as.numeric(map_data[,"INCLUDEINTERPOLATION"])
        interpolation <- ifelse((map_data[,"INCLUDEINTERPOLATION"] == 0 || map_data[,"INCLUDEINTERPOLATION"] == 1), as.logical( map_data[,"INCLUDEINTERPOLATION"]), FALSE)
      } else {
        interpolation <- FALSE
      }
##      2019-11-08/RD Added for Extrapolation to account for INCLUDEEXTRAPOLATION Flag
##
      if("INCLUDEEXTRAPOLATION" %in% names(map_data)){
        map_data[,"INCLUDEEXTRAPOLATION"] <- as.numeric(map_data[,"INCLUDEEXTRAPOLATION"])
        extrapolation <- ifelse((map_data[,"INCLUDEEXTRAPOLATION"] == 0 || map_data[,"INCLUDEEXTRAPOLATION"] == 1), as.logical(as.numeric(map_data[,"INCLUDEEXTRAPOLATION"])), FALSE)
      } else {
        extrapolation <- FALSE
      }
      
      if(nrow(tmp_df) > 0){
        orig_time <- tmp_df[,map_data$TIME]
        orig_conc <- tmp_df[,map_data$CONC]
        
        c_0 <- c0(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        if(comp_required[["DOSEC"]]) {
          dose_c <- dosec(data = tmp_df, map = map_data)
        }
###        if("CMAX" %in% parameter_list) {
        if(comp_required[["CMAX"]]) {
          c_max <- cmax(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        }
###        if("TMAX" %in% parameter_list) {
        if(comp_required[["TMAX"]]) {
          t_max <- tmax(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        }
###        if("CLAST" %in% parameter_list) {
        if(comp_required[["CLAST"]]) {
          c_last <- clast(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        }
###        if("TLAST" %in% parameter_list) {
        if(comp_required[["TLAST"]]) {
          t_last <- tlast(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        } else {
          t_last <- NULL
        }
###        if("AUCLAST" %in% parameter_list && 'TMAX' %in% parameter_list && 'TLAST' %in% parameter_list) {
        if(comp_required[["AUCLAST"]]) {
          auclast <- auc_last(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, exflag = auc_flag, t_last = t_last, t_max = t_max)
        }
        if(isTRUE(optimize_kel) && comp_required[["TMAXi"]] && comp_required[["TLASTi"]] && comp_required[["CMAXi"]] && comp_required[["CLASTi"]] && comp_required[["AUCLASTi"]] &&
           "FLGACCEPTKELCRIT" %in% names(map_data) && "FLGEXKEL" %in% names(map_data) && map_data$FLGEXKEL %in% names(data_data)){
          orig_time <- tmp_df[,map_data$TIME]
          orig_conc <- tmp_df[,map_data$CONC]
          tmp_time <- orig_time
          tmp_conc <- orig_conc

          flg_no_cmax_check <- FALSE
          if("FLGNOCMAX" %in% names(map_data)){
            if(!is.na(map_data$FLGNOCMAX) && (map_data$FLGNOCMAX == 1 || map_data$FLGNOCMAX == 0)){
              flg_no_cmax <- as.logical(as.numeric(map_data$FLGNOCMAX))
              if(isTRUE(flg_no_cmax)){
                if(!is.null(t_max) && !is.na(t_max) && !is.null(t_last) && !is.na(t_last)){
                  s_time <- match(t_max, orig_time)+1
                  e_time <- match(t_last, orig_time)
                  tmp_time <- orig_time[s_time:e_time]
                }
                if(!is.null(c_max) && !is.na(c_max) && !is.null(c_last) && !is.na(c_last)){
                  s_conc <- match(c_max, orig_conc)+1
                  e_conc <- match(c_last, orig_conc)
                  tmp_conc <- orig_conc[s_conc:e_conc]
                }
              } else {
                if(!is.null(t_max) && !is.na(t_max) && !is.null(t_last) && !is.na(t_last)){
                  s_time <- match(t_max, orig_time)
                  e_time <- match(t_last, orig_time)
                  tmp_time <- orig_time[s_time:e_time]
                }
                if(!is.null(c_max) && !is.na(c_max) && !is.null(c_last) && !is.na(c_last)){
                  s_conc <- match(c_max, orig_conc)
                  e_conc <- match(c_last, orig_conc)
                  tmp_conc <- orig_conc[s_conc:e_conc]
                }
              }
            }
          } else {
            if(!is.null(t_max) && !is.na(t_max) && !is.null(t_last) && !is.na(t_last)){
              s_time <- match(t_max, orig_time)+1
              e_time <- match(t_last, orig_time)
              tmp_time <- orig_time[s_time:e_time]
            }
            if(!is.null(c_max) && !is.na(c_max) && !is.null(c_last) && !is.na(c_last)){
              s_conc <- match(c_max, orig_conc)+1
              e_conc <- match(c_last, orig_conc)
              tmp_conc <- orig_conc[s_conc:e_conc]
            }
            if(!isTRUE(flg_no_cmax_check)){
              warning("Flag 'FLGNOCMAX' value provided via 'map' does not have a proper format! Please make sure the value is either '1' or '0'!")             
              flg_no_cmax_check <- TRUE
            }
          }

          if(all(c("KELNOPT", "KELRSQ") %in% flag_df$VAR) && ("AUCXPCTO" %in% flag_df$VAR || "AUCXPCTP" %in% flag_df$VAR)){
            kel_n <- as.numeric(flag_df$CRIT[match("KELNOPT", flag_df$VAR)])
            kel_op <- flag_df$OPR[match("KELNOPT", flag_df$VAR)]
  
            ulist <- list()
            if(length(tmp_time) >= kel_n){
              for(j in kel_n:length(tmp_time)){
                if(j <= length(tmp_time)) {
                  if(choose(length(tmp_time), j) == 1){
                    tlist <- list(as.vector(combn(tmp_time, j)))
                  } else {
                    cbn <- combn(tmp_time, j)
                    tlist <- lapply(seq(ncol(cbn)), function(i) cbn[,i])
                  }
                  ulist <- c(ulist,tlist)
                }
              }
            }
## /2019-11-22/RD This is the old optimize kel logic 
##            kelr_val <- kel_r(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])[["KELRSQ"]]
##            if("AUCXPCTO" %in% flag_df$VAR){
##              aucxpct <- auc_XpctO(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, aucflag = auc_flag)
##            } else if("AUCXPCTP" %in% flag_df$VAR){
##              aucxpct <- auc_XpctP(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, aucflag = auc_flag)
##            } else {
##              stop("Error in optimize kel")
##            }
            if(isTRUE("KEL" %in% flag_df$VAR)){
              kel_val <- as.numeric(flag_df$CRIT[match("KEL", flag_df$VAR)]) 
            }
            kelr_val <- as.numeric(flag_df$CRIT[match("KELRSQ", flag_df$VAR)])
            if("AUCXPCTO" %in% flag_df$VAR){
              aucxpct <- as.numeric(flag_df$CRIT[match("AUCXPCTO", flag_df$VAR)])
            } else if("AUCXPCTP" %in% flag_df$VAR){
              aucxpct <- as.numeric(flag_df$CRIT[match("AUCXPCTP", flag_df$VAR)])
            }
  
            selected_idx <- NA
            saved_kel_opt <- -1
            if(length(ulist) >= 1){
              for(k in 1:length(ulist)){
                sel_time <- ulist[[k]]
                sel_conc <- tmp_conc[match(sel_time, tmp_time)]
    
                all_kel <- kel(conc = sel_conc, time = sel_time)
                kel_tmp <- all_kel[["KEL"]]
                kelr_opt <- kel_r(conc = sel_conc, time = sel_time)[["KELRSQ"]]
                if("AUCXPCTO" %in% flag_df$VAR){
                  span_ratio <- ifelse("SPANRATIOCRIT" %in% names(map_data), suppressWarnings(as.numeric(map_data$SPANRATIOCRIT)), NA)
                  aucinfo_opt <- auc_inf_o(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, auclast = auclast, c_last = c_last, spanratio = span_ratio, kel = all_kel)
                  aucxpct_opt <- auc_XpctO(conc = sel_conc, time = sel_time, method = method, aucflag = auc_flag, auc_info = aucinfo_opt, auclast = auclast)
                } else if("AUCXPCTP" %in% flag_df$VAR){
                  span_ratio <- ifelse("SPANRATIOCRIT" %in% names(map_data), suppressWarnings(as.numeric(map_data$SPANRATIOCRIT)), NA)
                  aucinfp_opt <- auc_inf_p(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, auclast = auclast, t_last = t_last, spanratio = span_ratio, kel = all_kel)
                  aucxpct_opt <- auc_XpctP(conc = sel_conc, time = sel_time, method = method, aucflag = auc_flag, auc_infp = aucinfp_opt, auclast = auclast)
                } else {
                  stop("Error in optimize kel")
                }
    
                if(!is.na(kelr_opt) && !is.na(aucxpct_opt)){
                  kel_opt <- ((kelr_opt - kelr_val)/(1 - kelr_val)) + (length(sel_time)/length(tmp_time)) + ((aucxpct - aucxpct_opt)/aucxpct)
                } else {
                  kel_opt <- -1
                }
                if(isTRUE(optimize_kel_debug)){
                  kel_debug[debug_idx,] <- c(unique(data_data[,map_data$SDEID])[i], as.character(paste0(sel_time, sep = ", ", collapse = "")), as.character(paste0(sel_conc, sep = ", ", collapse = "")), kel_tmp, length(sel_time), kelr_opt, aucxpct_opt, ((kelr_opt - kelr_val)/(1 - kelr_val)), (length(sel_time)/length(tmp_time)), ((aucxpct - aucxpct_opt)/aucxpct), kel_opt)
                  debug_idx <- 1 + debug_idx
                }
    
                if(!is.na(kel_opt)){
                  if(kel_opt > saved_kel_opt){
                    if(isTRUE("KEL" %in% flag_df$VAR)){
                      if(kel_tmp > kel_val){
                        saved_kel_opt <- kel_opt
                        selected_idx <- match(sel_time, orig_time)
                      }
                    } else {
                      saved_kel_opt <- kel_opt
                      selected_idx <- match(sel_time, orig_time) 
                    }
                  }
                }
              }
            }
            tmp_kel_flag <- rep(1, length(kel_flag))
            tmp_kel_flag[selected_idx] <- 0
            kel_flag <- tmp_kel_flag
            kel_flag_optimized <- c(kel_flag_optimized, kel_flag)
          } else {
            if(!isTRUE(kel_opt_warning)){
              warning("Kel optimization cannot be performed because 'KELNOPT', 'KELRSQ' and 'AUCXPCTO' or 'AUCXPCTP' are not part of the Flag 'FLGACCEPTKELCRIT'")
              kel_opt_warning <- TRUE
            }
          }
        }

###        if("CMAXDN" %in% parameter_list && "CMAX" %in% parameter_list) {
        if(comp_required[["CMAXDN"]]) {
          tmpdose <- tmp_df[, as.character(map_data[c("DOSE1")])][1]
          c_maxdn <- cmax_dn(cmax = c_max, dose = tmpdose)
        }
###        if("CMIN" %in% parameter_list) {
        if(comp_required[["CMIN"]]) {
          c_min <- cmin(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        }
        if(comp_required[["CMINDN"]]){
          c_mindn <- cmin_dn(cmin = c_min, dose = unique(tmp_df[,unlist(dosevar)[1]]))
        }
###        if("TMIN" %in% parameter_list) {
        if(comp_required[["TMIN"]]) {
          t_min <- tmin(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        }
###        if("KEL" %in% parameter_list || "KELTMLO" %in% parameter_list || "KELTMHI" %in% parameter_list || "KELNOPT" %in% parameter_list || "THALF" %in% parameter_list || "THALFF" %in% parameter_list) {
        if(comp_required[["KEL"]] || comp_required[["KELTMLO"]] || comp_required[["KELTMHI"]] || comp_required[["KELNOPT"]] || comp_required[["THALF"]] || comp_required[["THALFF"]]) {
          span_ratio <- ifelse("SPANRATIOCRIT" %in% names(map_data), suppressWarnings(as.numeric(map_data$SPANRATIOCRIT)), NA)
          kel_v <- kel(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], exflag = kel_flag, spanratio = span_ratio)
        } else {
          kel_v <- NULL
        }
###        if("KELRSQ" %in% parameter_list || "KELRSQA" %in% parameter_list) {
        if(comp_required[["KELR"]] || comp_required[["KELRSQ"]] || comp_required[["KELRSQA"]]) {
          kelr_v <- kel_r(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], exflag = kel_flag)
        }
###        if("LASTTIME" %in% parameter_list) {
        if(comp_required[["LASTTIME"]]) {
          last_time <- lasttime(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        }
### 2019-08-30/TGT/ Remove "CMAXC"/"CMAXCi" since only valid for M2 Bolus administration applications
###        if("CMAXC" %in% parameter_list && "CMAX" %in% parameter_list && "TMAX" %in% parameter_list && "KEL" %in% parameter_list) {
###          c_maxc <- cmaxc(kel = kel_v[["KEL"]], cmax = c_max, c0 = c_0, tmax = t_max)
###        }
###        if("AUCALL" %in% parameter_list && 'TMAX' %in% parameter_list) {
        if(comp_required[["AUCALL"]]) {
          aucall <- auc_all(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, exflag = auc_flag, t_max = t_max)
        }
### 2019-08-09/TGT/ Compute AUCINFO conditionally as needed for AUCINFO, AUCINFOC, AUCINFODN
###                 Use regular expression to determine if ANY of these parameters are requested
###        if("AUCINFO" %in% parameter_list && "AUCLAST" %in% parameter_list && "CLAST" %in% parameter_list && "KEL" %in% parameter_list) {
###        if(parameter_required("^AUCINFO", parameter_list) && "CLAST" %in% parameter_list && "KEL" %in% parameter_list) {
        if(comp_required[["AUCINFO"]]) {
          aucinf_o <- auc_inf_o(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag, auclast = auclast, c_last = c_last)
        }
###        if("CEST" %in% parameter_list && "TLAST" %in% parameter_list && "KEL" %in% parameter_list) {
###        if(comp_required[["CEST"]]) {
        if(comp_required[["CEST"]] || parameter_required("KEL", names(kel_v)) || parameter_required("KELC0", names(kel_v))) {
          span_ratio <- ifelse("SPANRATIOCRIT" %in% names(map_data), suppressWarnings(as.numeric(map_data$SPANRATIOCRIT)), NA)
          c_est <- cest(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], kelflag = kel_flag, t_last = t_last, spanratio = span_ratio, kel = kel_v[["KEL"]])
        }
### 2019-08-09/TGT/ Compute AUCINFP conditionally as needed for AUCINFP, AUCINFPC, AUCINFPDN
###                 Use regular expression to determine if ANY of these parameters are requested
###        if("AUCINFP" %in% parameter_list && "AUCLAST" %in% parameter_list && "TLAST" %in% parameter_list && "KEL" %in% parameter_list) {
###        if(parameter_required("^AUCINFP", parameter_list) && "TLAST" %in% parameter_list && "KEL" %in% parameter_list) {
        if(comp_required[["AUCINFP"]]) {
          aucinf_p <- auc_inf_p(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag, auclast = auclast, t_last = t_last)
        }
###        if("AUCT" %in% parameter_list && 'TMAXi' %in% parameter_list) {
        if(comp_required[["AUCT"]] && auc_len > 1) {
          auct <- list()
        }
###        if("AUCTDN" %in% parameter_list && 'TMAXi' %in% parameter_list) {
        if(comp_required[["AUCTDN"]] && auc_len > 1) {
          auctdn <- list()
        }
###        if("AUCT" %in% parameter_list || 'AUCTDN' %in% parameter_list) {
        if((comp_required[["AUCT"]] || comp_required[["AUCTDN"]]) && auc_len > 1) {
          auc_int <- list()
        }
###        if("AUCT1_T2" %in% parameter_list && 'TMAXi' %in% parameter_list && auc_pair_check) {
        if(comp_required[["AUCT1_T2"]] && auc_pair_check) {
          auct1_t2 <- list()
          auct1_t2_names <- c(rep(paste0("AUC.", 1:auc_par_len, ".T1")), rep(paste0("AUC.", 1:auc_par_len, ".T2")))

          if(!all(auct1_t2_names %in% names(map_data))){
            par_col <- rep(paste0("'", auct1_t2_names[!auct1_t2_names %in% names(map_data)], "'"))
            stop(paste0("Dataset provided via 'map' does not contain the required columns for partial areas ", par_col))
          }
        }

        for(d in 1:di_col){
          #tau_val <- unique(tmp_df[,as.character(map_data[c(paste0("TAU",d))])])[1]
          tmp_di_df <- tmp_df[tmp_df[c(paste0("DI", d, "F"))] == 1,]
          tmp_di_df <- tmp_di_df[order(tmp_di_df[,map_data$TIME]),]
          norm_bs <- ifelse("NORMBS" %in% names(map_data), ifelse(map_data$NORMBS %in% names(tmp_di_df), tmp_di_df[,map_data$NORMBS][1], NA), NA)
          tmp_dose <- tmp_di_df[, as.character(map_data[c(paste0("DOSE",d))])][1]

          if(comp_required[["DOSECi"]] || comp_required[["DOSEC"]]) {
            if(!is.na(tmp_dose)) { 
              dose_c_i[[d]] <- dosec(data = tmp_di_df, map = map_data, idose=d)
            } else {
              dose_c_i[[d]] <- dose_c
            }
          }
          dof[[d]] <- ifelse(paste0("DOF",d) %in% names(map_data), ifelse(map_data[c(paste0("DOF",d))] %in% names(data_data), unique(tmp_di_df[,as.character(map_data[c(paste0("DOF",d))])])[1], NA), NA)
###          if("CMAXi" %in% parameter_list) {
          if(comp_required[["CMAXi"]]) {
            c_maxi[[d]] <- cmax(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME])
          }
###          if("TMAXi" %in% parameter_list) {
          if(comp_required[["TMAXi"]]) {
            t_maxi[[d]] <- tmax(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME])
          }
          if(comp_required[["CENDINFi"]]){
            cend_inf[[d]] <- cendinf(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME], dof = dof[[d]], cmax = c_maxi[[d]])
          }
          if(comp_required[["CENDINFDNi"]]){
            cend_infdn[[d]] <- cendinf_dn(cendinf = cend_inf[[d]], dose = tmp_dose)
          }
          if(comp_required[["TENDINFi"]]){
            tend_inf[[d]] <- tendinf(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME], dof = dof[[d]], tmax = t_maxi[[d]])
          }
###          if("TAUi" %in% parameter_list) {
          if(comp_required[["TAUi"]] || comp_required[["TAU"]]) {
            tau[[d]] <- tmp_di_df[, as.character(map_data[c(paste0("TAU",d))])][1]
            tau[[d]] <- as.numeric(tau[[d]])
          }
###          if("CMINi" %in% parameter_list) {
          if(comp_required[["CMINi"]]) {
            c_mini[[d]] <- cmin(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME])
          }
###          if("CLASTi" %in% parameter_list) {
          if(comp_required[["CLASTi"]]) {
            c_lasti[[d]] <- clast(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME])
          }
###          if("CMAXDNi" %in% parameter_list && "CMAXi" %in% parameter_list) {
          if(comp_required[["CMAXDNi"]]) {
            c_maxdni[[d]] <- cmax_dn(cmax = c_maxi[[d]], dose = tmp_dose)
          }
          if(comp_required[["CMINDNi"]]){
            c_mindni[[d]] <- cmin_dn(cmin = c_mini[[d]], dose = tmp_dose)
          }
###          if("TMINi" %in% parameter_list) {
          if(comp_required[["TMINi"]]) {
            t_mini[[d]] <- tmin(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME])
          }
###          if("TLASTi" %in% parameter_list) {
          if(comp_required[["TLASTi"]]) {
            t_lasti[[d]] <- tlast(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME])
          }
###          if("LASTTIMEi" %in% parameter_list) {
          if(comp_required[["LASTTIMEi"]]) {
            last_timei[[d]] <- lasttime(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME])
          }
### 2019-08-30/TGT/ Remove "CMAXC"/"CMAXCi" since only valid for M2 Bolus administration applications
###          if("CMAXCi" %in% parameter_list && "CMAXi" %in% parameter_list && "TMAXi" %in% parameter_list && "KEL" %in% parameter_list) {
###            c_maxci[[d]] <- cmaxc(kel = kel_v[["KEL"]], cmax = c_maxi[[d]], c0 = c_0, tmax = t_maxi[[d]])
###          }
###          if("AUCINFOi" %in% parameter_list && "AUCLASTi" %in% parameter_list && "CLASTi" %in% parameter_list && "KEL" %in% parameter_list) {
          if(comp_required[["AUCINFOi"]]) {
            span_ratio <- ifelse("SPANRATIOCRIT" %in% names(map_data), suppressWarnings(as.numeric(map_data$SPANRATIOCRIT)), NA)
            aucinfoi[[d]] <- auc_inf_o(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag, spanratio = span_ratio, kel = kel_v)
          }
          if(comp_required[["AUCINFOCi"]]){
            aucinfoi_c[[d]] <- auc_inf_oc(kel = kel_v[["KEL"]], aucinfo = aucinfoi[[d]], c0 = c_0)
          }
          if(comp_required[["AUCINFODNi"]]){
            aucinfoi_dn[[d]] <- auc_dn(auc = aucinfoi[[d]], dose = tmp_dose)
          }
###          if("AUCINFPi" %in% parameter_list && "CEST" %in% parameter_list && "AUCLASTi" %in% parameter_list && "TLASTi" %in% parameter_list && "KEL" %in% parameter_list) {
          if(comp_required[["AUCINFPi"]]) {
            aucinfpi[[d]] <- auc_inf_p(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag)
          }
          if(comp_required[["AUCINFPCi"]]){
            aucinfpi_c[[d]] <- auc_inf_pc(kel = kel_v[["KEL"]], aucinfp = aucinfpi[[d]], c0 = c_0)
          }
          if(comp_required[["AUCINFPDNi"]]){
            aucinfpi_dn[[d]] <- auc_dn(auc = aucinfpi[[d]], dose = tmp_dose)
          }
###          if("AUMCINFOi" %in% parameter_list && "AUCLASTi" %in% parameter_list && "CLASTi" %in% parameter_list && "KEL" %in% parameter_list) {
          if(comp_required[["AUMCINFOi"]]) {
            aumcinfoi[[d]] <- aumc_inf_o(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag)
          }
###          if("AUMCINFPi" %in% parameter_list && "CEST" %in% parameter_list && "AUCLASTi" %in% parameter_list && "TLASTi" %in% parameter_list && "KEL" %in% parameter_list) {
          if(comp_required[["AUMCINFPi"]]) {
            aumcinfpi[[d]] <- aumc_inf_p(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag)
          }
###          if("AUCDN" %in% parameter_list && "AUCALL" %in% parameter_list) {
### 2019-10-10/TGT/ replace AUCDN with AUCALLDN
###          if(comp_required[["AUCDN"]]) {
###            aucdn[[d]] <- auc_dn(auc = aucall, dose = tmp_dose)
          if(comp_required[["AUCALLDN"]]) {
            aucalldn[[d]] <- auc_dn(auc = aucall, dose = tmp_dose)
          }
###          if("AUCLASTi" %in% parameter_list && 'TMAXi' %in% parameter_list && 'TLASTi' %in% parameter_list) {
          if(comp_required[["AUCLASTi"]]) {
            auclasti[[d]] <- auc_last(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME], method = method, exflag = auc_flag)
          }
###          if("AUCLASTCi" %in% parameter_list && "AUCLASTi" %in% parameter_list && "TLASTi" %in% parameter_list && "KEL" %in% parameter_list) {
          if(comp_required[["AUCLASTCi"]]) {
            auclasti_c[[d]] <- auc_lastc(kel = kel_v[["KEL"]], auclast = auclasti[[d]], c0 = c_0, tlast = t_lasti[[d]])
          }
###          if("AUCLASTDNi" %in% parameter_list && "AUCLASTi" %in% parameter_list) {
          if(comp_required[["AUCLASTDNi"]]) {
            auclasti_dn[[d]] <- auc_dn(auc = auclasti[[d]], dose = tmp_dose)
          }
###          if("AUMCLASTi" %in% parameter_list && 'TMAXi' %in% parameter_list && 'TLASTi' %in% parameter_list) {
          if(comp_required[["AUMCLASTi"]]) {
            aumclasti[[d]] <- aumc_last(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME], method = method, exflag = auc_flag)
          }
###          if("AUCTAUi" %in% parameter_list) {
          if(comp_required[["AUCTAUi"]]) {
            auctau[[d]] <- auc_tau(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME], method = method, exflag = auc_flag, tau = tau[[d]], orig_conc = orig_conc, orig_time = orig_time, last_crit_factor = last_crit_factor, kel = kel_v, auclast = auclasti[[d]])
          }
###          if("AUCTAUDNi" %in% parameter_list && "AUCTAUi" %in% parameter_list) {
          if(comp_required[["AUCTAUDNi"]]) {
            auctaudn[[d]] <- auc_dn(auc = auctau[[d]], dose = tmp_dose)
          }
###          if("AUMCTAUi" %in% parameter_list && "TMAXi" %in% parameter_list) {
          if(comp_required[["AUMCTAUi"]]) {
            aumctaui[[d]] <- aumc_tau(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME], method = 1, exflag = auc_flag, tau = tau[[d]], t_max = t_maxi[[d]], orig_conc = tmp_df[,map_data$CONC], orig_time = tmp_df[,map_data$TIME])
          }
###          if("MRTLASTi" %in% parameter_list && "AUCLASTi" %in% parameter_list && "AUMCLASTi" %in% parameter_list) {
          if(comp_required[["MRTLASTi"]]) {
            mrtlasti[[d]] <- mrt_last(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME], method = method, model = "M2", aucflag = auc_flag, dof = dof[[d]], auclast = auclasti[[d]])
          }
###          if("MRTIVIFOi" %in% parameter_list && "AUCINFOi" %in% parameter_list && "AUCTAUi" %in% parameter_list && "AUMCTAUi" %in% parameter_list){
          if(comp_required[["MRTIVIFOi"]]){
            mrtivifoi[[d]] <- mrt_ivif_o(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME], method = method, parameter = "SS", kelflag = kel_flag, aucflag = auc_flag, tau = tau[[d]], dof = dof[[d]], orig_conc = tmp_df[,map_data$CONC], orig_time = tmp_df[,map_data$TIME], aucinfo = aucinfoi[[d]], aumcinfo = aumcinfoi[[d]], auctau = auctau[[d]], aumctau = aumctaui[[d]])
          }
###          if("MRTIVIFPi" %in% parameter_list && "AUCINFPi" %in% parameter_list && "AUCTAUi" %in% parameter_list && "AUMCTAUi" %in% parameter_list){
          if(comp_required[["MRTIVIFPi"]]){
            mrtivifpi[[d]] <- mrt_ivif_p(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME], method = method, parameter = "SS", kelflag = kel_flag, aucflag = auc_flag, tau = tau[[d]], dof = dof[[d]], orig_conc = tmp_df[,map_data$CONC], orig_time = tmp_df[,map_data$TIME], aucinfp = aucinfpi[[d]], aumcinfp = aumcinfpi[[d]], auctau = auctau[[d]], aumctau = aumctaui[[d]])
          }
###          if("AUCXPCTOi" %in% parameter_list && "AUCINFOi" %in% parameter_list && "AUCLASTi" %in% parameter_list){
          if(comp_required[["AUCXPCTOi"]]){
            aucxpctoi[[d]] <- auc_XpctO(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag, auc_info = aucinfoi[[d]], auclast = auclasti[[d]])
          }
###          if("AUCXPCTPi" %in% parameter_list && "AUCINFPi" %in% parameter_list && "AUCLASTi" %in% parameter_list){
          if(comp_required[["AUCXPCTPi"]]){
            aucxpctpi[[d]] <- auc_XpctP(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag, auc_infp = aucinfpi[[d]], auclast = auclasti[[d]])
          }
###          if("AUMCXPTOi" %in% parameter_list && "AUMCINFOi" %in% parameter_list && "AUMCLAST" %in% parameter_list){
          if(comp_required[["AUMCXPTOi"]]){
            aumcxptoi[[d]] <- aumc_XpctO(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag)
          }
###          if("AUMCXPTPi" %in% parameter_list && "AUMCINFPi" %in% parameter_list && "AUMCLAST" %in% parameter_list){
          if(comp_required[["AUMCXPTPi"]]){
            aumcxptpi[[d]] <- aumc_XpctP(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag)
          }
###          if("TOLDi" %in% parameter_list) {
          if(comp_required[["TOLDi"]]) {
            told[[d]] <- tmp_di_df[, as.character(map_data[c(paste0("TOLD",d))])][1]
            told[[d]] <- as.numeric(told[[d]])
          }
###          if("CAVi" %in% parameter_list && "AUCTAUi" %in% parameter_list) {
          if(comp_required[["CAVi"]]) {
            ca_v[[d]] <- cav(auctau = auctau[[d]], tau = tau[[d]])
          }
###          if("DIi" %in% parameter_list) {
          if(comp_required[["DIi"]]) {
            if(is.na(tmp_di_df[,map_data$TIME][1])){
              di[[d]] <- NA
            } else {
              di[[d]] <- paste0(tmp_di_df[,map_data$TIME][1], "-", tmp_di_df[,map_data$TIME][nrow(tmp_di_df)])
            }
          }
###          if("DOSEi" %in% parameter_list) {
          if(comp_required[["DOSEi"]]) {
            dose[[d]] <- tmp_dose
          }
          if(comp_required[["CTROUGHi"]]){
            c_troughi[[d]] <- ctrough(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME], tau = tau[[d]], told = told[[d]])
          }
          if(comp_required[["CTROUGHENDi"]]){
            c_troughendi[[d]] <- ctroughend(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME], tau = tau[[d]], told = told[[d]])
          }
          if(comp_required[["PTROUGHRi"]]){
            p_troughri[[d]] <- ptroughr(cmax = c_maxi[[d]], ctrough = c_troughi[[d]])
          }
          if(comp_required[["PTROUGHRENDi"]]){
            p_troughrendi[[d]] <- ptroughrend(cmax = c_maxi[[d]], ctrough = c_troughendi[[d]])
          }
###          if("CLTAUi" %in% parameter_list && "AUCTAUi" %in% parameter_list) {
          if(comp_required[["CLTAUi"]]) {
###            cl_tau[[d]] <- cltau(auctau = auctau[[d]], dose = tmp_dose)
            cl_tau[[d]] <- cltau(auctau = auctau[[d]], dose = tmp_dose)
          }
###          if("CLTAUWi" %in% parameter_list && "CLTAUi" %in% parameter_list && "AUCTAUi" %in% parameter_list) {
          if(comp_required[["CLTAUWi"]]) {
            cl_tauw[[d]] <- cltauw(cltau = cl_tau[[d]], normbs = norm_bs)
          }
###          if("PTFi" %in% parameter_list && "CMAXi" %in% parameter_list && "CMINi" %in% parameter_list && "CAVi" %in% parameter_list && "AUCTAUi" %in% parameter_list) {
          if(comp_required[["PTFi"]]) {
            pt_f[[d]] <- ptf(cmax = c_maxi[[d]], cmin = c_mini[[d]], cav = ca_v[[d]])
          }
###          if("PTRi" %in% parameter_list && "CMAXi" %in% parameter_list && "CMINi" %in% parameter_list) {
          if(comp_required[["PTRi"]]) {
            pt_r[[d]] <- ptr(cmax = c_maxi[[d]], cmin = c_mini[[d]])
          }
          if(comp_required[["VSSOi"]]) {
            vsso[[d]] <- vss(cl = cl_tau[[d]], mrt = mrtivifoi[[d]])
          }
          if(comp_required[["VSSPi"]]) {
            vssp[[d]] <- vss(cl = cl_tau[[d]], mrt = mrtivifpi[[d]])
          }
          if(comp_required[["VSSOWi"]]) {
            vssow[[d]] <- vssw(vss = vsso[[d]], normbs = norm_bs)
          }
          if(comp_required[["VSSPWi"]]) {
            vsspw[[d]] <- vssw(vss = vssp[[d]], normbs = norm_bs)
          }
          if(comp_required[["VZTAUi"]]) {
            vz_tau[[d]] <- vzftau(kel = kel_v[["KEL"]], auctau = auctau[[d]], dose = tmp_dose)
          }
          if(comp_required[["VZTAUWi"]]) {
            vz_tauw[[d]] <- vzftauw(vzftau = vz_tau[[d]], normbs = norm_bs)
          }
          
          if((comp_required[["AUCT"]] || comp_required[["AUCTDN"]]) && auc_len > 1) {
            time <- sort(tmp_df[,map_data$TIME])
            time_di <- sort(tmp_di_df[,map_data$TIME])
            if(d == di_col){
              if(sum(time > time_di[length(time_di)]) > 0){
                time_di <- c(time_di, time[time > time_di[length(time_di)]])
              }
            }
            prev_na <- FALSE
            prev_auc <- NA
            prev_auc_dn <- NA
  
            if(length(time) > 1){
              for(t in 2:(length(time))){
                if(time[t] %in% time_di[-1]) {
                  tmp <- auc_t1_t2(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], t1 = time_di[1], t2 = time[t], method = method, exflag = auc_flag, t_max = t_maxi[[d]])
                  tmp_dn <- auc_dn(auc = tmp, dose = tmp_dose)
                  tmp_int <- paste0(time[1], "_", time[t])
                } else {
                  tmp <- NA
                  tmp_dn <- NA
                  tmp_int <- paste0(time[1], "_", time[t])
                }
  
                if(d == 1){
  ###                if("AUCT" %in% parameter_list){
                  if(comp_required[["AUCT"]]){
                    auct[[t-1]] <- tmp
                  }
  ###                if("AUCTDN" %in% parameter_list){
                  if(comp_required[["AUCTDN"]]){
                    auctdn[[t-1]] <- tmp_dn
                  }
                  auc_int[[t-1]] <- tmp_int
                } else {
                  if(prev_na){
                    prev_na <- FALSE
                    if(is.numeric(tmp)){
  ###                    if("AUCT" %in% parameter_list){
                      if(comp_required[["AUCT"]]){
                        prev_auc <- unlist(auct[[t-2]])
                        auct[[t-1]] <- sum(c(prev_auc, tmp), na.rm = TRUE)
                      }
                    }
                    if(is.numeric(tmp_dn)){
  ###                    if("AUCTDN" %in% parameter_list){
                      if(comp_required[["AUCTDN"]]){
                        prev_auc_dn <- unlist(auctdn[[t-2]])
                        auctdn[[t-1]] <- sum(c(prev_auc_dn, tmp_dn), na.rm = TRUE)
                      }
                    }
                  } else {
  ###                  if("AUCT" %in% parameter_list){
                    if(comp_required[["AUCT"]]){
                      if(!is.na(prev_auc)){
                        auct[[t-1]] <- sum(c(prev_auc, tmp), na.rm = TRUE)
                      } else {
                        if(is.na(auct[[t-1]]) && is.na(tmp)){
                          auct[[t-1]] <- NA
                        } else {
                          auct[[t-1]] <- sum(c(auct[[t-1]], tmp), na.rm = TRUE)
                        }
                      }
                    }
  ###                  if("AUCTDN" %in% parameter_list){
                    if(comp_required[["AUCTDN"]]){
                      if(!is.na(prev_auc_dn)){
                        auctdn[[t-1]] <- sum(c(prev_auc_dn, tmp_dn), na.rm = TRUE)
                      } else {
                        if(is.na(auctdn[[t-1]]) && is.na(tmp_dn)){
                          auctdn[[t-1]] <- NA
                        } else {
                          auctdn[[t-1]] <- sum(c(auctdn[[t-1]], tmp_dn), na.rm = TRUE)
                        }
                      }
                    }
                  }
                  auc_int[[t-1]] <- ifelse(auc_int[[t-1]] != tmp_int, tmp_int, auc_int[[t-1]])
  
                  if(is.na(tmp)){
                    prev_na <- TRUE
                  } else {
                    prev_na <- FALSE
                  }
                }
              }
            } else {
              auct <- rep(NA, auc_len)
              auctdn <- rep(NA, auc_len)
              auc_int <- rep(NA, auc_len)
            }
            if(d == di_col){
###              if("AUCT" %in% parameter_list){
              if(comp_required[["AUCT"]]){
                if(length(auct) < auc_len) {
                  auct <- c(auct, rep(NA, (auc_len - length(auct))))
                }
              }
###              if("AUCTDN" %in% parameter_list){
              if(comp_required[["AUCTDN"]]){
                if(length(auctdn) < auc_len) {
                  auctdn <- c(auctdn, rep(NA, (auc_len - length(auctdn))))
                }
              }
              if(length(auc_int) < auc_len) {
                auc_int <- c(auc_int, rep(NA, (auc_len - length(auc_int))))
              }
            }
          }
###          if("AUCT1_T2" %in% parameter_list && 'TMAXi' %in% parameter_list && auc_pair_check) {
          if(comp_required[["AUCT1_T2"]] && auc_pair_check) {
            time <- sort(unique(data_data[,map_data$TIME]))
            time_di <- sort(tmp_di_df[,map_data$TIME])

##            2019-11-11/RD Added for Interpolation to account for error handling
##
            if((isTRUE(interpolation) || isTRUE(extrapolation)) && !(c(paste0("TOLD",d)) %in% names(map_data))){
              stop(paste0("Dataset provided via 'map' does not contain the required columns for interpolating partial areas ", paste0("TOLD",d)))
            } else if((isTRUE(interpolation) || isTRUE(extrapolation)) && (c(paste0("TOLD",d)) %in% names(map_data))) {
              if((isTRUE(interpolation) || isTRUE(extrapolation)) && !(map_data[, c(paste0("TOLD",d))] %in% names(tmp_di_df))){
                stop(paste0("Dataset provided via 'data' does not contain the required columns for interpolating partial areas ", paste0("TOLD",d)))
              } else if((isTRUE(interpolation) || isTRUE(extrapolation)) && (map_data[, c(paste0("TOLD",d))] %in% names(tmp_di_df))){
                tmp_told <- as.numeric(tmp_di_df[, as.character(map_data[c(paste0("TOLD",d))])][1])
              } else {
                tmp_told <- NA
              }
            } else {
              tmp_told <- NA
            }
            for(t in 1:(auc_par_len)){
              if(!(is.numeric(as.numeric(map_data[, paste0("AUC.", t, ".T1")])) && is.numeric(as.numeric(map_data[, paste0("AUC.", t, ".T2")])))){
                stop(paste0("'AUC.", t, ".T1' and/or 'AUC.", t, ".T2' value provided via 'map' is not a numeric value"))
              }
              auc_t1 <- as.numeric(map_data[, paste0("AUC.", t, ".T1")])
              auc_t2 <- as.numeric(map_data[, paste0("AUC.", t, ".T2")])
##              2019-11-08/RD Changed the call for partial AUCs to account for interpolation
##
              if((isTRUE(interpolation) || isTRUE(extrapolation))){
                tmp <- auc_t1_t2(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], t1 = auc_t1, t2 = auc_t2, method = method, exflag = auc_flag, t_max = t_max, interpolate = interpolation, extrapolate = extrapolation, model = "M3", dosing_type = "SS", told = tmp_told, kel = kel_v, orig_conc = orig_conc, orig_time = orig_time, includeNA = TRUE)
                if(is.list(tmp)){
                  tmp_auc <- tmp[[1]]
                  if(t == 1){
                    cest_tmp <- tmp[[2]]
                  } else {
                    cest_tmp <- rbind(cest_tmp, tmp[[2]])
                  }
                  cest_tmp <- unique(na.omit(cest_tmp))
                } else {
                  tmp_auc <- tmp
                }
              } else {
                tmp_auc <- auc_t1_t2(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], t1 = auc_t1, t2 = auc_t2, method = method, exflag = auc_flag, t_max = t_max, includeNA = TRUE)
              }
              
              if(d == 1){
                auct1_t2[[t]] <- tmp_auc
              } else {
                auct1_t2[[t]] <- sum(c(unlist(auct1_t2[[t]]), tmp_auc), na.rm = TRUE)
              }
            }
          }
          #c_max_c <- cmaxc(kel = kel_v[["KEL"]], cmax = c_max, c0 = c_0, tmax = t_max)
        }
        #c_max_c <- cmaxc(kel = kel_v[["KEL"]], cmax = c_max, c0 = c_0, tmax = t_max)
        #auclast_c <- auc_lastc(kel = kel_v[["KEL"]], auclast = auclast, c0 = c_0, tlast = t_last)

###        if("AUCLASTC" %in% parameter_list && "AUCLAST" %in% parameter_list && "TLAST" %in% parameter_list && "KEL" %in% parameter_list) {
        if(comp_required[["AUCLASTC"]]) {
          auclastc <- auc_lastc(kel = kel_v[["KEL"]], auclast = auclast, c0 = c_0, tlast = t_last)
        }
###        if("AUCLASTDN" %in% parameter_list && "AUCLAST" %in% parameter_list) {
        if(comp_required[["AUCLASTDN"]]) {
          auclastdn <- auc_dn(auc = auclast, dose = tmp_dose)
        }
###        if("AUCINFOC" %in% parameter_list && "KEL" %in% parameter_list && "AUCINFO" %in% parameter_list) {
        if(comp_required[["AUCINFOC"]]) {
          aucinf_oc <- auc_inf_oc(kel = kel_v[["KEL"]], aucinfo = aucinf_o, c0 = c_0)
        }
###        if("AUCINFPC" %in% parameter_list && "KEL" %in% parameter_list && "AUCINFP" %in% parameter_list) {
        if(comp_required[["AUCINFPC"]]) {
          aucinf_pc <- auc_inf_pc(kel = kel_v[["KEL"]], aucinfp = aucinf_p, c0 = c_0)
        }
###        if("AUCINFODN" %in% parameter_list && "AUCINFO" %in% parameter_list) {
        if(comp_required[["AUCINFODN"]]) {
          aucinf_odn <- auc_dn(auc = aucinf_o, dose = tmp_dose)
        }
###        if("AUCINFPDN" %in% parameter_list && "AUCINFP" %in% parameter_list) {
        if(comp_required[["AUCINFPDN"]]) {
          aucinf_pdn <- auc_dn(auc = aucinf_p, dose = tmp_dose)
        }
###        if("MRTLAST" %in% parameter_list && "AUCLAST" %in% parameter_list && "AUMCLAST" %in% parameter_list) {
        if(comp_required[["MRTLAST"]]) {
          mrtlast <- mrt_last(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, model = "M2", aucflag = auc_flag, dof = dof[[d]], auclast = auclast)
        }
###        if("AUCXPCTO" %in% parameter_list && "AUCINFO" %in% parameter_list && "AUCLAST" %in% parameter_list){
        if(comp_required[["AUCXPCTO"]]){
          aucxpcto <- auc_XpctO(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag, auc_info = aucinf_o, auclast = auclast)
        }
###        if("AUCXPCTP" %in% parameter_list && "AUCINFP" %in% parameter_list && "AUCLAST" %in% parameter_list){
        if(comp_required[["AUCXPCTP"]]){
          aucxpctp <- auc_XpctP(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag, auc_infp = aucinf_p, auclast = auclast)
        }
###        if("AUMCXPTO" %in% parameter_list && "AUMCINFO" %in% parameter_list && "AUMCLAST" %in% parameter_list){
        if(comp_required[["AUMCXPTO"]]){
          aumcxpto <- aumc_XpctO(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag)
        }
###        if("AUMCXPTP" %in% parameter_list && "AUMCINFP" %in% parameter_list && "AUMCLAST" %in% parameter_list){
        if(comp_required[["AUMCXPTP"]]){
          aumcxptp <- aumc_XpctP(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag)
        }
###        if("KEL" %in% parameter_list){
        if(comp_required[["KEL"]]){
          exflag <- !as.logical(kel_flag)

          pkdataid <- tmp_df[,"PKDATAROWID"][exflag]
### 2019-08-05/TGT/ Following incorrectly identifies CONC for TIME
###          time <- tmp_df[,map_data$CONC][exflag]
          time <- tmp_df[,map_data$TIME][exflag]
### 2019-08-05/TGT/ Following incorrectly identifies TIME for CONC
###          conc <- tmp_df[,map_data$TIME][exflag]
          conc <- tmp_df[,map_data$CONC][exflag]
          cest_kel <- rep(NA, length(conc))
          
            if(!is.na(kel_v[["KEL"]])){
### 2019-08-05/TGT/ following algorithm for estimation of intercept is not correct            
###            intercept <- sum(conc-(-1*kel_v[["KEL"]]*time))/length(conc)
### is is replaced with
### 2019-08-05/TGT/ following algorithm for estimation of concentration is not correct            
###            slope <- -1*kel_v[["KEL"]]
###            cest_kel <- (slope*time)+intercept
### is is replaced with
### the equivalent of    cest_kel <- exp(intercept + (slope)*time)
###            cest_kel <- cest(time, conc, slope=kel_v[["KEL"]])
### in new function estimate_concentration
              cest_kel <- estimate_concentration(time, conc, slope=kel_v[["KEL"]])
            }
###          } else {
###            cest_kel <- rep(NA, length(conc))
        } else {
          pkdataid <- NULL
        }

        tmp_est_data <- data.frame(matrix(ncol = length(elist), nrow = 0))
        names(tmp_est_data) <- elist
        est_idx <- 1
        if(length(pkdataid) > 0){
          ## 2019-11-25/RD Added logic to account for Interpolated/Extrapolated values that are generated before CEST KEL timepoints
          if(nrow(cest_tmp) > 0){
            cest_idx0 <- c()
            for(c in 1:nrow(cest_tmp)){
              if(cest_tmp[c,"TIME"] < time[1]){
                if(cest_tmp[c,"INT_EXT"] == "INT"){
                  tmp_est_row <- c(NA, unique(data_data[,map_data$SDEID])[i], cest_tmp[c,"TIME"], NA, cest_tmp[c,"CONC"], NA, NA, NA) 
                } else if(cest_tmp[c,"INT_EXT"] == "EXT"){
                  tmp_est_row <- c(NA, unique(data_data[,map_data$SDEID])[i], cest_tmp[c,"TIME"], NA, NA, cest_tmp[c,"CONC"], NA, NA)
                }
                cest_idx0 <- c(cest_idx0, c)
                tmp_est_data[est_idx,] <- tmp_est_row
                est_idx <- est_idx + 1
              }
            }
            if(length(cest_idx0) > 0){
              cest_tmp <- cest_tmp[-cest_idx0,]
            }
          }
          for(e in 1:length(pkdataid)){
            est_row <- c(pkdataid[e], unique(data_data[,map_data$SDEID])[i], time[e], cest_kel[e], NA, NA, NA, NA)
### 2019-10-06/TGT/ Add CEST at TLAST
            ## 2019-11-24/RD Added check for NA to account for all NAs concentration data
            if(comp_required[["TLAST"]]) { if(!is.na(t_last)){ if(time[e]==t_last) { est_row[8] <- c_est } } }
            
            if(nrow(cest_tmp) > 0){
              cest_idx <- which(cest_tmp$TIME == time[e])
              if(length(cest_idx) > 0){
                curr_cest <- cest_tmp[cest_idx,]
                if(curr_cest[,"INT_EXT"] == "INT"){
                  est_row[5] <- curr_cest[,"CONC"]
                } else if(curr_cest[,"INT_EXT"] == "EXT"){
                  est_row[6] <- curr_cest[,"CONC"]
                }
                cest_tmp <- cest_tmp[-cest_idx,]
              }
            }
            tmp_est_data[est_idx,] <- est_row
            est_idx <- est_idx + 1
          }
        }
        if(nrow(cest_tmp) > 0){
          for(c in 1:nrow(cest_tmp)){
            if(cest_tmp[c,"INT_EXT"] == "INT"){
              tmp_est_row <- c(NA, unique(data_data[,map_data$SDEID])[i], cest_tmp[c,"TIME"], NA, cest_tmp[c,"CONC"], NA, NA, NA) 
            } else if(cest_tmp[c,"INT_EXT"] == "EXT"){
              tmp_est_row <- c(NA, unique(data_data[,map_data$SDEID])[i], cest_tmp[c,"TIME"], NA, NA, cest_tmp[c,"CONC"], NA, NA)
            }
            tmp_est_data[est_idx,] <- tmp_est_row
            est_idx <- est_idx + 1
          }
        }
        tmp_est_data <- tmp_est_data[order(tmp_est_data$TIME), ]
        est_data <- rbind(est_data, tmp_est_data)

        #computation_df[i,] <- c(unique(data_data[,map_data$SDEID])[i], unlist(c_max), unlist(c_min), unlist(c_last),
        #                        unlist(cmaxdn), unlist(t_max), unlist(t_min), unlist(t_last), t_lag, kel_v[["KEL"]], kel_v[["KELTMLO"]],
        #                        kel_v[["KELTMHI"]], kel_v[["KELNOPT"]], kelr_v[["KELR"]], kelr_v[["KELRSQ"]], kelr_v[["KELRSQA"]],
        #                        kel_v[["THALF"]], last_time, unlist(di), unlist(tau), unlist(told), unlist(dose), aucall, unlist(aucdn), unlist(auclast),
        #                        unlist(auclastdn), auct, auc_int, aucinf_o, aucinf_p, aucinf_oc, aucinf_pc, unlist(aucinfo_dn),
        #                        unlist(aucinfp_dn), unlist(auctau), unlist(auctaudn), mrto, mrtp, aucxpcto, aucxpctp, unlist(vzf_p),
        #                        unlist(cl_o), unlist(clf_o), unlist(ca_v), unlist(clf_tau), unlist(clf_tauw), unlist(pt_f), unlist(pt_r),
        #                        unlist(vzf_tau), unlist(vzf_tauw))

##        row_data <- c(unique(data_data[,map_data$SDEID])[i])
        computation_df[i, "SDEID"] <- unique(data_data[,map_data$SDEID])[i]
        if(disp_required[["DOSEi"]]) {
##          row_data <- c(row_data, unlist(dose))
          computation_df[i, dosenames] <- unlist(dose)
        }
        if(disp_required[["DOSEC"]]) {
##          row_data <- c(row_data, dose_c)
          computation_df[i, "DOSEC"] <- dose_c
        }
        if(disp_required[["DOSECi"]]) {
##          row_data <- c(row_data, unlist(dose_c_i))
          computation_df[i, paste0("DOSEC",1:di_col)] <- unlist(dose_c_i)
        }
        if(disp_required[["DOFi"]]){
##          row_data <- c(row_data, unlist(dof))
          computation_df[i, paste0("DOF",1:di_col)] <- unlist(dof)
        }
        if(disp_required[["CENDINFi"]]){
##          row_data <- c(row_data, unlist(cend_inf))
          computation_df[i, paste0("CENDINF",1:di_col)] <- unlist(cend_inf)
        }
        if(disp_required[["CENDINFDNi"]]){
##          row_data <- c(row_data, unlist(cend_infdn))
          computation_df[i, paste0("CENDINFDN",1:di_col)] <- unlist(cend_infdn)
        }
        if(disp_required[["TENDINFi"]]){
##          row_data <- c(row_data, unlist(tend_inf))
          computation_df[i, paste0("TENDINF",1:di_col)] <- unlist(tend_inf)
        }
###        if("CMAX" %in% parameter_list) {
        if(disp_required[["CMAX"]]) {
##          row_data <- c(row_data, c_max)
          computation_df[i, "CMAX"] <- c_max
        }
###        if("CMAXi" %in% parameter_list) {
        if(disp_required[["CMAXi"]]) {
##          row_data <- c(row_data, unlist(c_maxi))
          computation_df[i, paste0("CMAX",1:di_col)] <- unlist(c_maxi)
        }
### 2019-08-30/TGT/ Remove "CMAXC"/"CMAXCi" since only valid for M2 Bolus administration applications
###        if("CMAXC" %in% parameter_list && "CMAX" %in% parameter_list && "TMAX" %in% parameter_list && "KEL" %in% parameter_list) {
###          row_data <- c(row_data, c_maxc)
###        }
### 2019-08-30/TGT/ Remove "CMAXC"/"CMAXCi" since only valid for M2 Bolus administration applications
###        if("CMAXCi" %in% parameter_list && "CMAXi" %in% parameter_list && "TMAXi" %in% parameter_list && "KEL" %in% parameter_list) {
###          row_data <- c(row_data, unlist(c_maxci))
###        }
###        if("CMAXDN" %in% parameter_list && "CMAX" %in% parameter_list) {
        if(disp_required[["CMAXDN"]]) {
##          row_data <- c(row_data, c_maxdn)
          computation_df[i, "CMAXDN"] <- c_maxdn
        }
###        if("CMAXDNi" %in% parameter_list && "CMAXi" %in% parameter_list) {
        if("CMAXDNi" %in% parameter_list && "CMAXi" %in% parameter_list) {
##          row_data <- c(row_data, unlist(c_maxdni))
          computation_df[i, paste0("CMAXDN",1:di_col)] <- unlist(c_maxdni)
        }
### 2019-08-09/TGT/ Following should not require CMAX to be part of the parameter_list. 
###                 It could be that CMAX is not called for a result but the criteria 
###                 could still rely upon CMAX. This needs to be fixed.
        if(disp_required[["FLGACCEPTPREDOSE"]] && "FLGACCEPTPREDOSECRIT" %in% names(map_data)){
          pre_dose_crit <- suppressWarnings(as.numeric(map_data$FLGACCEPTPREDOSECRIT))
          if(is.numeric(pre_dose_crit) && !is.na(pre_dose_crit)){
### 2019-08-09/TGT/ Following assumes that the predose timepoint has a time value of zero "0"
###                 With ACTUAL times being used this may not be the case. This needs to be fixed.
            pre_dose <- tmp_df[,map_data$CONC][tmp_df[,map_data$TIME] == 0][1]
            if(is.numeric(c_maxi[[1]])){
##              row_data <- c(row_data, ifelse(pre_dose > (c_maxi[[1]] * pre_dose_crit), 0, 1))
              computation_df[i, "FLGACCEPTPREDOSE"] <- ifelse(pre_dose > (c_maxi[[1]] * pre_dose_crit), 0, 1)
            } else {
##              row_data <- c(row_data, 1)
              computation_df[i, "FLGACCEPTPREDOSE"] <- 1
            }
          } else {
##            row_data <- c(row_data, 1)
            computation_df[i, "FLGACCEPTPREDOSE"] <- 1
          }
        }
###        if("CMIN" %in% parameter_list) {
        if(disp_required[["CMIN"]]) {
##          row_data <- c(row_data, c_min)
          computation_df[i, "CMIN"] <- c_min
        }
        if(disp_required[["CMINDN"]]){
          computation_df[i, "CMINDN"] <- c_mindn
        }
        if(disp_required[["CMINDNi"]]){
          computation_df[i, paste0("CMINDN",1:di_col)] <- unlist(c_mindni)
        }
###        if("CMINi" %in% parameter_list) {
        if(disp_required[["CMINi"]]) {
##          row_data <- c(row_data, unlist(c_mini))
          computation_df[i, paste0("CMIN",1:di_col)] <- unlist(c_mini)
        }
###        if("CLAST" %in% parameter_list) {
        if(disp_required[["CLAST"]]) {
##          row_data <- c(row_data, c_last)
          computation_df[i, "CLAST"] <- c_last
        }
###        if("CLASTi" %in% parameter_list) {
        if(disp_required[["CLASTi"]]) {
##          row_data <- c(row_data, unlist(c_lasti))
          computation_df[i, paste0("CLAST",1:di_col)] <- unlist(c_lasti)
        }
###        if("TMAX" %in% parameter_list) {
        if(disp_required[["TMAX"]]) {
##          row_data <- c(row_data, t_max)
          computation_df[i, "TMAX"] <- t_max
        }
###        if("TMAXi" %in% parameter_list) {
        if(disp_required[["TMAXi"]]) {
##          row_data <- c(row_data, unlist(t_maxi))
          computation_df[i, paste0("TMAX",1:di_col)] <- unlist(t_maxi)
        }
        if(disp_required[["FLGACCEPTTMAX"]] && "FLGEMESIS" %in% names(map_data)){
##          row_data <- c(row_data, 1)
          computation_df[i, "FLGACCEPTTMAX"] <- 1
        }
###        if("TMIN" %in% parameter_list) {
        if(disp_required[["TMIN"]]) {
##          row_data <- c(row_data, t_min)
          computation_df[i, "TMIN"] <- t_min
        }
###        if("TMINi" %in% parameter_list) {
        if(disp_required[["TMINi"]]) {
##          row_data <- c(row_data, unlist(t_mini))
          computation_df[i, paste0("TMIN",1:di_col)] <- unlist(t_mini)
        }
###        if("TLAST" %in% parameter_list) {
        if(disp_required[["TLAST"]]) {
##          row_data <- c(row_data, t_last)
          computation_df[i, "TLAST"] <- t_last
        }
###        if("TLASTi" %in% parameter_list) {
        if(disp_required[["TLASTi"]]) {
##          row_data <- c(row_data, unlist(t_lasti))
          computation_df[i, paste0("TLAST",1:di_col)] <- unlist(t_lasti)
        }
        if(disp_required[["CTROUGHi"]]){
##          row_data <- c(row_data, unlist(c_troughi))
          computation_df[i, paste0("CTROUGH",1:di_col)] <- unlist(c_troughi)
        }
        if(disp_required[["CTROUGHENDi"]]){
##          row_data <- c(row_data, unlist(c_troughendi))
          computation_df[i, paste0("CTROUGHEND",1:di_col)] <- unlist(c_troughendi)
        }
        if(disp_required[["PTROUGHRi"]]){
##          row_data <- c(row_data, unlist(p_troughri))
          computation_df[i, paste0("PTROUGHR",1:di_col)] <- unlist(p_troughri)
        }
        if(disp_required[["PTROUGHRENDi"]]){
##          row_data <- c(row_data, unlist(p_troughrendi))
          computation_df[i, paste0("PTROUGHREND",1:di_col)] <- unlist(p_troughrendi)
        }
###        if("KEL" %in% parameter_list) {
        if(disp_required[["KEL"]]) {
##          row_data <- c(row_data, kel_v[["KEL"]])
          computation_df[i, "KEL"] <- ifelse("KEL" %in% names(kel_v), kel_v[["KEL"]], NA)
        }
###        if("KELC0" %in% parameter_list) {
        if(disp_required[["KELC0"]]) {
##          row_data <- c(row_data, kel_v[["KELC0"]])
          computation_df[i, "KELC0"] <- ifelse("KELC0" %in% names(kel_v), kel_v[["KELC0"]], NA)
        }
###        if("KELTMLO" %in% parameter_list) {
        if(disp_required[["KELTMLO"]]) {
##          row_data <- c(row_data, kel_v[["KELTMLO"]])
          computation_df[i, "KELTMLO"] <- ifelse("KELTMLO" %in% names(kel_v), kel_v[["KELTMLO"]], NA)
        }
###        if("KELTMHI" %in% parameter_list) {
        if(disp_required[["KELTMHI"]]) {
##          row_data <- c(row_data, kel_v[["KELTMHI"]])
          computation_df[i, "KELTMHI"] <- ifelse("KELTMHI" %in% names(kel_v), kel_v[["KELTMHI"]], NA)
        }
###        if("KELNOPT" %in% parameter_list) {
        if(disp_required[["KELNOPT"]]) {
##          row_data <- c(row_data, kel_v[["KELNOPT"]])
          computation_df[i, "KELNOPT"] <- ifelse("KELNOPT" %in% names(kel_v), kel_v[["KELNOPT"]], NA)
        }
### 2019-08-12/TGT/ Modify this to explicitly refer to KELR rather than impute it
###        if("KELRSQ" %in% parameter_list || "KELRSQA" %in% parameter_list){
###          row_data <- c(row_data, kelr_v[["KELR"]])
###        }
###        if(parameter_required("^KELR$", parameter_list)){
        if(disp_required[["KELR"]]){
##          row_data <- c(row_data, kelr_v[["KELR"]])
          computation_df[i, "KELR"] <- ifelse("KELR" %in% names(kelr_v), kelr_v[["KELR"]], NA)
        }
###        if("KELRSQ" %in% parameter_list){
        if(disp_required[["KELRSQ"]]){
##          row_data <- c(row_data, kelr_v[["KELRSQ"]])
          computation_df[i, "KELRSQ"] <- ifelse("KELRSQ" %in% names(kelr_v), kelr_v[["KELRSQ"]], NA)
        }
###        if("KELRSQA" %in% parameter_list){
        if(disp_required[["KELRSQA"]]){
##          row_data <- c(row_data, kelr_v[["KELRSQA"]])
          computation_df[i, "KELRSQA"] <- ifelse("KELRSQA" %in% names(kelr_v), kelr_v[["KELRSQA"]], NA)
        }
###        if("FLGACCEPTKELCRIT" %in% names(map_data) && (("KEL" %in% parameter_list && "KELNOPT" %in% parameter_list) || "KELRSQ" %in% parameter_list)) {
        if(disp_required[["FLGACCEPTKEL"]] && "FLGACCEPTKELCRIT" %in% names(map_data)) {
          if(length(unlist(strsplit(as.character(map_data$FLGACCEPTKELCRIT), ","))) > 0){
##            row_data <- c(row_data, 0)
            computation_df[i, "FLGACCEPTKEL"] <- 0
          } else {
##            row_data <- c(row_data, 0)
            computation_df[i, "FLGACCEPTKEL"] <- 0
          }
        }
###        if("THALF" %in% parameter_list) {
        if(disp_required[["THALF"]]) {
##          row_data <- c(row_data, kel_v[["THALF"]])
          computation_df[i, "THALF"] <- ifelse("THALF" %in% names(kel_v), kel_v[["THALF"]], NA)
        }
###        if("THALFF" %in% parameter_list) {
        if(disp_required[["THALFF"]]) {
##          row_data <- c(row_data, kel_v[["THALFF"]])
          computation_df[i, "THALFF"] <- ifelse("THALFF" %in% names(kel_v), kel_v[["THALFF"]], NA)
        }
###        if("LASTTIME" %in% parameter_list) {
        if(disp_required[["LASTTIME"]]) {
##          row_data <- c(row_data, last_time)
          computation_df[i, "LASTTIME"] <- last_time
        }
###        if("LASTTIMEi" %in% parameter_list) {
        if(disp_required[["LASTTIMEi"]]) {
##          row_data <- c(row_data, unlist(last_timei))
          computation_df[i, paste0("LASTTIME",1:di_col)] <- unlist(last_timei)
        }
        if(disp_required[["FLGACCEPTTAU"]] && "LASTTIMEACCEPTCRIT" %in% names(map_data)) {
          if(!is.na(last_crit_factor)){
            if(paste0("TAU",di_col) %in% names(map_data)){
              if(map_data[, paste0("TAU",di_col)] %in% names(data_data)) {
                tau_val <- unique(tmp_df[, map_data[, paste0("TAU",di_col)]])[1]
                if(!is.na(tau_val) && is.numeric(tau_val) && !is.na(last_crit_factor) && is.numeric(last_crit_factor)){
                  lt_accept_crit <- tau_val * last_crit_factor
##                  row_data <- c(row_data, ifelse(last_time >= lt_accept_crit, 1, 0))
                  computation_df[i, "FLGACCEPTTAU"] <- ifelse(last_time >= lt_accept_crit, 1, 0)
                } else {
##                  row_data <- c(row_data, 0)
                  computation_df[i, "FLGACCEPTTAU"] <- 0
                }
              } else {
##                row_data <- c(row_data, 0)
                computation_df[i, "FLGACCEPTTAU"] <- 0
              }
            } else {
##              row_data <- c(row_data, 0)
              computation_df[i, "FLGACCEPTTAU"] <- 0
            }
          } else {
##            row_data <- c(row_data, 0)
            computation_df[i, "FLGACCEPTTAU"] <- 0
          }
        }
###        if("AUCALL" %in% parameter_list && 'TMAX' %in% parameter_list) {
        if(disp_required[["AUCALL"]]) {
##          row_data <- c(row_data, aucall)
          computation_df[i, "AUCALL"] <- aucall
        }
###        if("AUCDN" %in% parameter_list && "AUCALL" %in% parameter_list) {
### 2019-10-10/TGT/ replace AUCDN with AUCALLDN
###        if(disp_required[["AUCDN"]]) {
###          row_data <- c(row_data, unlist(aucdn))
        if(disp_required[["AUCALLDN"]]) {
##          row_data <- c(row_data, unlist(aucalldn))
          computation_df[i, paste0("AUCALLDN",1:di_col)] <- unlist(aucalldn)
        }
###        if("AUCLAST" %in% parameter_list && 'TMAX' %in% parameter_list && 'TLAST' %in% parameter_list) {
        if(disp_required[["AUCLAST"]]) {
##          row_data <- c(row_data, auclast)
          computation_df[i, "AUCLAST"] <- auclast
        }
###        if("AUCLASTC" %in% parameter_list && "AUCLAST" %in% parameter_list && "TLAST" %in% parameter_list && "KEL" %in% parameter_list){
        if(disp_required[["AUCLASTC"]]){
##          row_data <- c(row_data, auclastc)
          computation_df[i, "AUCLASTC"] <- auclastc
        }
###        if("AUCLASTDN" %in% parameter_list && "AUCLAST" %in% parameter_list) {
        if(disp_required[["AUCLASTDN"]]) {
##          row_data <- c(row_data, auclastdn)
          computation_df[i, "AUCLASTDN"] <- auclastdn
        }
###        if("AUCLASTi" %in% parameter_list && 'TMAXi' %in% parameter_list && 'TLASTi' %in% parameter_list) {
        if(disp_required[["AUCLASTi"]]) {
##          row_data <- c(row_data, unlist(auclasti))
          computation_df[i, paste0("AUCLAST",1:di_col)] <- unlist(auclasti)
        }
###        if("AUCLASTCi" %in% parameter_list && "AUCLASTi" %in% parameter_list && "TLASTi" %in% parameter_list && "KEL" %in% parameter_list){
        if(disp_required[["AUCLASTCi"]]){
##          row_data <- c(row_data, unlist(auclasti_c))
          computation_df[i, paste0("AUCLASTC",1:di_col)] <- unlist(auclasti_c)
        }
###        if("AUCLASTDNi" %in% parameter_list && "AUCLASTi" %in% parameter_list) {
        if(disp_required[["AUCLASTDNi"]]) {
##          row_data <- c(row_data, unlist(auclasti_dn))
          computation_df[i, paste0("AUCLASTDN",1:di_col)] <- unlist(auclasti_dn)
        }
###        if("AUMCLASTi" %in% parameter_list && 'TMAXi' %in% parameter_list && 'TLASTi' %in% parameter_list) {
        if(disp_required[["AUMCLASTi"]]) {
##          row_data <- c(row_data, unlist(aumclasti))
          computation_df[i, paste0("AUMCLAST",1:di_col)] <- unlist(aumclasti)
        }
###        if("AUCT" %in% parameter_list && "TMAXi" %in% parameter_list) {
        if(disp_required[["AUCT"]] && auc_len > 1) {
##          row_data <- c(row_data, unlist(auct))
          computation_df[i, paste0("AUC",1:auc_len)] <- unlist(auct)
        }
###        if("AUCTDN" %in% parameter_list && "TMAXi" %in% parameter_list) {
        if(disp_required[["AUCTDN"]] && auc_len > 1) {
##          row_data <- c(row_data, unlist(auctdn))
          computation_df[i, paste0("AUC",1:auc_len,"DN")] <- unlist(auctdn)
        }
###        if("AUCT" %in% parameter_list || "AUCTDN" %in% parameter_list) {
        if((disp_required[["AUCT"]] || disp_required[["AUCTDN"]]) && auc_len > 1){
##          row_data <- c(row_data, unlist(auc_int))
          computation_df[i, paste0("AUCINT",1:auc_len)] <- unlist(auc_int)
        }
###        if("AUCT1_T2" %in% parameter_list && "TMAXi" %in% parameter_list && auc_pair_check) {
        if(disp_required[["AUCT1_T2"]] && auc_pair_check) {
##          row_data <- c(row_data, unlist(auct1_t2))
          computation_df[i, paste0("AUC", map_data[,rep(paste0("AUC.", 1:auc_par_len, ".T1"))], "_", map_data[,rep(paste0("AUC.", 1:auc_par_len, ".T2"))])] <- unlist(auct1_t2)
        }
###        if("AUCINFO" %in% parameter_list && "AUCLAST" %in% parameter_list && "CLAST" %in% parameter_list && "KEL" %in% parameter_list) {
        if(disp_required[["AUCINFO"]]) {
##          row_data <- c(row_data, aucinf_o)
          computation_df[i, "AUCINFO"] <- aucinf_o
        }
###        if("AUCINFOi" %in% parameter_list && "AUCLASTi" %in% parameter_list && "CLASTi" %in% parameter_list && "KEL" %in% parameter_list) {
        if(disp_required[["AUCINFOi"]]) {
##          row_data <- c(row_data, unlist(aucinfoi))
          computation_df[i, paste0("AUCINFO",1:di_col)] <- unlist(aucinfoi)
        }
###        if("AUCINFOC" %in% parameter_list && "KEL" %in% parameter_list && "AUCINFO" %in% parameter_list) {
        if(disp_required[["AUCINFOC"]]) {
##          row_data <- c(row_data, aucinf_oc)
          computation_df[i, "AUCINFOC"] <- aucinf_oc
        }
        if(disp_required[["AUCINFOCi"]]){
          computation_df[i, paste0("AUCINFOC",1:di_col)] <- unlist(aucinfoi_c)
        }
###        if("AUCINFODN" %in% parameter_list && "AUCINFO" %in% parameter_list) {
        if(disp_required[["AUCINFODN"]]) {
##          row_data <- c(row_data, aucinf_odn)
          computation_df[i, "AUCINFODN"] <- aucinf_odn
        }
        if(disp_required[["AUCINFODNi"]]){
          computation_df[i, paste0("AUCINFODN",1:di_col)] <- unlist(aucinfoi_dn)
        }
###        if("CEST" %in% parameter_list && "TLAST" %in% parameter_list && "KEL" %in% parameter_list) {
        if(disp_required[["CEST"]]) {
##          row_data <- c(row_data, c_est)
          computation_df[i, "CEST"] <- c_est
        }
###        if("AUCINFP" %in% parameter_list && "CEST" %in% parameter_list && "AUCLAST" %in% parameter_list && "TLAST" %in% parameter_list && "KEL" %in% parameter_list) {
        if(disp_required[["AUCINFP"]]) {
##          row_data <- c(row_data, aucinf_p)
          computation_df[i, "AUCINFP"] <- aucinf_p
        }
###        if("AUCINFPi" %in% parameter_list && "CEST" %in% parameter_list && "AUCLASTi" %in% parameter_list && "TLASTi" %in% parameter_list && "KEL" %in% parameter_list) {
        if(disp_required[["AUCINFPi"]]) {
##          row_data <- c(row_data, unlist(aucinfpi))
          computation_df[i, paste0("AUCINFP",1:di_col)] <- unlist(aucinfpi) 
        }
###        if("AUCINFPC" %in% parameter_list && "KEL" %in% parameter_list && "AUCINFP" %in% parameter_list) {
        if(disp_required[["AUCINFPC"]]) {
##          row_data <- c(row_data, aucinf_pc)
          computation_df[i, "AUCINFPC"] <- aucinf_pc
        }
        if(disp_required[["AUCINFPCi"]]){
          computation_df[i, paste0("AUCINFPC",1:di_col)] <- unlist(aucinfpi_c)
        }
###        if("AUCINFPDN" %in% parameter_list && "AUCINFP" %in% parameter_list) {
        if(disp_required[["AUCINFPDN"]]) {
##          row_data <- c(row_data, aucinf_pdn)
          computation_df[i, "AUCINFPDN"] <- aucinf_pdn
        }
        if(disp_required[["AUCINFPDNi"]]){
          computation_df[i, paste0("AUCINFPDN",1:di_col)] <- unlist(aucinfpi_dn)
        }
###        if("AUMCINFOi" %in% parameter_list && "AUMCLASTi" %in% parameter_list && "CLASTi" %in% parameter_list && "KEL" %in% parameter_list) {
        if(disp_required[["AUMCINFOi"]]) {
##          row_data <- c(row_data, unlist(aumcinfoi))
          computation_df[i, paste0("AUMCINFO",1:di_col)] <- unlist(aumcinfoi)
        }
###        if("AUMCINFPi" %in% parameter_list && "CEST" %in% parameter_list && "AUMCLASTi" %in% parameter_list && "TLASTi" %in% parameter_list && "KEL" %in% parameter_list) {
        if(disp_required[["AUMCINFPi"]]) {
##          row_data <- c(row_data, unlist(aumcinfpi))
          computation_df[i, paste0("AUMCINFP",1:di_col)] <- unlist(aumcinfpi)
        }
###        if("AUCTAUi" %in% parameter_list) {
        if(disp_required[["AUCTAUi"]]) {
##          row_data <- c(row_data, unlist(auctau))
          computation_df[i, paste0("AUCTAU",1:di_col)] <- unlist(auctau)
        }
###        if("AUCTAUDNi" %in% parameter_list && "AUCTAUi" %in% parameter_list) {
        if(disp_required[["AUCTAUDNi"]]) {
##          row_data <- c(row_data, unlist(auctaudn))
          computation_df[i, paste0("AUCTAUDN",1:di_col)] <- unlist(auctaudn)
        }
###        if("AUMCTAUi" %in% parameter_list && "TMAXi" %in% parameter_list) {
        if(disp_required[["AUMCTAUi"]]) {
##          row_data <- c(row_data, unlist(aumctaui))
          computation_df[i, paste0("AUMCTAU",1:di_col)] <- unlist(aumctaui)
        }
###        if("MRTLAST" %in% parameter_list && "AUCLAST" %in% parameter_list && "AUMCLAST" %in% parameter_list) {
        if(disp_required[["MRTLAST"]]) {
##          row_data <- c(row_data, mrtlast)
          computation_df[i, "MRTLAST"] <- mrtlast
        }
###        if("MRTLASTi" %in% parameter_list && "AUCLASTi" %in% parameter_list && "AUMCLASTi" %in% parameter_list) {
        if(disp_required[["MRTLASTi"]]) {
##          row_data <- c(row_data, unlist(mrtlasti))
          computation_df[i, paste0("MRTLAST",1:di_col)] <- unlist(mrtlasti)
        }
###        if("MRTIVIFOi" %in% parameter_list && "AUCINFOi" %in% parameter_list && "AUCTAUi" %in% parameter_list && "AUMCTAUi" %in% parameter_list){
        if(disp_required[["MRTIVIFOi"]]){
##          row_data <- c(row_data, unlist(mrtivifoi))
          computation_df[i, paste0("MRTIVIFO",1:di_col)] <- unlist(mrtivifoi)
        }
###        if("MRTIVIFPi" %in% parameter_list && "AUCINFPi" %in% parameter_list && "AUCTAUi" %in% parameter_list && "AUMCTAUi" %in% parameter_list){
        if(disp_required[["MRTIVIFPi"]]){
##          row_data <- c(row_data, unlist(mrtivifpi))
          computation_df[i, paste0("MRTIVIFP",1:di_col)] <- unlist(mrtivifpi)
        }
###        if("AUCXPCTO" %in% parameter_list && "AUCINFO" %in% parameter_list && "AUCLAST" %in% parameter_list){
        if(disp_required[["AUCXPCTO"]]){
##          row_data <- c(row_data, aucxpcto)
          computation_df[i, "AUCXPCTO"] <- aucxpcto
        }
###        if("AUCXPCTOi" %in% parameter_list && "AUCINFOi" %in% parameter_list && "AUCLASTi" %in% parameter_list){
        if(disp_required[["AUCXPCTOi"]]){
##          row_data <- c(row_data, unlist(aucxpctoi))
          computation_df[i, paste0("AUCXPCTO",1:di_col)] <- unlist(aucxpctoi)
        }
###        if("AUCXPCTP" %in% parameter_list && "AUCINFP" %in% parameter_list && "AUCLAST" %in% parameter_list){
        if(disp_required[["AUCXPCTP"]]){
##          row_data <- c(row_data, aucxpctp)
          computation_df[i, "AUCXPCTP"] <- aucxpctp
        }
###        if("AUCXPCTPi" %in% parameter_list && "AUCINFPi" %in% parameter_list && "AUCLASTi" %in% parameter_list){
        if(disp_required[["AUCXPCTPi"]]){
##          row_data <- c(row_data, unlist(aucxpctpi))
          computation_df[i, paste0("AUCXPCTP",1:di_col)] <- unlist(aucxpctpi)
        }
###        if("AUMCXPTO" %in% parameter_list && "AUMCINFO" %in% parameter_list && "AUMCLAST" %in% parameter_list){
        if(disp_required[["AUMCXPTO"]]){
##          row_data <- c(row_data, aumcxpto)
          computation_df[i, "AUMCXPTO"] <- aumcxpto
        }
###        if("AUMCXPTOi" %in% parameter_list && "AUMCINFOi" %in% parameter_list && "AUMCLAST" %in% parameter_list){
        if(disp_required[["AUMCXPTOi"]]){
##          row_data <- c(row_data, unlist(aumcxptoi))
          computation_df[i, paste0("AUMCXPTO",1:di_col)] <- unlist(aumcxptoi)
        }
###        if("AUMCXPTP" %in% parameter_list && "AUMCINFP" %in% parameter_list && "AUMCLAST" %in% parameter_list){
        if(disp_required[["AUMCXPTP"]]){
##          row_data <- c(row_data, aumcxptp)
          computation_df[i, "AUMCXPTP"] <- aumcxptp
        }
###        if("AUMCXPTPi" %in% parameter_list && "AUMCINFPi" %in% parameter_list && "AUMCLAST" %in% parameter_list){
        if(disp_required[["AUMCXPTPi"]]){
##          row_data <- c(row_data, unlist(aumcxptpi))
          computation_df[i, paste0("AUMCXPTP",1:di_col)] <- unlist(aumcxptpi)
        }
###        if("CAVi" %in% parameter_list && "AUCTAUi" %in% parameter_list) {
        if(disp_required[["CAVi"]]) {
##          row_data <- c(row_data, unlist(ca_v))
          computation_df[i, paste0("CAV",1:di_col)] <- unlist(ca_v)
        }
###        if("CLTAUi" %in% parameter_list && "AUCTAUi" %in% parameter_list) {
        if(disp_required[["CLTAUi"]]) {
##          row_data <- c(row_data, unlist(cl_tau))
          computation_df[i, paste0("CLTAU",1:di_col)] <- unlist(cl_tau)
        }
###        if("CLTAUWi" %in% parameter_list && "CLTAUi" %in% parameter_list && "AUCTAUi" %in% parameter_list) {
        if(disp_required[["CLTAUWi"]]) {
##          row_data <- c(row_data, unlist(cl_tauw))
          computation_df[i, paste0("CLTAUW",1:di_col)] <- unlist(cl_tauw)
        }
###        if("PTFi" %in% parameter_list && "CMAXi" %in% parameter_list && "CMINi" %in% parameter_list && "CAVi" %in% parameter_list && "AUCTAUi" %in% parameter_list) {
        if(disp_required[["PTFi"]]) {
##          row_data <- c(row_data, unlist(pt_f))
          computation_df[i, paste0("PTF",1:di_col)] <- unlist(pt_f)
        }
###        if("PTRi" %in% parameter_list && "CMAXi" %in% parameter_list && "CMINi" %in% parameter_list) {
        if(disp_required[["PTRi"]]) {
##          row_data <- c(row_data, unlist(pt_r))
          computation_df[i, paste0("PTR",1:di_col)] <- unlist(pt_r)
        }
        if(disp_required[["VSSOi"]]) {
##          row_data <- c(row_data, unlist(vsso))
          computation_df[i, paste0("VSSO",1:di_col)] <- unlist(vsso)
        }
        if(disp_required[["VSSPi"]]) {
##          row_data <- c(row_data, unlist(vssp))
          computation_df[i, paste0("VSSP",1:di_col)] <- unlist(vssp)
        }
        if(disp_required[["VSSOWi"]]) {
##          row_data <- c(row_data, unlist(vssow))
          computation_df[i, paste0("VSSOW",1:di_col)] <- unlist(vssow)
        }
        if(disp_required[["VSSPWi"]]) {
##          row_data <- c(row_data, unlist(vsspw))
          computation_df[i, paste0("VSSPW",1:di_col)] <- unlist(vsspw)
        }
        if(disp_required[["VZTAUi"]]) {
##          row_data <- c(row_data, unlist(vz_tau))
          computation_df[i, paste0("VZTAU",1:di_col)] <- unlist(vz_tau)
        }
        if(disp_required[["VZTAUWi"]]) {
##          row_data <- c(row_data, unlist(vz_tauw))
          computation_df[i, paste0("VZTAUW",1:di_col)] <- unlist(vz_tauw)
        }
##        row_data <- c(row_data,
##                      c(tmp_df[,map_data$CONC], rep(NA, ((auc_len+1) - length(tmp_df[,map_data$CONC])))),
##                      c(tmp_df[,map_data$TIME], rep(NA, ((auc_len+1) - length(tmp_df[,map_data$TIME]))))
##        )
        computation_df[i, paste0("CONC",1:(auc_len+1))] <- c(tmp_df[,map_data$CONC], rep(NA, ((auc_len+1) - length(tmp_df[,map_data$CONC]))))
        computation_df[i, paste0("CONCTIME",1:(auc_len+1))] <- c(tmp_df[,map_data$TIME], rep(NA, ((auc_len+1) - length(tmp_df[,map_data$TIME]))))
###        if("DIi" %in% parameter_list) {
        if(disp_required[["DIi"]]) {
##          row_data <- c(row_data, unlist(di))
          computation_df[i, paste0("DI",1:di_col)] <- unlist(di)
        }
###        if("TAUi" %in% parameter_list) {
        if(disp_required[["TAUi"]]) {
##          row_data <- c(row_data, unlist(tau))
          computation_df[i, paste0("TAU",1:di_col)] <- unlist(tau)
        }
###        if("TOLD" %in% parameter_list) {
        if(disp_required[["TOLD"]]) {
##          row_data <- c(row_data, unlist(told))
          computation_df[i, paste0("TOLD",1:di_col)] <- unlist(told)
        }
### 2019-10-20/TGT/ Reposition        
###        if("DOSEi" %in% parameter_list) {
###        if(disp_required[["DOSEi"]]) {
###          row_data <- c(row_data, unlist(dose))
###        }

##        computation_df[i,] <- row_data
      } else {
        if(isTRUE(optimize_kel)){
          kel_flag_optimized <- c(kel_flag_optimized, kel_flag)
        }
##        computation_df[i,] <- c(unique(data_data[,map_data$SDEID])[i], rep(NA, length(names(computation_df))-1))
        computation_df[i, "SDEID"] <- unique(data_data[,map_data$SDEID])[i]
      }
    }, error = function(e) {
      stop(paste0(e, "For SDEID ", unique(data_data[,map_data$SDEID])[i]))
    })
  }

##  2019-11-13/RD/ Added to account for incorrect handling of FLGACCEPTKELCRIT
## 
  if(disp_required[["FLGACCEPTKEL"]] && "FLGACCEPTKELCRIT" %in% names(map_data)) {
    if(length(unlist(strsplit(as.character(map_data$FLGACCEPTKELCRIT), ","))) > 0){
      if(all(as.character(flag_df$VAR) %in% names(computation_df))){
        for(f in 1:length(flag_df$VAR)){
          computation_df[,flag_df$VAR[f]] <- as.numeric(computation_df[,flag_df$VAR[f]])
        }
        if(nrow(computation_df[eval(parse(text=flag_subset)),]) > 0){
          computation_df[eval(parse(text=flag_subset)),][,"FLGACCEPTKEL"] <- 1
        }
        for(f in 1:length(flag_df$VAR)){
          computation_df[,flag_df$VAR[f]] <- as.character(computation_df[,flag_df$VAR[f]])
        }
      } else {
        warning(paste0("Flag 'FLGACCEPTKELCRIT' values provided via 'map' does not have a parameter name that is generated as an output '", as.character(flag_df$VAR)[as.character(flag_df$VAR) %in% names(computation_df)], "'"))
      }
    }
  }
  if(disp_required[["FLGACCEPTTAU"]] && "LASTTIMEACCEPTCRIT" %in% names(map_data)) {
    if(nrow(computation_df[!is.na(computation_df[,"FLGACCEPTKEL"]) & computation_df[,"FLGACCEPTKEL"] != 1,]) > 0){
      computation_df[!is.na(computation_df[,"FLGACCEPTKEL"]) & computation_df[,"FLGACCEPTKEL"] != 1,][,"FLGACCEPTTAU"] <- 0  
    }
  }
  if(disp_required[["FLGACCEPTTMAX"]] && "FLGEMESIS" %in% names(map_data) && map_data$FLGEMESIS %in% names(data_data)){
    for(f in 1:length(unique(computation_df[,map_data$SDEID]))){
      tmp_df <- data_data[data_data[,map_data$SDEID] == unique(computation_df[,map_data$SDEID])[f],]
      emesis_flag_check <- ifelse(any(as.logical(as.numeric(tmp_df[,map_data$FLGEMESIS]))), TRUE, FALSE)
      tmp_comp_df <- computation_df[computation_df[,map_data$SDEID] == unique(computation_df[,map_data$SDEID])[f],]
      for(e in 1:di_col){
        if(all(c(paste0("DOSE", e), paste0("TMAX", e)) %in% names(computation_df))){
          test_df_3 <- computation_df[computation_df[,paste0("DOSE", e)] == tmp_comp_df[,paste0("DOSE", e)],]
          tmp_median <- median(as.numeric(test_df_3[,paste0("TMAX", e)]), na.rm = TRUE) 
        } else {
          tmp_median <- NULL
        }
        tmp_tmax <- as.numeric(tmp_comp_df[,paste0("TMAX", e)])
        if(!is.null(tmp_median) && !is.na(tmp_median) && !is.null(tmp_tmax) && !is.na(tmp_tmax)){
          if(computation_df[computation_df[,map_data$SDEID] == unique(computation_df[,map_data$SDEID])[f],"FLGACCEPTTMAX"] != 0){
            computation_df[computation_df[,map_data$SDEID] == unique(computation_df[,map_data$SDEID])[f],"FLGACCEPTTMAX"] <- ifelse((isTRUE(emesis_flag_check) && (tmp_tmax < (2 * tmp_median))), 1, ifelse(!isTRUE(emesis_flag_check), 1 , 0))  
          }
        }
      }
    }
  }

  for(n in 1:length(regular_int_type)){
    tmp_int_type <- computation_df[,names(computation_df) == as.character(regular_int_type[n])]
    if(!is.null(ncol(tmp_int_type))){
      for(r in 1:length(tmp_int_type)){
        suppressWarnings(computation_df[,names(computation_df) == as.character(regular_int_type[n])][,r] <- as.numeric(computation_df[,names(computation_df) == as.character(regular_int_type[n])][,r]))
      }
    } else {
      suppressWarnings(computation_df[,names(computation_df) == as.character(regular_int_type[n])] <- as.numeric(computation_df[,names(computation_df) == as.character(regular_int_type[n])]))
    }
  }

  computation_df <- unit_conversion(data = data_data, map = map_data, result = computation_df, unit_class = "ALL")

  if(is.list(return_list) && !is.null(return_list) && length(return_list) > 0){
    if(!map_data$SDEID %in% return_list && length(return_list) > 1){
      return_list <- return_list[[length(return_list)+1]] <- map_data$SDEID
    }
### 2019-08-01/TGT/ Ensure that only returning a single row of data/profile regardless return_list.
###                 The approach below should change so that the names are reconciled after the merge
###    return_df <- unique(data_data[names(data_data) %in% return_list])
    k <- names(data_data) %in% return_list
    return_df <- data_data[!duplicated(data_data$SDEID),unlist(k)]

### remove columns that are empty, note that this may remove columns incorporated into the SDEID/Profile computation
### but if they are empty, it should not impact the result of the computation if recomputed even if the SDEIDs themselves
### have different values
###    j <- lapply(return_df, FUN=function(x) { all(is.na(x)) } )
###    return_df <- return_df[,-match(names(j[j==TRUE]), names(return_df))]

### 2019-09-19/TGT/ Except for SDEID, remove overlapping elements from return_list
### retaining those in parameter dataset
    ck <- intersect(names(return_df), names(computation_df))
    if(length(ck)>1) {
      ck <- setdiff(ck, map_data$SDEID)
      return_df <- return_df[,-match(ck, names(return_df))]
    }

    merged_computation <- merge(x = computation_df, y = return_df, by.x = map_data$SDEID, by.y = map_data$SDEID)
    colnames(merged_computation) <- gsub('.x','.dataset',names(merged_computation))
    colnames(merged_computation) <- gsub('.y','',names(merged_computation))
    computation_df <- merged_computation
  }

### 2019-09-17/TGT/ Always return est_data and full results_list
  results_list <- list()
  results_list$data_out <- computation_df
  results_list$est_data <- est_data
  
  if(isTRUE(optimize_kel) && comp_required[["TMAXi"]] && comp_required[["TLASTi"]] && comp_required[["CMAXi"]] && comp_required[["CLASTi"]] && comp_required[["AUCLASTi"]] &&
     "FLGACCEPTKELCRIT" %in% names(map_data) && "FLGEXKEL" %in% names(map_data) && map_data$FLGEXKEL %in% names(data_data)){
###    if("KEL" %in% parameter_list){
###      results_list <- list()
###      results_list$data_out <- computation_df
      results_list$optimized_kel_flag <- kel_flag_optimized
###      results_list$est_data <- est_data
###    } else {
###      results_list <- list()
###      results_list$data_out <- computation_df
###      results_list$optimized_kel_flag <- kel_flag_optimized
###    }
###    return(results_list)
###  } else {
###    if("KEL" %in% parameter_list){
###      results_list <- list()
###      results_list$data_out <- computation_df
###      results_list$est_data <- est_data
###      return(results_list)
###    } else {
###      return(computation_df)
###    }
  }
  if(isTRUE(optimize_kel_debug)){
    if(isTRUE(optimize_kel)){
      kel_debug$KEL <- as.numeric(kel_debug$KEL)
      kel_debug$KELNOPT <- as.numeric(kel_debug$KELNOPT)
      kel_debug$KELRSQ <- as.numeric(kel_debug$KELRSQ)
      kel_debug$KELNOPT_W <- as.numeric(kel_debug$KELNOPT_W)
      kel_debug$KELRSQ_W <- as.numeric(kel_debug$KELRSQ_W)
      kel_debug$TOTAL_W <- as.numeric(kel_debug$TOTAL_W)
      if("AUCXPCTO" %in% flag_df$VAR){
        kel_debug$AUCXPCTO <- as.numeric(kel_debug$AUCXPCTO)
        kel_debug$AUCXPCTO_W <- as.numeric(kel_debug$AUCXPCTO_W)
      } else if("AUCXPCTP" %in% flag_df$VAR){
        kel_debug$AUCXPCTP <- as.numeric(kel_debug$AUCXPCTP)
        kel_debug$AUCXPCTP_W <- as.numeric(kel_debug$AUCXPCTP_W)
      }
      
      results_list$optimize_kel_data <- kel_debug
    } else {
      results_list$optimize_kel_data <- NULL
    }
  }
  
  return(results_list)
}
