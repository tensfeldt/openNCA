#' Run M3 SD Computation
#'
#' This function will compute all the relevant parameters for a M3 model Single Dose (SD).\cr
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
#' You can specify the options to subset the list of parameters that are returned, please refer to \code{\link{mct_template}} for more information. \cr
#' The following are the functions that this function uses to generate parameters: \cr
#' \strong{Return List options} \cr
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
#'  \item \strong{clo}: Refer to \code{\link{clo}} for more details
#'  \item \strong{clow}: Refer to \code{\link{clow}} for more details
#'  \item \strong{clp}: Refer to \code{\link{clp}} for more details
#'  \item \strong{clpw}: Refer to \code{\link{clpw}} for more details
#'  \item \strong{vzo}: Refer to \code{\link{vzfo}} for more details
#'  \item \strong{vzp}: Refer to \code{\link{vzfp}} for more details
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
#' Please note that this function does not contain all the features of a M3 SD computation, so it is recommended that you
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
#'  \item M3SD_Parameters: Caculated default/specified parameters
#' }
#' OR \cr
#' \strong{List} \cr
#' \itemize{
#'  \item data_out: Calculated default/specified M3SD Parameters
#'  \item optimized_kel_flag: Optimized KEL flag data used to calulate KEL based parameters
#'  \item est_data: Calculated Estimated Parameters
#' }
#'
#' @examples
#'
#' @author
#' \itemize{
#'  \item \strong{Thomas Tensfeldt, Pfizer & Rudraya Technical Team}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{thomas.g.tensfeldt@pfizer.com}
#' }
### 2019-10-10/TGT/ Remove display_list argument and incorporate model_regex argument
###run_M3_SD_computation <- function(data = NULL, map = NULL, method = 1, parameter_list = list(), display_list = list(), return_list = list()){
run_M3_SD_computation <- function(data = NULL, map = NULL, method = 1, model_regex = "^M3(SD)*?$", parameter_list = list(), return_list = list()){
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

###  model_regex <- "^M3(SD)*?$"

  auc_list <- c("AUCT")
  auc_par <- c("AUCT1_T2")
### 2019-08-30/TGT/ Remove "CMAXC"/"CMAXCi" since only valid for M2 Bolus administration applications
###  regular_list <- c("CMAX", "CLAST", "CMAXC", "CMAXDN", "TMAX", "TLAST", "KEL", "CEST",
  regular_list <- c("CMAX", "CLAST", "CMAXDN", "TMAX", "TLAST", "KEL", "CEST",
                    "KELTMLO", "KELTMHI", "KELNOPT", "KELRSQ", "KELRSQA", "THALF", "THALFF", "LASTTIME", "AUCALL", "AUCDN", "AUCLAST",
                    "AUCLASTC", "AUCLASTDN", "AUMCLAST", "AUCINFO", "AUCINFP", "AUCINFOC", "AUCINFPC", "AUCINFODN",
                    "AUCINFPDN", "AUMCINFO", "AUMCINFP", "MRTLAST", "MRTIVIFO", "MRTIVIFP", "AUCXPCTO", "AUCXPCTP",
                    "AUMCXPTO", "AUMCXPTP", "CLO", "CLOW", "CLP", "CLPW", "VZO", "VZOW", "VZP", "VZPW")
  optional_list <- c("DOSEi", "TAUi", "TOLDi")
  opt_list <- c("DOSE1", "TAU1", "TOLD1")
  regular_int_type <- NULL
  auc_pair_check <- FALSE
  #auc_col <- length(unique(data_data[,map_data[[map_data$TIME]]]))-1
  #col <- 43 + 2*auc_col + 1

### 2019-08-08/TGT/ auc_len is intended to represent the maximum # of AUC intervals that could be generated from the # of
###                 unique time points available for any of the profiles in the dataset. This assumes that data removed
###                 (FLGEXSDE for example) from the dataset do not contribute to these time-intervals.
  index1 <- data_data[,map_data$SDEID]
  auc_len <- max(tapply(index1, index1, length))-1
  reg_col <- sum(regular_list %in% parameter_list) + ifelse(any(c("KELRSQ","KELRSQA") %in% parameter_list), 1, 0)
  auc_col <- ifelse(sum(auc_list %in% parameter_list) == 1, 2, 0)
### 2019-08-29/TGT/ Validate # of Partial AUCs not from AUCNPAIR (remove from Computation Engine Specification)
###                  but rather directly from the entries of the AUC.#.T1,AUC.#.T2 values themselves. 
###    auc_par_len <- ifelse(auc_par %in% parameter_list && 'AUCNPAIR' %in% names(map_data), ifelse(!(is.null(map_data$AUCNPAIR) || is.na(suppressWarnings(as.numeric(map_data$AUCNPAIR)))), suppressWarnings(as.numeric(map_data$AUCNPAIR)), 0), 0)
###
  aucpari <- grep('^AUC.([0-9]+?).T[1-2]$', names(map_data), ignore.case=TRUE, perl=TRUE)
  if(length(aucpari)>0) {
      auc_par_len <- length(aucpari)/2
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

  opt_sel <- opt_list[optional_list %in% parameter_list]
  opt_col <- ifelse(length(opt_sel) > 0, ifelse(sum(opt_sel %in% names(map_data)) > 0 && any(map_data[, opt_sel[opt_sel %in% names(map_data)]] %in% names(data_data)), sum(map_data[, opt_sel[opt_sel %in% names(map_data)]] %in% names(data_data)), 0), 0)
  col <- reg_col + (auc_col * auc_len) + 1 + (2 * (auc_len+1)) + opt_col

  ### Determine DOSEs in dosevar, a vector of dose names pointing into map_data
  doselist <- names(parameter_indices("^DOSELIST$", names(map_data), simplify=FALSE))
  dosevar <- unlist(strsplit(map_data[,doselist], ";"))
  ### assuming here there is a single dose
  dosevar <- map[,dosevar]

### 2019-09-09/TGT/ identify DOSE/DOSE1 from map
###  dosevar <- names(parameter_indices("^DOSE(i{1}|[0-9]*?)$", names(map_data), simplify=FALSE))
### as per run_computation.R must assume DOSE1 at moment
###  dosevar <- names(parameter_indices("^DOSE1$", names(map_data), simplify=FALSE))
###  opt_list[1] <- map_data[,dosevar]

### 2019-09-16/TGT/ Precompute list of required parameters for col_names, parameter function evaluation and row_data generation  
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
  if("LASTTIMEACCEPTCRIT" %in% names(map_data) && ("LASTTIME" %in% parameter_list)){
    col <- col + 1
  }
  if("FLGEMESIS" %in% names(map_data) && ("TMAX" %in% parameter_list)){
    col <- col + 1
  }
  if("FLGACCEPTPREDOSECRIT" %in% names(map_data) && ("CMAX" %in% parameter_list)){
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
  
  if(disp_required[["DOSE"]] || disp_required[["DOSEi"]]) {
    col_names <- c(col_names, dosevar)
    regular_int_type <- c(regular_int_type, dosevar)
  }
  if(disp_required[["DOSEC"]]) {
    col_names <- c(col_names, "DOSEC")
    regular_int_type <- c(regular_int_type, "DOSEC")
  }
  if(disp_required[["DOFi"]]){
    col_names <- c(col_names, "DOF")
    regular_int_type <- c(regular_int_type, "DOF")
  }
  if(disp_required[["CENDINF"]]){
    col_names <- c(col_names, "CENDINF")
    regular_int_type <- c(regular_int_type, "CENDINF")
  }
  if(disp_required[["CENDINFDN"]]){
    col_names <- c(col_names, "CENDINFDN")
    regular_int_type <- c(regular_int_type, "CENDINFDN")
  }
  if(disp_required[["TENDINF"]]){
    col_names <- c(col_names, "TENDINF")
    regular_int_type <- c(regular_int_type, "TENDINF")
  }
###  if("CMAX" %in% parameter_list) {
  if(disp_required[["CMAX"]]) {
    col_names <- c(col_names, "CMAX")
    regular_int_type <- c(regular_int_type, "CMAX")
  }
  if("FLGACCEPTPREDOSECRIT" %in% names(map_data) && ("CMAX" %in% parameter_list)){
    col_names <- c(col_names, "FLGACCEPTPREDOSE")
  }
###  if("CLAST" %in% parameter_list) {
  if(disp_required[["CLAST"]]) {
    col_names <- c(col_names, "CLAST")
    regular_int_type <- c(regular_int_type, "CLAST")
  }
### 2019-08-30/TGT/ Remove "CMAXC"/"CMAXCi" since only valid for M2 Bolus administration applications
###  if("CMAXC" %in% parameter_list && "CMAX" %in% parameter_list && "KEL" %in% parameter_list) {
###    col_names <- c(col_names, "CMAXC")
###    regular_int_type <- c(regular_int_type, "CMAXC")
###  }
### 2019-08-15/TGT/ missing configuration for CEST    
###  if(parameter_required("^CEST$",parameter_list)) {
###  if(parameter_required("^CEST$",parameter_list)) {
  if(disp_required[["CEST"]]) {
    col_names <- c(col_names, "CEST")
    regular_int_type <- c(regular_int_type, "CEST")
  }
###  if("CMAXDN" %in% parameter_list && "CMAX" %in% parameter_list) {
  if(disp_required[["CMAXDN"]]) {
    col_names <- c(col_names, "CMAXDN")
    regular_int_type <- c(regular_int_type, "CMAXDN")
  }
###  if("TMAX" %in% parameter_list) {
  if(disp_required[["TMAX"]]) {
    col_names <- c(col_names, "TMAX")
    regular_int_type <- c(regular_int_type, "TMAX")
  }
  if("FLGEMESIS" %in% names(map_data) && ("TMAX" %in% parameter_list)){
    col_names <- c(col_names, "FLGACCEPTTMAX")
  }
###  if("TLAST" %in% parameter_list) {
  if(disp_required[["TLAST"]]) {
    col_names <- c(col_names, "TLAST")
    regular_int_type <- c(regular_int_type, "TLAST")
  }
###  if("KEL" %in% parameter_list) {
  if(disp_required[["KEL"]]) {
    col_names <- c(col_names, "KEL")
    regular_int_type <- c(regular_int_type, "KEL")
  }
### 2019-09-18/TGT/
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
  if("FLGACCEPTKELCRIT" %in% names(map_data) && (("KEL" %in% parameter_list && "KELNOPT" %in% parameter_list) || "KELRSQ" %in% parameter_list)) {
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
  if("LASTTIMEACCEPTCRIT" %in% names(map_data) && ("LASTTIME" %in% parameter_list)) {
    col_names <- c(col_names, "FLGACCEPTTAU")
  }
###  if("AUCALL" %in% parameter_list) {
  if(disp_required[["AUCALL"]]) {
    col_names <- c(col_names, "AUCALL")
    regular_int_type <- c(regular_int_type, "AUCALL")
  }
###  if("AUCDN" %in% parameter_list) {
### 2019-10-10/TGT/ replace AUCDN with AUCALLDN
###  if(disp_required[["AUCDN"]]) {
###    col_names <- c(col_names, "AUCDN")
###    regular_int_type <- c(regular_int_type, "AUCDN")
  if(disp_required[["AUCALLDN"]]) {
    col_names <- c(col_names, "AUCALLDN")
    regular_int_type <- c(regular_int_type, "AUCALLDN")
  }
###  if("AUCLAST" %in% parameter_list) {
  if(disp_required[["AUCLAST"]]) {
    col_names <- c(col_names, "AUCLAST")
    regular_int_type <- c(regular_int_type, "AUCLAST")
  }
###  if("AUCLASTC" %in% parameter_list) {
  if(disp_required[["AUCLASTC"]]) {
    col_names <- c(col_names, "AUCLASTC")
    regular_int_type <- c(regular_int_type, "AUCLASTC")
  }
###  if("AUCLASTDN" %in% parameter_list) {
  if(disp_required[["AUCLASTDN"]]) {
    col_names <- c(col_names, "AUCLASTDN")
    regular_int_type <- c(regular_int_type, "AUCLASTDN")
  }
###  if("AUMCLAST" %in% parameter_list) {
  if(disp_required[["AUMCLAST"]]) {
    col_names <- c(col_names, "AUMCLAST")
    regular_int_type <- c(regular_int_type, "AUMCLAST")
  }
###  if("AUCT" %in% parameter_list && "TMAX" %in% parameter_list) {
  if(disp_required[["AUCT"]]) {
    col_names <- c(col_names, rep(paste0("AUC",1:auc_len)))
    regular_int_type <- c(regular_int_type, paste0("AUC",1:auc_len))
  }
  if(disp_required[["AUCTDN"]]){
    col_names <- c(col_names, rep(paste0("AUC",1:auc_len,"DN")))
    regular_int_type <- c(regular_int_type, paste0("AUC",1:auc_len,"DN"))
  }
  if(disp_required[["AUCT"]] || disp_required[["AUCTDN"]]){
    col_names <- c(col_names, rep(paste0("AUCINT",1:auc_len)))
  }
###  if("AUCT1_T2" %in% parameter_list && "TMAX" %in% parameter_list && auc_par_len > 0) {
  if(disp_required[["AUCT1_T2"]] && auc_pair_check) {
    col_names <- c(col_names, rep(paste0("AUC", map_data[,rep(paste0("AUC.", 1:auc_par_len, ".T1"))], "_", map_data[,rep(paste0("AUC.", 1:auc_par_len, ".T2"))])))
    regular_int_type <- c(regular_int_type, rep(paste0("AUC", map_data[,rep(paste0("AUC.", 1:auc_par_len, ".T1"))], "_", map_data[,rep(paste0("AUC.", 1:auc_par_len, ".T2"))])))
  }
###  if("AUCINFO" %in% parameter_list) {
  if(disp_required[["AUCINFO"]]) {
    col_names <- c(col_names, "AUCINFO")
    regular_int_type <- c(regular_int_type, "AUCINFO")
  }
###  if("AUCINFP" %in% parameter_list) {
  if(disp_required[["AUCINFP"]]) {
    col_names <- c(col_names, "AUCINFP")
    regular_int_type <- c(regular_int_type, "AUCINFP")
  }
###  if("AUCINFOC" %in% parameter_list) {
  if(disp_required[["AUCINFOC"]]) {
    col_names <- c(col_names, "AUCINFOC")
    regular_int_type <- c(regular_int_type, "AUCINFOC")
  }
###  if("AUCINFPC" %in% parameter_list) {
  if(disp_required[["AUCINFPC"]]) {
    col_names <- c(col_names, "AUCINFPC")
    regular_int_type <- c(regular_int_type, "AUCINFPC")
  }
###  if("AUCINFODN" %in% parameter_list) {
  if(disp_required[["AUCINFODN"]]) {
    col_names <- c(col_names, "AUCINFODN")
    regular_int_type <- c(regular_int_type, "AUCINFODN")
  }
###  if("AUCINFPDN" %in% parameter_list) {
  if(disp_required[["AUCINFPDN"]]) {
    col_names <- c(col_names, "AUCINFPDN")
    regular_int_type <- c(regular_int_type, "AUCINFPDN")
  }
###  if("AUMCINFO" %in% parameter_list) {
  if(disp_required[["AUMCINFO"]]) {
    col_names <- c(col_names, "AUMCINFO")
    regular_int_type <- c(regular_int_type, "AUMCINFO")
  }
###  if("AUMCINFP" %in% parameter_list) {
  if(disp_required[["AUMCINFP"]]) {
    col_names <- c(col_names, "AUMCINFP")
    regular_int_type <- c(regular_int_type, "AUMCINFP")
  }
###  if("MRTLAST" %in% parameter_list){
  if(disp_required[["MRTLAST"]]){
    col_names <- c(col_names, "MRTLAST")
    regular_int_type <- c(regular_int_type, "MRTLAST")
  }
###  if("MRTIVIFO" %in% parameter_list){
  if(disp_required[["MRTIVIFO"]]){
    col_names <- c(col_names, "MRTIVIFO")
    regular_int_type <- c(regular_int_type, "MRTIVIFO")
  }
###  if("MRTIVIFP" %in% parameter_list){
  if(disp_required[["MRTIVIFP"]]){
    col_names <- c(col_names, "MRTIVIFP")
    regular_int_type <- c(regular_int_type, "MRTIVIFP")
  }
###  if("AUCXPCTO" %in% parameter_list){
  if(disp_required[["AUCXPCTO"]]){
    col_names <- c(col_names, "AUCXPCTO")
    regular_int_type <- c(regular_int_type, "AUCXPCTO")
  }
###  if("AUCXPCTP" %in% parameter_list){
  if(disp_required[["AUCXPCTP"]]){
    col_names <- c(col_names, "AUCXPCTP")
    regular_int_type <- c(regular_int_type, "AUCXPCTP")
  }
###  if("AUMCXPTO" %in% parameter_list){
  if(disp_required[["AUMCXPTO"]]){
    col_names <- c(col_names, "AUMCXPTO")
    regular_int_type <- c(regular_int_type, "AUMCXPTO")
  }
###  if("AUMCXPTP" %in% parameter_list){
  if(disp_required[["AUMCXPTP"]]){
    col_names <- c(col_names, "AUMCXPTP")
    regular_int_type <- c(regular_int_type, "AUMCXPTP")
  }
###  if("CLO" %in% parameter_list) {
  if(disp_required[["CLO"]]) {
    col_names <- c(col_names, "CLO")
    regular_int_type <- c(regular_int_type, "CLO")
  }
###  if("CLOW" %in% parameter_list) {
  if(disp_required[["CLOW"]]) {
    col_names <- c(col_names, "CLOW")
    regular_int_type <- c(regular_int_type, "CLOW")
  }
###  if("CLP" %in% parameter_list) {
  if(disp_required[["CLP"]]) {
    col_names <- c(col_names, "CLP")
    regular_int_type <- c(regular_int_type, "CLP")
  }
###  if("CLPW" %in% parameter_list) {
  if(disp_required[["CLPW"]]) {
    col_names <- c(col_names, "CLPW")
    regular_int_type <- c(regular_int_type, "CLPW")
  }
###  if("VZO" %in% parameter_list) {
  if(disp_required[["VZO"]]) {
    col_names <- c(col_names, "VZO")
    regular_int_type <- c(regular_int_type, "VZO")
  }
###  if("VZOW" %in% parameter_list) {
  if(disp_required[["VZOW"]]) {
    col_names <- c(col_names, "VZOW")
    regular_int_type <- c(regular_int_type, "VZOW")
  }
###  if("VZP" %in% parameter_list) {
  if(disp_required[["VZP"]]) {
    col_names <- c(col_names, "VZP")
    regular_int_type <- c(regular_int_type, "VZP")
  }
###  if("VZPW" %in% parameter_list) {
  if(disp_required[["VZPW"]]) {
    col_names <- c(col_names, "VZPW")
    regular_int_type <- c(regular_int_type, "VZPW")
  }
### 2019-10-10/TGT/ Added VSSO
  if(disp_required[["VSSO"]]) {
    col_names <- c(col_names, "VSSO")
    regular_int_type <- c(regular_int_type, "VSSO")
  }
### 2019-10-10/TGT/ Added VSSP
  if(disp_required[["VSSP"]]) {
    col_names <- c(col_names, "VSSP")
    regular_int_type <- c(regular_int_type, "VSSP")
  }
### 2019-10-11/TGT/ Added VSSOW
  if(disp_required[["VSSOW"]]) {
    col_names <- c(col_names, "VSSOW")
    regular_int_type <- c(regular_int_type, "VSSOW")
  }
### 2019-10-11/TGT/ Added VSSPW
  if(disp_required[["VSSPW"]]) {
    col_names <- c(col_names, "VSSPW")
    regular_int_type <- c(regular_int_type, "VSSPW")
  }
  col_names <- c(col_names, rep(paste0("CONC",1:(auc_len+1))), rep(paste0("CONCTIME",1:(auc_len+1))))
  regular_int_type <- c(regular_int_type, rep(paste0("CONC",1:(auc_len+1))), rep(paste0("CONCTIME",1:(auc_len+1))))
### 2019-10-20/TGT/ Reposition 2019-11-21/RD Commenting it 
###  if("DOSEi" %in% parameter_list && opt_list[1] %in% names(map_data)) {
###  if(disp_required[["DOSE"]]) {
###    if(map_data[, opt_list[1]] %in% names(data_data)) {
###    if(map_data[, dosevar] %in% names(data_data)) {
###      col_names <- c(col_names, "DOSE1")
###      col_names <- c(col_names, dosevar)
###      regular_int_type <- c(regular_int_type, "DOSE1")
###      regular_int_type <- c(regular_int_type, dosevar)
###    }
###  }
  
###  if('TAUi' %in% parameter_list && opt_list[2] %in% names(map_data)) {
  if(disp_required[["TAU"]] && parameter_required(opt_list[2], names(map_data))) {
      if(parameter_required(opt_list[2], names(map_data))) { 
          if(map_data[, opt_list[2]] %in% names(data_data)) {
              col_names <- c(col_names, "TAU1")
              regular_int_type <- c(regular_int_type, "TAU1")
          }
      }
  }
###  if('TOLDi' %in% parameter_list && opt_list[3] %in% names(map_data)) {
  if(disp_required[["TOLD"]] && parameter_required(opt_list[3], names(map_data))) {
      if(parameter_required(opt_list[3], names(map_data))) { 
          if(map_data[, opt_list[3]] %in% names(data_data)) {
              col_names <- c(col_names, "TOLD1")
              regular_int_type <- c(regular_int_type, "TOLD1")
          }
      }
  }
### 2019-08-09/TGT/ Reposition location of template computaton_df creation to following
###                 the generation of "col_names" and use planned "length(col_names)"
###                 instead of "col"
###  computation_df <- data.frame(matrix(ncol = col, nrow = 0))
  computation_df <- data.frame(matrix(ncol = length(col_names), nrow = 0))
    names(computation_df) <- col_names

  #names(computation_df) <- c("SDEID", "CMAX", "CLAST", "CMAXC", "CMAXDN", "TMAX", "TLAST", "KEL", "KELTMLO", "KELTHMI", "KELNOPT",
  #                           "KELR", "KELRSQ", "KELRSQA", "THALF", "LASTTIME", "AUCALL", "AUCDN", "AUCLAST", "AUCLASTC", "AUCLASTDN",
  #                           "AUMCLAST", rep(paste0("AUC",1:auc_col)), rep(paste0("AUCINT",1:auc_col)), "AUCINFO", "AUCINFP", "AUCINFOC",
  #                           "AUCINFPC", "AUCINFODN", "AUCINFPDN","AUMCINFO", "AUMCINFP", "MRTLAST", "MRTO", "MRTP", "AUCXPCTO",
  #                           "AUCXPCTP", "AUMCXPTO", "AUMCXPTP", "CLOW", "CLP", "CLPW", "VZO", "VZOW", "VZP", "VZPW")

  if("FLGACCEPTKELCRIT" %in% names(map_data) && (("KEL" %in% parameter_list && "KELNOPT" %in% parameter_list) || "KELRSQ" %in% parameter_list)) {
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
  if("LASTTIMEACCEPTCRIT" %in% names(map_data) && ("LASTTIME" %in% parameter_list)){
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
    if(opt_list[2] %in% names(map_data)){
      if(!map_data[, opt_list[2]] %in% names(data_data)) {
        warning("Flag 'FLGACCEPTTAU' cannot be computed if 'TAUi' is not provided")
      }
    } else {
      warning("Flag 'FLGACCEPTTAU' cannot be computed if 'TAUi' is not provided")
    }
  } else {
    if(!("LASTTIMEACCEPTCRIT" %in% names(map_data))){
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
  if(!("FLGEMESIS" %in% names(map_data) && map_data$FLGEMESIS %in% names(data_data) && "TMAX" %in% parameter_list)){
    warning("Flag 'FLGEMESIS' is not present in the dataset")
  }
  if(!("FLGACCEPTPREDOSECRIT" %in% names(map_data) && "CMAX" %in% parameter_list)){
    warning("Flag 'FLGACCEPTPREDOSECRIT' is not present in the dataset")
  } else if("FLGACCEPTPREDOSECRIT" %in% names(map_data)){
    if(!("CMAX" %in% parameter_list)){
      warning("Flag 'FLGACCEPTPREDOSECRIT' cannot be computed if 'CMAX' is not part of the calculated parameters")
    }
    if(!(is.numeric(suppressWarnings(as.numeric(map_data$FLGACCEPTPREDOSECRIT)))) || (is.na(suppressWarnings(as.numeric(map_data$FLGACCEPTPREDOSECRIT))))){
      warning("Flag 'FLGACCEPTPREDOSECRIT' does not have valid form! Please try again with numeric value")
    }
  }
  if(!("LASTTIMEACCEPTCRIT" %in% names(map_data) && "LASTTIME" %in% parameter_list)){
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
##  2019-11-08/RD Added for Interpolation to account for error handling
##
  if("INCLUDEINTERPOLATION" %in% names(map_data) && (map_data[, "INCLUDEINTERPOLATION"] != 0 && map_data[, "INCLUDEINTERPOLATION"] != 1)){
    warning("Flag 'INCLUDEINTERPOLATION' does not have a valid value! Please try again with numeric value (either 0 or 1)")
  }
  if("INCLUDEEXTRAPOLATION" %in% names(map_data) && (map_data[, "INCLUDEEXTRAPOLATION"] != 0 && map_data[, "INCLUDEEXTRAPOLATION"] != 1)){
    warning("Flag 'INCLUDEEXTRAPOLATION' does not have a valid value! Please try again with numeric value (either 0 or 1)")
  }
  #if((!"LLOQPATTERNS" %in% names(map_data)) && generate_nominal_conc){
  #  warning("Flag 'LLOQPATTERNS' is not present in the map dataset")
  #  if("CONCRAW" %in% names(map_data) && "CONCRAW" %in% names(map_data)){
  #    if(map_data$CONCRAW %in% data_data && map_data$CONC %in% names(data_data)){
  #      tmp_conc <- suppressWarnings(as.numeric(data_data[,map_data$CONCRAW]))
  #    }
  #  }
  #}
  if(isTRUE(optimize_kel) && (!"TMAX" %in% parameter_list || !"TLAST" %in% parameter_list || !"CMAX" %in% parameter_list || !"CLAST" %in% parameter_list || !"AUCLAST" %in% parameter_list ||
     !"FLGACCEPTKELCRIT" %in% names(map_data) || !"FLGEXKEL" %in% names(map_data) || !map_data$FLGEXKEL %in% names(data_data))){
    warning("Kel optimization cannot be performed because 'TMAX', 'TLAST', 'CMAX', 'CLAST', 'AUCLAST' are not part of the calulcated parameters AND Flag 'FLGACCEPTKELCRIT' and Flag 'FLGXKEL' are not present in the dataset")
  }

  if(isTRUE(optimize_kel) && "TMAX" %in% parameter_list && "TLAST" %in% parameter_list && "CMAX" %in% parameter_list && "CLAST" %in% parameter_list && "AUCLAST" %in% parameter_list &&
     "FLGACCEPTKELCRIT" %in% names(map_data) && "FLGEXKEL" %in% names(map_data) && map_data$FLGEXKEL %in% names(data_data)){
    kel_flag_optimized <- integer()
    kel_opt_warning <- FALSE
  }

  for(i in 1:length(unique(data_data[,map_data$SDEID]))){
    tryCatch({
      tmp_df <- data_data[data_data[,map_data$SDEID] == unique(data_data[,map_data$SDEID])[i],]
      tmp_df <- tmp_df[order(tmp_df[,map_data$TIME]),]
      tmp_df[,map_data$CONC] <- as.numeric(tmp_df[,map_data$CONC])
      tmp_df[,map_data$TIME] <- as.numeric(tmp_df[,map_data$TIME])
      test_df <- tmp_df[,c(map_data$CONC, map_data$TIME)]
      if(any(duplicated(test_df))){
        tmp_df <- tmp_df[!duplicated(test_df),]
      }
      cest_tmp <- data.frame("CONC" = numeric(), "TIME" = numeric(), "INT_EXT" = character())
      tmp_dose <- unique(tmp_df[, dosevar])[1]
      
      if("FLGEXSDE" %in% names(map_data) && map_data$FLGEXSDE %in% names(data_data)){
        ex_flag <- as.numeric(tmp_df[,map_data$FLGEXSDE])
        tmp_df <- tmp_df[!as.logical(ex_flag),]
      } else {
        ex_flag <- NULL
      }
      if("FLGEXKEL" %in% names(map_data) && map_data$FLGEXKEL %in% names(data_data)){
        kel_flag <- as.numeric(tmp_df[,map_data$FLGEXKEL])
      } else {
        kel_flag <- NULL
      }
      if("FLGEXAUC" %in% names(map_data) && map_data$FLGEXAUC %in% names(data_data)){
        auc_flag <- as.numeric(tmp_df[,map_data$FLGEXAUC])
      } else {
        auc_flag <- NULL
      }
      if("FLGEMESIS" %in% names(map_data) && map_data$FLGEMESIS %in% names(data_data)){
        emesis_flag <- as.numeric(tmp_df[,map_data$FLGEMESIS])
      } else {
        emesis_flag <- NULL
      }
##      2019-11-08/RD Added for Interpolation to account for INCLUDEINTERPOLATION Flag
##
      if("INCLUDEINTERPOLATION" %in% names(map_data)){
        interpolation <- ifelse((map_data[,"INCLUDEINTERPOLATION"] == 0 || map_data[,"INCLUDEINTERPOLATION"] == 1), as.logical( map_data[,"INCLUDEINTERPOLATION"]), FALSE)
      } else {
        interpolation <- FALSE
      }
##      2019-11-08/RD Added for Extrapolation to account for INCLUDEEXTRAPOLATION Flag
##
      if("INCLUDEEXTRAPOLATION" %in% names(map_data)){
        extrapolation <- ifelse((map_data[,"INCLUDEEXTRAPOLATION"] == 0 || map_data[,"INCLUDEEXTRAPOLATION"] == 1), as.logical(as.numeric(map_data[,"INCLUDEEXTRAPOLATION"])), FALSE)
      } else {
        extrapolation <- FALSE
      }
      
      dof <- ifelse("DOF1" %in% names(map_data), ifelse(map_data$DOF1 %in% names(data_data), unique(tmp_df[,map_data$DOF1])[1], NA), NA)

      if(nrow(tmp_df) > 0){
        orig_time <- tmp_df[,map_data$TIME]
        orig_conc <- tmp_df[,map_data$CONC]
        
        c_0 <- c0(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        if(comp_required[["DOSEC"]]) {
          dose_c <- dosec(data = tmp_df, map = map_data)
        }
        if(comp_required[["CENDINF"]]){
          cend_inf <- cendinf(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], dof = dof)
        }
        if(comp_required[["CENDINFDN"]]){
          cend_infdn <- cendinf_dn(cendinf = cend_inf, dose = tmp_dose)
        }
        if(comp_required[["TENDINF"]]){
          tend_inf <- tendinf(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], dof = dof)
        }
###        if("CMAX" %in% parameter_list) {
        if(comp_required[["CMAX"]]) {
          c_max <- cmax(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        }
###        if("CLAST" %in% parameter_list) {
        if(comp_required[["CLAST"]]) {
          c_last <- clast(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        }
###        if("TMAX" %in% parameter_list) {
        if(comp_required[["TMAX"]]) {
          t_max <- tmax(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        }
###        if("TLAST" %in% parameter_list) {
        if(comp_required[["TLAST"]]) {
          t_last <- tlast(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        }
###        if("AUCLAST" %in% parameter_list) {
        if(comp_required[["AUCLAST"]]) {
          auclast <- auc_last(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, exflag = auc_flag)
        }
        if(isTRUE(optimize_kel) && "TMAX" %in% parameter_list && "TLAST" %in% parameter_list && "CMAX" %in% parameter_list && "CLAST" %in% parameter_list && "AUCLAST" %in% parameter_list &&
           "FLGACCEPTKELCRIT" %in% names(map_data) && "FLGEXKEL" %in% names(map_data) && map_data$FLGEXKEL %in% names(data_data)){
          orig_time <- tmp_df[,map_data$TIME]
          orig_conc <- tmp_df[,map_data$CONC]
          tmp_time <- orig_time
          tmp_conc <- orig_conc

          if("FLGNOCMAX" %in% names(map_data) && (map_data$FLGNOCMAX == 1 || map_data$FLGNOCMAX == 0)){
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
          }

          if(all(c("KELNOPT", "KELRSQ") %in% flag_df$VAR) && ("AUCXPCTO" %in% flag_df$VAR || "AUCXPCTP" %in% flag_df$VAR)){
            kel_n <- as.numeric(flag_df$CRIT[match("KELNOPT", flag_df$VAR)])
            kel_op <- flag_df$OPR[match("KELNOPT", flag_df$VAR)]
  
            ulist <- list()
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
  
            kelr_val <- kel_r(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])[["KELRSQ"]]
            if("AUCXPCTO" %in% flag_df$VAR){
              aucxpct <- auc_XpctO(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, aucflag = auc_flag)
            } else if("AUCXPCTP" %in% flag_df$VAR){
              aucxpct <- auc_XpctP(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, aucflag = auc_flag)
            } else {
              stop("Error in optimize kel")
            }
  
            selected_idx <- NA
            saved_kel_opt <- -1
            for(k in 1:length(ulist)){
              sel_time <- ulist[[k]]
              sel_conc <- tmp_conc[match(sel_time, tmp_time)]
  
              kelr_opt <- kel_r(conc = sel_conc, time = sel_time)[["KELRSQ"]]
              if("AUCXPCTO" %in% flag_df$VAR){
                span_ratio <- ifelse("SPANRATIOCRIT" %in% names(map_data), suppressWarnings(as.numeric(map_data$SPANRATIOCRIT)), NA)
                aucinfo_opt <- auc_inf_o(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, auclast = auclast, c_last = c_last, spanratio = span_ratio)
                aucxpct_opt <- auc_XpctO(conc = sel_conc, time = sel_time, method = method, aucflag = auc_flag, auc_info = aucinfo_opt, auclast = auclast)
              } else if("AUCXPCTP" %in% flag_df$VAR){
                span_ratio <- ifelse("SPANRATIOCRIT" %in% names(map_data), suppressWarnings(as.numeric(map_data$SPANRATIOCRIT)), NA)
                aucinfp_opt <- auc_inf_p(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, auclast = auclast, t_last = t_last, spanratio = span_ratio)
                aucxpct_opt <- auc_XpctP(conc = sel_conc, time = sel_time, method = method, aucflag = auc_flag, auc_infp = aucinfp_opt, auclast = auclast)
              } else {
                stop("Error in optimize kel")
              }
  
              if(!is.na(kelr_opt) && !is.na(aucxpct_opt)){
                kel_opt <- ((kelr_opt - kelr_val)/(1 - kelr_val)) + (length(sel_time)/length(tmp_time)) + ((aucxpct - aucxpct_opt)/aucxpct)
              } else {
                kel_opt <- 0
              }
  
              if(!is.na(kel_opt)){
                if(kel_opt > saved_kel_opt){
                  saved_kel_opt <- kel_opt
                  selected_idx <- match(sel_time, orig_time)
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

### 2019-10-11/TGT/ Normalized Body Size/Weight Value        
        norm_bs <- ifelse("NORMBS" %in% names(map_data), ifelse(map_data$NORMBS %in% names(tmp_df), unique(tmp_df[,map_data$NORMBS])[1], NA), NA)

###        if("CMAXDN" %in% parameter_list && "CMAX" %in% parameter_list) {
        if(comp_required[["CMAXDN"]]) {
###          cmaxdn <- cmax_dn(cmax = c_max, dose = unique(tmp_df[,map_data$DOSE1])[1])
          cmaxdn <- cmax_dn(cmax = c_max, dose = unique(tmp_df[,dosevar])[1])
        }
### 2019-09-18/TGT/ Added KELC0
###        if("KEL" %in% parameter_list || "KELTMLO" %in% parameter_list || "KELTMHI" %in% parameter_list || "KELNOPT" %in% parameter_list || "THALF" %in% parameter_list || "THALFF" %in% parameter_list) {
        if(comp_required[["KEL"]] || comp_required[["KELC0"]] || comp_required[["KELTMLO"]] || comp_required[["KELTMHI"]] || comp_required[["KELNOPT"]] || comp_required[["THALF"]] || comp_required[["THALFF"]]) {
          span_ratio <- ifelse("SPANRATIOCRIT" %in% names(map_data), suppressWarnings(as.numeric(map_data$SPANRATIOCRIT)), NA)
          kel_v <- kel(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], exflag = kel_flag, spanratio = span_ratio)
        }
###        if("KELRSQ" %in% parameter_list || "KELRSQA" %in% parameter_list) {
        if(comp_required[["KELRSQ"]] || comp_required[["KELRSQA"]]) {
          kelr_v <- kel_r(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], exflag = kel_flag)
        }
###        if("LASTTIME" %in% parameter_list) {
        if(comp_required[["LASTTIME"]]) {
          last_time <- lasttime(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        }
### 2019-08-30/TGT/ Remove "CMAXC"/"CMAXCi" since only valid for M2 Bolus administration applications
###        if("CMAXC" %in% parameter_list && "CMAX" %in% parameter_list && "KEL" %in% parameter_list) {
###          c_max_c <- cmaxc(kel = kel_v[["KEL"]], cmax = c_max, c0 = c_0, tmax = t_max)
###        }
### 2019-08-15/TGT/ missing CEST computation added
###        if(parameter_required("^CEST$",parameter_list)) {
###        if(comp_required[["CEST"]]) {
        if(comp_required[["CEST"]] || parameter_required("KEL", names(kel_v)) || parameter_required("KELC0", names(kel_v))) {
          c_est <- cest(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], kelflag=kel_flag, t_last=t_last, spanratio=span_ratio, kel=kel_v[["KEL"]], kelc0=kel_v[["KELC0"]])
        }
###        if("AUCALL" %in% parameter_list) {
        if(comp_required[["AUCALL"]]) {
          aucall <- auc_all(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, exflag = auc_flag)
        }
###        if("AUCDN" %in% parameter_list) {
### 2019-10-10/TGT/ replace AUCDN with AUCALLDN
###        if(comp_required[["AUCDN"]]) {
###          aucdn <- auc_dn(auc = aucall, dose = unique(tmp_df[,map_data$DOSE1])[1])
        if(comp_required[["AUCALLDN"]]) {
###          aucalldn <- auc_dn(auc = aucall, dose = unique(tmp_df[,map_data$DOSE1])[1])
          aucalldn <- auc_dn(auc = aucall, dose = unique(tmp_df[,dosevar])[1])
        }
###        if("AUCLASTC" %in% parameter_list) {
        if(comp_required[["AUCLASTC"]]) {
          auclast_c <- auc_lastc(kel = kel_v[["KEL"]], auclast = auclast, c0 = c_0, tlast = t_last)
        }
###        if("AUCLASTDN" %in% parameter_list) {
        if(comp_required[["AUCLASTDN"]]) {
###          auclastdn <- auc_dn(auc = auclast, dose = unique(tmp_df[,map_data$DOSE1])[1])
          auclastdn <- auc_dn(auc = auclast, dose = unique(tmp_df[,dosevar])[1])
        }
###        if("AUMCLAST" %in% parameter_list) {
        if(comp_required[["AUMCLAST"]]) {
          aumclast <- aumc_last(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, exflag = auc_flag)
        }

###        if("AUCT" %in% parameter_list && 'TMAX' %in% parameter_list) {
        if(comp_required[["AUCT"]]) {
          auct <- NULL
          auctdn <- NULL
          auc_int <- NULL
          for(t in 2:(auc_len+1)){
            tmp <- auc_t1_t2(conc = tmp_df[,map_data$CONC], time = na.omit(tmp_df[,map_data$TIME]), t1 = tmp_df[,map_data$TIME][1], t2 = tmp_df[,map_data$TIME][t], method = method, exflag = auc_flag, t_max = t_max)
            tmp_dn <- auc_dn(auc = tmp, dose = tmp_dose)
            if(!is.na(unique(tmp_df[,map_data$TIME])[1]) && !is.na(unique(tmp_df[,map_data$TIME])[t])){
              tmp_int <- paste0(unique(tmp_df[,map_data$TIME])[1], "_", unique(tmp_df[,map_data$TIME])[t])
            } else {
              tmp_int <- NA
            }

            if(comp_required[["AUCT"]]){
              if(is.null(auct)){
                auct <- tmp
              } else {
                auct <- c(auct, tmp)
              }
            }
            if(comp_required[["AUCTDN"]]){
              if(is.null(auctdn)){
                auctdn <- tmp_dn
              } else {
                auctdn <- c(auctdn, tmp_dn)
              }
            }
            if(is.null(auc_int)){
              auc_int <- tmp_int
            } else {
              auc_int <- c(auc_int, tmp_int)
            }
          }
          if(comp_required[["AUCT"]]){
            if(length(auct) < auc_col) {
              auct <- c(auct, rep(NA, (auc_col - length(auct))))
            }
          }
          if(comp_required[["AUCTDN"]]){
            if(length(auctdn) < auc_col) {
              auctdn <- c(auctdn, rep(NA, (auc_col - length(auctdn))))
            }
          }
          if(length(auc_int) < auc_col) {
            auc_int <- c(auc_int, rep(NA, (auc_col - length(auc_int))))
          }
        }
###        if("AUCT1_T2" %in% parameter_list && 'TMAX' %in% parameter_list && auc_pair_check) {
        if(comp_required[["AUCT1_T2"]] && auc_pair_check) {
          auct1_t2 <- NULL
          auct1_t2_names <- c(rep(paste0("AUC.", 1:auc_par_len, ".T1")), rep(paste0("AUC.", 1:auc_par_len, ".T2")))
          if(!all(auct1_t2_names %in% names(map_data))){
            par_col <- rep(paste0("'", auct1_t2_names[!auct1_t2_names %in% names(map_data)], "'"))
            stop(paste0("Dataset provided via 'map' does not contain the required columns for partial areas ", par_col))
          }
##         2019-11-11/RD Removed Error handling for Interpolation because we agreed dose time is 0 for SD
##
          if((isTRUE(interpolation) || isTRUE(extrapolation))){
            tmp_told <- 0
          }
          for(t in 1:(auc_par_len)){
            if(!(is.numeric(as.numeric(map_data[, paste0("AUC.", t, ".T1")])) && is.numeric(as.numeric(map_data[, paste0("AUC.", t, ".T2")])))){
              stop(paste0("'AUC.", t, ".T1' and/or 'AUC.", t, ".T2' value provided via 'map' is not a numeric value"))
            }
            auc_t1 <- as.numeric(map_data[, paste0("AUC.", t, ".T1")])
            auc_t2 <- as.numeric(map_data[, paste0("AUC.", t, ".T2")])
##            2019-11-08/RD Changed the call for partial AUCs to account for interpolation
##
            if((isTRUE(interpolation) || isTRUE(extrapolation))){
              tmp <- auc_t1_t2(conc = tmp_df[,map_data$CONC], time = na.omit(tmp_df[,map_data$TIME]), t1 = auc_t1, t2 = auc_t2, method = method, exflag = auc_flag, t_max = t_max, interpolate = interpolation, extrapolate = extrapolation, model = "M3", dosing_type = "SD", told = tmp_told, kel = kel_v, orig_conc = orig_conc, orig_time = orig_time)
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
              tmp_auc <- auc_t1_t2(conc = tmp_df[,map_data$CONC], time = na.omit(tmp_df[,map_data$TIME]), t1 = auc_t1, t2 = auc_t2, method = method, exflag = auc_flag, t_max = t_max)
            }
            
            if(is.null(auct1_t2)){
              auct1_t2 <- tmp_auc
            } else {
              auct1_t2 <- c(auct1_t2, tmp_auc)
            }
          }
        }

### 2019-08-09/TGT/ Compute AUCINFO conditionally as needed for AUCINFO, AUCINFOC, AUCINFODN
###                 Use regular expression to determine if ANY of these parameters are requested
###        if("AUCINFO" %in% parameter_list) {
###        if(parameter_required("^AUCINFO", parameter_list)) {
        if(comp_required[["AUCINFO"]]) {
          aucinf_o <- auc_inf_o(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag)
        }
### 2019-08-09/TGT/ It should not be required that AUCINFO be in the parameter_list.
###                 It should simply be computed if required by a dependent parameter.
###        if("AUCINFOC" %in% parameter_list && "AUCINFO" %in% parameter_list) {
###        if(parameter_required("^AUCINFOC", parameter_list)) {
        if(comp_required[["AUCINFOC"]]) {
          aucinf_oc <- auc_inf_oc(kel = kel_v[["KEL"]], aucinfo = aucinf_o, c0 = c_0)
        }
### 2019-08-09/TGT/ It should not be required that AUCINFO be in the parameter_list.
###                 It should simply be computed if required by a dependent parameter.        
###        if("AUCINFODN" %in% parameter_list && "AUCINFO" %in% parameter_list) {
###        if(parameter_required("^AUCINFODN", parameter_list)) {
        if(comp_required[["AUCINFODN"]]) {
###          aucinfo_dn <- auc_dn(auc = aucinf_o, dose = unique(tmp_df[,map_data$DOSE1])[1])
          aucinfo_dn <- auc_dn(auc = aucinf_o, dose = unique(tmp_df[,dosevar])[1])
        }
### 2019-08-09/TGT/ Compute AUCINFP conditionally as needed for AUCINFP, AUCINFPC, AUCINFPDN
###                 Use regular expression to determine if ANY of these parameters are requested
###        if("AUCINFP" %in% parameter_list) {
###        if(parameter_required("^AUCINFP", parameter_list)) {
        if(comp_required[["AUCINFP"]]) {
          aucinf_p <- auc_inf_p(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag)
        }
### 2019-08-09/TGT/ It should not be required that AUCINFP be in the parameter_list.
###                 It should simply be computed if required by a dependent parameter.
###        if("AUCINFPC" %in% parameter_list && "AUCINFP" %in% parameter_list) {
###        if(parameter_required("^AUCINFPC", parameter_list)) {
        if(comp_required[["AUCINFPC"]]) {
          aucinf_pc <- auc_inf_pc(kel = kel_v[["KEL"]], aucinfp = aucinf_p, c0 = c_0)
        }
### 2019-08-09/TGT/ It should not be required that AUCINFP be in the parameter_list.
###                 It should simply be computed if required by a dependent parameter.        
###        if("AUCINFPDN" %in% parameter_list && "AUCINFP" %in% parameter_list) {
###        if(parameter_required("^AUCINFPDN", parameter_list)) {
        if(comp_required[["AUCINFPDN"]]) {
###          aucinfp_dn <- auc_dn(auc = aucinf_p, dose = unique(tmp_df[,map_data$DOSE1])[1])
          aucinfp_dn <- auc_dn(auc = aucinf_p, dose = unique(tmp_df[,dosevar])[1])
        }
###        if("AUMCINFO" %in% parameter_list) {
        if(comp_required[["AUMCINFO"]]) {
          aumcinf_o <- aumc_inf_o(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag)
        }
###        if("AUMCINFP" %in% parameter_list) {
        if(comp_required[["AUMCINFP"]]) {
          aumcinf_p <- aumc_inf_p(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag)
        }
###        if("MRTLAST" %in% parameter_list) {
        if(comp_required[["MRTLAST"]]) {
          mrtlast <- mrt_last(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, model = "M3", aucflag = auc_flag, dof = dof)
        }
###        if("MRTIVIFO" %in% parameter_list){
        if(comp_required[["MRTIVIFO"]]){
          mrto <- mrt_ivif_o(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, model = "M3", parameter = "SD", kelflag = kel_flag, aucflag = auc_flag, dof = dof)
        }
###        if("MRTIVIFP" %in% parameter_list){
        if(comp_required[["MRTIVIFP"]]){
          mrtp <- mrt_ivif_p(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, model = "M3", parameter = "SD", kelflag = kel_flag, aucflag = auc_flag, dof = dof)
        }
###        if("AUCXPCTO" %in% parameter_list){
        if(comp_required[["AUCXPCTO"]]){
          aucxpcto <- auc_XpctO(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag)
        }
###        if("AUCXPCTP" %in% parameter_list){
        if(comp_required[["AUCXPCTP"]]){
          aucxpctp <- auc_XpctP(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag)
        }
###        if("AUMCXPTO" %in% parameter_list){
        if(comp_required[["AUMCXPTO"]]){
          AUMCXPTo <- aumc_XpctO(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag)
        }
###        if("AUMCXPTP" %in% parameter_list){
        if(comp_required[["AUMCXPTP"]]){
          AUMCXPTp <- aumc_XpctP(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag)
        }
###        if("CLO" %in% parameter_list) {
        if(comp_required[["CLO"]]) {
###          cl_o <- clo(aucinfo = aucinf_o, dose = unique(tmp_df[,map_data$DOSE1])[1])
###          cl_o <- clo(aucinfo = aucinf_o, dose = unique(tmp_df[,dosevar])[1])
          cl_o <- clo(aucinfo = aucinf_o, dose = dose_c)
        }
###        if("CLOW" %in% parameter_list) {
        if(comp_required[["CLOW"]]) {
          cl_ow <- clow(clo = cl_o, normbs = norm_bs)
        }
###        if("CLP" %in% parameter_list) {
        if(comp_required[["CLP"]]) {
###          cl_p <- clp(aucinfp = aucinf_p, dose = unique(tmp_df[,map_data$DOSE1])[1])
###          cl_p <- clp(aucinfp = aucinf_p, dose = unique(tmp_df[,dosevar])[1])
          cl_p <- clp(aucinfp = aucinf_p, dose = dose_c)
        }
###        if("CLPW" %in% parameter_list) {
        if(comp_required[["CLPW"]]) {
          cl_pw <- clpw(clp = cl_p, normbs = unique(tmp_df[,map_data$NORMBS])[1])
        }
###        if("VZO" %in% parameter_list) {
        if(comp_required[["VZO"]]) {
###          vz_o <- vzo(kel = kel_v[["KEL"]], aucinfo = aucinf_o, dose = unique(tmp_df[,map_data$DOSE1])[1])
###          vz_o <- vzo(kel = kel_v[["KEL"]], aucinfo = aucinf_o, dose = unique(tmp_df[,dosevar])[1])
          vz_o <- vzo(kel = kel_v[["KEL"]], aucinfo = aucinf_o, dose = dose_c)
        }
###        if("VZOW" %in% parameter_list) {
        if(comp_required[["VZOW"]]) {
          norm_bs <- ifelse("NORMBS" %in% names(map_data), ifelse(map_data$NORMBS %in% names(tmp_df), unique(tmp_df[,map_data$NORMBS])[1], NA), NA)
          vz_ow <- vzow(vzo = vz_o, normbs = norm_bs)
        }
###        if("VZP" %in% parameter_list) {
        if(comp_required[["VZP"]]) {
###          vz_p <- vzp(kel = kel_v[["KEL"]], aucinfp = aucinf_p, dose = unique(tmp_df[,map_data$DOSE1])[1])
###          vz_p <- vzp(kel = kel_v[["KEL"]], aucinfp = aucinf_p, dose = unique(tmp_df[,dosevar])[1])
          vz_p <- vzp(kel = kel_v[["KEL"]], aucinfp = aucinf_p, dose = dose_c)
        }
###        if("VZPW" %in% parameter_list) {
        if(comp_required[["VZPW"]]) {
          norm_bs <- ifelse("NORMBS" %in% names(map_data), ifelse(map_data$NORMBS %in% names(tmp_df), unique(tmp_df[,map_data$NORMBS])[1], NA), NA)
          vz_pw <- vzpw(vzp = vz_p, normbs = norm_bs)
        }
### 2019-10-10/TGT/ Added VSSO
        if(comp_required[["VSSO"]]) {
          vsso <- vss(cl = cl_o, mrt = mrto)
        }
### 2019-10-10/TGT/ Added VSSP
        if(comp_required[["VSSP"]]) {
          vssp <- vss(cl = cl_p, mrt = mrtp)
        }
### 2019-10-11/TGT/ Added VSSOW
        if(comp_required[["VSSOW"]]) {
          vssow <- vssw(vss = vsso, normbs = norm_bs)
        }
### 2019-10-11/TGT/ Added VSSPW
        if(comp_required[["VSSPW"]]) {
          vsspw <- vssw(vss = vssp, normbs = norm_bs)
        }
###        if("KEL" %in% parameter_list){
        if(comp_required[["KEL"]]){
          exflag <- !as.logical(kel_flag)

          pkdataid <- tmp_df[,"PKDATAROWID"][exflag]
### 2019-08-05/TGT/ Following incorrectly identifies CONC for TIME
###          time <- tmp_df[,map_data$CONC][exflag]
          time <- tmp_df[,map_data$TIME][exflag]
### 2019-08-05/TGT/ Following incorrectly identifies TIME for CONC
###          conc <- tmp_df[,map_data[[map_data$TIME]]][exflag]
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
###          } else {
###            cest_kel <- rep(NA, length(conc))
          }

          tmp_est_data <- data.frame(matrix(ncol = length(elist), nrow = 0))
          names(tmp_est_data) <- elist
          est_idx <- 1
          if(length(pkdataid) > 0){
            for(e in 1:length(pkdataid)){
              est_row <- c(pkdataid[e], unique(data_data[,map_data$SDEID])[i], time[e], cest_kel[e], NA, NA, NA, NA)
              ### 2019-10-06/TGT/ Add CEST at TLAST
              if(time[e]==t_last) { est_row[8] <- c_est }
              
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
        }

        #computation_df[i,] <- c(unique(data_data[,map_data$SDEID])[i], c_max, c_last, c_max_c, cmaxdn, t_max, t_last,
        #                        kel_v[["KEL"]], kel_v[["KELTMLO"]], kel_v[["KELTMHI"]], kel_v[["KELNOPT"]], kelr_v[["KELR"]],
        #                        kelr_v[["KELRSQ"]], kelr_v[["KELRSQA"]], kel_v[["THALF"]], last_time, aucall, aucdn, auclast,
        #                        auclast_c, auclastdn, aumclast, auct, auc_int, aucinf_o, aucinf_p, aucinf_oc, aucinf_pc,
        #                        aucinfo_dn, aucinfp_dn, aumcinf_o, aumcinf_p, mrtlast, mrto, mrtp, aucxpcto, aucxpctp,
        #                        AUMCXPTo, AUMCXPTp, cl_ow, cl_p, cl_pw, vz_o, vz_ow, vz_p, vz_pw)

        row_data <- c(unique(data_data[,map_data$SDEID])[i])

        if(disp_required[["DOSE"]] || disp_required[["DOSEi"]]){
          if(parameter_required(dosevar, names(data_data))) {
              row_data <- c(row_data, unique(tmp_df[, dosevar])[1])
          }
        }
        
###        if(disp_required[["DOSE"]]){
###          if(map_data[, dosevar] %in% names(data_data)) {
###            row_data <- c(row_data, unique(tmp_df[, map_data[, dosevar]])[1])
###          }
###        }

        if(disp_required[["DOSEC"]]) {
          row_data <- c(row_data, dose_c)
        }
        if(disp_required[["DOFi"]]){
          row_data <- c(row_data, dof)
        }
        if(disp_required[["CENDINF"]]){
          row_data <- c(row_data, cend_inf)
        }
        if(disp_required[["CENDINFDN"]]){
          row_data <- c(row_data, cend_infdn)
        }
        if(disp_required[["TENDINF"]]){
          row_data <- c(row_data, tend_inf)
        }

###        if("CMAX" %in% parameter_list) {
        if(disp_required[["CMAX"]]) {
          row_data <- c(row_data, c_max)
        }
### 2019-08-09/TGT/ Following should not require CMAX to be part of the parameter_list. 
###                 It could be that CMAX is not called for a result but the criteria 
###                 could still rely upon CMAX. This needs to be fixed.
        if("FLGACCEPTPREDOSECRIT" %in% names(map_data) && ("CMAX" %in% parameter_list)){
          pre_dose_crit <- suppressWarnings(as.numeric(map_data$FLGACCEPTPREDOSECRIT))
          if(is.numeric(pre_dose_crit)){
### 2019-08-09/TGT/ Following assumes that the predose timepoint has a time value of zero "0"
###                 With ACTUAL times being used this may not be the case. This needs to be fixed.
            pre_dose <- tmp_df[,map_data$CONC][tmp_df[,map_data$TIME] == 0][1]
            if(is.numeric(c_max)){
              row_data <- c(row_data, ifelse(pre_dose > (c_max * pre_dose_crit), 0, 1))
            } else {
              row_data <- c(row_data, 1)
            }
          } else {
            row_data <- c(row_data, 1)
          }
        }
###        if("CLAST" %in% parameter_list) {
        if(disp_required[["CLAST"]]) {
          row_data <- c(row_data, c_last)
        }
### 2019-08-30/TGT/ Remove "CMAXC"/"CMAXCi" since only valid for M2 Bolus administration applications
###        if("CMAXC" %in% parameter_list && "CMAX" %in% parameter_list && "KEL" %in% parameter_list) {
###         row_data <- c(row_data, c_max_c)
###        }
### 2019-08-15/TGT/ added CEST
###        if(parameter_required("^CEST$",parameter_list)) {
        if(disp_required[["CEST"]]) {
            row_data <- c(row_data, c_est)
        }
###        if("CMAXDN" %in% parameter_list && "CMAX" %in% parameter_list) {
        if(disp_required[["CMAXDN"]]) {
          row_data <- c(row_data, cmaxdn)
        }
###        if("TMAX" %in% parameter_list) {
        if(disp_required[["TMAX"]]) {
          row_data <- c(row_data, t_max)
        }
###        if("FLGEMESIS" %in% names(map_data) && ("TMAX" %in% parameter_list)){
        if("FLGEMESIS" %in% names(map_data) && ("TMAX" %in% parameter_list)){
          row_data <- c(row_data, NA)
        }
###        if("TLAST" %in% parameter_list) {
        if(disp_required[["TLAST"]]) {
          row_data <- c(row_data, t_last)
        }
###        if("KEL" %in% parameter_list) {
        if(disp_required[["KEL"]]) {
          row_data <- c(row_data, kel_v[["KEL"]])
        }
### 2019-09-18/TGT/ Added KELC0
        if(disp_required[["KELC0"]]) {
          row_data <- c(row_data, kel_v[["KELC0"]])
        }
###        if("KELTMLO" %in% parameter_list) {
        if(disp_required[["KELTMLO"]]) {
          row_data <- c(row_data, kel_v[["KELTMLO"]])
        }
###        if("KELTMHI" %in% parameter_list) {
        if(disp_required[["KELTMHI"]]) {
          row_data <- c(row_data, kel_v[["KELTMHI"]])
        }
###        if("KELNOPT" %in% parameter_list) {
        if(disp_required[["KELNOPT"]]) {
          row_data <- c(row_data, kel_v[["KELNOPT"]])
        }
### 2019-08-12/TGT/ Modify this to explicitly refer to KELR rather than impute it
###        if("KELRSQ" %in% parameter_list || "KELRSQA" %in% parameter_list){
###          row_data <- c(row_data, kelr_v[["KELR"]])
###        }
###        if(parameter_required("^KELR$", parameter_list)){
        if(disp_required[["KELR"]]){
          row_data <- c(row_data, kelr_v[["KELR"]])
        }
###        if("KELRSQ" %in% parameter_list){
        if(disp_required[["KELRSQ"]]){
          row_data <- c(row_data, kelr_v[["KELRSQ"]])
        }
###        if("KELRSQA" %in% parameter_list){
        if(disp_required[["KELRSQA"]]){
          row_data <- c(row_data, kelr_v[["KELRSQA"]])
        }
        if("FLGACCEPTKELCRIT" %in% names(map_data) && (("KEL" %in% parameter_list && "KELNOPT" %in% parameter_list) || "KELRSQ" %in% parameter_list)) {
          if(length(unlist(strsplit(as.character(map_data$FLGACCEPTKELCRIT), ","))) > 0){
            row_data <- c(row_data, 0)
          } else {
            row_data <- c(row_data, 0)
          }
        }
###        if("THALF" %in% parameter_list) {
        if(disp_required[["THALF"]]) {
          row_data <- c(row_data, kel_v[["THALF"]])
        }
###        if("THALFF" %in% parameter_list) {
        if(disp_required[["THALFF"]]) {
          row_data <- c(row_data, kel_v[["THALFF"]])
        }
###        if("LASTTIME" %in% parameter_list) {
        if(disp_required[["LASTTIME"]]) {
          row_data <- c(row_data, last_time)
        }
        if("LASTTIMEACCEPTCRIT" %in% names(map_data) && ("LASTTIME" %in% parameter_list)) {
          if(!is.na(last_crit_factor)){
            if(opt_list[2] %in% names(map_data)){
              if(map_data[, opt_list[2]] %in% names(data_data)) {
                tau_val <- unique(tmp_df[, map_data[, opt_list[2]]])[1]
                lt_accept_crit <- tau_val * last_crit_factor
                if(is.numeric(tau_val) && is.numeric(last_crit_factor)){
                  row_data <- c(row_data, ifelse(last_time >= lt_accept_crit, 1, 0))
                } else {
                  row_data <- c(row_data, 0)
                }
              } else {
                row_data <- c(row_data, 0)
              }
            } else {
              row_data <- c(row_data, 0)
            }
          } else {
            row_data <- c(row_data, 0)
          }
        }
###        if("AUCALL" %in% parameter_list) {
        if(disp_required[["AUCALL"]]) {
          row_data <- c(row_data, aucall)
        }
###        if("AUCDN" %in% parameter_list) {
### 2019-10-10/TGT/ replace AUCDN with AUCALLDN
###        if(disp_required[["AUCDN"]]) {
###          row_data <- c(row_data, aucdn)
        if(disp_required[["AUCALLDN"]]) {
          row_data <- c(row_data, aucalldn)
        }
###        if("AUCLAST" %in% parameter_list) {
        if(disp_required[["AUCLAST"]]) {
          row_data <- c(row_data, auclast)
        }
###        if("AUCLASTC" %in% parameter_list) {
        if(disp_required[["AUCLASTC"]]) {
          row_data <- c(row_data, auclast_c)
        }
###        if("AUCLASTDN" %in% parameter_list) {
        if(disp_required[["AUCLASTDN"]]) {
          row_data <- c(row_data, auclastdn)
        }
###        if("AUMCLAST" %in% parameter_list) {
        if(disp_required[["AUMCLAST"]]) {
          row_data <- c(row_data, aumclast)
        }
###        if("AUCT" %in% parameter_list && "TMAX" %in% parameter_list) {
        if(disp_required[["AUCT"]]) {
          row_data <- c(row_data, auct)
        }
        if(disp_required[["AUCTDN"]]) {
          row_data <- c(row_data, auctdn)
        }
        if(disp_required[["AUCT"]] || disp_required[["AUCTDN"]]) {
          row_data <- c(row_data, auc_int)
        }
###        if("AUCT1_T2" %in% parameter_list && "TMAX" %in% parameter_list && auc_pair_check) {
        if("AUCT1_T2" %in% parameter_list && "TMAX" %in% parameter_list && auc_pair_check) {
          row_data <- c(row_data, auct1_t2)
        }
###        if("AUCINFO" %in% parameter_list) {
        if(disp_required[["AUCINFO"]]) {
          row_data <- c(row_data, aucinf_o)
        }
###        if("AUCINFP" %in% parameter_list) {
        if(disp_required[["AUCINFP"]]) {
          row_data <- c(row_data, aucinf_p)
        }
###        if("AUCINFOC" %in% parameter_list) {
        if(disp_required[["AUCINFOC"]]) {
          row_data <- c(row_data, aucinf_oc)
        }
###        if("AUCINFPC" %in% parameter_list) {
        if(disp_required[["AUCINFPC"]]) {
          row_data <- c(row_data, aucinf_pc)
        }
###        if("AUCINFODN" %in% parameter_list) {
        if(disp_required[["AUCINFODN"]]) {
          row_data <- c(row_data, aucinfo_dn)
        }
###        if("AUCINFPDN" %in% parameter_list) {
        if(disp_required[["AUCINFPDN"]]) {
          row_data <- c(row_data, aucinfp_dn)
        }
###        if("AUMCINFO" %in% parameter_list) {
        if(disp_required[["AUMCINFO"]]) {
          row_data <- c(row_data, aumcinf_o)
        }
###        if("AUMCINFP" %in% parameter_list) {
        if(disp_required[["AUMCINFP"]]) {
          row_data <- c(row_data, aumcinf_p)
        }
###        if("MRTLAST" %in% parameter_list){
        if(disp_required[["MRTLAST"]]){
          row_data <- c(row_data, mrtlast)
        }
###        if("MRTIVIFO" %in% parameter_list){
        if(disp_required[["MRTIVIFO"]]){
          row_data <- c(row_data, mrto)
        }
###        if("MRTIVIFP" %in% parameter_list){
        if(disp_required[["MRTIVIFP"]]){
          row_data <- c(row_data, mrtp)
        }
###        if("AUCXPCTO" %in% parameter_list){
        if(disp_required[["AUCXPCTO"]]){
          row_data <- c(row_data, aucxpcto)
        }
###        if("AUCXPCTP" %in% parameter_list){
        if(disp_required[["AUCXPCTP"]]){
          row_data <- c(row_data, aucxpctp)
        }
###        if("AUMCXPTO" %in% parameter_list){
        if(disp_required[["AUMCXPTO"]]){
          row_data <- c(row_data, AUMCXPTo)
        }
###        if("AUMCXPTP" %in% parameter_list){
        if(disp_required[["AUMCXPTP"]]){
          row_data <- c(row_data, AUMCXPTp)
        }
###        if("CLO" %in% parameter_list) {
        if(disp_required[["CLO"]]) {
          row_data <- c(row_data, cl_o)
        }
###        if("CLOW" %in% parameter_list) {
        if(disp_required[["CLOW"]]) {
          row_data <- c(row_data, cl_ow)
        }
###        if("CLP" %in% parameter_list) {
        if(disp_required[["CLP"]]) {
          row_data <- c(row_data, cl_p)
        }
###        if("CLPW" %in% parameter_list) {
        if(disp_required[["CLPW"]]) {
          row_data <- c(row_data, cl_pw)
        }
###        if("VZO" %in% parameter_list) {
        if(disp_required[["VZO"]]) {
          row_data <- c(row_data, vz_o)
        }
###        if("VZOW" %in% parameter_list) {
        if(disp_required[["VZOW"]]) {
          row_data <- c(row_data, vz_ow)
        }
###        if("VZP" %in% parameter_list) {
        if(disp_required[["VZP"]]) {
          row_data <- c(row_data, vz_p)
        }
###        if("VZPW" %in% parameter_list) {
        if(disp_required[["VZPW"]]) {
          row_data <- c(row_data, vz_pw)
        }
### 2019-10-10/TGT/ Added VSSO          
        if(disp_required[["VSSO"]]) {
          row_data <- c(row_data, vsso)
        }
### 2019-10-10/TGT/ Added VSSP
        if(disp_required[["VSSP"]]) {
          row_data <- c(row_data, vssp)
        }
### 2019-10-11/TGT/ Added VSSOW
        if(disp_required[["VSSOW"]]) {
          row_data <- c(row_data, vssow)
        }
### 2019-10-10/TGT/ Added VSSPW
        if(disp_required[["VSSPW"]]) {
          row_data <- c(row_data, vsspw)
        }
        row_data <- c(row_data,
                      c(tmp_df[,map_data$CONC], rep(NA, ((auc_len+1) - length(tmp_df[,map_data$CONC])))),
                      c(tmp_df[,map_data$TIME], rep(NA, ((auc_len+1) - length(tmp_df[,map_data$TIME]))))
                      )

### 2019-10-20/TGT/ Reposition
###        if("DOSEi" %in% parameter_list && opt_list[1] %in% names(map_data)){
###        if(disp_required[["DOSE"]]){
###          if(map_data[, opt_list[1]] %in% names(data_data)) {
###          if(map_data[, dosevar] %in% names(data_data)) {
###            row_data <- c(row_data, unique(tmp_df[, map_data[, opt_list[1]]])[1])
###            row_data <- c(row_data, unique(tmp_df[, map_data[, dosevar]])[1])
###          }
###        }
        
###        if('TAUi' %in% parameter_list && opt_list[2] %in% names(map_data)){
        if(disp_required[["TAU"]] && parameter_required(opt_list[2], names(map_data))){
            if(parameter_required(opt_list[2], names(map_data))) { 
                if(map_data[, opt_list[2]] %in% names(data_data)) {
                    row_data <- c(row_data, unique(tmp_df[, map_data[, opt_list[2]]])[1])
                }
            }
        }
###        if('TOLDi' %in% parameter_list && opt_list[3] %in% names(map_data)){
        if(disp_required[["TOLD"]] && parameter_required(opt_list[3], names(map_data))){
            if(parameter_required(opt_list[3], names(map_data))) { 
                if(map_data[, opt_list[3]] %in% names(data_data)) {
                    row_data <- c(row_data, unique(tmp_df[, map_data[, opt_list[3]]])[1])
                }
            }
        }
        computation_df[i,] <- row_data
      } else {
        computation_df[i,] <- c(unique(data_data[,map_data$SDEID])[i], rep(NA, length(names(computation_df))-1))
      }
    }, error = function(e) {
      stop(paste0(e, "For SDEID ", unique(data_data[,map_data$SDEID])[i]))
    })
  }

##  2019-11-13/RD/ Added to account for incorrect handling of FLGACCEPTKELCRIT
## 
  if("FLGACCEPTKELCRIT" %in% names(map_data) && (("KEL" %in% parameter_list && "KELNOPT" %in% parameter_list) || "KELRSQ" %in% parameter_list)) {
    if(length(unlist(strsplit(as.character(map_data$FLGACCEPTKELCRIT), ","))) > 0){
      if(as.character(flag_df$VAR[j]) %in% names(computation_df)){
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
        warning(paste0("Flag 'FLGACCEPTKELCRIT' values provided via 'map' does not have a parameter name that is generated as an output '", flag_df$VAR[j], "'"))
      }
    }
  }
  if("FLGEMESIS" %in% names(map_data) && ("TMAX" %in% parameter_list)){
    comutation_df$FLGACCEPTTMAX <- ifelse(computation_df$FLGEMESIS == 1 & computation_df$TMAX1 < 2 * median(computation_df$TMAX1), TRUE, FALSE)
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
### check units processing
###  print(computation_df)
###  print(map_data$AUCOUTPUTUNIT)
###  map_data$AUCOUTPUTUNIT <- "NG.HR/ML"
###  print(map_data$AUCOUTPUTUNIT)
  
  computation_df <- unit_conversion(data = data_data, map = map_data, result = computation_df, unit_class = "ALL")
###  print(computation_df)
  
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
    j <- lapply(return_df, FUN=function(x) { all(is.na(x)) } )
    return_df <- return_df[,-match(names(j[j==TRUE]), names(return_df))]

### 2019-09-18/TGT/ Except for SDEID, remove overlapping elements from return_list
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

### 2019-09-16/TGT/ Always return est_data and full results_list
  results_list <- list()
  results_list$data_out <- computation_df
  results_list$est_data <- est_data

  if(isTRUE(optimize_kel) && "TMAX" %in% parameter_list && "TLAST" %in% parameter_list && "CMAX" %in% parameter_list && "CLAST" %in% parameter_list && "AUCLAST" %in% parameter_list &&
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
  return(results_list)
}

