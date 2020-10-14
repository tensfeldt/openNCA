#' Run M1 SD Computation
#'
#' This function will compute all the relevant parameters for a M1 model Single Dose (SD).\cr
#'
#' @details
#' \strong{Methods:} You can use the following methods to calculate AUC: \cr
#' \enumerate{
#'  \item \strong{1: Linear-Log Trapazoidal Rule}(default method): The linear method is used up to Tmax (the
#'  first occurrence of Cmax) and the log trapezoidal method is used for the remainder of the profile. If
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
#'  \item \strong{tlag}: Refer to \code{\link{tlag}} for more details
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
#'  \item \strong{mrt_last}: Refer to \code{\link{mrt_last}} for more details
#'  \item \strong{mrt_evif_o}: Refer to \code{\link{mrt_evif_o}} for more details
#'  \item \strong{mrt_evif_p}: Refer to \code{\link{mrt_evif_p}} for more detail
#'  \item \strong{auc_XpctO}: Refer to \code{\link{auc_XpctO}} for more details
#'  \item \strong{auc_XpctP}: Refer to \code{\link{auc_XpctP}} for more details
#'  \item \strong{aumc_XpctO}: Refer to \code{\link{aumc_XpctO}} for more details
#'  \item \strong{aumc_XpctP}: Refer to \code{\link{aumc_XpctP}} for more details
#'  \item \strong{clfo}: Refer to \code{\link{clfo}} for more details
#'  \item \strong{clfow}: Refer to \code{\link{clfow}} for more details
#'  \item \strong{clfp}: Refer to \code{\link{clfp}} for more details
#'  \item \strong{clfpw}: Refer to \code{\link{clfpw}} for more details
#'  \item \strong{cav}: Refer to \code{\link{cav}} for more details
#'  \item \strong{vzfo}: Refer to \code{\link{vzfo}} for more details
#'  \item \strong{vzfow}: Refer to \code{\link{vzfow}} for more details
#'  \item \strong{vzfp}: Refer to \code{\link{vzfp}} for more details
#'  \item \strong{vzfpw}: Refer to \code{\link{vzfpw}} for more details
#' }
#'
#' @section Note:
#' By default all the return list options are selected and calculated if 'parameter_list' is not specified. Please refer to MCT
#' to get more calcification on how to specify which parameters to calculate to this function if you wish to subset the default calculated parameters. \cr
#' By default 'display_list' argument is empty, which means that this function will return all calculated parameters specified by the 'parameter_list' argument.
#' Only specify a list of parameters to the 'display_list' if you want to subset the calculated parameters returned as a result of this function. \cr
#' By default 'return_list' argument is empty, which means that this function will not append parameters passed from 'data' argument.
#' Only specify a list of parameters to the 'return_list' if you want to return them as a result of this function. \cr
#' If 'optimize_kel' is FALSE AND KEL is not one of the parameters (default or specified by 'parameter_list' argument) then
#' this functions will return a dataframe. If 'optimize_kel' is FALSE AND KEL is one of the parameters (default or specified by 'parameter_list' argument) then
#' this functions will return a list with 'data_out' and 'est_data'. If 'optimize_kel' is TRUE AND KEL is one of the parameters (default or specified
#' by 'parameter_list' argument) then this functions will return a list with 'data_out', 'optimized_kel_flag' and 'est_data'.
#' If 'optimize_kel' is TRUE AND KEL is not one of the parameters (default or specified by 'parameter_list' argument) then
#' this functions will return a list with 'data_out' and 'optimized_kel_flag'. \cr
#' Please note that this function does not contain all the features of a M1 SD computation, so it is recommended that you
#' use the parent function \code{\link{run_computation}}
#'
#' @param data The dataframe that contains the raw data
#' @param map The dataframe that contains the map data
#' @param method The method that will be used to calculate AUC (use either 1, 2, 3, or 4)\cr
#' \enumerate{
#' \item Linear-Log Trapezoidal Rule (default)
#' \item Linear Trapezoidal Rule
#' \item Log Trapezoidal Rule
#' \item Linear Up - Log Down Trapezoidal Rule
#' }
#' Note: check 'Methods' section below for more details \cr
#' @param model_regex The regular expression that identifies the model and dosing type (default: "^M1(SD)*?$")
#' @param parameter_list The list of parameters to calculate (empty by default)
#' @param return_list The list of parameters to return from the original data (empty by default)
#' @param raw_results_debug The value that determines the raw results is returned prior to unit conversion (logical value)
#' @param optimize_kel_debug The value that determines the optimize kel analysis is returned (logical value)
#' @param ... Additional arguments
#'
#' @section Returns:
#' \strong{Dataframe} \cr
#' \itemize{
#'  \item M1SD_Parameters: Calculated default/specified parameters
#' }
#' OR \cr
#' \strong{List} \cr
#' \itemize{
#'  \item data_out: Calculated default/specified M1SD Parameters
#'  \item optimized_kel_flag: Optimized KEL flag data used to calculate KEL based parameters
#'  \item est_data: Calculated Estimated Parameters
#' }
#'
#' @examples
#' #No appropriate examples
#'
#' @author
#' \itemize{
#'  \item \strong{Thomas Tensfeldt, Pfizer & Rudraya Technical Team}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{thomas.g.tensfeldt@pfizer.com}
#' }
#' @export
run_M1_SD_computation <- function(data = NULL, map = NULL, method = 1, model_regex = "^M1(SD)*?$", parameter_list = list(), return_list = list(), raw_results_debug = FALSE, optimize_kel_debug = FALSE, ...){
  function_name <- as.list(sys.call())[[1]]
  additional_inputs <- list(...)
  
  if(isTRUE("print_version" %in% names(additional_inputs))){
    if(isTRUE(additional_inputs$print_version)){
      opennca_version()
      cat(paste0("Computation Run Date/Time: ", Sys.time(), "\n"))
    }
  } else {
    opennca_version()
    cat(paste0("Computation Run Date/Time: ", Sys.time(), "\n"))
  }
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
  if(!is.list(return_list)){
    stop("Invalid list provided for 'return_list'! Please provide a valid list")
  }
    
  if(!(parameter_required("^SDEID$",names(data_data)))) {
    stop("Value for 'SDEID' provided via 'map' is not present in the dataset provided via 'data'")
  }

  auc_list <- c("AUCT", "AUCTDN")
  auc_par <- c("AUCT1_T2")
  regular_list <- c("CMAX", "CMIN", "CLAST", "CMAXDN", "TMAX", "TMIN", "TLAST", "TLAG", "KEL", "KELC0", "CEST", 
                    "KELTMLO", "KELTMHI", "KELNOPT", "KELRSQ", "KELRSQA", "THALF", "THALFF", "LASTTIME", "AUCALL", "AUCDN", "AUCLAST",
                    "AUCLASTC", "AUCLASTDN", "AUMCLAST", "AUCINFO", "AUCINFP", "AUCINFOC", "AUCINFPC", "AUCINFODN",
                    "AUCINFPDN", "AUMCINFO", "AUMCINFP", "MRTLAST", "MRTEVIFO", "MRTEVIFP", "AUCXPCTO", "AUCXPCTP",
                    "AUMCXPTO", "AUMCXPTP", "CLO", "CLFO", "CLFOW", "CLFP", "CLFPW", "VZFO", "VZFOW", "VZFP", "VZFPW")
  
  optional_list <- c("DOSEi", "TAUi", "TOLDi")
  opt_list <- c("DOSE1", "TAU1", "TOLD1")
  regular_int_type <- NULL
  auc_pair_check <- FALSE
    
  index1 <- data_data[,map_data$SDEID]
  auc_len <- max(tapply(index1, index1, length))-1
  reg_col <- sum(regular_list %in% parameter_list) + ifelse(any(c("KELRSQ","KELRSQA") %in% parameter_list), 1, 0)
  auc_col <- ifelse(sum(auc_list %in% parameter_list) == 1, 2, 0)

  aucpari <- grep('^AUC.([0-9]+?).T[1-2]$', names(map_data), ignore.case=TRUE, perl=TRUE)
  if(length(aucpari)>0) {
     auc_par_len <- floor(length(aucpari)/2)
     g <- names(map_data)[aucpari]
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

  doselist <- names(parameter_indices("^DOSELIST$", names(map_data), simplify=FALSE))
  dosenames <- unlist(strsplit(map_data[,doselist], ";"))
  dosevar <- as.character(map[,dosenames])
  if(!any(duplicated(as.character(unlist(dosevar))))){
    dosenames <- dosenames[!duplicated(as.character(unlist(dosevar)))]
  }
  
  ###comp_required <- list()
  ###disp_required <- list()
  ###plist <- parameter_list
  ###for(i in model_parameters()) {
  ###  rg <- parameter_regex(i)
  ###  pr <- parameter_required(rg, parameter_list=plist)
  ###  dp <- parameter_required(dependent_parameters(rg), plist)
  ###  comp_required[[i]] <- pr || dp
  ###  disp_required[[i]] <- pr
  ###}
  all_parameters <- model_parameters("m1sd")
  disp_required <- rep(FALSE, length(all_parameters))
  names(disp_required) <- all_parameters
  disp_required[names(disp_required) %in% parameter_list] <- TRUE
  
  if("FLGACCEPTKELCRIT" %in% names(map_data) && (("KEL" %in% parameter_list && "KELNOPT" %in% parameter_list) || "KELRSQ" %in% parameter_list)) {
    if(length(unlist(strsplit(as.character(map_data$FLGACCEPTKELCRIT), ","))) > 0){
      col <- col + 1
    }
  }
  if("LASTTIMEACCEPTCRIT" %in% names(map_data) && "LASTTIME" %in% parameter_list){
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
  elist <- c("PKDATAROWID", "SDEID","TIME","CEST_KEL","CEST_INT","CEST_EXT","CEST_C0","CEST_TLAST")
  est_data <- data.frame(matrix(ncol = length(elist), nrow = 0))
  tmp_est_data <- data.frame(matrix(ncol = length(elist), nrow = 0))
  names(est_data) <- elist
  names(tmp_est_data) <- elist
  
  if(parameter_required("^DOSE(i{1}|[0-9]*?)U$", names(map_data))) {
    vdoseu <- parameter_indices("^DOSE(i{1}|[0-9]*?)U$", names(map_data))
    vdoseu <- names(vdoseu)
    doseu_name <- as.character(map_data[, vdoseu][1])
    if(doseu_name %in% names(data_data)){
      if(length(grep("/", as.character(unique(data_data[, doseu_name])[1])) > 0)){
        dose_by_mass <- TRUE
      } else {
        dose_by_mass <- FALSE
      }
    } else {
      dose_by_mass <- FALSE
    }
  } else {
    dose_by_mass <- FALSE
  }
  
  if("MRSDEID" %in% names(map_data)){
    if(map_data$MRSDEID %in% names(data_data)){
      secondary <- TRUE
    } else {
      secondary <- FALSE
    }
  } else {
    secondary <- FALSE
  }
  if(isTRUE(secondary)){
    secondary_prereqs <- c('PKTERMPARENT', 'PKTERM', 'MW', 'METABOLITEPARAMETEREXCLUSIONLIST')
    secondary_mappings <- c('PKTERM', 'MW')
    if(!(all(secondary_prereqs %in% names(map_data)))){
      missing_prereqs <- secondary_prereqs[!(secondary_prereqs %in% names(map_data))]
      if(length(missing_prereqs) > 1){
        warning(paste0("'", missing_prereqs, "' is not present in the dataset provided via 'map'! Cannot compute secondary parameters!"))
      } else {
        missing_msg <- ""
        for(m in 1:length(missing_prereqs)){ 
          if(m == 1){
            missing_msg <- paste0(missing_msg, "'", missing_prereqs[i], "'")
          } else if(m == length(missing_prereqs)){
            missing_msg <- paste0(missing_msg, " and '", missing_prereqs[i], "'")
          } else {
            missing_msg <- paste0(missing_msg, ", '", missing_prereqs[i], "'")
          }
        }
        warning(paste0(missing_msg, " are not present in the dataset provided via 'map'! Cannot compute secondary parameters!"))
      }
      secondary <- FALSE
    } 
    secondary_values <- as.character(map_data[,secondary_mappings])
    if(!(all(secondary_values %in% names(data_data)))){
      missing_values <- secondary_values[!(secondary_values %in% names(data_data))]
      if(length(missing_values) > 1){
        warning(paste0("'", missing_values, "' is not present in the dataset provided via 'data'! Cannot compute secondary parameters!"))
      } else {
        missing_msg <- ""
        for(m in 1:length(missing_values)){ 
          if(m == 1){
            missing_msg <- paste0(missing_msg, "'", missing_values[i], "'")
          } else if(m == length(missing_values)){
            missing_msg <- paste0(missing_msg, " and '", missing_values[i], "'")
          } else {
            missing_msg <- paste0(missing_msg, ", '", missing_values[i], "'")
          }
        }
        warning(paste0(missing_msg, " are not present in the dataset provided via 'data'! Cannot compute secondary parameters!"))
      }
      secondary <- FALSE
    } 
  }
  if(isTRUE(secondary)){
    exclusion_list <- unlist(strsplit(map_data$METABOLITEPARAMETEREXCLUSIONLIST, ";"))
    #FEEDBACK: Commented the code to account for exclusion of metabolite exclusion list
    #Based of 3.0a scope item (Based on tc1606_M1SD)
    #disp_required[names(disp_required) %in% exclusion_list] <- FALSE
  }

  col_names <- c("SDEID")
  if(disp_required[["DOSE"]] || disp_required[["DOSEi"]]) {
    col_names <- c(col_names, dosenames)
    regular_int_type <- c(regular_int_type, dosenames)
  }
  if(disp_required[["DOSEC"]]) {
    col_names <- c(col_names, "DOSEC")
    regular_int_type <- c(regular_int_type, "DOSEC")
  }
  if(disp_required[["C0"]]) {
    col_names <- c(col_names, "C0")
    regular_int_type <- c(regular_int_type, "C0")
  }
  if(disp_required[["CMAX"]]) {
    col_names <- c(col_names, "CMAX")
    regular_int_type <- c(regular_int_type, "CMAX")
  }
  if(disp_required[["FLGACCEPTPREDOSE"]] && "FLGACCEPTPREDOSECRIT" %in% names(map_data)){
    col_names <- c(col_names, "FLGACCEPTPREDOSE")
  }
  if(disp_required[["CMIN"]]) {
    col_names <- c(col_names, "CMIN")
    regular_int_type <- c(regular_int_type, "CMIN")
  }
  if(disp_required[["CLAST"]]) {
    col_names <- c(col_names, "CLAST")
    regular_int_type <- c(regular_int_type, "CLAST")
  }
  if(disp_required[["CEST"]]) {
    col_names <- c(col_names, "CEST")
    regular_int_type <- c(regular_int_type, "CEST")
  }
  if(disp_required[["CMAXDN"]]) {
    col_names <- c(col_names, "CMAXDN")
    regular_int_type <- c(regular_int_type, "CMAXDN")
  }
  if(disp_required[["CMINDN"]]) {
    col_names <- c(col_names, "CMINDN")
    regular_int_type <- c(regular_int_type, "CMINDN")
  }
  if(disp_required[["TMAX"]]) {
    col_names <- c(col_names, "TMAX")
    regular_int_type <- c(regular_int_type, "TMAX")
  }
  if(disp_required[["FLGACCEPTTMAX"]] && "FLGEMESIS" %in% names(map_data)) {
    col_names <- c(col_names, "FLGACCEPTTMAX")
  }
  if(disp_required[["TMIN"]]) {
    col_names <- c(col_names, "TMIN")
    regular_int_type <- c(regular_int_type, "TMIN")
  }
  if(disp_required[["TLAST"]]) {
    col_names <- c(col_names, "TLAST")
    regular_int_type <- c(regular_int_type, "TLAST")
  }
  if(disp_required[["TLAG"]]) {
    col_names <- c(col_names, "TLAG")
    regular_int_type <- c(regular_int_type, "TLAG")
  }
  if(disp_required[["KEL"]]) {
    col_names <- c(col_names, "KEL")
    regular_int_type <- c(regular_int_type, "KEL")
  }
  if(disp_required[["KELC0"]]) {
    col_names <- c(col_names, "KELC0")
    regular_int_type <- c(regular_int_type, "KELC0")
  }
  if(disp_required[["KELTMLO"]]) {
    col_names <- c(col_names, "KELTMLO")
    regular_int_type <- c(regular_int_type, "KELTMLO")
  }
  if(disp_required[["KELTMHI"]]) {
    col_names <- c(col_names, "KELTMHI")
    regular_int_type <- c(regular_int_type, "KELTMHI")
  }
  if(disp_required[["KELNOPT"]]) {
    col_names <- c(col_names, "KELNOPT")
    regular_int_type <- c(regular_int_type, "KELNOPT")
  }
  if(disp_required[["KELR"]]) {
    col_names <- c(col_names, "KELR")
    regular_int_type <- c(regular_int_type, "KELR")
  }
  if(disp_required[["KELRSQ"]]) {
    col_names <- c(col_names, "KELRSQ")
    regular_int_type <- c(regular_int_type, "KELRSQ")
  }
  if(disp_required[["KELRSQA"]]) {
    col_names <- c(col_names, "KELRSQA")
    regular_int_type <- c(regular_int_type, "KELRSQA")
  }
  if(disp_required[["FLGACCEPTKEL"]] && "FLGACCEPTKELCRIT" %in% names(map_data)) {
    if(length(unlist(strsplit(as.character(map_data$FLGACCEPTKELCRIT), ","))) > 0){
      col_names <- c(col_names, "FLGACCEPTKEL")
    }
  }
  if(disp_required[["THALF"]]) {
    col_names <- c(col_names, "THALF")
    regular_int_type <- c(regular_int_type, "THALF")
  }
  if(disp_required[["THALFF"]]) {
    col_names <- c(col_names, "THALFF")
  }
  if(disp_required[["LASTTIME"]]) {
    col_names <- c(col_names, "LASTTIME")
    regular_int_type <- c(regular_int_type, "LASTTIME")
  }
  if(disp_required[["FLGACCEPTTAU"]] && "LASTTIMEACCEPTCRIT" %in% names(map_data)) {
    col_names <- c(col_names, "FLGACCEPTTAU")
  }
  if(disp_required[["AUCALL"]]) {
    col_names <- c(col_names, "AUCALL")
    regular_int_type <- c(regular_int_type, "AUCALL")
  }
  if(disp_required[["AUCALLDN"]]) {
    col_names <- c(col_names, "AUCALLDN")
    regular_int_type <- c(regular_int_type, "AUCALLDN")
  }
  if(disp_required[["AUCLAST"]]) {
    col_names <- c(col_names, "AUCLAST")
    regular_int_type <- c(regular_int_type, "AUCLAST")
  }
  if(disp_required[["AUCLASTC"]]) {
    col_names <- c(col_names, "AUCLASTC")
    regular_int_type <- c(regular_int_type, "AUCLASTC")
  }
  if(disp_required[["AUCLASTDN"]]) {
    col_names <- c(col_names, "AUCLASTDN")
    regular_int_type <- c(regular_int_type, "AUCLASTDN")
  }
  if(disp_required[["AUMCLAST"]]) {
    col_names <- c(col_names, "AUMCLAST")
    regular_int_type <- c(regular_int_type, "AUMCLAST")
  }
  if(disp_required[["AUCT"]] && auc_len > 1) {
    col_names <- c(col_names, rep(paste0("AUC",1:auc_len)))
    regular_int_type <- c(regular_int_type, paste0("AUC",1:auc_len))
  }
  if(disp_required[["AUCTDN"]] && auc_len > 1){
    col_names <- c(col_names, rep(paste0("AUC",1:auc_len,"DN")))
    regular_int_type <- c(regular_int_type, paste0("AUC",1:auc_len,"DN"))
  }
  if((disp_required[["AUCT"]] || disp_required[["AUCTDN"]]) && auc_len > 1){
    col_names <- c(col_names, rep(paste0("AUCINT",1:auc_len)))
  }
  if(disp_required[["AUCT1_T2"]] && auc_pair_check) {
    col_names <- c(col_names, rep(paste0("AUC", map_data[,rep(paste0("AUC.", 1:auc_par_len, ".T1"))], "_", map_data[,rep(paste0("AUC.", 1:auc_par_len, ".T2"))])))
    regular_int_type <- c(regular_int_type, rep(paste0("AUC", map_data[,rep(paste0("AUC.", 1:auc_par_len, ".T1"))], "_", map_data[,rep(paste0("AUC.", 1:auc_par_len, ".T2"))])))
  }
  if(disp_required[["AUCINFO"]]) {
    col_names <- c(col_names, "AUCINFO")
    regular_int_type <- c(regular_int_type, "AUCINFO")
  }
  if(disp_required[["AUCINFP"]]) {
    col_names <- c(col_names, "AUCINFP")
    regular_int_type <- c(regular_int_type, "AUCINFP")
  }
  if(disp_required[["AUCINFOC"]]) {
    col_names <- c(col_names, "AUCINFOC")
    regular_int_type <- c(regular_int_type, "AUCINFOC")
  }
  if(disp_required[["AUCINFPC"]]) {
    col_names <- c(col_names, "AUCINFPC")
    regular_int_type <- c(regular_int_type, "AUCINFPC")
  }
  if(disp_required[["AUCINFODN"]]) {
    col_names <- c(col_names, "AUCINFODN")
    regular_int_type <- c(regular_int_type, "AUCINFODN")
  }
  if(disp_required[["AUCINFPDN"]]) {
    col_names <- c(col_names, "AUCINFPDN")
    regular_int_type <- c(regular_int_type, "AUCINFPDN")
  }
  if(disp_required[["AUMCINFO"]]) {
    col_names <- c(col_names, "AUMCINFO")
    regular_int_type <- c(regular_int_type, "AUMCINFO")
  }
  if(disp_required[["AUMCINFP"]]) {
    col_names <- c(col_names, "AUMCINFP")
    regular_int_type <- c(regular_int_type, "AUMCINFP")
  }
  if(disp_required[["MRTLAST"]]) {
    col_names <- c(col_names, "MRTLAST")
    regular_int_type <- c(regular_int_type, "MRTLAST")
  }
  if(disp_required[["MRTEVIFO"]]) {
    col_names <- c(col_names, "MRTEVIFO")
    regular_int_type <- c(regular_int_type, "MRTEVIFO")
  }
  if(disp_required[["MRTEVIFP"]]) {
    col_names <- c(col_names, "MRTEVIFP")
    regular_int_type <- c(regular_int_type, "MRTEVIFP")
  }
  if(disp_required[["AUCXPCTO"]]) {
    col_names <- c(col_names, "AUCXPCTO")
    regular_int_type <- c(regular_int_type, "AUCXPCTO")
  }
  if(disp_required[["AUCXPCTP"]]) {
    col_names <- c(col_names, "AUCXPCTP")
    regular_int_type <- c(regular_int_type, "AUCXPCTP")
  }
  if(disp_required[["AUMCXPTO"]]) {
    col_names <- c(col_names, "AUMCXPTO")
    regular_int_type <- c(regular_int_type, "AUMCXPTO")
  }
  if(disp_required[["AUMCXPTP"]]) {
    col_names <- c(col_names, "AUMCXPTP")
    regular_int_type <- c(regular_int_type, "AUMCXPTP")
  }
  if(disp_required[["CLFO"]]) {
    col_names <- c(col_names, "CLFO")
    regular_int_type <- c(regular_int_type, "CLFO")
  }
  if(disp_required[["CLFOW"]]) {
    col_names <- c(col_names, "CLFOW")
    regular_int_type <- c(regular_int_type, "CLFOW")
  }
  if(disp_required[["CLFP"]]) {
    col_names <- c(col_names, "CLFP")
    regular_int_type <- c(regular_int_type, "CLFP")
  }
  if(disp_required[["CLFPW"]]) {
    col_names <- c(col_names, "CLFPW")
    regular_int_type <- c(regular_int_type, "CLFPW")
  }
  if(disp_required[["VZFO"]]) {
    col_names <- c(col_names, "VZFO")
    regular_int_type <- c(regular_int_type, "VZFO")
  }
  if(disp_required[["VZFOW"]]) {
    col_names <- c(col_names, "VZFOW")
    regular_int_type <- c(regular_int_type, "VZFOW")
  }
  if(disp_required[["VZFP"]]) {
    col_names <- c(col_names, "VZFP")
    regular_int_type <- c(regular_int_type, "VZFP")
  }
  if(disp_required[["VZFPW"]]) {
    col_names <- c(col_names, "VZFPW")
    regular_int_type <- c(regular_int_type, "VZFPW")
  }
  if(isTRUE(secondary)){
    if(disp_required[["MRAUCINFO"]]) {
      col_names <- c(col_names, "MRAUCINFO")
      regular_int_type <- c(regular_int_type, "MRAUCINFO")
    }
    if(disp_required[["MRAUCINFP"]]) {
      col_names <- c(col_names, "MRAUCINFP")
      regular_int_type <- c(regular_int_type, "MRAUCINFP")
    }
    if(disp_required[["MRAUCLAST"]]) {
      col_names <- c(col_names, "MRAUCLAST")
      regular_int_type <- c(regular_int_type, "MRAUCLAST")
    }
    if(disp_required[["MRCMAX"]]) {
      col_names <- c(col_names, "MRCMAX")
      regular_int_type <- c(regular_int_type, "MRCMAX")
    }
  }
  col_names <- c(col_names, rep(paste0("CONC",1:(auc_len+1))), rep(paste0("CONCTIME",1:(auc_len+1))))
  regular_int_type <- c(regular_int_type, rep(paste0("CONC",1:(auc_len+1))), rep(paste0("CONCTIME",1:(auc_len+1))))
  if(disp_required[["TAU"]] && parameter_required(opt_list[2], names(map_data))) {
    if(parameter_required(opt_list[2], names(map_data))) { 
        if(map_data[, opt_list[2]] %in% names(data_data)) {
            col_names <- c(col_names, "TAU1")
            regular_int_type <- c(regular_int_type, "TAU1")
        }
    }
  }
  if(disp_required[["TOLD"]] && parameter_required(opt_list[3], names(map_data))) {
      if(parameter_required(opt_list[3], names(map_data))) { 
          if(map_data[, opt_list[3]] %in% names(data_data)) {
              col_names <- c(col_names, "TOLD1")
              regular_int_type <- c(regular_int_type, "TOLD1")
          }
      }
  }
  if(disp_required[["CTOLDesti"]]){
    col_names <- c(col_names, rep(paste0("CTOLDest1")))
    regular_int_type <- c(regular_int_type, rep(paste0("CTOLDest1")))
  }
  computation_df <- data.frame(matrix(ncol = length(col_names), nrow = length(unique(data_data[,map_data$SDEID]))))
  names(computation_df) <- col_names

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
      warning("Flag 'LASTTIMEACCEPTCRIT' does not have a valid form! Please make sure it contains '*'")
    }
    if(opt_list[2] %in% names(map_data)){
      if(!map_data[, opt_list[2]] %in% names(data_data)) {
        warning("Flag 'FLGACCEPTTAU' cannot be computed if 'TAUi' is not provided")
      }
    } else {
      warning("Flag 'FLGACCEPTTAU' cannot be computed if 'TAUi' is not provided")
    }
  } else {
    last_crit_factor <- NA
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
  if(disp_required[["FLGACCEPTTMAX"]] && !("FLGEMESIS" %in% names(map_data) && map_data$FLGEMESIS %in% names(data_data))){
    warning("Flag 'FLGEMESIS' is not present in the dataset")
  }
  if(disp_required[["FLGACCEPTPREDOSE"]] && !("FLGACCEPTPREDOSECRIT" %in% names(map_data))){
    warning("Flag 'FLGACCEPTPREDOSECRIT' is not present in the dataset")
  } else if("FLGACCEPTPREDOSECRIT" %in% names(map_data)){
    if(!("CMAX" %in% parameter_list)){
      warning("Flag 'FLGACCEPTPREDOSE' cannot be computed if 'CMAX' is not part of the calculated parameters")
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
      if(map_data[,"OPTIMIZEKEL"] != 1 && map_data[,"OPTIMIZEKEL"] != 2){
        warning("Map 'OPTIMIZEKEL' does not have a valid value! Not using KEL optimization for this computation")
        optimize_kel <- FALSE
        optimize_kel_method <- as.character(as.numeric(map_data[,"OPTIMIZEKEL"]))
      } else {
        optimize_kel <- as.logical(as.numeric(map_data[,"OPTIMIZEKEL"]))
        optimize_kel_method <- as.character(as.numeric(map_data[,"OPTIMIZEKEL"]))
      }
    } else {
      optimize_kel <- FALSE
      optimize_kel_method <- as.character(as.numeric(map_data[,"OPTIMIZEKEL"]))
    }
  } else {
    optimize_kel <- FALSE
    optimize_kel_method <- NA
  }
  if(isTRUE(optimize_kel)){
    ###comp_required[["KEL"]] <- TRUE
    ###comp_required[["TMAX"]] <- TRUE
    ###comp_required[["TLAST"]] <- TRUE
    ###comp_required[["CMAX"]] <- TRUE
    ###comp_required[["CLAST"]] <- TRUE 
    ###comp_required[["AUCLAST"]] <- TRUE
    if(isTRUE(optimize_kel_debug)){
      debug_idx <- 1
      if("AUCXPCTO" %in% flag_df$VAR){
        kel_debug <- data.frame("SDEID" = numeric(), "TIME" = numeric(), "CONC" = numeric(), "KEL" = numeric(), "KELNOPT" = numeric(), "KELRSQ" = numeric(), "AUCXPCTO" = numeric(), "KELNOPT_W" = numeric(), "KELRSQ_W" = numeric(), "AUCXPCTO_W" = numeric(), "TOTAL_W" = numeric())
      } else if("AUCXPCTP" %in% flag_df$VAR){
        kel_debug <- data.frame("SDEID" = numeric(), "TIME" = numeric(), "CONC" = numeric(), "KEL" = numeric(), "KELNOPT" = numeric(), "KELRSQ" = numeric(), "AUCXPCTP" = numeric(), "KELNOPT_W" = numeric(), "KELRSQ_W" = numeric(), "AUCXPCTP_W" = numeric(), "TOTAL_W" = numeric())
      }
    }
  }
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
  
  ###if(isTRUE(optimize_kel) && (!comp_required[["TMAX"]] || !comp_required[["TLAST"]] || !comp_required[["CMAX"]] || !comp_required[["CLAST"]] || !comp_required[["AUCLAST"]] ||
  ###   !"FLGACCEPTKELCRIT" %in% names(map_data) || !"FLGEXKEL" %in% names(map_data) || !map_data$FLGEXKEL %in% names(data_data))){
  ###  warning("Kel optimization cannot be performed because 'TMAX', 'TLAST', 'CMAX', 'CLAST', 'AUCLAST' are not part of the calculated parameters AND Flag 'FLGACCEPTKELCRIT' and Flag 'FLGEXKEL' are not present in the dataset")
  ###}
  
  if(isTRUE(optimize_kel) && isTRUE(optimize_kel_method != "1" && optimize_kel_method != "2")){
    optimize_kel <- FALSE
    warning("Flag 'OPTIMIZEKEL' does not have a valid value! Please try again with numeric value (either 1 or 2)")
  }

  ###if(isTRUE(optimize_kel) && comp_required[["TMAX"]] && comp_required[["TLAST"]] && comp_required[["CMAX"]] && comp_required[["CLAST"]] && comp_required[["AUCLAST"]] &&
  ###   "FLGACCEPTKELCRIT" %in% names(map_data) && "FLGEXKEL" %in% names(map_data) && map_data$FLGEXKEL %in% names(data_data)){
  if(isTRUE(optimize_kel) && "FLGACCEPTKELCRIT" %in% names(map_data) && "FLGEXKEL" %in% names(map_data) && map_data$FLGEXKEL %in% names(data_data)){
    kel_flag_optimized <- integer()
    kel_opt_warning <- FALSE
  }
  
  ###if(disp_required[["FLGACCEPTTMAX"]] && "FLGEMESIS" %in% names(map_data) && map_data$FLGEMESIS %in% names(data_data)){
  ###  comp_required[["TMAX"]] <- TRUE
  ###}

  for(i in 1:length(unique(data_data[,map_data$SDEID]))){
    tryCatch({
      tmp_df <- data_data[data_data[,map_data$SDEID] == unique(data_data[,map_data$SDEID])[i],]
      default_df <- tmp_df
      suppressWarnings(default_df <- default_df[order(as.numeric(default_df[,map_data$TIME])),])
      tmp_df[,map_data$CONC] <- as.numeric(tmp_df[,map_data$CONC])
      tmp_df[,map_data$TIME] <- as.numeric(tmp_df[,map_data$TIME])
      tmp_df <- tmp_df[order(tmp_df[,map_data$TIME]),]
      
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
        tmp_df <- tmp_df[rep(FALSE, nrow(tmp_df)),]
        
        warning(paste0("Removing SDEID: '", unique(data_data[,map_data$SDEID])[i], "' due to duplicate TIME but different CONC values"))
      }
      cest_tmp <- data.frame("CONC" = numeric(), "TIME" = numeric(), "INT_EXT" = character())
      norm_bs <- ifelse("NORMBS" %in% names(map_data), ifelse(map_data$NORMBS %in% names(tmp_df), unique(tmp_df[,map_data$NORMBS])[1], NA), NA)
      tmp_dose <- unique(tmp_df[, dosevar])[1]
      
      if("INCLUDEINTERPOLATION" %in% names(map_data)){
        map_data[,"INCLUDEINTERPOLATION"] <- as.numeric(map_data[,"INCLUDEINTERPOLATION"])
        interpolation <- ifelse((map_data[,"INCLUDEINTERPOLATION"] == 0 || map_data[,"INCLUDEINTERPOLATION"] == 1), as.logical(as.numeric(map_data[,"INCLUDEINTERPOLATION"])), FALSE)
      } else {
        interpolation <- FALSE
      }
      if("INCLUDEEXTRAPOLATION" %in% names(map_data)){
        map_data[,"INCLUDEEXTRAPOLATION"] <- as.numeric(map_data[,"INCLUDEEXTRAPOLATION"])
        extrapolation <- ifelse((map_data[,"INCLUDEEXTRAPOLATION"] == 0 || map_data[,"INCLUDEEXTRAPOLATION"] == 1), as.logical(as.numeric(map_data[,"INCLUDEEXTRAPOLATION"])), FALSE)
      } else {
        extrapolation <- FALSE
      }
      
      dose_inf <- dof(tmp_df, map_data, dof_name = "DOF1")
      conc_check <- TRUE
      time_check <- TRUE
      suppressWarnings(blq_lloq_check <- default_df[,map_data$CONC][is.na(default_df[,map_data$CONC])])
      if(isTRUE(length(blq_lloq_check) > 0)){
        if(!isTRUE(all(toupper(blq_lloq_check) %in% c("BLQ", "LLOQ", NA)))){
          warning(paste0("Parameters not generated due to invalid concentration values for SDEID: '", unique(data_data[,map_data$SDEID])[i], "'"))
          conc_check <- FALSE
        }
      }
      suppressWarnings(na_time_check <- default_df[,map_data$TIME][is.na(default_df[,map_data$TIME])])
      if(isTRUE(length(na_time_check) > 0)){
        warning(paste0("Parameters not generated due to invalid time values for SDEID: '", unique(data_data[,map_data$SDEID])[i], "'"))
        time_check <- FALSE
      }
      
      if(isTRUE(nrow(tmp_df) > 0 & all(tmp_df[,map_data$TIME][!is.na(tmp_df[,map_data$TIME])] >= 0)) & isTRUE(time_check) & isTRUE(conc_check)){
        orig_time <- tmp_df[,map_data$TIME]
        orig_conc <- tmp_df[,map_data$CONC]
        
        #FEEDBACK: Default TOLD value for SD models is 0, Based of 3.10 scope item (Based on tc1510_M1SD)
        tmp_told <- ifelse(opt_list[3] %in% names(map_data), ifelse(map_data[, opt_list[3]] %in% names(tmp_df), as.numeric(tmp_df[, map_data[, opt_list[3]]][1]), 0), 0)
        ctold_exists <- FALSE 
        if(tmp_told %in% tmp_df[,map_data$NOMTIME]){
          idx <- which(tmp_df[,map_data$NOMTIME] == tmp_told)
          tmp_ctold <- tmp_df[,map_data$CONC][length(idx)]
          if(!is.na(tmp_ctold)){
            ctold_exists <- TRUE
          }
        }
        tmp_tau <- ifelse(opt_list[2] %in% names(map_data), ifelse(map_data[, opt_list[2]] %in% names(tmp_df), as.numeric(tmp_df[, map_data[, opt_list[2]]][1]), NA), NA)
        tmp_tau <- tmp_tau + tmp_told
        ctau_exists <- FALSE 
        if(tmp_tau %in% tmp_df[,map_data$NOMTIME]){
          idx <- which(tmp_df[,map_data$NOMTIME] == tmp_tau)
          tmp_ctau <- tmp_df[,map_data$CONC][length(idx)]
          if(!is.na(tmp_ctau)){
            ctau_exists <- TRUE
          }
        }
        if(!isTRUE(ctold_exists) && !is.na(tmp_told)){
          tmp_conc <- c(NA, tmp_df[,map_data$CONC])
          if(tmp_told %in% tmp_df[,map_data$NOMTIME]){
            tmp_time <- c(tmp_df[,map_data$TIME])
          } else {
            tmp_time <- c(tmp_told, tmp_df[,map_data$TIME])
          }
          est_tmp <- estimate_told_concentration(conc = tmp_conc, time = tmp_time, interpolate = TRUE, extrapolate = TRUE, auc_method = "LIN", model = "M1", dosing_type = "SD", told = tmp_told, orig_conc = orig_conc, orig_time = orig_time)
          tmp_conc <- est_tmp[[1]]
          ctold_est <- tmp_conc[1]
        } else {
          tmp_conc <- tmp_df[,map_data$CONC]
          tmp_time <- tmp_df[,map_data$TIME]
          ctold_est <- NA
        }
        
        obs_c_0 <- c0(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        ###if(comp_required[["DOSEC"]]) {
          dose_c <- dosec(data = tmp_df, map = map_data)
        ###}
        ###if(comp_required[["CMAX"]]) {
          c_max <- cmax(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        ###}
        ###if(comp_required[["CLAST"]]) {
          c_last <- clast(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        ###}
        ###if(comp_required[["TMAX"]]) {
          t_max <- tmax(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        ###}
        ###if(comp_required[["TLAST"]]) {
          t_last <- tlast(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        ###} else {
        ###  t_last <- NULL
        ###}
        ###if(comp_required[["AUCLAST"]]) {
          auclast <- auc_last(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, exflag = auc_flag, t_last = t_last, t_max = t_max)
        ###}
        ###if(isTRUE(optimize_kel) && comp_required[["TMAX"]] && comp_required[["TLAST"]] && comp_required[["CMAX"]] && comp_required[["CLAST"]] && comp_required[["AUCLAST"]] &&
        ###   "FLGACCEPTKELCRIT" %in% names(map_data) && "FLGEXKEL" %in% names(map_data) && map_data$FLGEXKEL %in% names(data_data)){
        if(isTRUE(optimize_kel) && "FLGACCEPTKELCRIT" %in% names(map_data) && "FLGEXKEL" %in% names(map_data) && map_data$FLGEXKEL %in% names(data_data)){
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
                  if(s_time <= e_time && e_time <= length(orig_time)){
                    tmp_time <- orig_time[s_time:e_time]
                  } else {
                    tmp_time <- c()
                  }
                }
                if(!is.null(c_max) && !is.na(c_max) && !is.null(c_last) && !is.na(c_last)){
                  s_conc <- match(c_max, orig_conc)+1
                  e_conc <- match(c_last, orig_conc)
                  if(s_conc <= e_conc && e_conc <= length(orig_conc)){
                    tmp_conc <- orig_conc[s_conc:e_conc]
                  } else {
                    tmp_conc <- c()
                  } 
                }
              } else {
                if(!is.null(t_max) && !is.na(t_max) && !is.null(t_last) && !is.na(t_last)){
                  s_time <- match(t_max, orig_time)
                  e_time <- match(t_last, orig_time)
                  if(s_time <= e_time && e_time <= length(orig_time)){
                    tmp_time <- orig_time[s_time:e_time]
                  } else {
                    tmp_time <- c()
                  }
                }
                if(!is.null(c_max) && !is.na(c_max) && !is.null(c_last) && !is.na(c_last)){
                  s_conc <- match(c_max, orig_conc)
                  e_conc <- match(c_last, orig_conc)
                  if(s_conc <= e_conc && e_conc <= length(orig_conc)){
                    tmp_conc <- orig_conc[s_conc:e_conc]
                  } else {
                    tmp_conc <- c()
                  }
                }
              }
            }
          } else {
            if(!is.null(t_max) && !is.na(t_max) && !is.null(t_last) && !is.na(t_last)){
              s_time <- match(t_max, orig_time)+1
              e_time <- match(t_last, orig_time)
              if(s_time <= e_time && e_time <= length(orig_time)){
                tmp_time <- orig_time[s_time:e_time]
              } else {
                tmp_time <- c()
              }
            }
            if(!is.null(c_max) && !is.na(c_max) && !is.null(c_last) && !is.na(c_last)){
              s_conc <- match(c_max, orig_conc)+1
              e_conc <- match(c_last, orig_conc)
              if(s_conc <= e_conc && e_conc <= length(orig_conc)){
                tmp_conc <- orig_conc[s_conc:e_conc]
              } else {
                tmp_conc <- c()
              }
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
              if(isTRUE(optimize_kel_method == "1")){
                idx <- c(1:length(tmp_time))
                ulist <- list(idx)
                
                if(length(tmp_time) >= kel_n){
                  for (j in kel_n:(length(tmp_time)-1) ){
                    fit <- lm( tmp_conc[idx] ~ tmp_time[idx] )
                    idx <- idx[-which(abs(residuals(fit)) == max(abs(residuals(fit))))]
                    ulist <- c(ulist,list(idx))
                  }
                }
              } else if(isTRUE(optimize_kel_method == "2")){
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
            }
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
            first_kel_saved <- FALSE
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
                  if(kel_opt > saved_kel_opt || (!isTRUE(first_kel_saved) && kel_opt >= saved_kel_opt)){
                    if(!isTRUE(first_kel_saved) && kel_opt >= saved_kel_opt){
                      first_kel_saved <- TRUE
                    }
                    if(isTRUE("KEL" %in% flag_df$VAR)){
                      if(isTRUE(kel_tmp > kel_val)){
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
            } else {
              if(isTRUE(optimize_kel_debug)){
                kel_debug[debug_idx,] <- c(unique(data_data[,map_data$SDEID])[i], as.character(paste0(c(), sep = ", ", collapse = "")), as.character(paste0(c(), sep = ", ", collapse = "")), NA, 0, NA, NA, NA, NA, NA, NA)
                debug_idx <- 1 + debug_idx
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

        ###if(comp_required[["CMIN"]]) {
          c_min <- cmin(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        ###}
        ###if(comp_required[["CMAXDN"]]) {
          cmaxdn <- cmax_dn(cmax = c_max, dose = tmp_dose)
        ###}
        ###if(comp_required[["CMINDN"]]){
          cmindn <- cmin_dn(cmin = c_min, dose = tmp_dose)
        ###}
        ###if(comp_required[["TMIN"]]) {
          t_min <- tmin(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        ###}
        ###if(comp_required[["TLAG"]]) {
          t_lag <- tlag(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        ###}
        ###if(comp_required[["KEL"]] || comp_required[["KELC0"]] || comp_required[["KELTMLO"]] || comp_required[["KELTMHI"]] || comp_required[["KELNOPT"]] || comp_required[["THALF"]] || comp_required[["THALF"]]) {
          span_ratio <- ifelse("SPANRATIOCRIT" %in% names(map_data), suppressWarnings(as.numeric(map_data$SPANRATIOCRIT)), NA)
          kel_v <- kel(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], exflag = kel_flag, spanratio = span_ratio)
        ###} else {
        ###  kel_v <- NULL
        ###}
        ###if(comp_required[["KELR"]] || comp_required[["KELRSQ"]] || comp_required[["KELRSQA"]]) {
          kelr_v <- kel_r(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], exflag = kel_flag)
        ###}
        ###if(comp_required[["LASTTIME"]]) {
          last_time <- lasttime(conc = default_df[,map_data$CONC], time = default_df[,map_data$TIME])
        ###}
        ###if(comp_required[["CEST"]] || parameter_required("KEL", names(kel_v)) || parameter_required("KELC0", names(kel_v))) {
          c_est <- cest(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], kelflag=kel_flag, t_last=t_last, spanratio=span_ratio, kel=kel_v[["KEL"]], kelc0=kel_v[["KELC0"]])
        ###}
        ###if(comp_required[["AUCALL"]]) {
          aucall <- auc_all(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, exflag = auc_flag, t_max = t_max)
        ###}
        ###if(comp_required[["AUCALLDN"]]) {
          aucalldn <- auc_dn(auc = aucall, dose = tmp_dose)
        ###}
        ###if(comp_required[["AUCLASTC"]] && parameter_required("KEL", names(kel_v))) {
          auclast_c <- auc_lastc(kel = kel_v[["KEL"]], auclast = auclast, c0 = obs_c_0, tlast = t_last)
        ###}
        ###if(comp_required[["AUCLASTDN"]]) {
          auclastdn <- auc_dn(auc = auclast, dose = tmp_dose)
        ###}
        ###if(comp_required[["AUMCLAST"]]) {
          aumclast <- aumc_last(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, exflag = auc_flag, t_max = t_max)
        ###}

        ###if((comp_required[["AUCT"]] || comp_required[["AUCTDN"]]) && auc_len > 1) {
        if(auc_len > 1) {
          auct <- NULL
          auctdn <- NULL
          auc_int <- NULL
          for(t in 2:(auc_len+1)){ 
            tmp <- auc_t1_t2(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], t1 = tmp_df[,map_data$TIME][1], t2 = tmp_df[,map_data$TIME][t], method = method, exflag = auc_flag, t_max = t_max)
            tmp_dn <- auc_dn(auc = tmp, dose = tmp_dose)
            if(!is.na(unique(tmp_df[,map_data$TIME])[1]) && !is.na(unique(tmp_df[,map_data$TIME])[t])){
              tmp_int <- paste0(unique(tmp_df[,map_data$TIME])[1], "_", unique(tmp_df[,map_data$TIME])[t])
            } else {
              tmp_int <- NA
            }

            ###if(comp_required[["AUCT"]]){
              if(is.null(auct)){
                auct <- tmp
              } else {
                auct <- c(auct, tmp)
              }
            ###}
            ###if(comp_required[["AUCTDN"]]){
              if(is.null(auctdn)){
                auctdn <- tmp_dn
              } else {
                auctdn <- c(auctdn, tmp_dn)
              }
            ###}
            if(is.null(auc_int)){
              auc_int <- tmp_int
            } else {
              auc_int <- c(auc_int, tmp_int)
            }
          }
          ###if(comp_required[["AUCT"]]){
            if(length(auct) < auc_col) {
              auct <- c(auct, rep(NA, (auc_col - length(auct))))
            }
          ###}
          ###if(comp_required[["AUCTDN"]]){
            if(length(auctdn) < auc_col) {
              auctdn <- c(auctdn, rep(NA, (auc_col - length(auctdn))))
            }
          ###}
          if(length(auc_int) < auc_col) {
            auc_int <- c(auc_int, rep(NA, (auc_col - length(auc_int))))
          }
        }
      
        ###if(comp_required[["AUCT1_T2"]] && auc_pair_check) {
        if(auc_pair_check) {
          auct1_t2 <- NULL
          auct1_t2_names <- c(rep(paste0("AUC.", 1:auc_par_len, ".T1")), rep(paste0("AUC.", 1:auc_par_len, ".T2")))
          if(!all(auct1_t2_names %in% names(map_data))){
            par_col <- rep(paste0("'", auct1_t2_names[!auct1_t2_names %in% names(map_data)], "'"))
            stop(paste0("Dataset provided via 'map' does not contain the required columns for partial areas ", par_col))
          }
          if((isTRUE(interpolation) || isTRUE(extrapolation))){
            tmp_told <- 0
          }
          for(t in 1:(auc_par_len)){
            if(!(is.numeric(as.numeric(map_data[, paste0("AUC.", t, ".T1")])) && is.numeric(as.numeric(map_data[, paste0("AUC.", t, ".T2")])))){
              stop(paste0("'AUC.", t, ".T1' and/or 'AUC.", t, ".T2' value provided via 'map' is not a numeric value"))
            }
            auc_t1 <- as.numeric(map_data[, paste0("AUC.", t, ".T1")])
            auc_t2 <- as.numeric(map_data[, paste0("AUC.", t, ".T2")])
            
            if((isTRUE(interpolation) || isTRUE(extrapolation))){
              tmp <- auc_t1_t2(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], t1 = auc_t1, t2 = auc_t2, method = method, exflag = auc_flag, t_max = t_max, interpolate = interpolation, extrapolate = extrapolation, model = "M1", dosing_type = "SD", told = tmp_told, kel = kel_v, orig_conc = orig_conc, orig_time = orig_time, includeNA = TRUE)
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
            
            if(is.null(auct1_t2)){
              auct1_t2 <- tmp_auc
            } else {
              auct1_t2 <- c(auct1_t2, tmp_auc)
            }
          }
        }

        ###if(comp_required[["AUCINFO"]]) {
          aucinf_o <- auc_inf_o(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag, auclast = auclast, c_last = c_last, kel = kel_v)
        ###}
        ###if(comp_required[["AUCINFOC"]] && parameter_required("KEL", names(kel_v))) {
          aucinf_oc <- auc_inf_oc(kel = kel_v[["KEL"]], aucinfo = aucinf_o, c0 = obs_c_0)
        ###}
        ###if(comp_required[["AUCINFODN"]]) {
          aucinfo_dn <- auc_dn(auc = aucinf_o, dose = tmp_dose)
        ###}
        ###if(comp_required[["AUCINFP"]]) {
          aucinf_p <- auc_inf_p(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag, auclast = auclast, t_last = t_last, kel = kel_v)
        ###}
        ###if(comp_required[["AUCINFPC"]] && parameter_required("KEL", names(kel_v))) {
          aucinf_pc <- auc_inf_pc(kel = kel_v[["KEL"]], aucinfp = aucinf_p, c0 = obs_c_0)
        ###}
        ###if(comp_required[["AUCINFPDN"]]) {
          aucinfp_dn <- auc_dn(auc = aucinf_p, dose = tmp_dose)
        ###}
        ###if(comp_required[["AUMCINFO"]]) {
          aumcinf_o <- aumc_inf_o(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag, aumclast = aumclast, c_last = c_last, t_last = t_last, kel = kel_v)
        ###}
        ###if(comp_required[["AUMCINFP"]]) {
          aumcinf_p <- aumc_inf_p(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag, aumclast = aumclast, t_last = t_last, kel = kel_v)
        ###}
        ###if(comp_required[["MRTLAST"]]) {
          mrtlast <- mrt_last(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, model = "M1", dof = dose_inf, auclast = auclast, aumclast = aumclast)
        ###}
        ###if(comp_required[["MRTEVIFO"]]) {
          mrto <- mrt_evif_o(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, parameter = "SD", kelflag = kel_flag, aucflag = auc_flag, aucinfo = aucinf_o, aumcinfo = aumcinf_o)
        ###}
        ###if(comp_required[["MRTEVIFP"]]) {
          mrtp <- mrt_evif_p(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, parameter = "SD", kelflag = kel_flag, aucflag = auc_flag, aucinfp = aucinf_p, aumcinfp = aumcinf_p)
        ###}
        ###if(comp_required[["AUCXPCTO"]]) {
          aucxpcto <- auc_XpctO(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag, auc_info = aucinf_o, auclast = auclast)
        ###}
        ###if(comp_required[["AUCXPCTP"]]) {
          aucxpctp <- auc_XpctP(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag, auc_infp = aucinf_p, auclast = auclast)
        ###}
        ###if(comp_required[["AUMCXPTO"]]) {
          aumcxpto <- aumc_XpctO(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag, aumcinfo = aumcinf_o, aumclast = aumclast)
        ###}
        ###if(comp_required[["AUMCXPTP"]]) {
          aumcxptp <- aumc_XpctP(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag, aumcinfp = aumcinf_p, aumclast = aumclast)
        ###}
        ###if(comp_required[["CLFO"]]) {
        ###  clf_o <- clfo(aucinfo = aucinf_o, dose = dose_c)
        ###}
        if(isTRUE(dose_by_mass)){
          clf_o <- NA
        } else {
          clf_o <- clfo(aucinfo = aucinf_o, dose = dose_c)
        }
        ###if(comp_required[["CLFOW"]]) {
        ###  clf_ow <- clfow(clfo = clf_o, normbs = norm_bs)
        ###}
        if(isTRUE(dose_by_mass)){
          clf_ow <- clfow(clfo = unique(tmp_df[, dosevar])[1], normbs = aucinf_o)
        } else {
          clf_ow <- clfow(clfo = clf_o, normbs = norm_bs)
        }
        ###if(comp_required[["CLFP"]]) {
        ###  clf_p <- clfp(aucinfp = aucinf_p, dose = dose_c)
        ###}
        if(isTRUE(dose_by_mass)){
          clf_p <- NA
        } else {
          clf_p <- clfp(aucinfp = aucinf_p, dose = dose_c)
        }
        ###if(comp_required[["CLFPW"]]) {
        ###  clf_pw <- clfpw(clfp = clf_p, normbs = norm_bs)
        ###}
        if(isTRUE(dose_by_mass)){
          clf_pw <- clfpw(clfp = unique(tmp_df[, dosevar])[1], normbs = aucinf_p)
        } else {
          clf_pw <- clfpw(clfp = clf_p, normbs = norm_bs)
        }
        ###vzf_o <- NA
        ###if(comp_required[["VZFO"]] && parameter_required("KEL", names(kel_v))) {
        ###  vzf_o <- vzfo(kel = kel_v[["KEL"]], aucinfo = aucinf_o, dose = dose_c)
        ###}
        if(isTRUE(dose_by_mass)){
          vzf_o <- NA
        } else {
          vzf_o <- vzfo(kel = kel_v[["KEL"]], aucinfo = aucinf_o, dose = dose_c)
        }
        ###if(comp_required[["VZFOW"]]) {
        ###  vzf_ow <- vzfow(vzfo = vzf_o, normbs = norm_bs)
        ###}
        if(isTRUE(dose_by_mass)){
          tmp_denom <- kel_v[["KEL"]] * aucinf_o
          vzf_ow <- vzfow(vzfo = unique(tmp_df[, dosevar])[1], normbs = tmp_denom)
        } else {
          vzf_ow <- vzfow(vzfo = vzf_o, normbs = norm_bs)
        }
        ###vzf_p <- NA
        ###if(comp_required[["VZFP"]] && parameter_required("KEL", names(kel_v))) {
        ###  vzf_p <- vzfp(kel = kel_v[["KEL"]], aucinfp = aucinf_p, dose = dose_c)
        ###}
        if(isTRUE(dose_by_mass)){
          vzf_p <- NA
        } else {
          vzf_p <- vzfp(kel = kel_v[["KEL"]], aucinfp = aucinf_p, dose = dose_c)
        }  
        ###if(comp_required[["VZFPW"]]) {
        ###  vzf_pw <- vzfpw(vzfp = vzf_p, normbs = norm_bs)
        ###}
        if(isTRUE(dose_by_mass)){
          tmp_denom <- kel_v[["KEL"]] * aucinf_p
          vzf_pw <- vzfpw(vzfp = unique(tmp_df[, dosevar])[1], normbs = tmp_denom)
        } else {
          vzf_pw <- vzfpw(vzfp = vzf_p, normbs = norm_bs)
        }
        ###if(comp_required[["KEL"]]) {
          exflag <- !as.logical(kel_flag)

          pkdataid <- tmp_df[,map_data$FLGMERGE][exflag]
          time <- tmp_df[,map_data$TIME][exflag]
          conc <- tmp_df[,map_data$CONC][exflag]
          cest_kel <- rep(NA, length(conc))
          if(!is.na(kel_v[["KEL"]])){
            cest_kel <- estimate_concentration(time, conc, slope=kel_v[["KEL"]])
          }
        ###} else {
        ###  pkdataid <- NULL  
        ###}

        tmp_est_data <- data.frame(matrix(ncol = length(elist), nrow = 0))
        names(tmp_est_data) <- elist
        est_idx <- 1
        if(length(pkdataid) > 0){
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
            ###if(comp_required[["TLAST"]]) { if(!is.na(t_last)){ if(time[e]==t_last) { est_row[8] <- c_est } } }
            if(!is.na(t_last)){ if(time[e]==t_last) { est_row[8] <- c_est } }
            
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

        computation_df[i, "SDEID"] <- unique(data_data[,map_data$SDEID])[i]
        if(disp_required[["DOSE"]] || disp_required[["DOSEi"]]){
          if(parameter_required(dosevar, names(data_data))) {
              computation_df[i, dosenames] <- unique(tmp_df[, dosevar])[1]
          }
        }
        if(disp_required[["DOSEC"]]) {
          computation_df[i, "DOSEC"] <- dose_c
        }
        if(disp_required[["C0"]]) {
          computation_df[i, "C0"] <- obs_c_0
        }
        if(disp_required[["CMAX"]]) {
          computation_df[i, "CMAX"] <- c_max
        }
        if(disp_required[["FLGACCEPTPREDOSE"]] && "FLGACCEPTPREDOSECRIT" %in% names(map_data)){
          pre_dose_crit <- suppressWarnings(as.numeric(map_data$FLGACCEPTPREDOSECRIT))
          if(is.numeric(pre_dose_crit) && !is.na(pre_dose_crit)){
            pre_dose <- tmp_df[,map_data$CONC][tmp_df[,map_data$TIME] == 0][1]
            if(is.numeric(c_max)){
              computation_df[i, "FLGACCEPTPREDOSE"] <- ifelse(pre_dose > (c_max * pre_dose_crit), 0, 1)
            } else {
              computation_df[i, "FLGACCEPTPREDOSE"] <- 1
            }
          } else {
            computation_df[i, "FLGACCEPTPREDOSE"] <- 1
          }
        }
        if(disp_required[["CMIN"]]) {
          computation_df[i, "CMIN"] <- c_min
        }
        if(disp_required[["CLAST"]]) {
          computation_df[i, "CLAST"] <- c_last
        }
        if(disp_required[["CEST"]]) {
          computation_df[i, "CEST"] <- c_est 
        }
        if(disp_required[["CMAXDN"]]) {
          computation_df[i, "CMAXDN"] <- cmaxdn
        }
        if(disp_required[["CMINDN"]]) {
          computation_df[i, "CMINDN"] <- cmindn
        }
        if(disp_required[["TMAX"]]) {
          computation_df[i, "TMAX"] <- t_max
        }
        if(disp_required[["FLGACCEPTTMAX"]] && "FLGEMESIS" %in% names(map_data)){
          computation_df[i, "FLGACCEPTTMAX"] <- 1
        }
        if(disp_required[["TMIN"]]) {
          computation_df[i, "TMIN"] <- t_min 
        }
        if(disp_required[["TLAST"]]) {
          computation_df[i, "TLAST"] <- t_last
        }
        if(disp_required[["TLAG"]]) {
          computation_df[i, "TLAG"] <- t_lag
        }
        if(disp_required[["KEL"]]) {
          computation_df[i, "KEL"] <- ifelse("KEL" %in% names(kel_v), kel_v[["KEL"]], NA)
        }
        if(disp_required[["KELC0"]]) {
          computation_df[i, "KELC0"] <- ifelse("KELC0" %in% names(kel_v), kel_v[["KELC0"]], NA)
        }
        if(disp_required[["KELTMLO"]]) {
          computation_df[i, "KELTMLO"] <- ifelse("KELTMLO" %in% names(kel_v), kel_v[["KELTMLO"]], NA)
        }
        if(disp_required[["KELTMHI"]]) {
         computation_df[i, "KELTMHI"] <- ifelse("KELTMHI" %in% names(kel_v), kel_v[["KELTMHI"]], NA)
        }
        if(disp_required[["KELNOPT"]]) {
          computation_df[i, "KELNOPT"] <- ifelse("KELNOPT" %in% names(kel_v), kel_v[["KELNOPT"]], NA)
        }
        if(disp_required[["KELR"]]) {
          computation_df[i, "KELR"] <- ifelse("KELR" %in% names(kelr_v), kelr_v[["KELR"]], NA)
        }
        if(disp_required[["KELRSQ"]]) {
          computation_df[i, "KELRSQ"] <- ifelse("KELRSQ" %in% names(kelr_v), kelr_v[["KELRSQ"]], NA)
        }
        if(disp_required[["KELRSQA"]]) {
          computation_df[i, "KELRSQA"] <- ifelse("KELRSQA" %in% names(kelr_v), kelr_v[["KELRSQA"]], NA)
        }
        if(disp_required[["FLGACCEPTKEL"]] && "FLGACCEPTKELCRIT" %in% names(map_data)) {
          if(length(unlist(strsplit(as.character(map_data$FLGACCEPTKELCRIT), ","))) > 0){
            computation_df[i, "FLGACCEPTKEL"] <- 0
          } else {
            computation_df[i, "FLGACCEPTKEL"] <- 0
          }
        }
        if(disp_required[["THALF"]]) {
          computation_df[i, "THALF"] <- ifelse("THALF" %in% names(kel_v), kel_v[["THALF"]], NA)
        }
        if(disp_required[["THALFF"]]) {
          computation_df[i, "THALFF"] <- ifelse("THALFF" %in% names(kel_v), kel_v[["THALFF"]], NA)
        }
        if(disp_required[["LASTTIME"]]) {
          computation_df[i, "LASTTIME"] <- last_time
        }
        if(disp_required[["FLGACCEPTTAU"]] && "LASTTIMEACCEPTCRIT" %in% names(map_data)) {
          if(!is.na(last_crit_factor)){
            if(opt_list[2] %in% names(map_data)){
              if(map_data[, opt_list[2]] %in% names(data_data)) {
                tau_val <- unique(tmp_df[, map_data[, opt_list[2]]])[1]
                if(!is.na(tau_val) && is.numeric(tau_val) && !is.na(last_crit_factor) && is.numeric(last_crit_factor)){
                  lt_accept_crit <- tau_val * last_crit_factor
                  kel_val <- ifelse("KEL" %in% names(kel_v), kel_v[["KEL"]], NA)
                  kel_nopt_val <- ifelse("KELNOPT" %in% names(kel_v), kel_v[["KELNOPT"]], NA)
                  #FEEDBACK: Commented the code to account for new logic for FLGACCEPTTAU (New scope item)
                  #computation_df[i, "FLGACCEPTTAU"] <- ifelse(last_time >= lt_accept_crit, 1, 0)
                  if(isTRUE(last_time >= tau_val)){
                    computation_df[i, "FLGACCEPTTAU"] <- 1
                  } else if(isTRUE(last_time < tau_val && last_time >= lt_accept_crit)){
                    if(isTRUE(is.na(kel_val))){
                      computation_df[i, "FLGACCEPTTAU"] <- 1
                    } else if(isTRUE(is.numeric(kel_val))){
                      if(isTRUE(kel_nopt_val >= 3 && kel_val > 0)){
                        computation_df[i, "FLGACCEPTTAU"] <- 1
                      } else {
                        computation_df[i, "FLGACCEPTTAU"] <- 0
                      }
                    } else {
                      computation_df[i, "FLGACCEPTTAU"] <- 0
                    }
                  } else if(isTRUE(last_time < lt_accept_crit)){
                    if(isTRUE(is.na(kel_val))){
                      computation_df[i, "FLGACCEPTTAU"] <- 0
                    } else {
                      computation_df[i, "FLGACCEPTTAU"] <- 1
                      #The logic for FLGACCEPTTAU with respect to FLGACCEPTKEL 
                      #will be updated after the main for loops ends (down below)
                    }
                  } else {
                    computation_df[i, "FLGACCEPTTAU"] <- 0
                  }
                } else {
                  computation_df[i, "FLGACCEPTTAU"] <- 0
                }
              } else {
                computation_df[i, "FLGACCEPTTAU"] <- 0
              }
            } else {
              computation_df[i, "FLGACCEPTTAU"] <- 0
            }
          } else {
            computation_df[i, "FLGACCEPTTAU"] <- 0
          }
        }
        if(disp_required[["AUCALL"]]) {
          computation_df[i, "AUCALL"] <- aucall
        }
        if(disp_required[["AUCALLDN"]]) {
          computation_df[i, "AUCALLDN"] <- aucalldn
        }
        if(disp_required[["AUCLAST"]]) {
          computation_df[i, "AUCLAST"] <- auclast
        }
        if(disp_required[["AUCLASTC"]]) {
          computation_df[i, "AUCLASTC"] <- auclast_c 
        }
        if(disp_required[["AUCLASTDN"]]) {
          computation_df[i, "AUCLASTDN"] <- auclastdn
        }
        if(disp_required[["AUMCLAST"]]) {
          computation_df[i, "AUMCLAST"] <- aumclast
        }
        if(disp_required[["AUCT"]] && auc_len > 1) {
          computation_df[i, paste0("AUC",1:auc_len)] <- auct
        }
        if(disp_required[["AUCTDN"]] && auc_len > 1) {
          computation_df[i, paste0("AUC",1:auc_len,"DN")] <- auctdn
        }
        if((disp_required[["AUCT"]] || disp_required[["AUCTDN"]]) && auc_len > 1) {
          computation_df[i, paste0("AUCINT",1:auc_len)] <- auc_int
        }
        if(disp_required[["AUCT1_T2"]] && auc_pair_check) {
          computation_df[i, paste0("AUC", map_data[,rep(paste0("AUC.", 1:auc_par_len, ".T1"))], "_", map_data[,rep(paste0("AUC.", 1:auc_par_len, ".T2"))])] <- auct1_t2
        }
        if(disp_required[["AUCINFO"]]) {
          computation_df[i, "AUCINFO"] <- aucinf_o
        }
        if(disp_required[["AUCINFP"]]) {
          computation_df[i, "AUCINFP"] <- aucinf_p
        }
        if(disp_required[["AUCINFOC"]]) {
          computation_df[i, "AUCINFOC"] <- aucinf_oc
        }
        if(disp_required[["AUCINFPC"]]) {
          computation_df[i, "AUCINFPC"] <- aucinf_pc
        }
        if(disp_required[["AUCINFODN"]]) {
          computation_df[i, "AUCINFODN"] <- aucinfo_dn
        }
        if(disp_required[["AUCINFPDN"]]) {
          computation_df[i, "AUCINFPDN"] <- aucinfp_dn
        }
        if(disp_required[["AUMCINFO"]]) {
          computation_df[i, "AUMCINFO"] <- aumcinf_o
        }
        if(disp_required[["AUMCINFP"]]) {
          computation_df[i, "AUMCINFP"] <- aumcinf_p
        }
        if(disp_required[["MRTLAST"]]) {
          computation_df[i, "MRTLAST"] <- mrtlast
        }
        if(disp_required[["MRTEVIFO"]]) {
          computation_df[i, "MRTEVIFO"] <- mrto
        }
        if(disp_required[["MRTEVIFP"]]) {
          computation_df[i, "MRTEVIFP"] <- mrtp
        }
        if(disp_required[["AUCXPCTO"]]) {
          computation_df[i, "AUCXPCTO"] <- aucxpcto
        }
        if(disp_required[["AUCXPCTP"]]) {
          computation_df[i, "AUCXPCTP"] <- aucxpctp
        }
        if(disp_required[["AUMCXPTO"]]) {
          computation_df[i, "AUMCXPTO"] <- aumcxpto
        }
        if(disp_required[["AUMCXPTP"]]) {
          computation_df[i, "AUMCXPTP"] <- aumcxptp
        }
        if(disp_required[["CLFO"]]) {
          computation_df[i, "CLFO"] <- clf_o
        }
        if(disp_required[["CLFOW"]]) {
          computation_df[i, "CLFOW"] <- clf_ow
        }
        if(disp_required[["CLFP"]]) {
          computation_df[i, "CLFP"] <- clf_p
        }
        if(disp_required[["CLFPW"]]) {
          computation_df[i, "CLFPW"] <- clf_pw
        }
        if(disp_required[["VZFO"]]) {
          computation_df[i, "VZFO"] <- vzf_o
        }
        if(disp_required[["VZFOW"]]) {
          computation_df[i, "VZFOW"] <- vzf_ow
        }
        if(disp_required[["VZFP"]]) {
          computation_df[i, "VZFP"] <- vzf_p
        }
        if(disp_required[["VZFPW"]]) {
          computation_df[i, "VZFPW"] <- vzf_pw
        }
        computation_df[i, paste0("CONC",1:(auc_len+1))] <- c(tmp_df[,map_data$CONC], rep(NA, ((auc_len+1) - length(tmp_df[,map_data$CONC]))))
        computation_df[i, paste0("CONCTIME",1:(auc_len+1))] <- c(tmp_df[,map_data$TIME], rep(NA, ((auc_len+1) - length(tmp_df[,map_data$TIME]))))

        if(disp_required[["TAU"]] && parameter_required(opt_list[2], names(map_data))){
            if(parameter_required(opt_list[2], names(map_data))) { 
                if(map_data[, opt_list[2]] %in% names(data_data)) {
                    computation_df[i, "TAU1"] <- unique(tmp_df[, map_data[, opt_list[2]]])[1]
                }
            }
        }
        if(disp_required[["TOLD"]] && parameter_required(opt_list[3], names(map_data))){
            if(parameter_required(opt_list[3], names(map_data))) { 
                if(map_data[, opt_list[3]] %in% names(data_data)) {
                    computation_df[i, "TOLD1"] <- unique(tmp_df[, map_data[, opt_list[3]]])[1]
                }
            }
        }
        if(disp_required[["CTOLDesti"]]){
          computation_df[i, paste0("CTOLDest1")] <- ctold_est
        }
      } else {
        if(isTRUE(optimize_kel)){
          kel_flag_optimized <- c(kel_flag_optimized, kel_flag)
        }
        computation_df[i, "SDEID"] <- unique(data_data[,map_data$SDEID])[i]
        if(isTRUE(any(tmp_df[,map_data$TIME] < 0))){
          warning(paste0("No parameters generated due to negative TIME values for SDEID: '", unique(data_data[,map_data$SDEID])[i], "'"))
        }
      }
    }, error = function(e) {
      stop(paste0(e, "For SDEID ", unique(data_data[,map_data$SDEID])[i]))
    })
  }
  
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
  #FEEDBACK: Commented the code to account for new logic for FLGACCEPTTAU (New scope item)
  #if(disp_required[["FLGACCEPTTAU"]] && "LASTTIMEACCEPTCRIT" %in% names(map_data)) {
  #  if("FLGACCEPTKEL" %in% names(computation_df)){
  #    if(nrow(computation_df[!is.na(computation_df[,"FLGACCEPTKEL"]) & computation_df[,"FLGACCEPTKEL"] != 1,]) > 0){
  #      computation_df[!is.na(computation_df[,"FLGACCEPTKEL"]) & computation_df[,"FLGACCEPTKEL"] != 1,][,"FLGACCEPTTAU"] <- 0  
  #    }
  #  }
  #}
  if(disp_required[["FLGACCEPTTAU"]] && "LASTTIMEACCEPTCRIT" %in% names(map_data)) {
    if("FLGACCEPTKEL" %in% names(computation_df)){
      if(nrow(computation_df[!is.na(computation_df[,"FLGACCEPTKEL"]) & computation_df[,"FLGACCEPTKEL"] != 1 & !is.na(computation_df[,"KEL"]) || is.numeric(computation_df[,"KEL"]),]) > 0){ 
        computation_df[!is.na(computation_df[,"FLGACCEPTKEL"]) & computation_df[,"FLGACCEPTKEL"] != 1 & !is.na(computation_df[,"KEL"]) || is.numeric(computation_df[,"KEL"]),][,"FLGACCEPTTAU"] <- 0  
      }
    }
  }
  if(disp_required[["FLGACCEPTTMAX"]] && "FLGEMESIS" %in% names(map_data) && map_data$FLGEMESIS %in% names(data_data)){
    for(f in 1:length(unique(computation_df[,map_data$SDEID]))){
      tmp_df <- data_data[data_data[,map_data$SDEID] == unique(computation_df[,map_data$SDEID])[f],]
      emesis_flag_check <- ifelse(any(as.logical(as.numeric(tmp_df[,map_data$FLGEMESIS]))), TRUE, FALSE)
      tmp_comp_df <- computation_df[computation_df[,map_data$SDEID] == unique(computation_df[,map_data$SDEID])[f],]
      if("DOSE" %in% names(computation_df)){
        test_df_3 <- computation_df[computation_df[,"DOSE"] == tmp_comp_df[,"DOSE"],]
        tmp_median <- median(as.numeric(test_df_3[,"TMAX"]), na.rm = TRUE) 
      } else {
        tmp_median <- NULL
      }
      tmp_tmax <- as.numeric(tmp_comp_df[,"TMAX"])
      if(!is.null(tmp_median) && !is.na(tmp_median) && !is.null(tmp_tmax) && !is.na(tmp_tmax)){
        if(computation_df[computation_df[,map_data$SDEID] == unique(computation_df[,map_data$SDEID])[f],"FLGACCEPTTMAX"] != 0){
          computation_df[computation_df[,map_data$SDEID] == unique(computation_df[,map_data$SDEID])[f],"FLGACCEPTTMAX"] <- ifelse((isTRUE(emesis_flag_check) && (tmp_tmax < (2 * tmp_median))), 1, ifelse(!isTRUE(emesis_flag_check), 1 , 0))  
        }
      }
    }
  }
  if(isTRUE(secondary)){
    pkterm_parent <- map_data$PKTERMPARENT
    secondary_mappings <- c("SDEID", 'MRSDEID', 'PKTERM', 'MW')
    secondary_values <- as.character(map_data[,secondary_mappings])
    secondary_data <- merge(x = computation_df, y = unique(data_data[,secondary_values]), by = map_data$SDEID, all.y = FALSE, all.x = FALSE)
    
    for(i in 1:length(unique(secondary_data[,map_data$SDEID]))){
      tryCatch({
        sdeid <- unique(secondary_data[,map_data$SDEID])[i]
        tmp_df <- secondary_data[secondary_data[,map_data$SDEID] == sdeid,]
        curr_mrsdeid <- unique(tmp_df[,map_data$MRSDEID])[1]
        tmp_mr_df <- secondary_data[secondary_data[,map_data$MRSDEID] == curr_mrsdeid,]
        tmp_orig_df <- data_data[data_data[,map_data$SDEID] == sdeid,]
        tmp_mr_orig_df <- data_data[data_data[,map_data$MRSDEID] == curr_mrsdeid,]
        
        if(isTRUE(pkterm_parent %in% tmp_mr_df[,map_data$PKTERM])){
          curr_pkterm <- unique(tmp_df[,map_data$PKTERM])[1]
          tmp_parent_df <- tmp_mr_df[tmp_mr_df[,map_data$PKTERM] == pkterm_parent,]
          tmp_parent_orig_df <- tmp_mr_orig_df[tmp_mr_orig_df[,map_data$PKTERM] == pkterm_parent,]
          
          if("CONCU" %in% names(map_data)){
            if(map_data$CONCU %in% names(data_data)){
              meta_unit <- unique(tmp_orig_df[,map_data$CONCU])[1]
              parent_unit <- unique(tmp_parent_orig_df[,map_data$CONCU])[1]
              if(parent_unit != meta_unit){
                warning(paste0("Warning concentration units for parent '", pkterm_parent, "' (", parent_unit, ") and metabolite '", curr_pkterm, "' (", meta_unit, ") differ! Review metabolite ratio parameters carefully before accepting for SDEID '", sdeid, "'!"))
              }
            }
          }
          if("TIMEU" %in% names(map_data)){
            if(map_data$TIMEU %in% names(data_data)){
              meta_unit <- unique(tmp_orig_df[,map_data$TIMEU])[1]
              parent_unit <- unique(tmp_parent_orig_df[,map_data$TIMEU])[1]
              if(parent_unit != meta_unit){
                warning(paste0("Warning time units for parent '", pkterm_parent, "' (", parent_unit, ") and metabolite '", curr_pkterm, "' (", meta_unit, ") differ! Review metabolite ratio parameters carefully before accepting for SDEID '", sdeid, "'!"))
              }
            }
          }
          if("DOSEU" %in% names(map_data)){
            if(map_data$DOSEU %in% names(data_data)){
              meta_unit <- unique(tmp_orig_df[,map_data$DOSEU])[1]
              parent_unit <- unique(tmp_parent_orig_df[,map_data$DOSEU])[1]
              if(parent_unit != meta_unit){
                warning(paste0("Warning dose units for parent '", pkterm_parent, "' (", parent_unit, ") and metabolite '", curr_pkterm, "' (", meta_unit, ") differ! Review metabolite ratio parameters carefully before accepting for SDEID '", sdeid, "'!"))
              }
            }
          }
          
          if(disp_required[["MRAUCINFO"]]) {
            if(isTRUE(curr_pkterm != pkterm_parent)){
              mr_aucinf_o <- mr_auc_inf_o(metabolite_aucinfo = tmp_df$AUCINFO, parent_aucinfo = tmp_parent_df$AUCINFO, parent_mw = tmp_parent_df$MW, metabolite_mw = tmp_df$MW)
              computation_df[computation_df[,map_data$SDEID] == sdeid, "MRAUCINFO"] <- mr_aucinf_o
            } 
          }
          if(disp_required[["MRAUCINFP"]]) {
            if(isTRUE(curr_pkterm != pkterm_parent)){
              mr_aucinf_p <- mr_auc_inf_p(metabolite_aucinfp = tmp_df$AUCINFP, parent_aucinfp = tmp_parent_df$AUCINFP, parent_mw = tmp_parent_df$MW, metabolite_mw = tmp_df$MW)
              computation_df[computation_df[,map_data$SDEID] == sdeid, "MRAUCINFP"] <- mr_aucinf_p
            }
          }
          if(disp_required[["MRAUCLAST"]]) {
            if(isTRUE(curr_pkterm != pkterm_parent)){
              mr_auclast <- mr_auc_last(metabolite_auclast = tmp_df$AUCLAST, parent_auclast = tmp_parent_df$AUCLAST, parent_mw = tmp_parent_df$MW, metabolite_mw = tmp_df$MW)
              computation_df[computation_df[,map_data$SDEID] == sdeid, "MRAUCLAST"] <- mr_auclast
            } 
          }
          if(disp_required[["MRCMAX"]]) {
            if(isTRUE(curr_pkterm != pkterm_parent)){
              mr_cmax <- mr_cmax(metabolite_cmax = tmp_df$CMAX, parent_cmax = tmp_parent_df$CMAX, parent_mw = tmp_parent_df$MW, metabolite_mw = tmp_df$MW)
              computation_df[computation_df[,map_data$SDEID] == sdeid, "MRCMAX"] <- mr_cmax
            }
          }
          
          #FEEDBACK: Commented the code to account for exclusion of metabolite exclusion list
          #Based of 3.0a scope item (Based on tc1606_M1SD)
          if(length(exclusion_list) > 0){
            if(isTRUE(any(names(computation_df) %in% exclusion_list))){
              if(isTRUE(curr_pkterm != pkterm_parent)){
                computation_df[computation_df[,map_data$SDEID] == sdeid, exclusion_list] <- NA
              }
            }
          }
        }
      }, error = function(e) {
        stop(paste0(e, "For SDEID ", sdeid))
      })
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
  raw_results_df <- computation_df
  computation_df <- unit_conversion(data = data_data, map = map_data, result = computation_df, unit_class = "ALL")

  return_list <- unlist(return_list)
  if(!is.null(return_list) && length(return_list) > 0){
    if(!map_data$SDEID %in% return_list && length(return_list) > 1){
      return_list <- c(map_data$SDEID, return_list)
    }

    k <- names(data_data) %in% return_list
    return_df <- data_data[!duplicated(data_data$SDEID),unlist(k)]
    ck <- intersect(names(return_df), names(computation_df))
    if(length(ck)>1) {
      ck <- setdiff(ck, map_data$SDEID)
      return_df <- return_df[,-match(ck, names(return_df))]
    }

    merged_computation <- merge(x = computation_df, y = return_df, by = map_data$SDEID)
    colnames(merged_computation) <- gsub('.x','.dataset',names(merged_computation))
    colnames(merged_computation) <- gsub('.y','',names(merged_computation))
    computation_df <- merged_computation
  }

  results_list <- list()
  results_list$data_out <- computation_df
  results_list$est_data <- est_data
  
  ###if(isTRUE(optimize_kel) && comp_required[["TMAX"]] && comp_required[["TLAST"]] && comp_required[["CMAX"]] && comp_required[["CLAST"]] && comp_required[["AUCLAST"]] &&
  ###   "FLGACCEPTKELCRIT" %in% names(map_data) && "FLGEXKEL" %in% names(map_data) && map_data$FLGEXKEL %in% names(data_data)){
  if(isTRUE(optimize_kel) && "FLGACCEPTKELCRIT" %in% names(map_data) && "FLGEXKEL" %in% names(map_data) && map_data$FLGEXKEL %in% names(data_data)){
    results_list$optimized_kel_flag <- kel_flag_optimized
  }
  if(isTRUE(raw_results_debug)){
    results_list$raw_results <- raw_results_df
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

