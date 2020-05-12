#' Run M3 SS Computation
#'
#' This function will compute all the relevant parameters for a M3 model Steady State (SS).\cr
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
#' By default all the return list options are selected and calculated if 'parameter_list' is not specified. Please refer to MCT
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
run_M3_SS_computation <- function(data = NULL, map = NULL, method = 1, model_regex = "^M3(SS)*?$", parameter_list = list(), return_list = list(), raw_results_debug = FALSE, optimize_kel_debug = FALSE){
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
  if(!is.list(return_list)){
    stop("Invalid list provided for 'return_list'! Please provide a valid list")
  }
  if(!(parameter_required("^SDEID$",names(data_data)))) {
    stop("Value for 'SDEID' provided via 'map' is not present in the dataset provided via 'data'")
  }
  ss_dose <- c("DI1F", "DI2F", "DI3F", "DI4F", "DI5F")

  if(any(ss_dose %in% names(data_data))){
    di_col <- sum(ss_dose %in% names(data_data))
  } else {
    stop("Unable to find dosing interval for Steady State data! Please provide a valid 'data' parameter")
  }

  auc_list <- c("AUCT", "AUCTDN")
  auc_par <- c("AUCT1_T2")

  interval_list <- c("CMAXi", "CMAXDNi", "CMINi", "CLASTi", "TMAXi", "TMINi", "TLASTi", "LASTTIMEi", "AUCDN",
                     "AUCLASTi", "AUCLASTCi", "AUCLASTDNi", "AUMCLASTi", "AUCINFOi", "AUCINFPi", "AUMCINFOi", "AUMCINFPi",
                     "AUCTAUi", "AUCTAUDNi", "AUMCTAUi", "MRTLASTi", "MRTIVIFOi", "MRTIVIFPi", "AUCXPCTOi", "AUCXPCTPi",
                     "AUMCXPTOi", "AUMCXPTPi", "CAVi", "CLTAUi", "CLTAUWi", "PTFi", "PTRi", "VZO", "VZP", "DIi", "TAUi",
                     "TOLD", "DOSEi")
  regular_list <- c("CMAX", "CMAXDN", "CMIN", "CLAST", "TMAX", "TMIN", "TLAST", "KEL", "KELC0", "KELTMLO",
                    "KELTMHI", "KELNOPT", "KELRSQ", "KELRSQA", "THALF", "THALFF", "LASTTIME", "AUCALL", "AUCLAST", "AUCLASTC",
                    "AUCLASTDN", "AUCINFO", "AUCINFOC", "AUCINFODN", "CEST", "AUCINFP", "AUCINFPC", "AUCINFPDN", "MRTLAST", "AUCXPCTO",
                    "AUCXPCTP", "AUMCXPTO", "AUMCXPTP")
  regular_int_type <- NULL
  auc_pair_check <- FALSE

  index1 <- data_data[,map_data$SDEID]
  auc_len <- max(tapply(index1, index1, length))-1
  reg_col <- sum(regular_list %in% parameter_list) + ifelse(any(c("KELRSQ","KELRSQA") %in% parameter_list), 1, 0)
  auc_col <- ifelse(sum(auc_list %in% parameter_list) > 0, sum(auc_list %in% parameter_list)+1, 0)
  interval_col <- sum(interval_list %in% parameter_list)

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

  col <- reg_col + (auc_col * auc_len) + (interval_col * di_col) + 1 + (2 * (auc_len+1))

  doselist <- names(parameter_indices("^DOSELIST$", names(map_data), simplify=FALSE))
  dosenames <- unlist(strsplit(map_data[,doselist], ";"))
  dosevar <- as.character(map[,dosenames])
  if(!any(duplicated(as.character(unlist(dosevar))))){
    dosenames <- dosenames[!duplicated(as.character(unlist(dosevar)))]
  }

  comp_required <- list()
  disp_required <- list()
  plist <- parameter_list
  for(i in model_parameters()) {
    rg <- parameter_regex(i)
    pr <- parameter_required(rg, parameter_list=plist)
    dp <- parameter_required(dependent_parameters(rg), plist)
    comp_required[[i]] <- pr || dp
    disp_required[[i]] <- pr
  }
  
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
  elist <- c("PKDATAROWID", "SDEID","TIME","CEST_KEL","CEST_INT","CEST_EXT","CEST_C0","CEST_TLAST")
  est_data <- data.frame(matrix(ncol = length(elist), nrow = 0))
  names(est_data) <- elist

  col_names <- c("SDEID")
  
  if(disp_required[["DOSEi"]]) {
    col_names <- c(col_names, dosenames)
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
  if(disp_required[["CMAX"]]) {
    col_names <- c(col_names, "CMAX")
    regular_int_type <- c(regular_int_type, "CMAX")
  }
  if(disp_required[["CMAXi"]]) {
    col_names <- c(col_names, rep(paste0("CMAX",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("CMAX",1:di_col)))
  }
  if(disp_required[["CMAXDN"]]) {
    col_names <- c(col_names, "CMAXDN")
    regular_int_type <- c(regular_int_type, "CMAXDN")
  }
  if(disp_required[["CMAXDNi"]]) {
    col_names <- c(col_names, rep(paste0("CMAXDN",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("CMAXDN",1:di_col)))
  }
  if(disp_required[["FLGACCEPTPREDOSE"]] && "FLGACCEPTPREDOSECRIT" %in% names(map_data)){
    col_names <- c(col_names, "FLGACCEPTPREDOSE")
  }
  if(disp_required[["CMIN"]]) {
    col_names <- c(col_names, "CMIN")
    regular_int_type <- c(regular_int_type, "CMIN")
  }
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
  if(disp_required[["CLAST"]]) {
    col_names <- c(col_names, "CLAST")
    regular_int_type <- c(regular_int_type, "CLAST")
  }
  if(disp_required[["CLASTi"]]) {
    col_names <- c(col_names, rep(paste0("CLAST",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("CLAST",1:di_col)))
  }
  if(disp_required[["TMAX"]]) {
    col_names <- c(col_names, "TMAX")
    regular_int_type <- c(regular_int_type, "TMAX")
  }
  if(disp_required[["TMAXi"]]) {
    col_names <- c(col_names, rep(paste0("TMAX",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("TMAX",1:di_col)))
  }
  if(disp_required[["FLGACCEPTTMAX"]] && "FLGEMESIS" %in% names(map_data)){
    col_names <- c(col_names, "FLGACCEPTTMAX")
  }
  if(disp_required[["TMIN"]]) {
    col_names <- c(col_names, "TMIN")
    regular_int_type <- c(regular_int_type, "TMIN")
  }
  if(disp_required[["TMINi"]]) {
    col_names <- c(col_names, rep(paste0("TMIN",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("TMIN",1:di_col)))
  }
  if(disp_required[["TLAST"]]) {
    col_names <- c(col_names, "TLAST")
    regular_int_type <- c(regular_int_type, "TLAST")
  }
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
  if(disp_required[["KELR"]]){
    col_names <- c(col_names, "KELR")
    regular_int_type <- c(regular_int_type, "KELR")
  }
  if(disp_required[["KELRSQ"]]){
    col_names <- c(col_names, "KELRSQ")
    regular_int_type <- c(regular_int_type, "KELRSQ")
  }
  if(disp_required[["KELRSQA"]]){
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
  if(disp_required[["LASTTIMEi"]]) {
    col_names <- c(col_names, rep(paste0("LASTTIME",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("LASTTIME",1:di_col)))
  }
  if(disp_required[["FLGACCEPTTAU"]] && "LASTTIMEACCEPTCRIT" %in% names(map_data)) {
    col_names <- c(col_names, "FLGACCEPTTAU")
  }
  if(disp_required[["AUCALL"]]) {
    col_names <- c(col_names, "AUCALL")
    regular_int_type <- c(regular_int_type, "AUCALL")
  }
  if(disp_required[["AUCALLDN"]]) {
    col_names <- c(col_names, rep(paste0("AUCALLDN",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUCALLDN",1:di_col)))
  }
  if(disp_required[["AUCLAST"]]) {
    col_names <- c(col_names, "AUCLAST")
    regular_int_type <- c(regular_int_type, "AUCLAST")
  }
  if(disp_required[["AUCLASTDN"]]) {
    col_names <- c(col_names, "AUCLASTDN")
    regular_int_type <- c(regular_int_type, "AUCLASTDN")
  }
  if(disp_required[["AUCLASTi"]]) {
    col_names <- c(col_names, rep(paste0("AUCLAST",1:di_col)))
    regular_int_type <-  c(regular_int_type, rep(paste0("AUCLAST",1:di_col)))
  }
  if(disp_required[["AUCLASTDNi"]]) {
    col_names <- c(col_names, rep(paste0("AUCLASTDN",1:di_col)))
    regular_int_type <-  c(regular_int_type, rep(paste0("AUCLASTDN",1:di_col)))
  }
  if(disp_required[["AUMCLASTi"]]) {
    col_names <- c(col_names, rep(paste0("AUMCLAST",1:di_col)))
    regular_int_type <-  c(regular_int_type, rep(paste0("AUMCLAST",1:di_col)))
  }
  if(disp_required[["AUCT"]] && auc_len > 1) {
    col_names <- c(col_names, rep(paste0("AUC",1:auc_len)))
    regular_int_type <- c(regular_int_type, paste0("AUC",1:auc_len))
  }
  if(disp_required[["AUCTDN"]] && auc_len > 1) {
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
  if(disp_required[["AUCINFOi"]]) {
    col_names <- c(col_names, rep(paste0("AUCINFO",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUCINFO",1:di_col)))
  }
  if(disp_required[["AUCINFOCi"]]){
    col_names <- c(col_names, rep(paste0("AUCINFOC",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUCINFOC",1:di_col)))
  }
  if(disp_required[["AUCINFODN"]]) {
    col_names <- c(col_names, "AUCINFODN")
    regular_int_type <- c(regular_int_type, "AUCINFODN")
  }
  if(disp_required[["AUCINFODNi"]]){
    col_names <- c(col_names, rep(paste0("AUCINFODN",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUCINFODN",1:di_col)))
  }
  if(disp_required[["CEST"]]) {
    col_names <- c(col_names, "CEST")
    regular_int_type <- c(regular_int_type, "CEST")
  }
  if(disp_required[["AUCINFP"]]) {
    col_names <- c(col_names, "AUCINFP")
    regular_int_type <- c(regular_int_type, "AUCINFP")
  }
  if(disp_required[["AUCINFPi"]]) {
    col_names <- c(col_names, rep(paste0("AUCINFP",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUCINFP",1:di_col)))
  }
  if(disp_required[["AUCINFPCi"]]){
    col_names <- c(col_names, rep(paste0("AUCINFPC",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUCINFPC",1:di_col)))
  }
  if(disp_required[["AUCINFPDN"]]) {
    col_names <- c(col_names, "AUCINFPDN")
    regular_int_type <- c(regular_int_type, "AUCINFPDN")
  }
  if(disp_required[["AUCINFPDNi"]]){
    col_names <- c(col_names, rep(paste0("AUCINFPDN",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUCINFPDN",1:di_col)))
  }
  if(disp_required[["AUMCINFOi"]]) {
    col_names <- c(col_names, rep(paste0("AUMCINFO",1:di_col)))
    regular_int_type <-  c(regular_int_type, rep(paste0("AUMCINFO",1:di_col)))
  }
  if(disp_required[["AUMCINFPi"]]) {
    col_names <- c(col_names, rep(paste0("AUMCINFP",1:di_col)))
    regular_int_type <-  c(regular_int_type, rep(paste0("AUMCINFP",1:di_col)))
  }
  if(disp_required[["AUCTAUi"]]) {
    col_names <- c(col_names, rep(paste0("AUCTAU",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUCTAU",1:di_col)))
  }
  if(disp_required[["AUCTAUDNi"]]) {
    col_names <- c(col_names, rep(paste0("AUCTAUDN",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUCTAUDN",1:di_col)))
  }
  if(disp_required[["AUMCTAUi"]]) {
    col_names <- c(col_names, rep(paste0("AUMCTAU",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUMCTAU",1:di_col)))
  }
  if(disp_required[["MRTLAST"]]){
    col_names <- c(col_names, "MRTLAST")
    regular_int_type <- c(regular_int_type, "MRTLAST")
  }
  if(disp_required[["MRTLASTi"]]){
    col_names <- c(col_names, rep(paste0("MRTLAST",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("MRTLAST",1:di_col)))
  }
  if(disp_required[["MRTIVIFOi"]]){
    col_names <- c(col_names, rep(paste0("MRTIVIFO",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("MRTIVIFO",1:di_col)))
  }
  if(disp_required[["MRTIVIFPi"]]){
    col_names <- c(col_names, rep(paste0("MRTIVIFP",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("MRTIVIFP",1:di_col)))
  }
  if(disp_required[["AUCXPCTO"]]){
    col_names <- c(col_names, "AUCXPCTO")
    regular_int_type <- c(regular_int_type, "AUCXPCTO")
  }
  if(disp_required[["AUCXPCTOi"]]){
    col_names <- c(col_names, rep(paste0("AUCXPCTO",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUCXPCTO",1:di_col)))
  }
  if(disp_required[["AUCXPCTP"]]){
    col_names <- c(col_names, "AUCXPCTP")
    regular_int_type <- c(regular_int_type, "AUCXPCTP")
  }
  if(disp_required[["AUCXPCTPi"]]){
    col_names <- c(col_names, rep(paste0("AUCXPCTP",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUCXPCTP",1:di_col)))
  }
  if(disp_required[["AUMCXPTO"]]){
    col_names <- c(col_names, "AUMCXPTO")
    regular_int_type <- c(regular_int_type, "AUMCXPTO")
  }
  if(disp_required[["AUMCXPTOi"]]){
    col_names <- c(col_names, rep(paste0("AUMCXPTO",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUMCXPTO",1:di_col)))
  }
  if(disp_required[["AUMCXPTP"]]){
    col_names <- c(col_names, "AUMCXPTP")
    regular_int_type <- c(regular_int_type, "AUMCXPTP")
  }
  if(disp_required[["AUMCXPTPi"]]){
    col_names <- c(col_names, rep(paste0("AUMCXPTP",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AUMCXPTP",1:di_col)))
  } 
  if(disp_required[["CAVi"]]) {
    col_names <- c(col_names, rep(paste0("CAV",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("CAV",1:di_col)))
  }
  if(disp_required[["CLTAUi"]]) {
    col_names <- c(col_names, rep(paste0("CLTAU",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("CLTAU",1:di_col)))
  }
  if(disp_required[["CLTAUWi"]]) {
    col_names <- c(col_names, rep(paste0("CLTAUW",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("CLTAUW",1:di_col)))
  }
  if(disp_required[["PTFi"]]) {
    col_names <- c(col_names, rep(paste0("PTF",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("PTF",1:di_col)))
  }
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

  if(disp_required[["DIi"]]) {
    col_names <- c(col_names, rep(paste0("DI",1:di_col)))
  }
  if(disp_required[["TAUi"]]) {
    col_names <- c(col_names, rep(paste0("TAU",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("TAU",1:di_col)))
  }
  if(disp_required[["TOLDi"]]) {
    col_names <- c(col_names, rep(paste0("TOLD",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("TOLD",1:di_col)))
  }
  if(disp_required[["CTOLDesti"]]){
    col_names <- c(col_names, rep(paste0("CTOLDest",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("CTOLDest",1:di_col)))
  }
  computation_df <- data.frame(matrix(ncol = length(col_names), nrow = 0))
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
    last_crit_factor <- NA
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
      if(comp_required[["CMAXi"]]) {
        c_maxi <- list()
      }
      if(comp_required[["CMAXDNi"]]) {
        c_maxdni <- list()
      }
      if(comp_required[["CMINi"]]) {
        c_mini <- list()
      }
      if(comp_required[["CMINDNi"]]){
        c_mindni <- list()
      }
      if(comp_required[["CLASTi"]]) {
        c_lasti <- list()
      }
      if(comp_required[["TMAXi"]]) {
        t_maxi <- list()
      }
      if(comp_required[["TMINi"]]) {
        t_mini <- list()
      }
      if(comp_required[["TLASTi"]]) {
        t_lasti <- list()
      }
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
      if(comp_required[["AUCLAST"]]){
        tmp_auclast <- list()
      }
      if(comp_required[["AUCALL"]]){
        tmp_aucall <- list()
      }
      if(comp_required[["AUMCLAST"]]){
        tmp_aumclast <- list()
      }
      if(comp_required[["AUCINFOi"]]) {
        aucinfoi <- list()
      }
      if(comp_required[["AUCINFOCi"]]){
        aucinfoi_c <- list()
      }
      if(comp_required[["AUCINFODNi"]]){
        aucinfoi_dn <- list()
      }
      if(comp_required[["AUCINFPi"]]) {
        aucinfpi <- list()
      }
      if(comp_required[["AUCINFPCi"]]){
        aucinfpi_c <- list()
      }
      if(comp_required[["AUCINFPDNi"]]){
        aucinfpi_dn <- list()
      }
      if(comp_required[["AUMCINFOi"]]) {
        aumcinfoi <- list()
      }
      if(comp_required[["AUMCINFPi"]]) {
        aumcinfpi <- list()
      }
      if(comp_required[["AUCALLDN"]]) {
        aucalldn <- list()
      }
      if(comp_required[["AUCLASTi"]]) {
        auclasti <- list()
      }
      if(comp_required[["AUCLASTDNi"]]) {
        auclasti_dn <- list()
      }
      if(comp_required[["AUMCLASTi"]]) {
        aumclasti <- list()
      }
      if(comp_required[["AUCXPCTOi"]]){
        aucxpctoi <- list()
      }
      if(comp_required[["AUCXPCTPi"]]){
        aucxpctpi <- list()
      }
      if(comp_required[["TAUi"]] || comp_required[["TAU"]]) {
        tau <- list()
      }
      if(comp_required[["TOLDi"]] || comp_required[["TOLD"]]) {
        told <- list()
      }
      if(comp_required[["AUCTAUi"]]) {
        auctau <- list()
      }
      if(comp_required[["AUCTAUDNi"]]) {
        auctaudn <- list()
      }
      if(comp_required[["AUMCTAUi"]]) {
        aumctaui <- list()
      }
      if(comp_required[["MRTLASTi"]]){
        mrtlasti <- list()
      }
      if(comp_required[["MRTIVIFOi"]]){
        mrtivifoi <- list()
      }
      if(comp_required[["MRTIVIFPi"]]){
        mrtivifpi <- list()
      }
      if(comp_required[["AUMCXPTOi"]]){
        aumcxptoi <- list()
      }
      if(comp_required[["AUMCXPTPi"]]){
        aumcxptpi <- list()
      }
      if(comp_required[["CAVi"]]) {
        ca_v <- list()
      }
      if(comp_required[["DIi"]]) {
        di <- list()
      }
      if(comp_required[["DOSEi"]]) {
        dose <- list()
      }
      if(comp_required[["CLTAUi"]]) {
        cl_tau <- list()
      }
      if(comp_required[["CLTAUWi"]]) {
        cl_tauw <- list()
      }
      if(comp_required[["PTFi"]]) {
        pt_f <- list()
      }
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
      ctold_est <- list()

      tmp_df <- data_data[data_data[,map_data$SDEID] == unique(data_data[,map_data$SDEID])[i],]
      default_df <- tmp_df
      default_df[,map_data$TIME] <- as.numeric(default_df[,map_data$TIME])
      default_df <- default_df[order(default_df[,map_data$TIME]),]
      tmp_df[,map_data$CONC] <- as.numeric(tmp_df[,map_data$CONC])
      tmp_df[,map_data$TIME] <- as.numeric(tmp_df[,map_data$TIME])
      tmp_df <- tmp_df[order(tmp_df[,map_data$TIME]),]
      main_dose <- tmp_df[, as.character(map_data["DOSE1"])][1]
      
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
     
      if("INCLUDEINTERPOLATION" %in% names(map_data)){
        map_data[,"INCLUDEINTERPOLATION"] <- as.numeric(map_data[,"INCLUDEINTERPOLATION"])
        interpolation <- ifelse((map_data[,"INCLUDEINTERPOLATION"] == 0 || map_data[,"INCLUDEINTERPOLATION"] == 1), as.logical( map_data[,"INCLUDEINTERPOLATION"]), FALSE)
      } else {
        interpolation <- FALSE
      }
      if("INCLUDEEXTRAPOLATION" %in% names(map_data)){
        map_data[,"INCLUDEEXTRAPOLATION"] <- as.numeric(map_data[,"INCLUDEEXTRAPOLATION"])
        extrapolation <- ifelse((map_data[,"INCLUDEEXTRAPOLATION"] == 0 || map_data[,"INCLUDEEXTRAPOLATION"] == 1), as.logical(as.numeric(map_data[,"INCLUDEEXTRAPOLATION"])), FALSE)
      } else {
        extrapolation <- FALSE
      }
      
      if(isTRUE(nrow(tmp_df) > 0 & all(tmp_df[,map_data$TIME] >= 0))){
        orig_time <- tmp_df[,map_data$TIME]
        orig_conc <- tmp_df[,map_data$CONC]
        
        obs_c_0 <- c0(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        if(comp_required[["DOSEC"]]) {
          dose_c <- dosec(data = tmp_df, map = map_data)
        }
        if(comp_required[["CMAX"]]) {
          c_max <- cmax(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        }
        if(comp_required[["TMAX"]]) {
          t_max <- tmax(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        }
        if(comp_required[["CLAST"]]) {
          c_last <- clast(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        }
        if(comp_required[["TLAST"]]) {
          t_last <- tlast(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        } else {
          t_last <- NULL
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

        if(comp_required[["CMAXDN"]]) {
          c_maxdn <- cmax_dn(cmax = c_max, dose = main_dose)
        }
        if(comp_required[["CMIN"]]) {
          c_min <- cmin(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        }
        if(comp_required[["CMINDN"]]){
          c_mindn <- cmin_dn(cmin = c_min, dose = main_dose)
        }
        if(comp_required[["TMIN"]]) {
          t_min <- tmin(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        }
        if(comp_required[["KEL"]] || comp_required[["KELTMLO"]] || comp_required[["KELTMHI"]] || comp_required[["KELNOPT"]] || comp_required[["THALF"]] || comp_required[["THALFF"]]) {
          span_ratio <- ifelse("SPANRATIOCRIT" %in% names(map_data), suppressWarnings(as.numeric(map_data$SPANRATIOCRIT)), NA)
          kel_v <- kel(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], exflag = kel_flag, spanratio = span_ratio)
        } else {
          kel_v <- NULL
        }
        if(comp_required[["KELR"]] || comp_required[["KELRSQ"]] || comp_required[["KELRSQA"]]) {
          kelr_v <- kel_r(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], exflag = kel_flag)
        }
        if(comp_required[["LASTTIME"]]) {
          last_time <- lasttime(conc = default_df[,map_data$CONC], time = default_df[,map_data$TIME])
        }
        if(comp_required[["CEST"]] || parameter_required("KEL", names(kel_v)) || parameter_required("KELC0", names(kel_v))) {
          span_ratio <- ifelse("SPANRATIOCRIT" %in% names(map_data), suppressWarnings(as.numeric(map_data$SPANRATIOCRIT)), NA)
          c_est <- cest(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], kelflag = kel_flag, t_last = t_last, spanratio = span_ratio, kel = kel_v[["KEL"]])
        }
        if(comp_required[["AUCT"]] && auc_len > 1) {
          auct <- list()
        }
        if(comp_required[["AUCTDN"]] && auc_len > 1) {
          auctdn <- list()
        }
        if((comp_required[["AUCT"]] || comp_required[["AUCTDN"]]) && auc_len > 1) {
          auc_int <- list()
        }
        if(comp_required[["AUCT1_T2"]] && auc_pair_check) {
          auct1_t2 <- list()
          auct1_t2_check <- as.list(rep(NA, auc_par_len))
          auct1_t2_names <- c(rep(paste0("AUC.", 1:auc_par_len, ".T1")), rep(paste0("AUC.", 1:auc_par_len, ".T2")))

          if(!all(auct1_t2_names %in% names(map_data))){
            par_col <- rep(paste0("'", auct1_t2_names[!auct1_t2_names %in% names(map_data)], "'"))
            stop(paste0("Dataset provided via 'map' does not contain the required columns for partial areas ", par_col))
          }
        }

        for(d in 1:di_col){
          default_di_df <- default_df[default_df[c(paste0("DI", d, "F"))] == 1,]
          default_di_df <- default_di_df[order(default_di_df[,map_data$TIME]),]
          tmp_di_df <- tmp_df[tmp_df[c(paste0("DI", d, "F"))] == 1,]
          tmp_di_df <- tmp_di_df[order(tmp_di_df[,map_data$TIME]),]
          norm_bs <- ifelse("NORMBS" %in% names(map_data), ifelse(map_data$NORMBS %in% names(tmp_di_df), tmp_di_df[,map_data$NORMBS][1], NA), NA)
          tmp_dose <- tmp_di_df[, as.character(map_data[c(paste0("DOSE",d))])][1]
          
          tmp_told <- as.numeric(tmp_di_df[, as.character(map_data[c(paste0("TOLD",d))])][1])
          ctold_exists <- FALSE 
          if(tmp_told %in% tmp_di_df[,map_data$TIME]){
            idx <- which(tmp_di_df[,map_data$TIME] == tmp_told)
            tmp_ctold <- tmp_di_df[,map_data$CONC][length(idx)]
            if(!is.na(tmp_ctold)){
              ctold_exists <- TRUE
            }
          }
          tmp_tau <- as.numeric(tmp_di_df[, as.character(map_data[c(paste0("TAU",d))])][1])
          tmp_tau <- tmp_tau + tmp_told
          ctau_exists <- FALSE 
          if(tmp_tau %in% tmp_di_df[,map_data$TIME]){
            idx <- which(tmp_di_df[,map_data$TIME] == tmp_tau)
            tmp_ctau <- tmp_di_df[,map_data$CONC][length(idx)]
            if(!is.na(tmp_ctau)){
              ctau_exists <- TRUE
            }
          }
          if((!isTRUE(ctold_exists) || !isTRUE(ctau_exists)) && di_col > 1){
            warning(paste0("Missing concentration at TAU and/or TOLD for SDEID: '", unique(data_data[,map_data$SDEID])[i], "'"))
          }
          
          if(!isTRUE(ctold_exists) && !is.na(tmp_told)){
            tmp_conc_di <- c(NA, tmp_di_df[,map_data$CONC])
            tmp_time_di <- c(tmp_told, tmp_di_df[,map_data$TIME])
            est_tmp <- estimate_told_concentration(conc = tmp_conc_di, time = tmp_time_di, interpolate = TRUE, extrapolate = TRUE, auc_method = "LIN", model = "M3", dosing_type = "SS", told = tmp_told, orig_conc = orig_conc, orig_time = orig_time)
            tmp_conc_di <- est_tmp[[1]]
            ctold_est[[d]] <- tmp_conc_di[1]
          } else {
            tmp_conc_di <- tmp_di_df[,map_data$CONC]
            tmp_time_di <- tmp_di_df[,map_data$TIME]
            ctold_est[[d]] <- NA
          }
          if(d == 1){ 
            aumc_time <- tmp_time_di
          } else {
            aumc_time <- tmp_time_di - as.numeric(tmp_di_df[, as.character(map_data[c(paste0("TOLD",(d-1)))])][1])
          }
          tmp_nomtime_di <- tmp_time_di
          if(isTRUE("NOMTIME" %in% names(map_data))){
            if(isTRUE(map_data$NOMTIME %in% names(tmp_di_df))){
              if(!isTRUE(ctold_exists) && !is.na(tmp_told)){
                tmp_nomtime_di <- c(tmp_told, tmp_di_df[,map_data$NOMTIME])
              } else {
                tmp_nomtime_di <- tmp_di_df[,map_data$NOMTIME] 
              }
            }
          }
          
          if(comp_required[["DOSECi"]] || comp_required[["DOSEC"]]) {
            if(!is.na(tmp_dose)) { 
              dose_c_i[[d]] <- dosec(data = tmp_di_df, map = map_data, idose=d)
            } else {
              dose_c_i[[d]] <- dose_c
            }
          }
          dof[[d]] <- ifelse(paste0("DOF",d) %in% names(map_data), ifelse(map_data[c(paste0("DOF",d))] %in% names(data_data), unique(tmp_di_df[,as.character(map_data[c(paste0("DOF",d))])])[1], NA), NA)
          
          if(comp_required[["TAUi"]]) {
            tau[[d]] <- tmp_di_df[, as.character(map_data[c(paste0("TAU",d))])][1]
            tau[[d]] <- as.numeric(tau[[d]])
          }
          if(comp_required[["TOLDi"]]) {
            told[[d]] <- tmp_di_df[, as.character(map_data[c(paste0("TOLD",d))])][1]
            told[[d]] <- as.numeric(told[[d]])
          }
          if(comp_required[["CMAXi"]]) {
            c_maxi[[d]] <- cmax(conc = tmp_conc_di, time = tmp_time_di)
          }
          if(comp_required[["TMAXi"]]){
            if(toupper(map_data$TIME) == "ACTUAL"){
              t_maxi[[d]] <- tmax(conc = tmp_conc_di, time = tmp_time_di, told = told[[d]])
            } else {
              t_maxi[[d]] <- tmax(conc = tmp_conc_di, time = tmp_time_di)
            }
          }
          if(comp_required[["CENDINFi"]]){
            cend_inf[[d]] <- cendinf(conc = tmp_conc_di, time = tmp_time_di, dof = dof[[d]], cmax = c_maxi[[d]])
          }
          if(comp_required[["CENDINFDNi"]]){
            cend_infdn[[d]] <- cendinf_dn(cendinf = cend_inf[[d]], dose = tmp_dose)
          }
          if(comp_required[["TENDINFi"]]){
            if(toupper(map_data$TIME) == "ACTUAL"){
              tend_inf[[d]] <- tendinf(conc = tmp_conc_di, time = tmp_time_di, dof = dof[[d]], tmax = t_maxi[[d]], told = told[[d]])
            } else {
              tend_inf[[d]] <- tendinf(conc = tmp_conc_di, time = tmp_time_di, dof = dof[[d]], tmax = t_maxi[[d]])
            }
          }
          if(comp_required[["CMINi"]]) {
            c_mini[[d]] <- cmin(conc = tmp_conc_di, time = tmp_time_di)
          }
          if(comp_required[["CLASTi"]]) {
            c_lasti[[d]] <- clast(conc = tmp_conc_di, time = tmp_time_di)
          }
          if(comp_required[["CMAXDNi"]]) {
            c_maxdni[[d]] <- cmax_dn(cmax = c_maxi[[d]], dose = tmp_dose)
          }
          if(comp_required[["CMINDNi"]]){
            c_mindni[[d]] <- cmin_dn(cmin = c_mini[[d]], dose = tmp_dose)
          }
          if(comp_required[["TMINi"]]){
            if(toupper(map_data$TIME) == "ACTUAL"){
              t_mini[[d]] <- tmin(conc = tmp_conc_di, time = tmp_time_di, told = told[[d]]) 
            } else {
              t_mini[[d]] <- tmin(conc = tmp_conc_di, time = tmp_time_di)
            }
          }
          if(comp_required[["TLASTi"]]) {
            t_lasti[[d]] <- tlast(conc = tmp_conc_di, time = tmp_time_di)
          }
          if(comp_required[["LASTTIMEi"]]) {
            last_timei[[d]] <- lasttime(conc = default_di_df[,map_data$CONC], time = default_di_df[,map_data$TIME])
          }
          if(comp_required[["AUCLASTi"]]) {
            auclasti[[d]] <- auc_last(conc = tmp_conc_di, time = tmp_time_di, method = method, exflag = auc_flag, t_last = t_lasti[[d]], t_max = t_maxi[[d]])
          }
          if(comp_required[["AUMCLASTi"]]) {
            aumclasti[[d]] <- aumc_last(conc = tmp_conc_di, time = aumc_time, method = method, exflag = auc_flag, t_max = t_maxi[[d]])
          }
          if(comp_required[["AUCINFOi"]]) {
            aucinfoi[[d]] <- auc_inf_o(conc = tmp_conc_di, time = tmp_time_di, method = method, kelflag = kel_flag, aucflag = auc_flag, auclast = auclasti[[d]], c_last = c_lasti[[d]], kel = kel_v)
          }
          if(comp_required[["AUCINFOCi"]]){
            aucinfoi_c[[d]] <- auc_inf_oc(kel = kel_v[["KEL"]], aucinfo = aucinfoi[[d]], c0 = obs_c_0)
          }
          if(comp_required[["AUCINFODNi"]]){
            aucinfoi_dn[[d]] <- auc_dn(auc = aucinfoi[[d]], dose = tmp_dose)
          }
          if(comp_required[["AUCINFPi"]]) {
            aucinfpi[[d]] <- auc_inf_p(conc = tmp_conc_di, time = tmp_time_di, method = method, kelflag = kel_flag, aucflag = auc_flag, auclast = auclasti[[d]], t_last = t_lasti[[d]], kel = kel_v)
          }
          if(comp_required[["AUCINFPCi"]]){
            aucinfpi_c[[d]] <- auc_inf_pc(kel = kel_v[["KEL"]], aucinfp = aucinfpi[[d]], c0 = obs_c_0)
          }
          if(comp_required[["AUCINFPDNi"]]){
            aucinfpi_dn[[d]] <- auc_dn(auc = aucinfpi[[d]], dose = tmp_dose)
          }
          if(comp_required[["AUMCINFOi"]]) {
            aumcinfoi[[d]] <- aumc_inf_o(conc = tmp_conc_di, time = aumc_time, method = method, kelflag = kel_flag, aucflag = auc_flag, aumclast = aumclasti[[d]], t_last = t_lasti[[d]], c_last = c_lasti[[d]], kel = kel_v)
          }
          if(comp_required[["AUMCINFPi"]]) {
            aumcinfpi[[d]] <- aumc_inf_p(conc = tmp_conc_di, time = aumc_time, method = method, kelflag = kel_flag, aucflag = auc_flag, aumclast = aumclasti[[d]], t_last = t_lasti[[d]], kel = kel_v)
          }
          if(comp_required[["AUCLASTDNi"]]) {
            auclasti_dn[[d]] <- auc_dn(auc = auclasti[[d]], dose = tmp_dose)
          }
          if(comp_required[["AUCTAUi"]]) {
            auctau[[d]] <- auc_tau(conc = tmp_conc_di, time = tmp_time_di, method = method, exflag = auc_flag, tau = told[[d]]+tau[[d]], t_max = t_maxi[[d]], orig_conc = orig_conc, orig_time = orig_time, last_crit_factor = last_crit_factor, kel = kel_v, auclast = auclasti[[d]])
          }
          if(comp_required[["AUCTAUDNi"]]) {
            auctaudn[[d]] <- auc_dn(auc = auctau[[d]], dose = tmp_dose)
          }
          if(comp_required[["AUMCTAUi"]]) {
            aumctaui[[d]] <- aumc_tau(conc = tmp_conc_di, time = aumc_time, method = method, exflag = auc_flag, tau = told[[d]]+tau[[d]], t_max = t_maxi[[d]], orig_conc = tmp_df[,map_data$CONC], orig_time = tmp_df[,map_data$TIME])
          }
          if(comp_required[["MRTLASTi"]]) {
            mrtlasti[[d]] <- mrt_last(conc = tmp_conc_di, time = aumc_time, method = method, model = "M2", aucflag = auc_flag, dof = dof[[d]], auclast = auclasti[[d]], aumclast = aumclasti[[d]])
          }
          if(comp_required[["MRTIVIFOi"]]){
            mrtivifoi[[d]] <- mrt_ivif_o(conc = tmp_conc_di, time = aumc_time, method = method, parameter = "SS", kelflag = kel_flag, aucflag = auc_flag, tau = tau[[d]], dof = dof[[d]], orig_conc = tmp_df[,map_data$CONC], orig_time = tmp_df[,map_data$TIME], aucinfo = aucinfoi[[d]], aumcinfo = aumcinfoi[[d]], auctau = auctau[[d]], aumctau = aumctaui[[d]])
          }
          if(comp_required[["MRTIVIFPi"]]){
            mrtivifpi[[d]] <- mrt_ivif_p(conc = tmp_conc_di, time = aumc_time, method = method, parameter = "SS", kelflag = kel_flag, aucflag = auc_flag, tau = tau[[d]], dof = dof[[d]], orig_conc = tmp_df[,map_data$CONC], orig_time = tmp_df[,map_data$TIME], aucinfp = aucinfpi[[d]], aumcinfp = aumcinfpi[[d]], auctau = auctau[[d]], aumctau = aumctaui[[d]])
          }
          if(comp_required[["AUCXPCTOi"]]){
            aucxpctoi[[d]] <- auc_XpctO(conc = tmp_conc_di, time = tmp_time_di, method = method, kelflag = kel_flag, aucflag = auc_flag, auc_info = aucinfoi[[d]], auclast = auclasti[[d]])
          }
          if(comp_required[["AUCXPCTPi"]]){
            aucxpctpi[[d]] <- auc_XpctP(conc = tmp_conc_di, time = tmp_time_di, method = method, kelflag = kel_flag, aucflag = auc_flag, auc_infp = aucinfpi[[d]], auclast = auclasti[[d]])
          }
          if(comp_required[["AUMCXPTOi"]]){
            aumcxptoi[[d]] <- aumc_XpctO(conc = tmp_conc_di, time = aumc_time, method = method, kelflag = kel_flag, aucflag = auc_flag, aumcinfo = aumcinfoi[[d]], aumclast = aumclasti[[d]])
          }
          if(comp_required[["AUMCXPTPi"]]){
            aumcxptpi[[d]] <- aumc_XpctP(conc = tmp_conc_di, time = aumc_time, method = method, kelflag = kel_flag, aucflag = auc_flag, aumcinfp = aumcinfpi[[d]], aumclast = aumclasti[[d]])
          }
          if(comp_required[["CAVi"]]) {
            ca_v[[d]] <- cav(auctau = auctau[[d]], tau = tau[[d]])
          }
          if(comp_required[["DIi"]]) {
            if(is.na(tmp_di_df[,map_data$TIME][1])){
              di[[d]] <- NA
            } else {
              di[[d]] <- paste0(tmp_di_df[,map_data$TIME][1], "-", tmp_di_df[,map_data$TIME][nrow(tmp_di_df)])
            }
          }
          if(comp_required[["DOSEi"]]) {
            dose[[d]] <- tmp_dose
          }
          if(comp_required[["CTROUGHi"]]){
            c_troughi[[d]] <- ctrough(conc = tmp_conc_di, time = tmp_nomtime_di, tau = tau[[d]], told = told[[d]])
          }
          if(comp_required[["PTROUGHRi"]]){
            p_troughri[[d]] <- ptroughr(cmax = c_maxi[[d]], ctrough = c_troughi[[d]])
          }
          if(comp_required[["AUCLAST"]]){
            tmp_auclast[[d]] <- auc_last(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME], method = method, exflag = auc_flag, t_last = t_lasti[[d]], t_max = t_maxi[[d]])
            if(d == di_col){
              overall_last_time <- tmp_df[nrow(tmp_df),map_data$TIME]
              tmp_last_time <- tmp_di_df[nrow(tmp_di_df),map_data$TIME]
              if((is.numeric(overall_last_time) && length(overall_last_time) > 0) && (is.numeric(tmp_last_time) && length(tmp_last_time) > 0)){
                if(isTRUE(overall_last_time > tmp_last_time)){
                  auclast <- auc_last(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, exflag = auc_flag, t_last = t_last, t_max = t_max)
                } else {
                  auclast <- sum(unlist(tmp_auclast)) 
                }
              } else {
                auclast <- sum(unlist(tmp_auclast)) 
              }
            }
          }
          if(comp_required[["AUCALL"]]){
            tmp_aucall[[d]] <- auc_all(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME], method = method, exflag = auc_flag, t_max = t_maxi[[d]])
            if(d == di_col){
              overall_last_time <- tmp_df[nrow(tmp_df),map_data$TIME]
              tmp_last_time <- tmp_di_df[nrow(tmp_di_df),map_data$TIME]
              if((is.numeric(overall_last_time) && length(overall_last_time) > 0) && (is.numeric(tmp_last_time) && length(tmp_last_time) > 0)){
                if(isTRUE(overall_last_time > tmp_last_time)){
                  aucall <- auc_all(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, exflag = auc_flag, t_max = t_max)
                } else {
                  aucall <- sum(unlist(tmp_aucall))
                }
              } else {
                aucall <- sum(unlist(tmp_aucall))
              }
            }
          }
          if(comp_required[["AUMCLAST"]]) {
            tmp_aumclast[[d]] <- aumc_last(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$TIME], method = method, exflag = auc_flag, t_max = t_maxi[[d]])
            if(d == di_col){
              overall_last_time <- tmp_df[nrow(tmp_df),map_data$TIME]
              tmp_last_time <- tmp_di_df[nrow(tmp_di_df),map_data$TIME]
              if((is.numeric(overall_last_time) && length(overall_last_time) > 0) && (is.numeric(tmp_last_time) && length(tmp_last_time) > 0)){
                if(isTRUE(overall_last_time > tmp_last_time)){
                  aumclast <- aumc_last(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, exflag = auc_flag, t_max = t_max)
                } else {
                  aumclast <- sum(unlist(tmp_aumclast))
                }
              } else {
                aumclast <- sum(unlist(tmp_aumclast))
              }
            }
          }
          if(comp_required[["CLTAUi"]]) {
            cl_tau[[d]] <- cltau(auctau = auctau[[d]], dose = dose_c_i[[d]])
          }
          if(comp_required[["CLTAUWi"]]) {
            cl_tauw[[d]] <- cltauw(cltau = cl_tau[[d]], normbs = norm_bs)
          }
          if(comp_required[["PTFi"]]) {
            pt_f[[d]] <- ptf(cmax = c_maxi[[d]], cmin = c_mini[[d]], cav = ca_v[[d]])
          }
          if(comp_required[["PTRi"]]) {
            pt_r[[d]] <- ptr(cmax = c_maxi[[d]], cmin = c_mini[[d]])
          }
          if(comp_required[["VSSOi"]]) {
            vsso[[d]] <- vsso(clo = cl_tau[[d]], mrto = mrtivifoi[[d]])
          }
          if(comp_required[["VSSPi"]]) {
            vssp[[d]] <- vssp(cltau = cl_tau[[d]], mrtp = mrtivifpi[[d]])
          }
          if(comp_required[["VSSOWi"]]) {
            vssow[[d]] <- vssow(vsso = vsso[[d]], normbs = norm_bs)
          }
          if(comp_required[["VSSPWi"]]) {
            vsspw[[d]] <- vsspw(vssp = vssp[[d]], normbs = norm_bs)
          }
          if(comp_required[["VZTAUi"]]) {
            vz_tau[[d]] <- vzftau(kel = kel_v[["KEL"]], auctau = auctau[[d]], dose = dose_c_i[[d]])
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
                  if(comp_required[["AUCT"]]){
                    auct[[t-1]] <- tmp
                  }
                  if(comp_required[["AUCTDN"]]){
                    auctdn[[t-1]] <- tmp_dn
                  }
                  auc_int[[t-1]] <- tmp_int
                } else {
                  if(prev_na){
                    prev_na <- FALSE
                    if(is.numeric(tmp)){
                      if(comp_required[["AUCT"]]){
                        prev_auc <- unlist(auct[[t-2]])
                        auct[[t-1]] <- sum(c(prev_auc, tmp), na.rm = TRUE)
                      }
                    }
                    if(is.numeric(tmp_dn)){
                      if(comp_required[["AUCTDN"]]){
                        prev_auc_dn <- unlist(auctdn[[t-2]])
                        auctdn[[t-1]] <- sum(c(prev_auc_dn, tmp_dn), na.rm = TRUE)
                      }
                    }
                  } else {
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
              if(comp_required[["AUCT"]]){
                if(length(auct) < auc_len) {
                  auct <- c(auct, rep(NA, (auc_len - length(auct))))
                }
              }
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
          if(comp_required[["AUCT1_T2"]] && auc_pair_check) {
            time <- sort(unique(data_data[,map_data$TIME]))
            time_di <- sort(tmp_di_df[,map_data$TIME])

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
              orig_auc_t1 <- as.numeric(map_data[, paste0("AUC.", t, ".T1")])
              orig_auc_t2 <- as.numeric(map_data[, paste0("AUC.", t, ".T2")])
              auc_t1 <- orig_auc_t1
              auc_t2 <- orig_auc_t2
              tmp_start_time <- time_di[1]
              tmp_end_time <- time_di[length(time_di)]
              
              if((tmp_start_time <= orig_auc_t1 && orig_auc_t1 < tmp_end_time) || (tmp_start_time < orig_auc_t2 && orig_auc_t2 <= tmp_end_time) || isTRUE(auct1_t2_check[[t]])){
                if(!isTRUE(auct1_t2_check[[t]]) && tmp_start_time <= orig_auc_t1 && orig_auc_t1 < tmp_end_time){
                  auct1_t2_check[[t]] <- TRUE
                }
                if(isTRUE(auct1_t2_check[[t]])){
                  if(d > 1 && orig_auc_t1 < tmp_start_time){
                    auc_t1 <- tmp_start_time
                  }
                  if(d < di_col && orig_auc_t2 > tmp_end_time){
                    auc_t2 <- tmp_end_time
                  }
                  
                  if((isTRUE(interpolation) || isTRUE(extrapolation))){
                    tmp <- auc_t1_t2(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], t1 = auc_t1, t2 = auc_t2, method = method, exflag = auc_flag, t_max = t_maxi[[d]], interpolate = interpolation, extrapolate = extrapolation, model = "M3", dosing_type = "SS", told = tmp_told, kel = kel_v, orig_conc = orig_conc, orig_time = orig_time, includeNA = TRUE)
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
                    tmp_auc <- auc_t1_t2(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], t1 = auc_t1, t2 = auc_t2, method = method, exflag = auc_flag, t_max = t_maxi[[d]], model = "M3", dosing_type = "SS", includeNA = TRUE)
                  }
                } else {
                  tmp_auc <- 0
                }
                if(tmp_start_time < orig_auc_t2 && orig_auc_t2 <= tmp_end_time){
                  auct1_t2_check[[t]] <- FALSE
                }
              } else {
                tmp_auc <- 0
              }
              
              if(d == 1){
                auct1_t2[[t]] <- tmp_auc
              } else {
                auct1_t2[[t]] <- sum(c(unlist(auct1_t2[[t]]), tmp_auc), na.rm = TRUE)
              }
            }
          }
        }
        for(d in 1:di_col){
          tmp_di_df  <- tmp_df[tmp_df[c(paste0("DI", d, "F"))] == 1,]
          tmp_di_df <- tmp_di_df[order(tmp_di_df[,map_data$TIME]),]
          tmp_dose <- tmp_di_df[, as.character(map_data[c(paste0("DOSE",d))])][1]
          
          tmp_told <- as.numeric(tmp_di_df[, as.character(map_data[c(paste0("TOLD",d))])][1])
          ctold_exists <- FALSE 
          if(tmp_told %in% tmp_di_df[,map_data$TIME]){
            idx <- which(tmp_di_df[,map_data$TIME] == tmp_told)
            tmp_ctold <- tmp_di_df[,map_data$CONC][length(idx)]
            if(!is.na(tmp_ctold)){
              ctold_exists <- TRUE
            }
          }
          tmp_tau <- as.numeric(tmp_di_df[, as.character(map_data[c(paste0("TAU",d))])][1])
          tmp_tau <- tmp_tau + tmp_told
          ctau_exists <- FALSE 
          if(tmp_tau %in% tmp_di_df[,map_data$TIME]){
            idx <- which(tmp_di_df[,map_data$TIME] == tmp_tau)
            tmp_ctau <- tmp_di_df[,map_data$CONC][length(idx)]
            if(!is.na(tmp_ctau)){
              ctau_exists <- TRUE
            }
          }
          if(!isTRUE(ctold_exists) && !is.na(tmp_told)){
            tmp_conc_di <- c(NA, tmp_di_df[,map_data$CONC])
            tmp_time_di <- c(tmp_told, tmp_di_df[,map_data$TIME])
            est_tmp <- estimate_told_concentration(conc = tmp_conc_di, time = tmp_time_di, interpolate = TRUE, extrapolate = TRUE, auc_method = "LIN", model = "M1", dosing_type = "SS", told = tmp_told, orig_conc = orig_conc, orig_time = orig_time)
            tmp_conc_di <- est_tmp[[1]]
          } else {
            tmp_conc_di <- tmp_di_df[,map_data$CONC]
            tmp_time_di <- tmp_di_df[,map_data$TIME]
          }
          tmp_nomtime_di <- tmp_time_di
          if(isTRUE("NOMTIME" %in% names(map_data))){
            if(isTRUE(map_data$NOMTIME %in% names(tmp_di_df))){
              if(!isTRUE(ctold_exists) && !is.na(tmp_told)){
                tmp_nomtime_di <- c(tmp_told, tmp_di_df[,map_data$NOMTIME])
              } else {
                tmp_nomtime_di <- tmp_di_df[,map_data$NOMTIME] 
              }
            }
          }
          
          if(comp_required[["AUCALLDN"]]){
            aucalldn[[d]] <- auc_dn(auc = aucall, dose = tmp_dose)
          }
          if((d+1) <= di_col){
            next_tmp_di_df  <- tmp_df[tmp_df[c(paste0("DI", d+1, "F"))] == 1,]
            next_tmp_di_df <- next_tmp_di_df[order(next_tmp_di_df[,map_data$TIME]),]
            next_tmp_told <- as.numeric(next_tmp_di_df[, as.character(map_data[c(paste0("TOLD",d+1))])][1])
            next_ctold_exists <- FALSE 
            if(next_tmp_told %in% next_tmp_di_df[,map_data$TIME]){
              idx <- which(next_tmp_di_df[,map_data$TIME] == next_tmp_told)
              next_tmp_ctold <- next_tmp_di_df[,map_data$CONC][length(idx)]
              if(!is.na(next_tmp_ctold)){
                next_ctold_exists <- TRUE
              }
            }
            if(!isTRUE(next_ctold_exists)){
              next_tmp_conc_di <- c(NA, next_tmp_di_df[,map_data$CONC])
              next_tmp_time_di <- c(next_tmp_told, next_tmp_di_df[,map_data$TIME])
              next_est_tmp <- estimate_told_concentration(conc = next_tmp_conc_di, time = next_tmp_time_di, interpolate = TRUE, extrapolate = TRUE, auc_method = "LIN", model = "M3", dosing_type = "SS", told = next_tmp_told, orig_conc = orig_conc, orig_time = orig_time)
              next_tmp_conc_di <- next_est_tmp[[1]]
              next_ctold_est <- next_tmp_conc_di[1]
              
              if(comp_required[["CTROUGHENDi"]]){
                c_troughendi[[d]] <- ctroughend(conc = tmp_conc_di, time = tmp_nomtime_di, tau = tau[[d]], told = told[[d]], ctold = next_ctold_est)
              }
              if(comp_required[["PTROUGHRENDi"]]){
                p_troughrendi[[d]] <- ptroughrend(cmax = c_maxi[[d]], ctrough = c_troughendi[[d]])
              }
            } else {
              if(comp_required[["CTROUGHENDi"]]){
                c_troughendi[[d]] <- ctroughend(conc = tmp_conc_di, time = tmp_nomtime_di, tau = tau[[d]], told = told[[d]])
              }
              if(comp_required[["PTROUGHRENDi"]]){
                p_troughrendi[[d]] <- ptroughrend(cmax = c_maxi[[d]], ctrough = c_troughendi[[d]])
              }
            }
          } else {
            if(comp_required[["CTROUGHENDi"]]){
              c_troughendi[[d]] <- ctroughend(conc = tmp_conc_di, time = tmp_nomtime_di, tau = tau[[d]], told = told[[d]])
            }
            if(comp_required[["PTROUGHRENDi"]]){
              p_troughrendi[[d]] <- ptroughrend(cmax = c_maxi[[d]], ctrough = c_troughendi[[d]])
            }
          }
        }
        if(comp_required[["AUCINFO"]]){
          aucinf_o <- auc_inf_o(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag, auclast = auclast, c_last = c_last, kel = kel_v)
        }
        if(comp_required[["AUCINFP"]]){
          aucinf_p <- auc_inf_p(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag, auclast = auclast, t_last = t_last, kel = kel_v)
        }
        if(comp_required[["AUCLASTDN"]]) {
          auclastdn <- auc_dn(auc = auclast, dose = main_dose)
        }
        if(comp_required[["AUCXPCTO"]]){
          aucxpcto <- auc_XpctO(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag, auc_info = aucinf_o, auclast = auclast)
        }
        if(comp_required[["AUCXPCTP"]]){
          aucxpctp <- auc_XpctP(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag, auc_infp = aucinf_p, auclast = auclast)
        }
        if(comp_required[["AUCINFODN"]]) {
          aucinf_odn <- auc_dn(auc = aucinf_o, dose = main_dose)
        }
        if(comp_required[["AUCINFPDN"]]) {
          aucinf_pdn <- auc_dn(auc = aucinf_p, dose = main_dose)
        }
        if(comp_required[["AUMCLAST"]]) {
          aumclast <- aumc_last(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, exflag = auc_flag, t_max = t_max)
        }
        if(comp_required[["AUMCINFO"]]) {
          aumcinf_o <- aumc_inf_o(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag, aumclast = aumclast, c_last = c_last, t_last = t_last, kel = kel_v)
        }
        if(comp_required[["AUMCINFP"]]) {
          aumcinf_p <- aumc_inf_p(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag, aumclast = aumclast, t_last = t_last, kel = kel_v)
        }
        if(comp_required[["MRTLAST"]]) {
          mrtlast <- mrt_last(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, model = "M2", aucflag = auc_flag, dof = dof[[d]], auclast = auclast, aumclast = aumclast)
        }
        if(comp_required[["AUMCXPTO"]]){
          aumcxpto <- aumc_XpctO(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag, aumcinfo = aumcinf_o, aumclast = aumclast)
        }
        if(comp_required[["AUMCXPTP"]]){
          aumcxptp <- aumc_XpctP(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, kelflag = kel_flag, aucflag = auc_flag, aumcinfp = aumcinf_p, aumclast = aumclast)
        }
        if(comp_required[["KEL"]]){
          exflag <- !as.logical(kel_flag)

          pkdataid <- tmp_df[,map_data$FLGMERGE][exflag]
          time <- tmp_df[,map_data$TIME][exflag]
          conc <- tmp_df[,map_data$CONC][exflag]
          cest_kel <- rep(NA, length(conc))
          
          if(!is.na(kel_v[["KEL"]])){
            cest_kel <- estimate_concentration(time, conc, slope=kel_v[["KEL"]])
          }
        } else {
          pkdataid <- NULL
        }

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

        computation_df[i, "SDEID"] <- unique(data_data[,map_data$SDEID])[i]
        if(disp_required[["DOSEi"]]) {
          computation_df[i, dosenames] <- unlist(dose)
        }
        if(disp_required[["DOSEC"]]) {
          computation_df[i, "DOSEC"] <- dose_c
        }
        if(disp_required[["DOSECi"]]) {
          computation_df[i, paste0("DOSEC",1:di_col)] <- unlist(dose_c_i)
        }
        if(disp_required[["DOFi"]]){
          computation_df[i, paste0("DOF",1:di_col)] <- unlist(dof)
        }
        if(disp_required[["CENDINFi"]]){
          computation_df[i, paste0("CENDINF",1:di_col)] <- unlist(cend_inf)
        }
        if(disp_required[["CENDINFDNi"]]){
          computation_df[i, paste0("CENDINFDN",1:di_col)] <- unlist(cend_infdn)
        }
        if(disp_required[["TENDINFi"]]){
          computation_df[i, paste0("TENDINF",1:di_col)] <- unlist(tend_inf)
        }
        if(disp_required[["CMAX"]]) {
          computation_df[i, "CMAX"] <- c_max
        }
        if(disp_required[["CMAXi"]]) {
          computation_df[i, paste0("CMAX",1:di_col)] <- unlist(c_maxi)
        }
        if(disp_required[["CMAXDN"]]) {
          computation_df[i, "CMAXDN"] <- c_maxdn
        }
        if("CMAXDNi" %in% parameter_list && "CMAXi" %in% parameter_list) {
          computation_df[i, paste0("CMAXDN",1:di_col)] <- unlist(c_maxdni)
        }
        if(disp_required[["FLGACCEPTPREDOSE"]] && "FLGACCEPTPREDOSECRIT" %in% names(map_data)){
          pre_dose_crit <- suppressWarnings(as.numeric(map_data$FLGACCEPTPREDOSECRIT))
          if(is.numeric(pre_dose_crit) && !is.na(pre_dose_crit)){
            pre_dose <- tmp_df[,map_data$CONC][tmp_df[,map_data$TIME] == 0][1]
            if(is.numeric(c_maxi[[1]])){
              computation_df[i, "FLGACCEPTPREDOSE"] <- ifelse(pre_dose > (c_maxi[[1]] * pre_dose_crit), 0, 1)
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
        if(disp_required[["CMINDN"]]){
          computation_df[i, "CMINDN"] <- c_mindn
        }
        if(disp_required[["CMINDNi"]]){
          computation_df[i, paste0("CMINDN",1:di_col)] <- unlist(c_mindni)
        }
        if(disp_required[["CMINi"]]) {
          computation_df[i, paste0("CMIN",1:di_col)] <- unlist(c_mini)
        }
        if(disp_required[["CLAST"]]) {
          computation_df[i, "CLAST"] <- c_last
        }
        if(disp_required[["CLASTi"]]) {
          computation_df[i, paste0("CLAST",1:di_col)] <- unlist(c_lasti)
        }
        if(disp_required[["TMAX"]]) {
          computation_df[i, "TMAX"] <- t_max
        }
        if(disp_required[["TMAXi"]]) {
          computation_df[i, paste0("TMAX",1:di_col)] <- unlist(t_maxi)
        }
        if(disp_required[["FLGACCEPTTMAX"]] && "FLGEMESIS" %in% names(map_data)){
          computation_df[i, "FLGACCEPTTMAX"] <- 1
        }
        if(disp_required[["TMIN"]]) {
          computation_df[i, "TMIN"] <- t_min
        }
        if(disp_required[["TMINi"]]) {
          computation_df[i, paste0("TMIN",1:di_col)] <- unlist(t_mini)
        }
        if(disp_required[["TLAST"]]) {
          computation_df[i, "TLAST"] <- t_last
        }
        if(disp_required[["TLASTi"]]) {
          computation_df[i, paste0("TLAST",1:di_col)] <- unlist(t_lasti)
        }
        if(disp_required[["CTROUGHi"]]){
          computation_df[i, paste0("CTROUGH",1:di_col)] <- unlist(c_troughi)
        }
        if(disp_required[["CTROUGHENDi"]]){
          computation_df[i, paste0("CTROUGHEND",1:di_col)] <- unlist(c_troughendi)
        }
        if(disp_required[["PTROUGHRi"]]){
          computation_df[i, paste0("PTROUGHR",1:di_col)] <- unlist(p_troughri)
        }
        if(disp_required[["PTROUGHRENDi"]]){
          computation_df[i, paste0("PTROUGHREND",1:di_col)] <- unlist(p_troughrendi)
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
        if(disp_required[["KELR"]]){
          computation_df[i, "KELR"] <- ifelse("KELR" %in% names(kelr_v), kelr_v[["KELR"]], NA)
        }
        if(disp_required[["KELRSQ"]]){
          computation_df[i, "KELRSQ"] <- ifelse("KELRSQ" %in% names(kelr_v), kelr_v[["KELRSQ"]], NA)
        }
        if(disp_required[["KELRSQA"]]){
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
        if(disp_required[["LASTTIMEi"]]) {
          computation_df[i, paste0("LASTTIME",1:di_col)] <- unlist(last_timei)
        }
        if(disp_required[["FLGACCEPTTAU"]] && "LASTTIMEACCEPTCRIT" %in% names(map_data)) {
          if(!is.na(last_crit_factor)){
            if(paste0("TAU",di_col) %in% names(map_data)){
              if(map_data[, paste0("TAU",di_col)] %in% names(data_data)) {
                tau_val <- unique(tmp_df[, map_data[, paste0("TAU",di_col)]])[1]
                if(!is.na(tau_val) && is.numeric(tau_val) && !is.na(last_crit_factor) && is.numeric(last_crit_factor)){
                  lt_accept_crit <- tau_val * last_crit_factor
                  computation_df[i, "FLGACCEPTTAU"] <- ifelse(last_time >= lt_accept_crit, 1, 0)
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
          computation_df[i, paste0("AUCALLDN",1:di_col)] <- unlist(aucalldn)
        }
        if(disp_required[["AUCLAST"]]) {
          computation_df[i, "AUCLAST"] <- auclast
        }
        if(disp_required[["AUCLASTDN"]]) {
          computation_df[i, "AUCLASTDN"] <- auclastdn
        }
        if(disp_required[["AUCLASTi"]]) {
          computation_df[i, paste0("AUCLAST",1:di_col)] <- unlist(auclasti)
        }
        if(disp_required[["AUCLASTDNi"]]) {
          computation_df[i, paste0("AUCLASTDN",1:di_col)] <- unlist(auclasti_dn)
        }
        if(disp_required[["AUMCLASTi"]]) {
          computation_df[i, paste0("AUMCLAST",1:di_col)] <- unlist(aumclasti)
        }
        if(disp_required[["AUCT"]] && auc_len > 1) {
          computation_df[i, paste0("AUC",1:auc_len)] <- unlist(auct)
        }
        if(disp_required[["AUCTDN"]] && auc_len > 1) {
          computation_df[i, paste0("AUC",1:auc_len,"DN")] <- unlist(auctdn)
        }
        if((disp_required[["AUCT"]] || disp_required[["AUCTDN"]]) && auc_len > 1){
          computation_df[i, paste0("AUCINT",1:auc_len)] <- unlist(auc_int)
        }
        if(disp_required[["AUCT1_T2"]] && auc_pair_check) {
          computation_df[i, paste0("AUC", map_data[,rep(paste0("AUC.", 1:auc_par_len, ".T1"))], "_", map_data[,rep(paste0("AUC.", 1:auc_par_len, ".T2"))])] <- unlist(auct1_t2)
        }
        if(disp_required[["AUCINFO"]]) {
          computation_df[i, "AUCINFO"] <- aucinf_o
        }
        if(disp_required[["AUCINFOi"]]) {
          computation_df[i, paste0("AUCINFO",1:di_col)] <- unlist(aucinfoi)
        }
        if(disp_required[["AUCINFOCi"]]){
          computation_df[i, paste0("AUCINFOC",1:di_col)] <- unlist(aucinfoi_c)
        }
        if(disp_required[["AUCINFODN"]]) {
          computation_df[i, "AUCINFODN"] <- aucinf_odn
        }
        if(disp_required[["AUCINFODNi"]]){
          computation_df[i, paste0("AUCINFODN",1:di_col)] <- unlist(aucinfoi_dn)
        }
        if(disp_required[["CEST"]]) {
          computation_df[i, "CEST"] <- c_est
        }
        if(disp_required[["AUCINFP"]]) {
          computation_df[i, "AUCINFP"] <- aucinf_p
        }
        if(disp_required[["AUCINFPi"]]) {
          computation_df[i, paste0("AUCINFP",1:di_col)] <- unlist(aucinfpi) 
        }
        if(disp_required[["AUCINFPCi"]]){
          computation_df[i, paste0("AUCINFPC",1:di_col)] <- unlist(aucinfpi_c)
        }
        if(disp_required[["AUCINFPDN"]]) {
          computation_df[i, "AUCINFPDN"] <- aucinf_pdn
        }
        if(disp_required[["AUCINFPDNi"]]){
          computation_df[i, paste0("AUCINFPDN",1:di_col)] <- unlist(aucinfpi_dn)
        }
        if(disp_required[["AUMCINFOi"]]) {
          computation_df[i, paste0("AUMCINFO",1:di_col)] <- unlist(aumcinfoi)
        }
        if(disp_required[["AUMCINFPi"]]) {
          computation_df[i, paste0("AUMCINFP",1:di_col)] <- unlist(aumcinfpi)
        }
        if(disp_required[["AUCTAUi"]]) {
          computation_df[i, paste0("AUCTAU",1:di_col)] <- unlist(auctau)
        }
        if(disp_required[["AUCTAUDNi"]]) {
          computation_df[i, paste0("AUCTAUDN",1:di_col)] <- unlist(auctaudn)
        }
        if(disp_required[["AUMCTAUi"]]) {
          computation_df[i, paste0("AUMCTAU",1:di_col)] <- unlist(aumctaui)
        }
        if(disp_required[["MRTLAST"]]) {
          computation_df[i, "MRTLAST"] <- mrtlast
        }
        if(disp_required[["MRTLASTi"]]) {
          computation_df[i, paste0("MRTLAST",1:di_col)] <- unlist(mrtlasti)
        }
        if(disp_required[["MRTIVIFOi"]]){
          computation_df[i, paste0("MRTIVIFO",1:di_col)] <- unlist(mrtivifoi)
        }
        if(disp_required[["MRTIVIFPi"]]){
          computation_df[i, paste0("MRTIVIFP",1:di_col)] <- unlist(mrtivifpi)
        }
        if(disp_required[["AUCXPCTO"]]){
          computation_df[i, "AUCXPCTO"] <- aucxpcto
        }
        if(disp_required[["AUCXPCTOi"]]){
          computation_df[i, paste0("AUCXPCTO",1:di_col)] <- unlist(aucxpctoi)
        }
        if(disp_required[["AUCXPCTP"]]){
          computation_df[i, "AUCXPCTP"] <- aucxpctp
        }
        if(disp_required[["AUCXPCTPi"]]){
          computation_df[i, paste0("AUCXPCTP",1:di_col)] <- unlist(aucxpctpi)
        }
        if(disp_required[["AUMCXPTO"]]){
          computation_df[i, "AUMCXPTO"] <- aumcxpto
        }
        if(disp_required[["AUMCXPTOi"]]){
          computation_df[i, paste0("AUMCXPTO",1:di_col)] <- unlist(aumcxptoi)
        }
        if(disp_required[["AUMCXPTP"]]){
          computation_df[i, "AUMCXPTP"] <- aumcxptp
        }
        if(disp_required[["AUMCXPTPi"]]){
          computation_df[i, paste0("AUMCXPTP",1:di_col)] <- unlist(aumcxptpi)
        }
        if(disp_required[["CAVi"]]) {
          computation_df[i, paste0("CAV",1:di_col)] <- unlist(ca_v)
        }
        if(disp_required[["CLTAUi"]]) {
          computation_df[i, paste0("CLTAU",1:di_col)] <- unlist(cl_tau)
        }
        if(disp_required[["CLTAUWi"]]) {
          computation_df[i, paste0("CLTAUW",1:di_col)] <- unlist(cl_tauw)
        }
        if(disp_required[["PTFi"]]) {
          computation_df[i, paste0("PTF",1:di_col)] <- unlist(pt_f)
        }
        if(disp_required[["PTRi"]]) {
          computation_df[i, paste0("PTR",1:di_col)] <- unlist(pt_r)
        }
        if(disp_required[["VSSOi"]]) {
          computation_df[i, paste0("VSSO",1:di_col)] <- unlist(vsso)
        }
        if(disp_required[["VSSPi"]]) {
          computation_df[i, paste0("VSSP",1:di_col)] <- unlist(vssp)
        }
        if(disp_required[["VSSOWi"]]) {
          computation_df[i, paste0("VSSOW",1:di_col)] <- unlist(vssow)
        }
        if(disp_required[["VSSPWi"]]) {
          computation_df[i, paste0("VSSPW",1:di_col)] <- unlist(vsspw)
        }
        if(disp_required[["VZTAUi"]]) {
          computation_df[i, paste0("VZTAU",1:di_col)] <- unlist(vz_tau)
        }
        if(disp_required[["VZTAUWi"]]) {
          computation_df[i, paste0("VZTAUW",1:di_col)] <- unlist(vz_tauw)
        }
        computation_df[i, paste0("CONC",1:(auc_len+1))] <- c(tmp_df[,map_data$CONC], rep(NA, ((auc_len+1) - length(tmp_df[,map_data$CONC]))))
        computation_df[i, paste0("CONCTIME",1:(auc_len+1))] <- c(tmp_df[,map_data$TIME], rep(NA, ((auc_len+1) - length(tmp_df[,map_data$TIME]))))

        if(disp_required[["DIi"]]) {
          computation_df[i, paste0("DI",1:di_col)] <- unlist(di)
        }
        if(disp_required[["TAUi"]]) {
          computation_df[i, paste0("TAU",1:di_col)] <- unlist(tau)
        }
        if(disp_required[["TOLD"]]) {
          computation_df[i, paste0("TOLD",1:di_col)] <- unlist(told)
        }
        if(disp_required[["CTOLDesti"]]){
          computation_df[i, paste0("CTOLDest",1:di_col)] <- unlist(ctold_est)
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
  if(disp_required[["FLGACCEPTTAU"]] && "LASTTIMEACCEPTCRIT" %in% names(map_data)) {
    if("FLGACCEPTKEL" %in% names(computation_df)){
      if(nrow(computation_df[!is.na(computation_df[,"FLGACCEPTKEL"]) & computation_df[,"FLGACCEPTKEL"] != 1,]) > 0){
        computation_df[!is.na(computation_df[,"FLGACCEPTKEL"]) & computation_df[,"FLGACCEPTKEL"] != 1,][,"FLGACCEPTTAU"] <- 0  
      }
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
  
  if(isTRUE(optimize_kel) && comp_required[["TMAXi"]] && comp_required[["TLASTi"]] && comp_required[["CMAXi"]] && comp_required[["CLASTi"]] && comp_required[["AUCLASTi"]] &&
     "FLGACCEPTKELCRIT" %in% names(map_data) && "FLGEXKEL" %in% names(map_data) && map_data$FLGEXKEL %in% names(data_data)){
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
