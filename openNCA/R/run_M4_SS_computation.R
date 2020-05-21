#' Run M4 SS Computation
#'
#' This function will compute all the relevant parameters for a M4 model Steady State (SS).\cr
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
#' The following are the functions that this function uses to generate parameters: \cr
#' \strong{Return List options} \cr
#' \enumerate{
#'  \item \strong{ae}: Refer to \code{\link{ae}} for more details
#'  \item \strong{aepct}: Refer to \code{\link{aepct}} for more details
#'  \item \strong{aet}: Refer to \code{\link{aet}} for more details
#'  \item \strong{aetpct}: Refer to \code{\link{aetpct}} for more details
#'  \item \strong{maxrate}: Refer to \code{\link{maxrate}} for more details
#'  \item \strong{tmaxrate}: Refer to \code{\link{tmaxrate}} for more details
#'  \item \strong{ratelast}: Refer to \code{\link{ratelast}} for more details
#'  \item \strong{midptlast}: Refer to \code{\link{midptlast}} for more details
#'  \item \strong{kel}: Refer to \code{\link{kel}} for more details
#'  \item \strong{kelr}: Refer to \code{\link{kel_r}} for more details
#'  \item \strong{lasttime}: Refer to \code{\link{lasttime}} for more details
#'  \item \strong{auc_dn}: Refer to \code{\link{auc_dn}} for more details
#'  \item \strong{auc_all}: Refer to \code{\link{auc_all}} for more details
#'  \item \strong{auc_last}: Refer to \code{\link{auc_last}} for more details
#'  \item \strong{auc_t1_t2}: Refer to \code{\link{auc_t1_t2}} for more details
#'  \item \strong{auc_inf_o}: Refer to \code{\link{auc_inf_o}} for more details
#'  \item \strong{auc_inf_p}: Refer to \code{\link{auc_inf_p}} for more details
#'  \item \strong{mrt_last}: Refer to \code{\link{mrt_last}} for more details
#'  \item \strong{mrto}: Refer to \code{\link{mrt_evif_o}} for more details
#'  \item \strong{mrtp}: Refer to \code{\link{mrt_evif_p}} for more details
#'  \item \strong{auc_xpct_o}: Refer to \code{\link{auc_XpctO}} for more details
#'  \item \strong{auc_xpct_p}: Refer to \code{\link{auc_XpctP}} for more details
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
#' Please note that this function does not contain all the features of a M4 SS computation, so it is recommended that you
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
#'  \item M4SS_Parameters: Caculated default/specified parameters
#' }
#' OR \cr
#' \strong{List} \cr
#' \itemize{
#'  \item data_out: Calculated default/specified M4SS Parameters
#'  \item optimized_kel_flag: Optimized KEL flag data used to calulate KEL based parameters
#'  \item est_data: Calculated Estimated Parameters
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
#' @export
run_M4_SS_computation <- function(data = NULL, map = NULL, method = 1, model_regex = "^M4(SS)*?$", parameter_list = list(), return_list = list(), raw_results_debug = FALSE, optimize_kel_debug = FALSE){
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
  
  if("AMOUNT" %in% names(map_data) && "AMOUNTU" %in% names(map_data)){
    if(!(parameter_required(map_data$AMOUNT, names(data_data)) && parameter_required(map_data$AMOUNTU, names(data_data)))) {
      stop("Amount: '", map_data$AMOUNT, "' and Amount Unit: '", map_data$AMOUNTU, "' isn't present in input dataset\n")
    }
  } else {
    stop("Dataset provided via 'map' does not contain the 'AMOUNT' and/or 'AMOUNTU' column")
  }

  ss_dose <- c("DI1F", "DI2F", "DI3F", "DI4F", "DI5F")

  if(any(ss_dose %in% names(data_data))){
    di_col <- sum(ss_dose %in% names(data_data))
  } else {
    stop("Unable to find dosing interval for Steady State data! Please provide a valid 'data' parameter")
  }

  auc_list <- c("AURCT1_T2")
  aet_list <- c("AET", "AETPCT")
  interval_list <- c("MAXRATEi", "TMAXRATEi", "RATELASTi", "MIDPTLASTi", "DIi", "TAUi", "TOLDi", "DOSEi")
  regular_list <- c("AE", "AEPCT", "TLAG", "KEL", "KELTMLO", "KELTMHI", "KELNOPT", "KELR", "KELRSQ", "KELRSQA", "THALF", "THALFF",
                    "AUCDN", "AURCALL", "AURCLAST", "AURCINFO", "AURCINFP", "AURCXPCTO", "AURCXPCTP", "VOLSUM")
  optional_list <- c("DOSEi", "TAUi", "TOLDi")
  regular_int_type <- NULL
  auc_pair_check <- FALSE

  tmp_mid_pt <- midpt(start_time = data_data[,map_data$TIME], end_time = data_data[,map_data$ENDTIME])
  mid_len <- length(unique(tmp_mid_pt))
  mid_col <- ifelse(sum(auc_list %in% parameter_list) == 1, 2, 0)
  aet_len <- length(unique(data_data[,map_data$ENDTIME]))
  row_len <- max(unlist(lapply(unique(data_data[,map_data$SDEID]), function(x){ nrow(data_data[data_data[,map_data$SDEID] == x,]) })))+1

  aucpari <- grep('^AUC.([0-9]+?).T[1-2]$', names(map_data), ignore.case=TRUE, perl=TRUE)
  if(length(aucpari)>0) {
    aurc_par_len <- floor(length(aucpari)/2)
    g <- names(map_data)[aucpari]
    aucpar1 <- grep('^AUC.([0-9]+?).T[1]$', names(map_data), ignore.case=TRUE, perl=TRUE)
    aucpar2 <- grep('^AUC.([0-9]+?).T[2]$', names(map_data), ignore.case=TRUE, perl=TRUE)
    if(length(aucpar1)!=length(aucpar2)) {
      msg <- paste0(function_name, ': unequal Partial AURCs specified: ', names(map_data)[aucpar1], ' vs ', names(map_data)[aucpar2])
      warning(msg)
      msg <- paste0(function_name, ': no Partial AURCs will be generated')
      warning(msg)
      aurc_par_len <- 0
    }
  } else { aurc_par_len <- 0 }
  
  interval_col <- sum(interval_list %in% parameter_list)
  reg_col <- sum(regular_list %in% parameter_list) + ifelse(any(c("KELRSQ","KELRSQA") %in% parameter_list), 1, 0)
  aet_col <- ifelse(sum(aet_list %in% parameter_list) >= 1, sum(aet_list %in% parameter_list), 0)
  tmp_aet_col <- ifelse("AET" %in% parameter_list, aet_col+1, aet_col)
  col <- reg_col + (tmp_aet_col * aet_len-1) + (mid_col * mid_len) + (interval_col * di_col) + 1 + (2 * (aet_len))
  
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
  if("LASTTIMEACCEPTCRIT" %in% names(map_data) && ("LASTTIME" %in% parameter_list)){
    col <- col + 1
  }
  if("FLGEMESIS" %in% names(map_data) && ("TMAX" %in% parameter_list)){
    col <- col + 1
  }
  if("FLGACCEPTPREDOSECRIT" %in% names(map_data) && ("CMAX" %in% parameter_list)){
    col <- col + 1
  }
  if(aurc_par_len > 0){
    for(t in 1:aurc_par_len){
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
  if(disp_required[["AT"]]) {
    col_names <- c(col_names, rep(paste0("AMT.", sprintf("%.2f", sort(unique(data_data[,map_data$ENDTIME])[1:aet_len])))))
    regular_int_type <- c(regular_int_type, rep(paste0("AMT.", sprintf("%.2f", sort(unique(data_data[,map_data$ENDTIME])[1:aet_len])))))
  }
  if(disp_required[["AET"]]) {
    col_names <- c(col_names, rep(paste0("AE.", sprintf("%.2f", sort(unique(data_data[,map_data$ENDTIME])[1:aet_len])))))
    regular_int_type <- c(regular_int_type, rep(paste0("AE.", sprintf("%.2f", sort(unique(data_data[,map_data$ENDTIME])[1:aet_len])))))
  }
  if(disp_required[["AETPCTi"]]) {
    col_names <- c(col_names, rep(paste0("AEPCT.", sprintf("%.2f", unique(data_data[,map_data$ENDTIME])[1:aet_len]))))
    regular_int_type <- c(regular_int_type, rep(paste0("AEPCT.", sprintf("%.2f", unique(data_data[,map_data$ENDTIME])[1:aet_len]))))
  }
  if(disp_required[["AE"]]) {
    col_names <- c(col_names, "AE")
    regular_int_type <- c(regular_int_type, "AE")
  }
  if(disp_required[["AEPCTi"]]) {
    col_names <- c(col_names, rep(paste0("AEPCT",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AEPCT",1:di_col)))
  }
  if(disp_required[["AETAUi"]]){
    col_names <- c(col_names, rep(paste0("AETAU",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AETAU",1:di_col)))
  }
  if(disp_required[["AETAUPTi"]]){
    col_names <- c(col_names, rep(paste0("AETAUPT",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("AETAUPT",1:di_col)))
  }
  if(disp_required[["MAXRATE"]]) {
    col_names <- c(col_names, "MAXRATE")
    regular_int_type <- c(regular_int_type, "MAXRATE")
  }
  if(disp_required[["TMAXRATE"]]) {
    col_names <- c(col_names, "TMAXRATE")
    regular_int_type <- c(regular_int_type, "TMAXRATE")
  }
  if(disp_required[["RATELAST"]]) {
    col_names <- c(col_names, "RATELAST")
    regular_int_type <- c(regular_int_type, "RATELAST")
  }
  if(disp_required[["MIDPTLAST"]]) {
    col_names <- c(col_names, "MIDPTLAST")
    regular_int_type <- c(regular_int_type, "MIDPTLAST")
  }
  if(disp_required[["MAXRATEi"]]) {
    col_names <- c(col_names, rep(paste0("MAXRATE",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("MAXRATE",1:di_col)))
  }
  if(disp_required[["TMAXRATEi"]]) {
    col_names <- c(col_names, rep(paste0("TMAXRATE",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("TMAXRATE",1:di_col)))
  }
  if(disp_required[["RATELASTi"]]) {
    col_names <- c(col_names, rep(paste0("RATELAST",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("RATELAST",1:di_col)))
  }
  if(disp_required[["MIDPTLASTi"]]) {
    col_names <- c(col_names, rep(paste0("MIDPTLAST",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("MIDPTLAST",1:di_col)))
  }
  if(disp_required[["TLAG"]]) {
    col_names <- c(col_names, "TLAG")
    regular_int_type <- c(regular_int_type, "TLAG")
  }
  if(disp_required[["KEL"]]) {
    col_names <- c(col_names, "KEL")
    regular_int_type <- c(regular_int_type, "KEL")
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
  if(disp_required[["AURCALL"]]) {
    col_names <- c(col_names, "AURCALL")
    regular_int_type <- c(regular_int_type, "AURCALL")
  }
  if(disp_required[["AURCLAST"]]) {
    col_names <- c(col_names, "AURCLAST")
    regular_int_type <- c(regular_int_type, "AURCLAST")
  }
  if(disp_required[["AURCT"]] && row_len >= 2) {
    col_names <- c(col_names, rep(paste0("AURC",1:(row_len-1))), rep(paste0("AURCINT",1:(row_len-1))))
    regular_int_type <- c(regular_int_type, paste0("AURC",1:(row_len-1)))
  }
  if(disp_required[["AURCT1_T2"]] && auc_pair_check) {
    col_names <- c(col_names, rep(paste0("AURC", map_data[,rep(paste0("AUC.", 1:aurc_par_len, ".T1"))], "_", map_data[,rep(paste0("AUC.", 1:aurc_par_len, ".T2"))])))
    regular_int_type <- c(regular_int_type, rep(paste0("AURC", map_data[,rep(paste0("AUC.", 1:aurc_par_len, ".T1"))], "_", map_data[,rep(paste0("AUC.", 1:aurc_par_len, ".T2"))])))
  }
  if(disp_required[["AURCINFO"]]) {
    col_names <- c(col_names, "AURCINFO")
    regular_int_type <- c(regular_int_type, "AURCINFO")
  }
  if(disp_required[["AURCINFP"]]) {
    col_names <- c(col_names, "AURCINFP")
    regular_int_type <- c(regular_int_type, "AURCINFP")
  }
  if(disp_required[["AURCXPCTO"]]){
    col_names <- c(col_names, "AURCXPCTO")
    regular_int_type <- c(regular_int_type, "AURCXPCTO")
  }
  if(disp_required[["AURCXPCTP"]]){
    col_names <- c(col_names, "AURCXPCTP")
    regular_int_type <- c(regular_int_type, "AURCXPCTP")
  }
  if(disp_required[["VOLSUM"]]) {
    col_names <- c(col_names, "VOLSUM")
    regular_int_type <- c(regular_int_type, "VOLSUM")
  }
  if(parameter_required("^(RATE)([0-9]*?|A|N)$", parameter_list) || parameter_required(dependent_parameters("^(RATE)([0-9]*?|A|N)$"), parameter_list)) {
    col_names <- c(col_names, rep(paste0("RATE",1:(row_len-1))))
    regular_int_type <- c(regular_int_type, rep(paste0("RATE",1:(row_len-1))))
  }
  if(parameter_required("^(MIDPT)([0-9]*?|A|N)$", parameter_list) || parameter_required(dependent_parameters("^(MIDPT)([0-9]*?|A|N)$"), parameter_list)) {
    col_names <- c(col_names, rep(paste0("MIDPT",1:(row_len-1))))
    regular_int_type <- c(regular_int_type, rep(paste0("MIDPT",1:(row_len-1))))
  }
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
  if(disp_required[["DOSEi"]]) {
    col_names <- c(col_names, rep(paste0("DOSE",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("DOSE",1:di_col)))
  }
  if(disp_required[["DOSEC"]]) {
    col_names <- c(col_names, "DOSEC")
    regular_int_type <- c(regular_int_type, "DOSEC")
  }
  if(disp_required[["DOSECi"]]) {
    col_names <- c(col_names, rep(paste0("DOSEC",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("DOSEC",1:di_col)))
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
      warning("Flag 'FLGACCEPTPREDOSECRIT' cannot be computed if 'CMAX' is not part of the calculated parameters")
    }
    if(!(is.numeric(suppressWarnings(as.numeric(map_data$FLGACCEPTPREDOSECRIT)))) || (is.na(suppressWarnings(as.numeric(map_data$FLGACCEPTPREDOSECRIT))))){
      warning("Flag 'FLGACCEPTPREDOSECRIT' does not have valid form! Please try again with numeric value")
    }
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
      if(comp_required[["DOSECi"]] || comp_required[["DOSEC"]]){
        dose_c_i <- list()
      }
      if(comp_required[["DIi"]]) {
        di <- list()
      }
      if(comp_required[["TAUi"]]) {
        tau <- list()
      }
      if(comp_required[["TOLDi"]]) {
        told <- list()
      }
      if(comp_required[["DOSEi"]]) {
        dose <- list()
      }
      if(comp_required[["AEPCTi"]]) {
        ae_pct_i <- list()
      }
      if(comp_required[["AETAUi"]]) {
        aetau_i <- list()
      }
      if(comp_required[["AETAUPTi"]]) {
        aetau_pt_i <- list()
      }
      if(comp_required[["AURCT"]] && row_len > 2) {
        aurct <- list()
        aurc_int <- list()
      }
      if(comp_required[["MAXRATEi"]]) {
        max_rate_i <- list()
      }
      if(comp_required[["MIDPTLASTi"]]) {
        midpt_last_i <- list()
      }
      if(comp_required[["TMAXRATEi"]]) {
        tmax_rate_i <- list()
      }
      if(comp_required[["RATELASTi"]]) {
        rate_last_i <- list()
      }

      tmp_df <- data_data[data_data[,map_data$SDEID] == unique(data_data[,map_data$SDEID])[i],]
      tmp_df[,map_data$CONC] <- as.numeric(tmp_df[,map_data$CONC])
      tmp_df[,map_data$TIME] <- as.numeric(tmp_df[,map_data$TIME])
      tmp_df[,map_data$ENDTIME] <- as.numeric(tmp_df[,map_data$ENDTIME])
      tmp_df <- tmp_df[order(tmp_df[,map_data$ENDTIME]),]
      
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
      test_df <- tmp_df[,c(map_data$CONC, map_data$TIME, map_data$ENDTIME)]
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
      test_df_2 <- tmp_df[,c(map_data$TIME, map_data$ENDTIME)]
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
      
      mid_pt <- midpt(start_time = tmp_df[,map_data$TIME], end_time = tmp_df[,map_data$ENDTIME])
      amt <- at(conc = tmp_df[,map_data$CONC], amt = as.numeric(tmp_df[,map_data$AMOUNT]), time = tmp_df[,map_data$ENDTIME], amt_units = tmp_df[,map_data$AMOUNTU])

      if(casefold(map_data$ORGTIME) == "nominal"){
        if(length(sort(unique(data_data[,map_data$ENDTIME])[1:aet_len])) > length(amt)){
          tmp_amt <- data.frame(amt = amt, time = tmp_df[,map_data$ENDTIME])
          amt <- as.numeric(unlist(lapply(sort(unique(data_data[,map_data$ENDTIME])[1:aet_len]), function(x){ return(ifelse(!(x %in% tmp_df[,map_data$ENDTIME]), NA, tmp_amt[tmp_amt$time == x,"amt"])) })))
        }
      } else if(casefold(map_data$ORGTIME) == "actual"){
        if(length(sort(unique(data_data[,map_data$ENDTIME])[1:aet_len])) > length(amt)){
          tmp_amt <- data.frame(amt = amt, time = tmp_df[,map_data$ENDTIME])
          amt <- as.numeric(unlist(lapply(sort(unique(data_data[,map_data$ENDTIME])[1:aet_len]), function(x){ return(ifelse(!(x %in% tmp_df[,map_data$ENDTIME]), NA, tmp_amt[tmp_amt$time == x,"amt"])) })))
        }
      }
      type <- "INTERVAL"
      if("SAMPLETYPE" %in% names(map_data)){
        if(map_data$SAMPLETYPE %in% names(tmp_df)){
          type <- as.character(unique(tmp_df[,map_data$SAMPLETYPE])[1])
        }
      }
      rt <- rate(start_time = tmp_df[,map_data$TIME], end_time = tmp_df[,map_data$ENDTIME], conc = tmp_df[,map_data$CONC], vol = as.numeric(tmp_df[,map_data$AMOUNT]), volu = tmp_df[,map_data$AMOUNTU], type = type, map = map_data)
      if(is.na(rt) && length(rt) == 1){
        rt <- rep(NA, length(tmp_df[,map_data$CONC]))
      }
      
      if(isTRUE(nrow(tmp_df) > 0 & all(tmp_df[,map_data$TIME][!is.na(tmp_df[,map_data$TIME])] >= 0) & all(tmp_df[,map_data$ENDTIME][!is.na(tmp_df[,map_data$ENDTIME])] >= 0))){
        orig_time <- rt
        orig_conc <- mid_pt
        if(!0 %in% mid_pt){
          auc_rt <- c(0, rt)
          auc_mid_pt <- c(0, mid_pt)
          auc_flag <- c(0, auc_flag)
          orig_auc_conc <- auc_rt
          orig_auc_time <- auc_mid_pt
        } else {
          auc_rt <- rt
          auc_mid_pt <- mid_pt
          orig_auc_conc <- auc_rt
          orig_auc_time <- auc_mid_pt
        }
        
        obs_c_0 <- c0(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
        if(comp_required[["DOSEC"]]) {
          dose_c <- dosec(data = tmp_df, map = map_data)
        }
        if(comp_required[["TLAG"]]) {
          t_lag <- tlag(conc = rt, time = mid_pt)
        }
        if(comp_required[["TLAST"]]) {
          t_last <- tlast(conc = rt, time = mid_pt)
        } else {
          t_last <- NULL
        }
        if(comp_required[["KEL"]] || comp_required[["KELTMLO"]] || comp_required[["KELTMHI"]] || comp_required[["KELNOPT"]] || comp_required[["THALF"]] || comp_required[["THALFF"]]) {
          span_ratio <- ifelse("SPANRATIOCRIT" %in% names(map_data), suppressWarnings(as.numeric(map_data$SPANRATIOCRIT)), NA)
          kel_v <- kel(conc = rt, time = mid_pt, exflag = kel_flag, spanratio = span_ratio)
        } else {
          kel_v <- NULL
        }
        if(comp_required[["KELR"]] || comp_required[["KELRSQ"]] || comp_required[["KELRSQA"]]) {
          kelr_v <- kel_r(conc = rt, time = mid_pt, exflag = kel_flag)
        }
        if(comp_required[["CEST"]] || parameter_required("KEL", names(kel_v)) || parameter_required("KELC0", names(kel_v))) {
          span_ratio <- ifelse("SPANRATIOCRIT" %in% names(map_data), suppressWarnings(as.numeric(map_data$SPANRATIOCRIT)), NA)
          c_est <- cest(conc = rt, time = mid_pt, kelflag=kel_flag, t_last=t_last, spanratio=span_ratio, kel=kel_v[["KEL"]], kelc0=kel_v[["KELC0"]])
        }
        if(comp_required[["AE"]]) {
          a_e <- ae(amt = amt, time = tmp_df[,map_data$ENDTIME], orig_time = tmp_df[,map_data$ENDTIME])
        }
        if(comp_required[["AET"]] || comp_required[["AETPCTi"]]) {
          ae_t <- NULL
          aet_pct <- NULL
          for(t in 1:length(sort(unique(data_data[,map_data$ENDTIME]))[1:aet_len])){
            tmp_curr_data <- unique(tmp_df[,c(map_data$TIME, map_data$ENDTIME)]) 
            tmp_curr_time_t <- tmp_curr_data[order(tmp_curr_data[,map_data$TIME], tmp_curr_data[,map_data$ENDTIME]),]
            tmp_orig_time <- tmp_curr_time_t[tmp_curr_time_t[,map_data$ENDTIME] %in% sort(unique(data_data[,map_data$ENDTIME]))[t],]
            tmp_data <- unique(data_data[,c(map_data$TIME, map_data$ENDTIME)]) 
            tmp_time_t <- tmp_data[order(tmp_data[,map_data$TIME], tmp_data[,map_data$ENDTIME]),]
            
            if(sort(unique(data_data[,map_data$ENDTIME]))[t] %in% tmp_df[,map_data$ENDTIME]){
              tmp <- aet(amt = amt, time = na.omit(sort(tmp_df[,map_data$ENDTIME])), t = sort(unique(data_data[,map_data$ENDTIME]))[t], orig_time = tmp_orig_time, curr_time = tmp_curr_time_t, all_time = tmp_time_t, end_time = sort(unique(data_data[,map_data$ENDTIME])))
            } else {
              tmp <- NA
            }
            tmp_pct <-  aetpct(aet = tmp, dose = dose_c)
            
            if(is.null(ae_t)){
              ae_t <- tmp
              aet_pct <- tmp_pct
            } else {
              ae_t <- c(ae_t, tmp)
              aet_pct <- c(aet_pct, tmp_pct)
            }
          } 
          if(length(ae_t) < aet_len) {
            ae_t <- c(ae_t, rep(NA, (aet_len - length(ae_t))))
          }
          if(length(aet_pct) < aet_len) {
            aet_pct <- c(aet_pct, rep(NA, (aet_len - length(aet_pct))))
          }
        }
        if(comp_required[["MAXRATE"]]) {
          max_rate <- maxrate(rate = rt)
        }
        if(comp_required[["TMAXRATE"]]) {
          tmax_rate <- tmaxrate(midpt = mid_pt, rate = rt)
        }
        if(comp_required[["RATELAST"]]) {
          rate_last <- ratelast(rate = rt)
        }
        if(comp_required[["MIDPTLAST"]]) {
          midpt_last <- midptlast(midpt = mid_pt, rate = rt)
        }
        if(comp_required[["AURCT1_T2"]] && auc_pair_check) {
          aurct1_t2 <- list()
          aurct1_t2_check <- as.list(rep(NA, aurc_par_len))
          aurct1_t2_names <- c(rep(paste0("AUC.", 1:aurc_par_len, ".T1")), rep(paste0("AUC.", 1:aurc_par_len, ".T2")))
          if(!all(aurct1_t2_names %in% names(map_data))){
            par_col <- rep(paste0("'", aurct1_t2_names[!aurct1_t2_names %in% names(map_data)], "'"))
            stop(paste0("Dataset provided via 'map' does not contain the required columns for partial areas ", par_col))
          }
        }

        for(d in 1:di_col){
          tmp_di_df <- tmp_df[tmp_df[c(paste0("DI", d, "F"))] == 1,]
          tmp_di_df <- tmp_di_df[order(tmp_di_df[,map_data$TIME]),]
          tmp_dose <- tmp_di_df[, as.character(map_data[c(paste0("DOSE",d))])][1]
          tmp_mid_pt <- midpt(start_time = tmp_di_df[,map_data$TIME], end_time = tmp_di_df[,map_data$ENDTIME])
          tmp_rt <- rate(start_time = tmp_di_df[,map_data$TIME], end_time = tmp_di_df[,map_data$ENDTIME], conc = tmp_di_df[,map_data$CONC], vol = as.numeric(tmp_di_df[,map_data$AMOUNT]), volu = tmp_di_df[,map_data$AMOUNTU], map=map_data)
          
          if(comp_required[["DOSECi"]] || comp_required[["DOSEC"]]) {
            if(!is.na(tmp_dose)) { 
              dose_c_i[[d]] <- dosec(data = tmp_di_df, map = map_data, idose=d)
            } else {
              dose_c_i[[d]] <- dose_c
            }
          }
          if(comp_required[["MAXRATEi"]]) {
            max_rate_i[[d]] <- maxrate(rate = tmp_rt)
          }
          if(comp_required[["TMAXRATEi"]]) {
            tmax_rate_i[[d]] <- tmaxrate(midpt = tmp_mid_pt, rate = tmp_rt)
          }
          if(comp_required[["RATELASTi"]]) {
            rate_last_i[[d]] <- ratelast(rate = tmp_rt)
          }
          if(comp_required[["MIDPTLASTi"]]) {
            midpt_last_i[[d]] <- midptlast(midpt = tmp_mid_pt, rate = tmp_rt)
          }
          if(comp_required[["TAUi"]]) {
            tau[[d]] <- tmp_di_df[, as.character(map_data[c(paste0("TAU",d))])][1]
            tau[[d]] <- as.numeric(tau[[d]])
          }
          if(comp_required[["TOLDi"]]) {
            told[[d]] <- tmp_di_df[, as.character(map_data[c(paste0("TOLD",d))])][1]
            told[[d]] <- as.numeric(told[[d]])
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
          if(comp_required[["AEPCTi"]]) {
            ae_pct_i[[d]] <- aepct(ae = a_e, dose = dose_c_i[[d]])
          }
          if(comp_required[["AETAUi"]]){
            tmp_amt <- amt[sort(unique(data_data[,map_data$ENDTIME]))[1:aet_len] %in% na.omit(tmp_df[,map_data$ENDTIME])]
            aetau_i[[d]] <- aetau(aet = tmp_amt, time = na.omit(tmp_df[,map_data$ENDTIME]), t = told[[d]]+tau[[d]], returnNA = FALSE)
          }
          if(comp_required[["AETAUPTi"]]) {
            aetau_pt_i[[d]] <- aepct(ae = aetau_i[[d]], dose = dose_c_i[[d]])
          }
          if(comp_required[["AURCT"]] && row_len > 2) {
            time <- sort(auc_mid_pt)
            time_di <- sort(tmp_mid_pt)
            if(d == 1 && !any(time_di == 0)){
              time_di <- c(0, time_di)
            }
            if(d == di_col){
              if(sum(time > time_di) > 0){
                time_di <- c(time_di, time[time > time_di[length(time_di)]])
              }
            }
            
            prev_na <- FALSE
            prev_aurc <- NA
            
            if(length(time) > 1){
              for(t in 2:(length(time))){
                if(time[t] %in% time_di[-1]) {
                  tmp <- auc_t1_t2(conc = auc_rt, time = auc_mid_pt, t1 = time_di[1], t2 = time[t], method = method, exflag = auc_flag, t_max = tmax_rate_i[[d]])
                  tmp_int <- paste0(time[1], "_", time[t])
                } else {
                  tmp <- NA
                  tmp_int <- paste0(time[1], "_", time[t])
                }
                
                if(d == 1){
                  aurct[[t-1]] <- tmp
                  aurc_int[[t-1]] <- tmp_int
                } else {
                  prev_aurc <- ifelse((t-2) >= 0, unlist(aurct[[t-2]]), NA)
                  if(prev_na){
                    prev_na <- FALSE
                    if(is.numeric(tmp)){
                      aurct[[t-1]] <- sum(c(prev_aurc, tmp), na.rm = TRUE)
                    }
                  } else {
                    if(!is.na(prev_aurc)){
                      aurct[[t-1]] <- sum(c(prev_aurc, tmp), na.rm = TRUE)
                    } else {
                      aurct[[t-1]] <- sum(c(aurct[[t-1]], tmp), na.rm = TRUE)
                    }
                  }
                  aurc_int[[t-1]] <- ifelse(aurc_int[[t-1]] != tmp_int, tmp_int, aurc_int[[t-1]])
                  
                  if(is.na(tmp)){
                    prev_na <- TRUE
                  } else {
                    prev_na <- FALSE
                  }
                }
              }
            } else {
              aurct <- rep(NA, row_len-1)
              aurc_int <- rep(NA, row_len-1)
            }
            
            if(d == di_col){
              if(length(aurct) < (row_len-1)) {
                aurct <- c(aurct, rep(NA, ((row_len-1) - length(aurct))))
              }
              if(length(aurc_int) < (row_len-1)) {
                aurc_int <- c(aurc_int, rep(NA, ((row_len-1) - length(aurc_int))))
              }
            }
          }
          if(comp_required[["AURCT1_T2"]] && auc_pair_check) {
            time <- sort(auc_mid_pt)
            time_di <- sort(tmp_mid_pt)
            
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
            for(t in 1:(aurc_par_len)){
              if(!(is.numeric(as.numeric(map_data[, paste0("AUC.", t, ".T1")])) && is.numeric(as.numeric(map_data[, paste0("AUC.", t, ".T2")])))){
                stop(paste0("'AUC.", t, ".T1' and/or 'AUC.", t, ".T2' value provided via 'map' is not a numeric value"))
              }
              orig_aurc_t1 <- as.numeric(map_data[, paste0("AUC.", t, ".T1")])
              orig_aurc_t2 <- as.numeric(map_data[, paste0("AUC.", t, ".T2")])
              aurc_t1 <- orig_aurc_t1
              aurc_t2 <- orig_aurc_t2
              tmp_start_time <- time_di[1]
              tmp_end_time <- time_di[length(time_di)]
              
              if(isTRUE(tmp_start_time <= orig_aurc_t1 && orig_aurc_t1 < tmp_end_time) || isTRUE(tmp_start_time < orig_aurc_t2 && orig_aurc_t2 <= tmp_end_time) || isTRUE(aurct1_t2_check[[t]])){
                if(!isTRUE(aurct1_t2_check[[t]]) && isTRUE(tmp_start_time <= orig_aurc_t1) && isTRUE(orig_aurc_t1 < tmp_end_time)){
                  aurct1_t2_check[[t]] <- TRUE
                }
                if(isTRUE(aurct1_t2_check[[t]])){
                  if(isTRUE(d > 1 && orig_aurc_t1 < tmp_start_time)){
                    aurc_t1 <- tmp_start_time
                  }
                  if(isTRUE(d < di_col && orig_aurc_t2 > tmp_end_time)){
                    aurc_t2 <- tmp_end_time
                  }
                  
                  if((isTRUE(interpolation) || isTRUE(extrapolation))){
                    tmp <- auc_t1_t2(conc = auc_rt, time = auc_mid_pt, t1 = aurc_t1, t2 = aurc_t2, method = method, exflag = auc_flag, t_max = tmax_rate_i[[d]], interpolate = interpolation, extrapolate = extrapolation, model = "M4", dosing_type = "SS", told = tmp_told, kel = kel_v, orig_conc = orig_conc, orig_time = orig_time, includeNA = TRUE)
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
                    tmp_auc <- auc_t1_t2(conc = auc_rt, time = auc_mid_pt, t1 = aurc_t1, t2 = aurc_t2, method = method, exflag = auc_flag, t_max = tmax_rate_i[[d]], model = "M4", dosing_type = "SS", includeNA = TRUE)
                  }
                } else {
                  tmp_auc <- 0
                }
                if(isTRUE(tmp_start_time < orig_aurc_t2) && isTRUE(orig_aurc_t2 <= tmp_end_time)){
                  aurct1_t2_check[[t]] <- FALSE
                }
              } else {
                tmp_auc <- 0
              }
              
              if(d == 1){
                aurct1_t2[[t]] <- tmp_auc
              } else {
                aurct1_t2[[t]] <- sum(c(unlist(aurct1_t2[[t]]), tmp_auc), na.rm = TRUE)
              }
            }
          }
        }
        if(comp_required[["AURCALL"]]) {
          aurcall <- auc_all(conc = auc_rt, time = auc_mid_pt, method = method, exflag = auc_flag, t_max = tmax_rate_i[[d]])
        }
        if(comp_required[["AURCLAST"]]) {
          aurclast <- auc_last(conc = auc_rt, time = auc_mid_pt, method = method, exflag = auc_flag, t_last = midpt_last_i[[d]], t_max = tmax_rate_i[[d]])
        }
        if(isTRUE(optimize_kel) && comp_required[["TMAXi"]] && comp_required[["TLASTi"]] && comp_required[["CMAXi"]] && comp_required[["CLASTi"]] && comp_required[["AUCLASTi"]] &&
           "FLGACCEPTKELCRIT" %in% names(map_data) && "FLGEXKEL" %in% names(map_data) && map_data$FLGEXKEL %in% names(data_data)){
          orig_time <- mid_pt
          orig_conc <- rt
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
          
          if(all(c("KELNOPT", "KELRSQ") %in% flag_df$VAR) && ("AURCXPCTO" %in% flag_df$VAR || "AURCXPCTP" %in% flag_df$VAR)){
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
                if("AURCXPCTO" %in% flag_df$VAR){
                  span_ratio <- ifelse("SPANRATIOCRIT" %in% names(map_data), suppressWarnings(as.numeric(map_data$SPANRATIOCRIT)), NA)
                  aucinfo_opt <- auc_inf_o(conc = rt, time = mid_pt, method = method, auclast = auclast, c_last = c_last, spanratio = span_ratio)
                  aucxpct_opt <- auc_XpctO(conc = sel_conc, time = sel_time, method = method, aucflag = auc_flag, auc_info = aucinfo_opt, auclast = auclast, kel = all_kel)
                } else if("AURCXPCTP" %in% flag_df$VAR){
                  span_ratio <- ifelse("SPANRATIOCRIT" %in% names(map_data), suppressWarnings(as.numeric(map_data$SPANRATIOCRIT)), NA)
                  aucinfp_opt <- auc_inf_p(conc = rt, time = mid_pt, method = method, auclast = auclast, t_last = t_last, spanratio = span_ratio)
                  aucxpct_opt <- auc_XpctP(conc = sel_conc, time = sel_time, method = method, aucflag = auc_flag, auc_infp = aucinfp_opt, auclast = auclast, kel = all_kel)
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
              warning("Kel optimization cannot be performed because 'KELNOPT', 'KELRSQ' and 'AURCXPCTO' or 'AURCXPCTP' are not part of the Flag 'FLGACCEPTKELCRIT'")
              kel_opt_warning <- TRUE
            }
          }
        }
        if(comp_required[["AURCINFO"]]) {
          aurcinf_o <- auc_inf_o(conc = auc_rt, time = auc_mid_pt, method = method, kelflag = kel_flag, aucflag = auc_flag, auclast = aurclast, c_last = rate_last, kel = kel_v)
        }
        if(comp_required[["AURCINFP"]]) {
          aurcinf_p <- auc_inf_p(conc = auc_rt, time = auc_mid_pt, method = method, kelflag = kel_flag, aucflag = auc_flag, auclast = aurclast, t_last = midpt_last, kel = kel_v)
        }
        if(comp_required[["AURCXPCTO"]]){
          aurcxpcto <- auc_XpctO(conc = auc_rt, time = auc_mid_pt, method = method, kelflag = kel_flag, aucflag = auc_flag, auc_info = aurcinf_o, auclast = aurclast)
        }
        if(comp_required[["AURCXPCTP"]]){
          aurcxpctp <- auc_XpctP(conc = auc_rt, time = auc_mid_pt, method = method, kelflag = kel_flag, aucflag = auc_flag, auc_infp = aurcinf_p, auclast = aurclast)
        }
        if(comp_required[["VOLSUM"]]) {
          volsum <- vol_sum(vol = tmp_df[,map_data$AMOUNT], volu = tmp_df[,map_data$AMOUNTU])
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
        if(disp_required[["AT"]]) {
          computation_df[i, paste0("AMT.", sprintf("%.2f", sort(unique(data_data[,map_data$ENDTIME]))[1:aet_len]))] <- amt
        }
        if(disp_required[["AET"]]) {
          computation_df[i, paste0("AE.", sprintf("%.2f", sort(unique(data_data[,map_data$ENDTIME]))[1:aet_len]))] <- ae_t
        }
        if(disp_required[["AETPCTi"]]) {
          computation_df[i, paste0("AEPCT.", sprintf("%.2f", sort(unique(data_data[,map_data$ENDTIME]))[1:aet_len]))] <- aet_pct
        }
        if(disp_required[["AE"]]) {
          computation_df[i, "AE"] <- a_e
        }
        if(disp_required[["AEPCTi"]]) {
          computation_df[i, paste0("AEPCT",1:di_col)] <- unlist(ae_pct_i)
        }
        if(disp_required[["AETAUi"]]) {
          computation_df[i, paste0("AETAU",1:di_col)] <- unlist(aetau_i)
        }
        if(disp_required[["AETAUPTi"]]) {
          computation_df[i, paste0("AETAUPT",1:di_col)] <- unlist(aetau_pt_i)
        }
        if(disp_required[["MAXRATE"]]) {
          computation_df[i, "MAXRATE"] <- max_rate
        }
        if(disp_required[["TMAXRATE"]]) {
          computation_df[i, "TMAXRATE"] <- tmax_rate
        }
        if(disp_required[["RATELAST"]]) {
          computation_df[i, "RATELAST"] <- rate_last
        }
        if(disp_required[["MIDPTLAST"]]) {
          computation_df[i, "MIDPTLAST"] <- midpt_last
        }
        if(disp_required[["MAXRATEi"]]) {
          computation_df[i, paste0("MAXRATE",1:di_col)] <- unlist(max_rate_i)
        }
        if(disp_required[["TMAXRATEi"]]) {
          computation_df[i, paste0("TMAXRATE",1:di_col)] <- unlist(tmax_rate_i)
        }
        if(disp_required[["RATELASTi"]]) {
          computation_df[i, paste0("RATELAST",1:di_col)] <- unlist(rate_last_i)
        }
        if(disp_required[["MIDPTLASTi"]]) {
          computation_df[i, paste0("MIDPTLAST",1:di_col)] <- unlist(midpt_last_i)
        }
        if(disp_required[["TLAG"]]) {
          computation_df[i, "TLAG"] <- t_lag
        }
        if(disp_required[["KEL"]]) {
          computation_df[i, "KEL"] <- ifelse("KEL" %in% names(kel_v), kel_v[["KEL"]], NA)
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
        if(disp_required[["AURCALL"]]) {
          computation_df[i, "AURCALL"] <- aurcall
        }
        if(disp_required[["AURCLAST"]]) {
          computation_df[i, "AURCLAST"] <- aurclast
        }
        if(disp_required[["AURCT"]] && row_len > 2) {
          computation_df[i, paste0("AURC",1:(row_len-1))] <- unlist(aurct)
          computation_df[i, paste0("AURCINT",1:(row_len-1))] <- unlist(aurc_int)
        }
        if(disp_required[["AURCT1_T2"]] && auc_pair_check) {
          computation_df[i, paste0("AURC", map_data[,rep(paste0("AUC.", 1:aurc_par_len, ".T1"))], "_", map_data[,rep(paste0("AUC.", 1:aurc_par_len, ".T2"))])] <- unlist(aurct1_t2)
        }
        if(disp_required[["AURCINFO"]]) {
          computation_df[i, "AURCINFO"] <- aurcinf_o
        }
        if(disp_required[["AURCINFP"]]) {
          computation_df[i, "AURCINFP"] <- aurcinf_p
        }
        if(disp_required[["AURCXPCTO"]]){
          computation_df[i, "AURCXPCTO"] <- aurcxpcto
        }
        if(disp_required[["AURCXPCTP"]]){
          computation_df[i, "AURCXPCTP"] <- aurcxpctp
        }
        if(disp_required[["VOLSUM"]]) {
          computation_df[i, "VOLSUM"] <- volsum
        }
        if(parameter_required("^(RATE)([0-9]*?|A|N)$", parameter_list) || parameter_required(dependent_parameters("^(RATE)([0-9]*?|A|N)$"), parameter_list)) {
          computation_df[i, paste0("RATE",1:(row_len-1))] <- c(rt, rep(NA, ((row_len-1) - length(rt))))
        }
        if(parameter_required("^(MIDPT)([0-9]*?|A|N)$", parameter_list) || parameter_required(dependent_parameters("^(MIDPT)([0-9]*?|A|N)$"), parameter_list)) {
          computation_df[i, paste0("MIDPT",1:(row_len-1))] <- c(mid_pt, rep(NA, ((row_len-1) - length(mid_pt))))
        }
        if(disp_required[["DIi"]]) {
          computation_df[i, paste0("DI",1:di_col)] <- unlist(di)
        }
        if(disp_required[["TAUi"]]) {
          computation_df[i, paste0("TAU",1:di_col)] <- unlist(tau)
        }
        if(disp_required[["TOLDi"]]) {
          computation_df[i, paste0("TOLD",1:di_col)] <- unlist(told)
        }
        if(disp_required[["DOSEi"]]) {
          computation_df[i, unlist(dosenames)] <- unlist(dose)
        }
        if(disp_required[["DOSEC"]]) {
          computation_df[i, "DOSEC"] <- dose_c
        }
        if(disp_required[["DOSECi"]]) {
          computation_df[i, paste0("DOSEC",1:di_col)] <- unlist(dose_c_i)
        }
      } else {
        if(isTRUE(optimize_kel)){
          kel_flag_optimized <- c(kel_flag_optimized, kel_flag)
        }
        computation_df[i, "SDEID"] <- unique(data_data[,map_data$SDEID])[i]
        if(isTRUE(any(tmp_df[,map_data$TIME] < 0) || any(tmp_df[,map_data$ENDTIME] < 0))){
          warning(paste0("No parameters generated due to negative TIME and/or ENDTIME values for SDEID: '", unique(data_data[,map_data$SDEID])[i], "'"))
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
    
    return_df <- unique(data_data[names(data_data) %in% return_list])
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
