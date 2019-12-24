#' Run M4 SS Computation
#'
#' This function will compute all the relevant parameters for a M4 model Stedy State (SS).\cr
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
### 2019-10-10/TGT/ Remove display_list argument and incorporate model_regex argument
###run_M4_SS_computation <- function(data = NULL, map = NULL, method = 1, parameter_list = list(), display_list = list(), return_list = list()){
run_M4_SS_computation <- function(data = NULL, map = NULL, method = 1, model_regex = "^M4(SS)*?$", parameter_list = list(), return_list = list()){
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

### 2019-09-03/TGT/ added standardized "validate_timeconc_data" routine to resolve issue
###                 that was not handled if TIME is set to a value in the input dataset directly
### 2019-09-11/TGT/ all map updates complete in run_computation now
###  timeconcvalues <- validate_timeconc_data(map_data, data_data)
###  map_data$TIME     <- timeconcvalues$time
###  map_data$TIMEU    <- timeconcvalues$timeu
###  map_data$ENDTIME  <- timeconcvalues$endtime
###  map_data$ENDTIMEU <- timeconcvalues$endtimeu
###  map_data$CONC     <- timeconcvalues$conc
###  map_data$CONCU    <- timeconcvalues$concu

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
###        map_data$ENDTIME <- "NOMENDTIME"
###        map_data$ENDTIMEU <- "NOMENDTIMEU"
###      } else if(casefold(map_data$TIME) == 'actual'){
###        map_data$TIME <- "ACTTIME"
###        map_data$TIMEU <- "ACTTIMEU"
###        map_data$ENDTIME <- "ACTENDTIME"
###        map_data$ENDTIMEU <- "ACTENDTIMEU"
###      }
###      if(!(map_data$TIME %in% names(map_data))){
###        stop("'TIME' value provided via 'map' is not present in 'map' dataset")
###      }
###    }
###  }
###  if(!(map_data$SDEID %in% names(data_data) && map_data[[map_data$TIME]] %in% names(data_data) && map_data$CONC %in% names(data_data))){
###    stop("Values for 'SDEID', 'TIME' and 'CONC' provided via 'map' are not present in the dataset provided via 'data'")
###  }

###  model_regex <- "^M4(SS)*?$"

  ss_dose <- c("DI1F", "DI2F", "DI3F", "DI4F", "DI5F")

  if(any(ss_dose %in% names(data_data))){
    di_col <- sum(ss_dose %in% names(data_data))
  } else {
    stop("Unable to find dosing interval for Stedy State data! Please provide a valid 'data' parameter")
  }

  auc_list <- c("AURCT1_T2")
  aet_list <- c("AET", "AETPCT")
  interval_list <- c("MAXRATEi", "TMAXRATEi", "RATELASTi", "MIDPTLASTi", "DIi", "TAUi", "TOLDi", "DOSEi")
### 2019-09-23/TGT/ VOLSUM parameter missing
###  regular_list <- c("AE", "AEPCT", "TLAG", "KEL", "KELTMLO", "KELTMHI", "KELNOPT", "KELR", "KELRSQ", "KELRSQA", "THALF", "THALFF",
###                    "AUCDN", "AURCALL", "AURCLAST", "AURCINFO", "AURCINFP", "AURCXPCTO", "AURCXPCTP")
  regular_list <- c("AE", "AEPCT", "TLAG", "KEL", "KELTMLO", "KELTMHI", "KELNOPT", "KELR", "KELRSQ", "KELRSQA", "THALF", "THALFF",
                    "AUCDN", "AURCALL", "AURCLAST", "AURCINFO", "AURCINFP", "AURCXPCTO", "AURCXPCTP", "VOLSUM")
  optional_list <- c("DOSEi", "TAUi", "TOLDi")
  regular_int_type <- NULL
  auc_pair_check <- FALSE

  #aet_col <- length(unique(data_data[,map_data$NOMENDTIME]))
  #col <- 10 + 2*aet_col
### 2019-09-03/TGT/ remap map_data[[map_data$TIME]] to map_data$TIME, map_data[map_data$ENDTIME]] to map_data$E
###  tmp_mid_pt <- midpt(start_time = data_data[,map_data[[map_data$TIME]]], end_time = data_data[,map_data[[map_data$ENDTIME]]])
### 2019-09-19/TGT/ following should be done on a NOMENDTIME and NOMTIME basis to compute # of nominal midpts
### even if midpt calculation is done on an ACTual time basis
  tmp_mid_pt <- midpt(start_time = data_data[,map_data$TIME], end_time = data_data[,map_data$ENDTIME])
  mid_len <- length(unique(tmp_mid_pt))
  mid_col <- ifelse(sum(auc_list %in% parameter_list) == 1, 2, 0)
### 2019-09-03/TGT/ remap map_data[[map_data$TIME]] to map_data$TIME
###  aet_len <- length(unique(data_data[,map_data[[map_data$TIME]]]))
### 2019-09-19/TGT/ aet_len must be done on NOMTIME basis
  aet_len <- length(unique(data_data[,map_data$ENDTIME]))

###
###  cat('map_data$TIME: ', map_data$TIME, ' ', 'map_data$NOMTIME: ', map_data$NOMTIME, ' ', length(unique(data_data[,map_data$TIME])), '\n')
###  print(unique(data_data[,map_data$TIME]))
  
###  cat('interval_list:\n');print(interval_list[interval_list %in% parameter_list])
  aucpari <- grep('^AUC.([0-9]+?).T[1-2]$', names(map_data), ignore.case=TRUE, perl=TRUE)
  if(length(aucpari)>0) {
    aurc_par_len <- floor(length(aucpari)/2)
    g <- names(map_data)[aucpari]
    ### Ensure pairs are coherent
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
##  aurc_par_len <- ifelse(auc_list %in% parameter_list && 'AUCNPAIR' %in% names(map_data), ifelse(!(is.null(map_data$AUCNPAIR) || is.na(suppressWarnings(as.numeric(map_data$AUCNPAIR)))), suppressWarnings(as.numeric(map_data$AUCNPAIR)), 0), 0)
  col <- reg_col + (tmp_aet_col * aet_len-1) + (mid_col * mid_len) + (interval_col * di_col) + 1 + (2 * (aet_len))
###
###  cat('tmp_mid_pt:', tmp_mid_pt, '\n')
###  cat(' mid_len: ', mid_len, ' mid_col: ', mid_col, ' interval_col: ', interval_col, ' reg_col: ', reg_col, ' aet_col: ', aet_col, ' tmp_aet_col: ', tmp_aet_col, ' aurc_par_len: ', aurc_par_len, ' col: ', col, '\n')
  
  ### Determine DOSEs in dosevar, a vector of dose names pointing into map_data
  doselist <- names(parameter_indices("^DOSELIST$", names(map_data), simplify=FALSE))
  dosevar <- unlist(strsplit(map_data[,doselist], ";"))
  ### assuming here there is a single dose
  if(!any(duplicated(as.character(unlist(map[,dosevar]))))){
    dosevar <- map[,dosevar] 
  }
   
  ### 2019-09-19/TGT/ Precompute list of required parameters for col_names, parameter function evaluation and row_data generation  
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
###  print(comp_required)
###  x <- unlist(comp_required)
###  x <- x[x]
###  print(names(x))
###cat('disp_required: \n')
###print(names(disp_required))
###  y <- unlist(disp_required)
###  y <- y[y]
###  print(names(y))
###  cat('length(disp_required)): ', length(names(y)), '\n')
  
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
###  if("KEL" %in% parameter_list){
    elist <- c("PKDATAROWID", "SDEID","TIME","CEST_KEL","CEST_INT","CEST_EXT","CEST_C0","CEST_TLAST")
    est_data <- data.frame(matrix(ncol = length(elist), nrow = 0))
    names(est_data) <- elist
###  }

### 2019-09-24/TGT/ Move creation of initial computation_df until after determine all of the disp_required col_names
### at this point "col" is no longer needed for anything and cane be removed in a code update
###  computation_df <- data.frame(matrix(ncol = col, nrow = 0))

  col_names <- c("SDEID")
###  if("AET" %in% parameter_list) {
  if(disp_required[["AET"]]) {
    col_names <- c(col_names, rep(paste0("AMT.", sprintf("%.2f", sort(unique(data_data[,map_data$ENDTIME])[1:aet_len])))), rep(paste0("AE.", sprintf("%.2f", sort(unique(data_data[,map_data$ENDTIME])[1:aet_len])))))
###    regular_int_type <- c(regular_int_type, rep(paste0("AMT.", sprintf("%.2f", unique(data_data[,map_data[[map_data$ENDTIME]]])[1:aet_len]))), rep(paste0("AE.", sprintf("%.2f", unique(data_data[,map_data[[map_data$ENDTIME]]])[1:aet_len]))))
    regular_int_type <- c(regular_int_type, rep(paste0("AMT.", sprintf("%.2f", sort(unique(data_data[,map_data$ENDTIME])[1:aet_len])))), rep(paste0("AE.", sprintf("%.2f", sort(unique(data_data[,map_data$ENDTIME])[1:aet_len])))))
  }
###  if("AETPCT" %in% parameter_list && "AET" %in% parameter_list) {
  if(disp_required[["AETPCT"]]) {
###    col_names <- c(col_names, rep(paste0("AEPCT.", sprintf("%.2f", unique(data_data[,map_data[[map_data$ENDTIME]]])[1:aet_len]))))
    col_names <- c(col_names, rep(paste0("AEPCT.", sprintf("%.2f", unique(data_data[,map_data$ENDTIME])[1:aet_len]))))
###    regular_int_type <- c(regular_int_type, rep(paste0("AEPCT.", sprintf("%.2f", unique(data_data[,map_data[[map_data$ENDTIME]]])[1:aet_len]))))
    regular_int_type <- c(regular_int_type, rep(paste0("AEPCT.", sprintf("%.2f", unique(data_data[,map_data$ENDTIME])[1:aet_len]))))
  }
###  if("AE" %in% parameter_list) {
  if(disp_required[["AE"]]) {
    col_names <- c(col_names, "AE")
    regular_int_type <- c(regular_int_type, "AE")
  }
###  if("AEPCT" %in% parameter_list && "AE" %in% parameter_list) {
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
###  if("MAXRATEi" %in% parameter_list) {
  if(disp_required[["MAXRATEi"]]) {
    col_names <- c(col_names, rep(paste0("MAXRATE",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("MAXRATE",1:di_col)))
  }
###  if("TMAXRATEi" %in% parameter_list) {
  if(disp_required[["TMAXRATEi"]]) {
    col_names <- c(col_names, rep(paste0("TMAXRATE",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("TMAXRATE",1:di_col)))
  }
###  if("RATELASTi" %in% parameter_list) {
  if(disp_required[["RATELASTi"]]) {
    col_names <- c(col_names, rep(paste0("RATELAST",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("RATELAST",1:di_col)))
  }
###  if("MIDPTLASTi" %in% parameter_list) {
  if(disp_required[["MIDPTLASTi"]]) {
    col_names <- c(col_names, rep(paste0("MIDPTLAST",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("MIDPTLAST",1:di_col)))
  }
###  if("TLAG" %in% parameter_list) {
  if(disp_required[["TLAG"]]) {
    col_names <- c(col_names, "TLAG")
    regular_int_type <- c(regular_int_type, "TLAG")
  }
###  if("KEL" %in% parameter_list) {
  if(disp_required[["KEL"]]) {
    col_names <- c(col_names, "KEL")
    regular_int_type <- c(regular_int_type, "KEL")
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
###  if("KELRSQ" %in% parameter_list || "KELRSQA" %in% parameter_list){
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
###  if("AUCDN" %in% parameter_list) {
### 2019-10-10/TGT/ Removed AUCDN
###  if(disp_required[["AUCDN"]]) {
###    col_names <- c(col_names, "AUCDN")
###    regular_int_type <- c(regular_int_type, "AUCDN")
###  }
###  if("AURCALL" %in% parameter_list) {
  if(disp_required[["AURCALL"]]) {
    col_names <- c(col_names, "AURCALL")
    regular_int_type <- c(regular_int_type, "AURCALL")
  }
###  if("AURCLAST" %in% parameter_list) {
  if(disp_required[["AURCLAST"]]) {
    col_names <- c(col_names, "AURCLAST")
    regular_int_type <- c(regular_int_type, "AURCLAST")
  }
  if(disp_required[["AURCT"]] && aet_len >= 2) {
    col_names <- c(col_names, rep(paste0("AURC",1:aet_len)), rep(paste0("AURCINT",1:aet_len)))
    regular_int_type <- c(regular_int_type, paste0("AURC",1:aet_len))
  }
###  if("AURCT1_T2" %in% parameter_list) {
  if(disp_required[["AURCT1_T2"]] && auc_pair_check) {
### mid_len is the length of unique mid points NOT the # of partial AUCs requested so the following is incorrect and needs to reflect the #
###      of names specific to the partial AUCs, i.e. aurc_par_len
      ### why does this currently increment col_names to include mid_len???
## 2019-11-21/RD Not needed as of now
##    col_names <- c(col_names, rep(paste0("AURC",1:mid_len)), rep(paste0("AURCINT",1:mid_len)))
##    regular_int_type <- c(regular_int_type, rep(paste0("AURC",1:mid_len)))
##    if(auc_pair_check){
      col_names <- c(col_names, rep(paste0("AURC", map_data[,rep(paste0("AUC.", 1:aurc_par_len, ".T1"))], "_", map_data[,rep(paste0("AUC.", 1:aurc_par_len, ".T2"))])))
      regular_int_type <- c(regular_int_type, rep(paste0("AURC", map_data[,rep(paste0("AUC.", 1:aurc_par_len, ".T1"))], "_", map_data[,rep(paste0("AUC.", 1:aurc_par_len, ".T2"))])))
##    }
  }
###  if("AURCINFO" %in% parameter_list) {
  if(disp_required[["AURCINFO"]]) {
    col_names <- c(col_names, "AURCINFO")
    regular_int_type <- c(regular_int_type, "AURCINFO")
  }
###  if("AURCINFP" %in% parameter_list) {
  if(disp_required[["AURCINFP"]]) {
    col_names <- c(col_names, "AURCINFP")
    regular_int_type <- c(regular_int_type, "AURCINFP")
  }
###  if("AURCXPCTO" %in% parameter_list){
  if(disp_required[["AURCXPCTO"]]){
    col_names <- c(col_names, "AURCXPCTO")
    regular_int_type <- c(regular_int_type, "AURCXPCTO")
  }
###  if("AURCXPCTP" %in% parameter_list){
  if(disp_required[["AURCXPCTP"]]){
    col_names <- c(col_names, "AURCXPCTP")
    regular_int_type <- c(regular_int_type, "AURCXPCTP")
  }
### 2019-09-23/TGT/ add "VOLSUM" to results
  if(disp_required[["VOLSUM"]]) {
    col_names <- c(col_names, "VOLSUM")
    regular_int_type <- c(regular_int_type, "VOLSUM")
  }
  col_names <- c(col_names, rep(paste0("RATE",1:(aet_len))), rep(paste0("MIDPT",1:(aet_len))))
  regular_int_type <- c(regular_int_type, rep(paste0("RATE",1:(aet_len))), rep(paste0("MIDPT",1:(aet_len))))
###  if("DIi" %in% parameter_list) {
  if(disp_required[["DIi"]]) {
    col_names <- c(col_names, rep(paste0("DI",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("DI",1:di_col)))
  }
###  if("TAUi" %in% parameter_list) {
  if(disp_required[["TAUi"]]) {
    col_names <- c(col_names, rep(paste0("TAU",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("TAU",1:di_col)))
  }
###  if("TOLD" %in% parameter_list) {
  if(disp_required[["TOLD"]]) {
    col_names <- c(col_names, rep(paste0("TOLD",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("TOLD",1:di_col)))
  }
###  if("DOSEi" %in% parameter_list) {
  if(disp_required[["DOSEi"]]) {
    col_names <- c(col_names, rep(paste0("DOSE",1:di_col)))
    regular_int_type <- c(regular_int_type, rep(paste0("DOSE",1:di_col)))
  }

### 2019-09-24/TGT/ Move creation of initial computation_df until after determine all of the disp_required col_names
  computation_df <- data.frame(matrix(ncol = length(col_names), nrow = 0))
  names(computation_df) <- col_names

  #names(computation_df) <- c("SDEID", rep(paste0("AMT", unique(data_data[,map_data$NOMENDTIME])[1:aet_col])),
  #                           rep(paste0("AE", unique(data_data[,map_data$NOMENDTIME])[1:aet_col])), "AE",
  #                           "KEL", "KELTMLO", "KELTHMI", "KELNOPT", "KELR", "KELRSQ", "KELRSQA", "THALF")

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
    if(!("CMAX" %in% parameter_list)){
      warning("Flag 'FLGACCEPTPREDOSECRIT' cannot be computed if 'CMAX' is not part of the calculated parameters")
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
    disp_required[["KEL"]] <- TRUE
  }
  
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
  if(isTRUE(optimize_kel) && (!comp_required[["TMAXi"]] || !comp_required[["TLASTi"]] || !comp_required[["CMAXi"]] || !comp_required[["CLASTi"]] || !comp_required[["AUCLASTi"]] ||
     !"FLGACCEPTKELCRIT" %in% names(map_data) || !"FLGEXKEL" %in% names(map_data) || !map_data$FLGEXKEL %in% names(data_data))){
    warning("Kel optimization cannot be performed because 'TMAXi', 'TLASTi', 'CMAXi', 'CLASTi', 'AUCLASTi' are not part of the calculated parameters AND Flag 'FLGACCEPTKELCRIT' and Flag 'FLGEXKEL' are not present in the dataset")
  }
  
  if(isTRUE(optimize_kel) && comp_required[["TMAXi"]] && comp_required[["TLASTi"]] && comp_required[["CMAXi"]] && comp_required[["CLASTi"]] && comp_required[["AUCLASTi"]] &&
     "FLGACCEPTKELCRIT" %in% names(map_data) && "FLGEXKEL" %in% names(map_data) && map_data$FLGEXKEL %in% names(data_data)){
    kel_flag_optimized <- integer()
    kel_opt_warning <- FALSE
  }

  for(i in 1:length(unique(data_data[,map_data$SDEID]))){
###    cat("############### SDEID: ", i, '\n')
    tryCatch({
###      if("DIi" %in% parameter_list) {
      if(comp_required[["DIi"]]) {
        di <- list()
      }
###      if("TAUi" %in% parameter_list) {
      if(comp_required[["TAUi"]]) {
        tau <- list()
      }
###      if("TOLD" %in% parameter_list) {
      if(comp_required[["TOLD"]]) {
        told <- list()
      }
###      if("DOSEi" %in% parameter_list) {
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
      if(comp_required[["AURCT"]] && aet_len >= 2) {
        aurct <- list()
        aurc_int <- list()
      }
###      if("MAXRATEi" %in% parameter_list) {
      if(comp_required[["MAXRATEi"]]) {
        max_rate_i <- list()
      }
###      if("MIDPTLASTi" %in% parameter_list) {
      if(comp_required[["MIDPTLASTi"]]) {
        midpt_last_i <- list()
      }
###      if("TMAXRATEi" %in% parameter_list) {
      if(comp_required[["TMAXRATEi"]]) {
        tmax_rate_i <- list()
      }
###      if("RATELASTi" %in% parameter_list) {
      if(comp_required[["RATELASTi"]]) {
        rate_last_i <- list()
      }

      tmp_df <- data_data[data_data[,map_data$SDEID] == unique(data_data[,map_data$SDEID])[i],]
      tmp_df <- tmp_df[order(tmp_df[,map_data$TIME]),]
      tmp_df[,map_data$CONC] <- as.numeric(tmp_df[,map_data$CONC])
      tmp_df[,map_data$TIME] <- as.numeric(tmp_df[,map_data$TIME])
      
      tmp_kel_flg <- as.numeric(tmp_df[,map_data$FLGEXKEL])
      if("FLGEXSDE" %in% names(map_data) && map_data$FLGEXSDE %in% names(data_data)){
        ex_flag <- as.numeric(tmp_df[,map_data$FLGEXSDE])
        if(all(is.na(ex_flag))){
          ex_flag[is.na(ex_flag)] <- 0
        }
        tmp_df <- tmp_df[!as.logical(ex_flag),]
      } else {
        ex_flag <- NULL
      }
      if("FLGEXKEL" %in% names(map_data) && map_data$FLGEXKEL %in% names(data_data)){
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
      if("FLGEXAUC" %in% names(map_data) && map_data$FLGEXAUC %in% names(data_data)){
        auc_flag <- as.numeric(tmp_df[,map_data$FLGEXAUC])
        if(all(is.na(auc_flag))){
          auc_flag[is.na(auc_flag)] <- 0
        }
      } else {
        auc_flag <- NULL
      }
      if("FLGEMESIS" %in% names(map_data) && map_data$FLGEMESIS %in% names(data_data)){
        emesis_flag <- as.numeric(tmp_df[,map_data$FLGEMESIS])
        if(all(is.na(emesis_flag))){
          emesis_flag[is.na(emesis_flag)] <- 0
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
      
### 2019-09-03/TGT/ remap map_data[[map_data$TIME]] to map_data$TIME, map_data[map_data$ENDTIME]] to map_data$ENDTIME
###      mid_pt <- midpt(start_time = tmp_df[,map_data[[map_data$TIME]]], end_time = tmp_df[,map_data[[map_data$ENDTIME]]])
      mid_pt <- midpt(start_time = tmp_df[,map_data$TIME], end_time = tmp_df[,map_data$ENDTIME])
### 2019-09-03/TGT/ remap map_data[[map_data$TIME]] to map_data$TIME
###      amt <- at(conc = tmp_df[,map_data$CONC], amt = as.numeric(tmp_df[,map_data$AMOUNT]), time = tmp_df[,map_data[[map_data$TIME]]], amt_units = tmp_df[,map_data$AMOUNTU])
      amt <- at(conc = tmp_df[,map_data$CONC], amt = as.numeric(tmp_df[,map_data$AMOUNT]), time = tmp_df[,map_data$TIME], amt_units = tmp_df[,map_data$AMOUNTU])
      if(length(sort(unique(data_data[,map_data$ENDTIME])[1:aet_len])) > length(tmp_df[,map_data$TIME])){
        tmp_amt <- data.frame(amt = amt, time = tmp_df[,map_data$TIME])
        amt <- as.numeric(unlist(lapply(sort(unique(data_data[,map_data$ENDTIME])[1:aet_len]), function(x){ return(ifelse(!(x %in% tmp_df[,map_data$TIME]), NA, tmp_amt[tmp_amt$time == x,"amt"])) })))
      }
### 2019-09-03/TGT/ remap map_data[[map_data$TIME]] to map_data$TIME, map_data[map_data$ENDTIME]] to map_data$ENDTIME
###      rt <- rate(start_time = tmp_df[,map_data[[map_data$TIME]]], end_time = tmp_df[,map_data[[map_data$ENDTIME]]], conc = tmp_df[,map_data$CONC], vol = as.numeric(tmp_df[,map_data$AMOUNT]))
### 2019-10-03/TGT/ rt <- rate(start_time = tmp_df[,map_data$TIME], end_time = tmp_df[,map_data$ENDTIME], conc = tmp_df[,map_data$CONC], vol = as.numeric(tmp_df[,map_data$AMOUNT]))
      type <- ifelse("SAMPLETYPE" %in% names(map_data), ifelse(map_data$SAMPLETYPE %in% names(tmp_df), as.character(unique(tmp_df[,map_data$SAMPLETYPE])[1]), NULL), NULL)
      rt <- rate(start_time = tmp_df[,map_data$TIME], end_time = tmp_df[,map_data$ENDTIME], conc = tmp_df[,map_data$CONC], vol = as.numeric(tmp_df[,map_data$AMOUNT]), volu = tmp_df[,map_data$AMOUNTU], type = type, map = map_data)
      
      if(nrow(tmp_df) > 0){
        orig_time <- rt
        orig_conc <- mid_pt
        
### 2019-09-03/TGT/ remap map_data[[map_data$TIME]] to map_data$TIME
###        c_0 <- c0(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data[[map_data$TIME]]])
        c_0 <- c0(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME])
###        if("TLAG" %in% parameter_list) {
        if(comp_required[["TLAG"]]) {
          t_lag <- tlag(conc = rt, time = mid_pt)
        }
        if(comp_required[["TLAST"]]) {
          t_last <- tlast(conc = rt, time = mid_pt)
        } else {
          t_last <- NULL
        }
###        if("KEL" %in% parameter_list || "KELTMLO" %in% parameter_list || "KELTMHI" %in% parameter_list || "KELNOPT" %in% parameter_list || "THALF" %in% parameter_list || "THALFF" %in% parameter_list) {
        if(comp_required[["KEL"]] || comp_required[["KELTMLO"]] || comp_required[["KELTMHI"]] || comp_required[["KELNOPT"]] || comp_required[["THALF"]] || comp_required[["THALFF"]]) {
          span_ratio <- ifelse("SPANRATIOCRIT" %in% names(map_data), suppressWarnings(as.numeric(map_data$SPANRATIOCRIT)), NA)
          kel_v <- kel(conc = rt, time = mid_pt, exflag = kel_flag, spanratio = span_ratio)
        } else {
          kel_v <- NULL
        }
###        if("KELRSQ" %in% parameter_list || "KELRSQA" %in% parameter_list) {
        if(comp_required[["KELR"]] || comp_required[["KELRSQ"]] || comp_required[["KELRSQA"]]) {
          kelr_v <- kel_r(conc = rt, time = mid_pt, exflag = kel_flag)
        }
        if(comp_required[["CEST"]] || parameter_required("KEL", names(kel_v)) || parameter_required("KELC0", names(kel_v))) {
          c_est <- cest(conc = rt, time = mid_pt, kelflag=kel_flag, t_last=t_last, spanratio=span_ratio, kel=kel_v[["KEL"]], kelc0=kel_v[["KELC0"]])
        }
###        if("AE" %in% parameter_list) {
        if(comp_required[["AE"]]) {
### 2019-09-03/TGT/ remap map_data[[map_data$TIME]] to map_data$TIME
###          a_e <- ae(amt = amt, time = tmp_df[,map_data[[map_data$TIME]]])
          a_e <- ae(amt = amt, time = tmp_df[,map_data$TIME])
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
### 2019-09-03/TGT/ remap map_data[[map_data$TIME]] to map_data$TIME
###          tmp_mid_pt <- midpt(start_time = tmp_di_df[,map_data[[map_data$TIME]]], end_time = tmp_di_df[,map_data$NOMENDTIME])
          tmp_mid_pt <- midpt(start_time = tmp_di_df[,map_data$TIME], end_time = tmp_di_df[,map_data$NOMENDTIME])
### 2019-09-03/TGT/ remap map_data[[map_data$TIME]] to map_data$TIME
###          tmp_rt <- rate(start_time = tmp_di_df[,map_data[[map_data$TIME]]], end_time = tmp_di_df[,map_data$NOMENDTIME], conc = tmp_di_df[,map_data$CONC], vol = as.numeric(tmp_di_df[,map_data$AMOUNT]))
### 2019-10-03/TGT/ tmp_rt <- rate(start_time = tmp_di_df[,map_data$TIME], end_time = tmp_di_df[,map_data$NOMENDTIME], conc = tmp_di_df[,map_data$CONC], vol = as.numeric(tmp_di_df[,map_data$AMOUNT]))
          tmp_rt <- rate(start_time = tmp_di_df[,map_data$TIME], end_time = tmp_di_df[,map_data$NOMENDTIME], conc = tmp_di_df[,map_data$CONC], vol = as.numeric(tmp_di_df[,map_data$AMOUNT]), volu = tmp_di_df[,map_data$AMOUNTU], map=map_data)

###          if("MAXRATEi" %in% parameter_list) {
          if(comp_required[["MAXRATEi"]]) {
            max_rate_i[[d]] <- maxrate(rate = tmp_rt)
            #print(max_rate[[d]])
          }
###          if("TMAXRATEi" %in% parameter_list) {
          if(comp_required[["TMAXRATEi"]]) {
            tmax_rate_i[[d]] <- tmaxrate(midpt = tmp_mid_pt, rate = tmp_rt)
          }
###          if("RATELASTi" %in% parameter_list) {
          if(comp_required[["RATELASTi"]]) {
            rate_last_i[[d]] <- ratelast(rate = tmp_rt)
          }
###          if("MIDPTLASTi" %in% parameter_list) {
          if(comp_required[["MIDPTLASTi"]]) {
            midpt_last_i[[d]] <- midptlast(midpt = tmp_mid_pt, rate = tmp_rt)
            #print(midpt_last[[d]])
          }
###          if("TAUi" %in% parameter_list) {
          if(comp_required[["TAUi"]]) {
            tau[[d]] <- tmp_di_df[, as.character(map_data[c(paste0("TAU",d))])][1]
            tau[[d]] <- as.numeric(tau[[d]])
          }
###          if("TOLD" %in% parameter_list) {
          if(comp_required[["TOLD"]]) {
            told[[d]] <- tmp_di_df[, as.character(map_data[c(paste0("TOLD",d))])][1]
            told[[d]] <- as.numeric(told[[d]])
          }
###          if("DIi" %in% parameter_list) {
          if(comp_required[["DIi"]]) {
### 2019-09-03/TGT/ remap map_data[[map_data$TIME]] to map_data$TIME
###            if(is.na(tmp_di_df[,map_data[[map_data$TIME]]][1])){
            if(is.na(tmp_di_df[,map_data$TIME][1])){
              di[[d]] <- NA
            } else {
### 2019-09-03/TGT/ remap map_data[[map_data$TIME]] to map_data$TIME
###              di[[d]] <- paste0(tmp_di_df[,map_data[[map_data$TIME]]][1], "-", tmp_di_df[,map_data[[map_data$TIME]]][nrow(tmp_di_df)])
              di[[d]] <- paste0(tmp_di_df[,map_data$TIME][1], "-", tmp_di_df[,map_data$TIME][nrow(tmp_di_df)])
            }
          }
###          if("DOSEi" %in% parameter_list) {
          if(comp_required[["DOSEi"]]) {
            dose[[d]] <- tmp_dose
          }
###          if("AEPCT" %in% parameter_list && "AE" %in% parameter_list) {
          if(comp_required[["AEPCTi"]]) {
            ae_pct_i[[d]] <- aepct(ae = a_e, dose = tmp_dose)
          }
###          if("AET" %in% parameter_list) {
### 2019-09-23/TGT/ Note the following computes "AE" (amt) "AET" (ae_t) and "AETPCT" (aet_pct)
          if(comp_required[["AET"]]) {
            ae_t <- NULL
            aet_pct <- NULL
### 2019-09-03/TGT/ remap map_data[[map_data$TIME]] to map_data$TIME
###            for(t in 1:length(unique(tmp_df[,map_data[[map_data$TIME]]]))){
            for(t in 1:length(unique(data_data[,map_data$TIME]))){
### 2019-09-03/TGT/ remap map_data[[map_data$TIME]] to map_data$TIME
###              tmp <- aet(amt = amt, time = na.omit(tmp_df[,map_data[[map_data$TIME]]]), t = na.omit(tmp_df[,map_data[[map_data$TIME]]])[t])
              tmp <- aet(amt = amt, time = na.omit(sort(tmp_df[,map_data$TIME])), t = sort(unique(data_data[,map_data$TIME]))[t], orig_time = sort(unique(data_data[,map_data$TIME])), returnNA = TRUE)
              tmp_map <- map_data
              tmp_res <- tmp_df[,c(map_data$SDEID, dosevar)]
              tmp_res$AET <- tmp
              aet_pct_dose <- unique(unit_conversion(tmp_df, tmp_map, tmp_res, unit_class = "DOSEU", verbose = FALSE)[,dosevar])[1]
              aet_pct_aet <- unique(unit_conversion(tmp_df, tmp_map, tmp_res, unit_class = "AMOUNTU", verbose = FALSE)[,"AET"])[1]
              tmp_pct <-  aetpct(aet = aet_pct_aet, dose = aet_pct_dose)
###              cat('i: ', i, ' tmp_dose: ', tmp_dose, ' d: ', d, ' dose[[d]]: ', dose[[d]], '\n')

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
### 2019-09-03/TGT/ remap map_data[[map_data$TIME]] to map_data$TIME
###            amt <- at(conc = tmp_df[,map_data$CONC], amt = tmp_df[,map_data$AMOUNT], time = tmp_df[,map_data[[map_data$TIME]]], amt_units = tmp_df[,map_data$AMOUNTU])
            #amt <- at(conc = tmp_df[,map_data$CONC], amt = tmp_df[,map_data$AMOUNT], time = tmp_df[,map_data$TIME], amt_units = tmp_df[,map_data$AMOUNTU])
          }
          if(comp_required[["AETAUi"]]){
            aetau_i[[d]] <- aetau(aet = ae_t, time = na.omit(tmp_df[,map_data$TIME]), t = tau[[d]])
          }
          if(comp_required[["AETAUPTi"]]) {
            aetau_pt_i[[d]] <- aepct(ae = aetau_i[[d]], dose = tmp_dose)
          }
          if(comp_required[["AURCT"]] && (aet_len) >= 2) {
            prev_na <- FALSE
            prev_aurc <- NA
            
            if(length(mid_pt) > 1){
              for(t in 2:(length(mid_pt))){
                if(mid_pt[t] %in% tmp_mid_pt[-1]) {
                  tmp <- auc_t1_t2(conc = rt, time =  mid_pt, t1 = tmp_mid_pt[1], t2 = mid_pt[t], method = method, exflag = auc_flag, t_max = tmax_rate_i[[d]])
                  tmp_int <- paste0(mid_pt[1], "_", mid_pt[t])
                } else {
                  tmp <- NA
                  tmp_int <- paste0(mid_pt[1], "_", mid_pt[t])
                }
                
                if(d == 1){
                  aurct[[t-1]] <- tmp
                  aurc_int[[t-1]] <- tmp_int
                } else {
                  if(prev_na){
                    prev_na <- FALSE
                    if(is.numeric(tmp)){
                      prev_aurc <- unlist(aurct[[t-2]])
                      aurct[[t-1]] <- sum(c(prev_aurc, tmp), na.rm = TRUE)
                    }
                  } else {
                    if(!is.na(prev_auc)){
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
              aurct <- rep(NA, aet_len)
              aurc_int <- rep(NA, aet_len)
            }
            if(d == di_col){
              if(length(aurct) < aet_len) {
                aurct <- c(aurct, rep(NA, (aet_len - length(aurct))))
              }
              if(length(aurc_int) < aet_len) {
                aurc_int <- c(aurc_int, rep(NA, (aet_len - length(aurc_int))))
              }
            }
          }
###        if("AURCT1_T2" %in% parameter_list && "TMAXRATEi" %in% parameter_list) {
###          cat('comp_required[["AURCT1_T2"]]: ', comp_required[["AURCT1_T2"]], ' auc_pair_check: ', auc_pair_check, '\n')
          if(comp_required[["AURCT1_T2"]] && auc_pair_check) {
## 2019-11-21/RD This is commented since it is not part of AURCT1_T2
##          aurct <- NULL
##          aurc_int <- NULL
##          for(t in 2:(mid_len+1)){
##            tmp <- auc_t1_t2(conc = rt, time = mid_pt, t1 = mid_pt[1], t2 = mid_pt[t], method = method, exflag = auc_flag, t_max = tmax_rate[[d]])
##            tmp_int <- paste0(unique(mid_pt)[1], "_", unique(mid_pt)[t])
##
##            if(is.null(aurct)){
##              aurct <- tmp
##              aurc_int <- tmp_int
##            } else {
##              aurct <- c(aurct, tmp)
##              aurc_int <- c(aurc_int, tmp_int)
##            }
##          }
##          if(length(aurct) < mid_col) {
##            aurct <- c(aurct, rep(NA, (mid_col - length(aurct))))
##          }
##          if(length(aurc_int) < mid_col) {
##            aurc_int <- c(aurc_int, rep(NA, (mid_col - length(aurc_int))))
##          }

###
###          print(aurct)
###          print(aurc_int)

##          if(auc_pair_check){
##          2019-11-26/RD Added for Interpolation to account for error handling
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
### 2019-09-23/TGT/ fix auc_par_len -> aurc_par_len
###            for(t in 1:(aurc_par_len)){
            for(t in 1:(aurc_par_len)){
              if(!(is.numeric(as.numeric(map_data[, paste0("AUC.", t, ".T1")])) && is.numeric(as.numeric(map_data[, paste0("AUC.", t, ".T2")])))){
                stop(paste0("'AUC.", t, ".T1' and/or 'AUC.", t, ".T2' value provided via 'map' is not a numeric value"))
              }
              aurc_t1 <- as.numeric(map_data[, paste0("AUC.", t, ".T1")])
              aurc_t2 <- as.numeric(map_data[, paste0("AUC.", t, ".T2")])
              
              if((isTRUE(interpolation) || isTRUE(extrapolation))){
                tmp <- auc_t1_t2(conc = rt, time = mid_pt, t1 = aurc_t1, t2 = aurc_t2, method = method, exflag = auc_flag, t_max = tmax_rate, interpolate = interpolation, extrapolate = extrapolation, model = "M4", dosing_type = "SS", told = tmp_told, kel = kel_v, orig_conc = orig_conc, orig_time = orig_time, includeNA = TRUE)
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
                tmp_auc <- auc_t1_t2(conc = rt, time = mid_pt, t1 = aurc_t1, t2 = aurc_t2, method = method, exflag = auc_flag, t_max = tmax_rate, includeNA = TRUE)
              }
              
              if(d == 1){
                aurct1_t2[[t]] <- tmp_auc
              } else {
                aurct1_t2[[t]] <- sum(c(unlist(aurct1_t2[[t]]), tmp_auc), na.rm = TRUE)
              }
            }
          }
        }

###        if("AUCDN" %in% parameter_list) {
### 2019-010-10/TGT/ Removed AUCDN
###        if(comp_required[["AUCDN"]]) {
### 2019-09-03/TGT/ remap map_data[[map_data$TIME]] to map_data$TIME
###          aucall <- auc_all(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data[[map_data$TIME]]], method = method, exflag = auc_flag)
###          aucall <- auc_all(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$TIME], method = method, exflag = auc_flag)
###          aucdn <- auc_dn(auc = aucall, dose = tmp_dose)
###        }
###        if("AURCALL" %in% parameter_list) {
        if(comp_required[["AURCALL"]]) {
          aurcall <- auc_all(conc = rt, time = mid_pt, method = method, exflag = auc_flag)
        }
###        if("AURCLAST" %in% parameter_list) {
        if(comp_required[["AURCLAST"]]) {
          aurclast <- auc_last(conc = rt, time = mid_pt, method = method, exflag = auc_flag)
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
          
          if(all(c("KELNOPT", "KELRSQ") %in% flag_df$VAR) && ("AURCXPCTO" %in% flag_df$VAR || "AURCXPCTP" %in% flag_df$VAR)){
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
            
            kelr_val <- kel_r(conc = rt, time = mid_pt)[["KELRSQ"]]
            if("AURCXPCTO" %in% flag_df$VAR){
              aucxpct <- auc_XpctO(conc = rt, time = mid_pt, method = method, aucflag = auc_flag)
            } else if("AURCXPCTP" %in% flag_df$VAR){
              aucxpct <- auc_XpctP(conc = rt, time = mid_pt, method = method, aucflag = auc_flag)
            } else {
              stop("Error in optimize kel")
            }
            
            selected_idx <- NA
            saved_kel_opt <- -1
            for(k in 1:length(ulist)){
              sel_time <- ulist[[k]]
              sel_conc <- tmp_conc[match(sel_time, tmp_time)]
              
              kelr_opt <- kel_r(conc = sel_conc, time = sel_time)[["KELRSQ"]]
              if("AURCXPCTO" %in% flag_df$VAR){
                span_ratio <- ifelse("SPANRATIOCRIT" %in% names(map_data), suppressWarnings(as.numeric(map_data$SPANRATIOCRIT)), NA)
                aucinfo_opt <- auc_inf_o(conc = rt, time = mid_pt, method = method, auclast = auclast, c_last = c_last, spanratio = span_ratio)
                aucxpct_opt <- auc_XpctO(conc = sel_conc, time = sel_time, method = method, aucflag = auc_flag, auc_info = aucinfo_opt, auclast = auclast)
              } else if("AURCXPCTP" %in% flag_df$VAR){
                span_ratio <- ifelse("SPANRATIOCRIT" %in% names(map_data), suppressWarnings(as.numeric(map_data$SPANRATIOCRIT)), NA)
                aucinfp_opt <- auc_inf_p(conc = rt, time = mid_pt, method = method, auclast = auclast, t_last = t_last, spanratio = span_ratio)
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
              warning("Kel optimization cannot be performed because 'KELNOPT', 'KELRSQ' and 'AURCXPCTO' or 'AURCXPCTP' are not part of the Flag 'FLGACCEPTKELCRIT'")
              kel_opt_warning <- TRUE
            }
          }
        }
###        if("AURCINFO" %in% parameter_list) {
        if(comp_required[["AURCINFO"]]) {
          aurcinf_o <- auc_inf_o(conc = rt, time = mid_pt, method = method, kelflag = kel_flag, aucflag = auc_flag)
        }
###        if("AURCINFP" %in% parameter_list) {
        if(comp_required[["AURCINFP"]]) {
          aurcinf_p <- auc_inf_p(conc = rt, time = mid_pt, method = method, kelflag = kel_flag, aucflag = auc_flag)
        }
###        if("AURCXPCTO" %in% parameter_list){
        if(comp_required[["AURCXPCTO"]]){
          aurcxpcto <- auc_XpctO(conc = rt, time = mid_pt, method = method, kelflag = kel_flag, aucflag = auc_flag)
        }
###        if("AURCXPCTP" %in% parameter_list){
        if(comp_required[["AURCXPCTP"]]){
          aurcxpctp <- auc_XpctP(conc = rt, time = mid_pt, method = method, kelflag = kel_flag, aucflag = auc_flag)
        }
### 2019-09-23/TGT/ Add VOLSUM
        if(comp_required[["VOLSUM"]]) {
          volsum <- vol_sum(vol = tmp_df[,map_data$SAMPLEVOLUME], volu = tmp_df[,map_data$SAMPLEVOLUMEU])
        }
###        if("KEL" %in% parameter_list){
        if(comp_required[["KEL"]]){
          exflag <- !as.logical(kel_flag)

          pkdataid <- tmp_df[,"PKDATAROWID"][exflag]
### 2019-09-03/TGT/ Following incorrectly identifies CONC for TIME
###          time <- tmp_df[,map_data$CONC][exflag]
          time <- tmp_df[,map_data$TIME][exflag]
### 2019-08-05/TGT/ Following incorrectly identifies TIME for CONC
###          conc <- tmp_df[,map_data[[map_data$TIME]]][exflag]
          conc <- tmp_df[,map_data$CONC][exflag]
          cest_kel <- rep(NA, length(conc))

          if(!is.na(kel_v[["KEL"]])){
### 2019-09-03/TGT/ following algorithm for estimation of intercept is not correct            
###            intercept <- sum(conc-(-1*kel_v[["KEL"]]*time))/length(conc)
### is replaced with
### 2019-09-03/TGT/ following algorithm for estimation of concentration is not correct            
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

        #computation_df[i,] <- c(unique(data_data[,map_data$SDEID])[i], amt, ae_t, a_e, kel_v[["KEL"]], kel_v[["KELTMLO"]],
        #                        kel_v[["KELTMHI"]], kel_v[["KELNOPT"]], kelr_v[["KELR"]], kelr_v[["KELRSQ"]],
        #                        kelr_v[["KELRSQA"]], kel_v[["THALF"]])

##        row_data <- c(unique(data_data[,map_data$SDEID])[i])
        computation_df[i, "SDEID"] <- unique(data_data[,map_data$SDEID])[i]  
###        if("AET" %in% parameter_list) {
### 2019-09-23/TGT/ Note the following reports "AE" (amt) "AET" (ae_t) and "AETPCT" (aet_pct)
        if(disp_required[["AET"]]) {
##            row_data <- c(row_data, amt, ae_t)
          computation_df[i, paste0("AMT.", sprintf("%.2f", unique(data_data[,map_data$ENDTIME])[1:aet_len]))] <- amt
          computation_df[i, paste0("AE.", sprintf("%.2f", unique(data_data[,map_data$ENDTIME])[1:aet_len]))] <- ae_t
        }
###        if("AETPCT" %in% parameter_list && "AET" %in% parameter_list) {
        if(disp_required[["AETPCT"]]) {
##          row_data <- c(row_data, aet_pct)
          computation_df[i, paste0("AEPCT.", sprintf("%.2f", unique(data_data[,map_data$ENDTIME])[1:aet_len]))] <- aet_pct
        }
###        if("AE" %in% parameter_list) {
        if(disp_required[["AE"]]) {
##          row_data <- c(row_data, a_e)
          computation_df[i, "AE"] <- a_e
        }
###        if("AEPCT" %in% parameter_list && "AE" %in% parameter_list) {
        if(disp_required[["AEPCTi"]]) {
##          row_data <- c(row_data, ae_pct)
          computation_df[i, paste0("AEPCT",1:di_col)] <- unlist(ae_pct_i)
        }
        if(disp_required[["AETAUi"]]) {
##          row_data <- c(row_data, unlist(aetau_i))
          computation_df[i, paste0("AETAU",1:di_col)] <- unlist(aetau_i)
        }
        if(disp_required[["AETAUPTi"]]) {
##          row_data <- c(row_data, unlist(aetau_pt_i))
          computation_df[i, paste0("AETAUPT",1:di_col)] <- unlist(aetau_pt_i)
        }
        if(disp_required[["MAXRATE"]]) {
##          row_data <- c(row_data, max_rate)
          computation_df[i, "MAXRATE"] <- max_rate
        }
        if(disp_required[["TMAXRATE"]]) {
##          row_data <- c(row_data, tmax_rate)
          computation_df[i, "TMAXRATE"] <- tmax_rate
        }
        if(disp_required[["RATELAST"]]) {
##          row_data <- c(row_data, rate_last)
          computation_df[i, "RATELAST"] <- rate_last
        }
        if(disp_required[["MIDPTLAST"]]) {
##          row_data <- c(row_data, midpt_last)
          computation_df[i, "MIDPTLAST"] <- midpt_last
        }
###        if("MAXRATEi" %in% parameter_list) {
        if(disp_required[["MAXRATEi"]]) {
##          row_data <- c(row_data, unlist(max_rate_i))
          computation_df[i, paste0("MAXRATE",1:di_col)] <- unlist(max_rate_i)
        }
###        if("TMAXRATEi" %in% parameter_list) {
        if(disp_required[["TMAXRATEi"]]) {
##          row_data <- c(row_data, unlist(tmax_rate_i))
          computation_df[i, paste0("TMAXRATE",1:di_col)] <- unlist(tmax_rate_i)
        }
###        if("RATELASTi" %in% parameter_list) {
        if(disp_required[["RATELASTi"]]) {
##          row_data <- c(row_data, unlist(rate_last_i))
          computation_df[i, paste0("RATELAST",1:di_col)] <- unlist(rate_last_i)
        }
###        if("MIDPTLASTi" %in% parameter_list) {
        if(disp_required[["MIDPTLASTi"]]) {
##          row_data <- c(row_data, unlist(midpt_last_i))
          computation_df[i, paste0("MIDPTLAST",1:di_col)] <- unlist(midpt_last_i)
        }
###        if("TLAG" %in% parameter_list) {
        if(disp_required[["TLAG"]]) {
##          row_data <- c(row_data, t_lag)
          computation_df[i, "TLAG"] <- t_lag
        }
###        if("KEL" %in% parameter_list) {
        if(disp_required[["KEL"]]) {
##          row_data <- c(row_data, kel_v[["KEL"]])
          computation_df[i, "KEL"] <- ifelse("KEL" %in% names(kel_v), kel_v[["KEL"]], NA)
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
###        if("KELRSQ" %in% parameter_list || "KELRSQA" %in% parameter_list){
### 2019-09-23/TGT/ KELR replaces KELRSQ/KELRSQA here - need to refer directly to KELR
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
###        if("AUCDN" %in% parameter_list) {
### 2019-010-10/TGT/ Removed AUCDN
###        if(disp_required[["AUCDN"]]) {
###          row_data <- c(row_data, aucdn)
###        }
###        if("AURCALL" %in% parameter_list) {
        if(disp_required[["AURCALL"]]) {
##          row_data <- c(row_data, aurcall)
          computation_df[i, "AURCALL"] <- aurcall
        }
###        if("AURCLAST" %in% parameter_list) {
        if(disp_required[["AURCLAST"]]) {
##          row_data <- c(row_data, aurclast)
          computation_df[i, "AURCLAST"] <- aurclast
        }
        if(disp_required[["AURCT"]] && (aet_len) >= 2) {
##          row_data <- c(row_data, unlist(aurct), unlist(aurc_int))
          computation_df[i, paste0("AURC",1:aet_len)] <- unlist(aurct)
          computation_df[i, paste0("AURCINT",1:aet_len)] <- unlist(aurc_int)
        }
###        if("AURCT1_T2" %in% parameter_list) {
        if(disp_required[["AURCT1_T2"]] && auc_pair_check) {
###            cat('AURC 1-x: and AURCINT 1-x\n'); print(row_data)
##          row_data <- c(row_data, unlist(aurct1_t2))
          computation_df[i, paste0("AURC", map_data[,rep(paste0("AUC.", 1:aurc_par_len, ".T1"))], "_", map_data[,rep(paste0("AUC.", 1:aurc_par_len, ".T2"))])] <- unlist(aurct1_t2)
        }
###        if("AURCINFO" %in% parameter_list) {
        if(disp_required[["AURCINFO"]]) {
##          row_data <- c(row_data, aurcinf_o)
          computation_df[i, "AURCINFO"] <- aurcinf_o
        }
###        if("AURCINFP" %in% parameter_list) {
        if(disp_required[["AURCINFP"]]) {
##          row_data <- c(row_data, aurcinf_p)
          computation_df[i, "AURCINFP"] <- aurcinf_p
        }
###        if("AURCXPCTO" %in% parameter_list){
        if(disp_required[["AURCXPCTO"]]){
##          row_data <- c(row_data, aurcxpcto)
          computation_df[i, "AURCXPCTO"] <- aurcxpcto
        }
###        if("AURCXPCTP" %in% parameter_list){
        if(disp_required[["AURCXPCTP"]]){
##          row_data <- c(row_data, aurcxpctp)
          computation_df[i, "AURCXPCTP"] <- aurcxpctp
        }
### 2019-09-23/TGT/ Add VOLSUM
        if(disp_required[["VOLSUM"]]) {
##          row_data <- c(row_data, volsum)
          computation_df[i, "VOLSUM"] <- volsum
        }
### 2019-09-23/TRT print RATE1-x, MIDPT1-x
##        row_data <- c(row_data,
##                      c(rt, rep(NA, ((aet_len) - length(rt)))),
##                      c(mid_pt, rep(NA, ((aet_len) - length(mid_pt))))
##        )
        if(parameter_required("^(RATE)([0-9]*?|A|N)$", parameter_list) || parameter_required(dependent_parameters("^(RATE)([0-9]*?|A|N)$"), parameter_list)) {
          computation_df[i, paste0("RATE",1:(aet_len))] <- c(rt, rep(NA, ((aet_len) - length(rt))))
        }
        if(parameter_required("^(MIDPT)([0-9]*?|A|N)$", parameter_list) || parameter_required(dependent_parameters("^(MIDPT)([0-9]*?|A|N)$"), parameter_list)) {
          computation_df[i, paste0("MIDPT",1:(aet_len))] <- c(mid_pt, rep(NA, ((aet_len) - length(mid_pt))))
        }

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
### 2019-09-23/TGT/ Replace "TOLD" with "TOLDi"
###        if(disp_required[["TOLD"]]) {
        if(disp_required[["TOLDi"]]) {
##          row_data <- c(row_data, unlist(told))
          computation_df[i, paste0("TOLD",1:di_col)] <- unlist(told)
        }
###        if("DOSEi" %in% parameter_list) {
        if(disp_required[["DOSEi"]]) {
##          row_data <- c(row_data, unlist(dose))
          computation_df[i, unlist(dosevar)] <- unlist(dose)
        }
        #print(row_data)
###
###          cat('i: ', i, ' length(row_data): ', length(row_data), ' length(col_names): ', length(col_names), '\n')
###          cat('col_names: ', col_names, '\n')
###          print(row_data)
###          print(dim(computation_df))
##          computation_df[i,] <- row_data
###
###          cat('i: ', i, 'assigned to computation_df\n')
          
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
###  cat("dim(computation_df)): \n")
###  print(dim(computation_df))
  
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

### 2019-09-09/TGT/ Factoring out all display_list and display_parameters code  
###  if(is.list(display_list) && !is.null(display_list) && length(display_list) > 0){
  if(FALSE) {
    display_parameters <- c("SDEID")
###    if("AET" %in% display_list) {
    if(disp_required[["AET"]]) {
###      display_parameters <- c(display_parameters, rep(paste0("AMT.", sprintf("%.2f", unique(data_data[,map_data[[map_data$ENDTIME]]])[1:aet_len]))), rep(paste0("AE.", sprintf("%.2f", unique(data_data[,map_data[[map_data$ENDTIME]]])[1:aet_len]))))
      display_parameters <- c(display_parameters, rep(paste0("AMT.", sprintf("%.2f", unique(data_data[,map_data$ENDTIME])[1:aet_len]))), rep(paste0("AE.", sprintf("%.2f", unique(data_data[,map_data$ENDTIME])[1:aet_len]))))
    }
###    if("AETPCT" %in% display_list && "AET" %in% display_list) {
    if(disp_required[["AETPCT"]]) {
###      display_parameters <- c(display_parameters, rep(paste0("AEPCT.", sprintf("%.2f", unique(data_data[,map_data[[map_data$ENDTIME]]])[1:aet_len]))))
      display_parameters <- c(display_parameters, rep(paste0("AEPCT.", sprintf("%.2f", unique(data_data[,map_data$ENDTIME])[1:aet_len]))))
    }
###    if("AE" %in% display_list) {
    if(disp_required[["AE"]]) {
      display_parameters <- c(display_parameters, "AE")
    }
###    if("AEPCT" %in% display_list && "AE" %in% display_list) {
    if(disp_required[["AEPCT"]]) {
      display_parameters <- c(display_parameters, "AEPCT")
    }
###    if("MAXRATEi" %in% display_list) {
    if(disp_required[["MAXRATEi"]]) {
      display_parameters <- c(display_parameters, rep(paste0("MAXRATE",1:di_col)))
    }
###    if("TMAXRATEi" %in% display_list) {
    if(disp_required[["TMAXRATEi"]]) {
      display_parameters <- c(display_parameters, rep(paste0("TMAXRATE",1:di_col)))
    }
###    if("RATELASTi" %in% display_list) {
    if(disp_required[["RATELASTi"]]) {
      display_parameters <- c(display_parameters, rep(paste0("RATELAST",1:di_col)))
    }
###    if("MIDPTLASTi" %in% display_list) {
    if(disp_required[["MIDPTLASTi"]]) {
      display_parameters <- c(display_parameters, rep(paste0("MIDPTLAST",1:di_col)))
    }
###    if("TLAG" %in% display_list) {
    if(disp_required[["TLAG"]]) {
      display_parameters <- c(display_parameters, "TLAG")
    }
###    if("KEL" %in% display_list) {
    if(disp_required[["KEL"]]) {
      display_parameters <- c(display_parameters, "KEL")
    }
###    if("KELTMLO" %in% display_list) {
    if(disp_required[["KELTMLO"]]) {
      display_parameters <- c(display_parameters, "KELTMLO")
    }
###    if("KELTMHI" %in% display_list) {
    if(disp_required[["KELTMHI"]]) {
      display_parameters <- c(display_parameters, "KELTMHI")
    }
###    if("KELNOPT" %in% display_list) {
    if(disp_required[["KELNOPT"]]) {
      display_parameters <- c(display_parameters, "KELNOPT")
    }
###    if("KELRSQ" %in% display_list || "KELRSQA" %in% display_list){
    if(disp_required[["KELR"]]){
      display_parameters <- c(display_parameters, "KELR")
    }
###    if("KELRSQ" %in% display_list){
    if(disp_required[["KELRSQ"]]){
      display_parameters <- c(display_parameters, "KELRSQ")
    }
###    if("KELRSQA" %in% display_list){
    if(disp_required[["KELRSQA"]]){
      display_parameters <- c(display_parameters, "KELRSQA")
    }
    if(disp_required[["FLGACCEPTKEL"]] && "FLGACCEPTKELCRIT" %in% names(map_data)) {
      if(length(unlist(strsplit(as.character(map_data$FLGACCEPTKELCRIT), ","))) > 0){
        display_parameters <- c(display_parameters, "FLGACCEPTKEL")
      }
    }
###    if("THALF" %in% display_list) {
    if(disp_required[["THALF"]]) {
      display_parameters <- c(display_parameters, "THALF")
    }
###    if("THALFF" %in% display_list) {
    if(disp_required[["THALFF"]]) {
      display_parameters <- c(display_parameters, "THALFF")
    }
###    if("AUCDN" %in% display_list) {
### 2019-010-10/TGT/ Removed AUCDN
###    if(disp_required[["AUCDN"]]) {
###      display_parameters <- c(display_parameters, "AUCDN")
###    }
###    if("AURCALL" %in% display_list) {
    if(disp_required[["AURCALL"]]) {
      display_parameters <- c(display_parameters, "AURCALL")
    }
###    if("AURCLAST" %in% display_list) {
    if(disp_required[["AURCLAST"]]) {
      display_parameters <- c(display_parameters, "AURCLAST")
    }
###    if("AURCT1_T2" %in% display_list) {
    if(disp_required[["AURCT1_T2"]] && auc_pair_check) {
      display_parameters <- c(display_parameters, rep(paste0("AURC",1:mid_len)), rep(paste0("AURCINT",1:mid_len)))
      if(auc_pair_check){
        display_parameters <- c(display_parameters, rep(paste0("AURC", map_data[,rep(paste0("AUC.", 1:aurc_par_len, ".T1"))], "_", map_data[,rep(paste0("AUC.", 1:aurc_par_len, ".T2"))])))
      }
    }
###    if("AURCINFO" %in% display_list) {
    if(disp_required[["AURCINFO"]]) {
      display_parameters <- c(display_parameters, "AURCINFO")
    }
###    if("AURCINFP" %in% display_list) {
    if(disp_required[["AURCINFP"]]) {
      display_parameters <- c(display_parameters, "AURCINFP")
    }
###    if("AURCXPCTO" %in% display_list){
    if(disp_required[["AURCXPCTO"]]){
      display_parameters <- c(display_parameters, "AURCXPCTO")
    }
###    if("AURCXPCTP" %in% display_list){
    if(disp_required[["AURCXPCTP"]]){
      display_parameters <- c(display_parameters, "AURCXPCTP")
    }
### 2019-09-23/TGT/ Add VOLSUM
    if(disp_required[["VOLSUM"]]) {
      display_parameters <- c(display_parameters, "VOLSUM")
    }
###    if("DIi" %in% display_list) {
    if(disp_required[["DIi"]]) {
      display_parameters <- c(display_parameters, rep(paste0("DI",1:di_col)))
    }
###    if("TAUi" %in% display_list) {
    if(disp_required[["TAUi"]]) {
      display_parameters <- c(display_parameters, rep(paste0("TAU",1:di_col)))
    }
###    if("TOLD" %in% display_list) {
    if(disp_required[["TOLD"]]) {
      display_parameters <- c(display_parameters, rep(paste0("TOLD",1:di_col)))
    }
###    if("DOSEi" %in% display_list) {
    if(disp_required[["DOSEi"]]) {
      display_parameters <- c(display_parameters, rep(paste0("DOSE",1:di_col)))
    }

###    computation_df <- computation_df[names(computation_df) %in% display_parameters]
### 2019-08-06/TGT/ Add UNITS back in ...
###                 UNITS are removed since they may not be pre-specified in the
###                 PARAMETERSLIST. However the unit_conversion routine determines the
###                 appropriate list of units. Rather than simply ignoring these,
###                 the following code determines non-NA/missing units and restores
###                 them to the list of output variables.
###    computation_df <- computation_df[names(computation_df) %in% display_parameters]
    # Find unit value names in computation_df
    ku <- grep("U$", names(computation_df), ignore.case=TRUE, perl=TRUE)
    ku <- names(computation_df)[ku]
### Note need to pop off TAU if it gets added - have to double check this though
    if(length(grep("TAUU", ku, ignore.case=TRUE, perl=TRUE))>0) { ku <- ku[-match("TAUU", ku)] }
    k <- c(names(computation_df), ku)

### remove duplicate entries
    k <- k[!duplicated(k)]

### Remove entries that are not defined, i.e. NA values, in computation_df
    x <- computation_df[,k]
    y <- lapply(x, FUN=function(x) { any(is.na(x)) })
    j <- names(y[y==TRUE])
    x <- x[,-match(j, names(x))]
    # names of x are the entries to add back to computation_df
###    k <- c(names(computation_df)[names(computation_df) %in% display_parameters], names(x))
###    computation_df <- computation_df[,k]
  }

  if(is.list(return_list) && !is.null(return_list) && length(return_list) > 0){
    if(!map_data$SDEID %in% return_list && length(return_list) > 1){
      return_list <- return_list[[length(return_list)+1]] <- map_data$SDEID
    }
    return_df <- unique(data_data[names(data_data) %in% return_list])

### 2019-09-19/TGT/ Except for SDEID, remove overlapping elements from return_list
### retaining those in parameter dataset
    ck <- intersect(names(return_df), names(computation_df))
    if(length(ck)>1) {
      ck <- setdiff(ck, map_data$SDEID)
      return_df <- return_df[,-match(ck, names(return_df))]
    }

###    merged_computation <- merge(x = computation_df, y = return_df, by.x = map_data$SDEID, by.y = map_data$SDEID)
    merged_computation <- merge(x = computation_df, y = return_df, by = map_data$SDEID)
    colnames(merged_computation) <- gsub('.x','.dataset',names(merged_computation))
    colnames(merged_computation) <- gsub('.y','',names(merged_computation))
    computation_df <- merged_computation
  }
###    print(head(computation_df))

### 2019-09-17/TGT/ Always return est_data and full results_list
  results_list <- list()
  results_list$data_out <- computation_df
  results_list$est_data <- est_data
  
  if(isTRUE(optimize_kel) && comp_required[["TMAXi"]] && comp_required[["TLASTi"]] && comp_required[["CMAXi"]] && comp_required[["CLASTi"]] && comp_required[["AUCLASTi"]] &&
     "FLGACCEPTKELCRIT" %in% names(map_data) && "FLGEXKEL" %in% names(map_data) && map_data$FLGEXKEL %in% names(data_data)){
    results_list$optimized_kel_flag <- kel_flag_optimized
  }
  
###  if("KEL" %in% parameter_list){
###    results_list <- list()
###    results_list$data_out <- computation_df
###    results_list$est_data <- est_data
###    return(results_list)
###  } else {
###    return(computation_df)
###  }

  return(results_list)
}
