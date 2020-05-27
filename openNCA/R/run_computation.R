#' Run Computation
#'
#' This function will compute all the relevant parameters for based specified fields provided via map argument.\cr
#'
#' @param data The dataframe that contains the raw data
#' @param map The dataframe that contains the map data
#' @param flag The dataframe that contains the flag data
#' @param parameterset set to either "PARAMETERLIST" or "PARAMETERDISPLAYLIST"
#'
#' @section Returns:
#' \strong{List} \cr
#' \itemize{
#'  \item data_out: Calculated default/specified M1SS Parameters
#'  \item flag_data: Flag Data used during calculation of parameters
#'  \item est_data: Calculated Estimated Parameters
#' }
#'
#' @examples
#'
#' @author
#' \itemize{
#'  \item \strong{Thomas Tensfeldt & Rudraya Technical Team}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{Thomas.G.Tensfeldt@pfizer.com}
#' }
#' @export
run_computation <- function(data = NULL, map = NULL, flag = NULL, parameterset = NULL, return_merged_data_in = FALSE, return_partitioned_data_in = FALSE, merged_data_in_debug = FALSE, partitioned_data_in_debug = FALSE, raw_results_debug = FALSE, optimize_kel_debug = FALSE){
  function_name <- as.list(sys.call())[[1]]

  if(is.null(data)){
    stop("Please provide a valid path for the 'data' parameter")
  } else {
    if(is.data.frame(data)){
      data_data <- data
    } else {
      if(file.exists(data)){
        data_data <- read.csv(file = data, stringsAsFactors = FALSE)
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
  if(is.null(flag)){
    stop("Please provide a valid path for the 'flag' parameter")
  } else {
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

  if(!is.null(parameterset)){
    if(grep("(PARAMETERLIST|PARAMETERDISPLAYLIST)", parameterset, ignore.case = TRUE, perl=TRUE)!=1) {
      stop("Invalid value provided for PARAMETERSET argument. This needs to be either 'PARAMETERLIST' or ''")
    }
  }
  
  if("MODEL" %in% names(map_data)){
    if(!(any(toupper(map_data$MODEL) %in% c("M1", "M2", "M3", "M4")))){
      if(identical(toupper(map_data$MODEL), "")){
        stop("'MODEL' value provided via 'map' is empty! Please provide a valid 'MODEL' parameter (M1, M2, M3, M4)!")
      } else {
        stop("'MODEL' value provided via 'map' is not valid! Please provide a valid 'MODEL' parameter (M1, M2, M3, M4)!")
      }
    }
  } else {
    stop("Dataset provided via 'map' does not contain the 'MODEL' column")
  }

  map_data <- update_mct_data(map_data, data_data, flag_data, verbose=FALSE)

  k <- grep("DOSE", names(map_data), ignore.case=TRUE, perl=TRUE)

  if(!("SDEID" %in% names(map_data))){
    stop("Dataset provided via 'map' does not contain the 'SDEID' column")
  } else {
    if(!(map_data$SDEID %in% names(data_data))){
      stop("Value for 'SDEID' provided via 'map' is not present in the dataset provided via 'data'")
    }
  }

  if(!("FLGMERGE" %in% names(map_data))){
    stop("Dataset provided via 'map' does not contain the 'FLGMERGE' column")
  } else {
    if(!(map_data$FLGMERGE %in% names(flag_data))){
      stop("'FLGMERGE' value provided via 'map' is not present in 'flag' dataset")
    }
  }
  if("OPTIMIZEKEL" %in% names(map_data)){
    if(!(is.na(map_data[,"OPTIMIZEKEL"]))){
      if(map_data[,"OPTIMIZEKEL"] != 1 && map_data[,"OPTIMIZEKEL"] != 0){
        warning("Map 'OPTMIZEKEL' does not have a valid value! Not using KEL optmization for this computation")
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
  
  default_maxdosingintervals <- 5
  if(parameter_required("MAXDOSINGINTERVALS", names(map_data))) {
    maxdosingintervals <- ifelse(isTRUE(!is.null(map_data[["MAXDOSINGINTERVALS"]]) && !is.na(map_data[["MAXDOSINGINTERVALS"]])), map_data[["MAXDOSINGINTERVALS"]], default_maxdosingintervals)
  } else if(parameter_required("DOSELIST", names(map_data))) { # read # of doses from MCT/map DOSELIST value if present
    maxdosingintervals <- ifelse(isTRUE(!is.null(map_data$DOSELIST) && !is.na(map_data$DOSELIST)), length(unlist(strsplit(map_data$DOSELIST, ";"))), default_maxdosingintervals)
  } else { maxdosingintervals <- default_maxdosingintervals } # default to 5

  merged_data <- merge(x = data_data, y = flag_data, by = map_data$FLGMERGE)
  valid_tau_told_check <- TRUE
  valid_tau_told_names <- c()
  all_tau_told_names <- c()
  for(d in 1:maxdosingintervals){
    check_told <- grep(paste0("^TOLD",d,"$"), names(flag_data), ignore.case=TRUE, perl=TRUE)
    check_tau <- grep(paste0("^TAU",d,"$"), names(flag_data), ignore.case=TRUE, perl=TRUE)
    if(length(check_told) > 0 && length(check_tau) > 0){
      valid_tau_told_names <- c(valid_tau_told_names, names(flag_data[c(check_told, check_tau)]))
      all_tau_told_names <- c(all_tau_told_names, names(flag_data[c(check_told, check_tau)]))
    } else {
      if(length(check_told) == 0 && length(check_tau) == 0){
        if(d == 1){
          valid_tau_told_check <- FALSE
          warning("'TAUs and TOLDs values are not provided via 'flag'! Using values provided via 'data' instead (if present)!")
          break
        }
      } else {
        if(length(check_told) > 0){
          all_tau_told_names <- c(all_tau_told_names, names(flag_data[c(check_told)]))
        }
        if(length(check_tau) > 0){
          all_tau_told_names <- c(all_tau_told_names, names(flag_data[c(check_tau)]))
        }
        valid_tau_told_check <- FALSE
        warning("'TAUs and TOLDs values are provided via 'flag' are not in a valid format! Using values provided via 'data' instead!")
        break
      }
    }
  }
  if(isTRUE(valid_tau_told_check)){
    merged_data <- merged_data[,!(names(merged_data) %in% paste0(valid_tau_told_names, ".x"))]
    map_data[,valid_tau_told_names] <- valid_tau_told_names
  } else if(!isTRUE(valid_tau_told_check) && length(all_tau_told_names) > 0){
    merged_data <- merged_data[,!(names(merged_data) %in% paste0(all_tau_told_names, ".y"))]
    if(length(names(merged_data)[names(merged_data) %in% paste0(all_tau_told_names, ".x")]) > 0){
      names(merged_data)[names(merged_data) %in% paste0(all_tau_told_names, ".x")] <- gsub('.x', '', names(merged_data)[names(merged_data) %in% paste0(all_tau_told_names, ".x")])
    }
  }
  colnames(merged_data) <- gsub('.x','.dataset',names(merged_data))
  colnames(merged_data) <- gsub('.y','',names(merged_data))
  merged_data[,map_data$TIME] <- as.numeric(merged_data[,map_data$TIME])
  
  only_merged_data <- merged_data
  if(isTRUE(return_merged_data_in) && !isTRUE(return_partitioned_data_in)){
    cat("No computation performed! Returning merged input data (based on 'data' and 'flag') as specified!", "\n")
    return(only_merged_data)
  }
  
  if(parameter_required("^FLGTIME$", names(map_data))) {
    if(parameter_required(map_data$FLGTIME, names(merged_data))) {
      if(parameter_required("^ALTTIME$", names(map_data))) {
        if(parameter_required(map_data$ALTTIME, names(merged_data))) {
          if(!isTRUE(any(is.na(as.logical(merged_data[,map_data$FLGTIME]))))){
            alt_time <- merged_data[,map_data$ALTTIME][as.logical(merged_data[,map_data$FLGTIME])]
          } else {
            alt_time <- c()
          }
          if(length(alt_time) > 0){
            merged_data[,map_data$TIME][as.logical(merged_data[,map_data$FLGTIME])] <- alt_time
          }
        }
      }
    }
  }

  k <- grep("^(TAU)|(TOLD)(i)*?([0-9]*?)$", names(merged_data), ignore.case=TRUE, perl=TRUE)

  if("DOSINGTYPE" %in% names(map_data)){
    if(!(toupper(map_data$DOSINGTYPE) %in% c("SD", "SS"))){
      if(identical(toupper(map_data$DOSINGTYPE), "")){
        stop("'DOSINGTYPE' value provided via 'map' is empty! Please provide a valid 'DOSINGTYPE' parameter (SD, SS)!")
      } else {
        stop("'DOSINGTYPE' value provided via 'map' is not valid! Please provide a valid 'DOSINGTYPE' parameter (SD, SS)!")
      }
    } else {
      if(parameter_required("^IMPUTEDOSES$", names(map_data))) {
        imputeddoses <- strsplit(map_data$IMPUTEDOSES, ";")
        #print(imputeddoses)
        for(id in imputeddoses) {
          map_data[,id] <- id
          merged_data[,id] <- rep(1.0, nrow(merged_data))
        }
      }
      if(parameter_required("^IMPUTETAUS$", names(map_data)) && !isTRUE(valid_tau_told_check)) {
        imputedtaus <- strsplit(map_data$IMPUTETAUS, ";")
        #print(imputedtaus)
        if("TAU1" %in% imputedtaus){
          #print("TAU present")
          for(i in 1:length(unique(merged_data[,map_data$SDEID]))){
            tmp_logic <- merged_data[,map_data$SDEID] == unique(merged_data[,map_data$SDEID])[i]
            tmp_df <- merged_data[tmp_logic,]
            tmp_df[,map_data$TIME] <- as.numeric(tmp_df[,map_data$TIME])
            tmp_df <- tmp_df[order(tmp_df[,map_data$TIME]),]
            tmptold <- tmp_df[1,map_data$TIME]
            tmptau <- tmp_df[nrow(tmp_df),map_data$TIME]
            tmptau <- tmptau - tmptold
            #print(tmptau)
            map_data[,"TAU1"] <- "TAU1"
            merged_data[tmp_logic,"TAU1"] <- rep(tmptau, nrow(tmp_df))
          }
        } else {
          for(id in imputedtaus) {
            map_data[,id] <- id
            merged_data[,id] <- rep(NA, nrow(merged_data))
          }
        }
      }
      if(parameter_required("^IMPUTETOLDS$", names(map_data)) && !isTRUE(valid_tau_told_check)) {
        imputedtolds <- strsplit(map_data$IMPUTETOLDS, ";")
        #print(imputedtolds)
        if("TOLD1" %in% imputedtolds){
          #print("TOLD present")
          for(i in 1:length(unique(merged_data[,map_data$SDEID]))){
            tmp_logic <- merged_data[,map_data$SDEID] == unique(merged_data[,map_data$SDEID])[i]
            tmp_df <- merged_data[tmp_logic,]
            tmp_df[,map_data$TIME] <- as.numeric(tmp_df[,map_data$TIME])
            tmp_df <- tmp_df[order(tmp_df[,map_data$TIME]),]
            tmptold <- tmp_df[1,map_data$TIME]
            #print(tmptold)
            map_data[,"TOLD1"] <- "TOLD1"
            merged_data[tmp_logic,"TOLD1"] <- rep(tmptold, nrow(tmp_df))
          }
        } else {
          for(id in imputedtolds) {
            map_data[,id] <- id
            merged_data[,id] <- rep(NA, nrow(merged_data))
          }
        }
      }
      merged_data <- create_dosing_intervals(merged_data, map_data, flag_data, maxdosingintervals)
    }
  } else {
    stop("Dataset provided via 'map' does not contain the 'DOSINGTYPE' column")
  }

  if("AUCMETHOD" %in% names(map_data)){
    if(toupper(map_data$AUCMETHOD) %in% c("LINLOG", "LIN", "LOG", "LINUP-LOGDOWN")){
      if(toupper(map_data$AUCMETHOD) == "LINLOG"){
        method <- 1
      } else if(toupper(map_data$AUCMETHOD) == "LIN"){
        method <- 2
      } else if(toupper(map_data$AUCMETHOD) == "LOG"){
        method <- 3
      } else if(toupper(map_data$AUCMETHOD) == "LINUP-LOGDOWN"){
        method <- 4
      }
    } else {
      stop("'AUCMETHOD' value provided via 'map' is not valid! Please provide a valid 'AUCMETHOD' parameter (LINLOG LIN LOG LINUP-LOGDOWN)!")
    }
  } else {
    stop("Dataset provided via 'map' does not contain the 'AUCMETHOD' column")
  }
  
  dependency_list <- list()

  model_regex <- paste0("^", map_data$MODEL, "(", map_data$DOSINGTYPE, ")*?$")

  parameter_list <- list()
  if(is.null(map_data$PARAMETERLIST) || is.na(map_data$PARAMETERLIST)) { parameter_list <- model_parameters(model_regex) }
  else { parameter_list <- unlist(strsplit(map_data$PARAMETERLIST, ";")) }
  
  if(parameterset=="PARAMETERLIST"){
    if("RETURNCOLS" %in% names(map_data)){
      if(!is.null(map_data$RETURNCOLS) && !is.na(map_data$RETURNCOLS) && map_data$RETURNCOLS != ""){
        return_list <- as.list(strsplit(map_data$RETURNCOLS, ";")[[1]])
      } else {
        return_list <- list()
        warning("'RETURNCOLS' values provided via 'map' are not used for this computation")
      }
    } else {
      return_list <- list()
      warning("Dataset provided via 'map' does not contain the 'RETURNCOLS' column")
    }
  }
  
  if(parameterset=="PARAMETERDISPLAYLIST") { 
    if("DATADISPLAYLIST" %in% names(map_data)){
      if(!is.null(map_data$DATADISPLAY) && !is.na(map_data$DATADISPLAYLIST) && map_data$DATADISPLAYLIST != ""){
        return_list <- as.list(strsplit(map_data$DATADISPLAYLIST, ";")[[1]])
      } else {
        return_list <- list()
        warning("'DATADISPLAYLIST' values provided via 'map' are not used for this computation")
      }
    } else {
      return_list <- list()
      warning("Dataset provided via 'map' does not contain the 'DATADISPLAYLIST' column")
    }
  }
 
  if(paste0(map_data$TIME, "U") %in% names(map_data)){
    if(map_data[, paste0(map_data$TIME, "U")] %in% names(merged_data)){
      if(length(na.omit(unique(data_data[, map_data[, paste0(map_data$TIME, "U")]]))[1]) > 1){
        warning(paste0("'", map_data$TIME, "U"), "' value provided via 'map' has unit inconsistency in the the data provided via 'data'")
      }
    }
  }
  if("CONCU" %in% names(map_data)){
    if(map_data$CONCU %in% names(merged_data)){
      if(length(na.omit(unique(data_data[, map_data$CONCU]))[1]) > 1){
        warning("'CONCU' value provided via 'map' has unit inconsistency in the the data provided via 'data'")
      }
    }
  }
  if("AMOUNTU" %in% names(map_data)){
    if(map_data$AMOUNTU %in% names(merged_data)){
      if(length(na.omit(unique(data_data[, map_data$AMOUNTU]))[1]) > 1){
        warning("'AMOUNTU' value provided via 'map' has unit inconsistency in the the data provided via 'data'")
      }
    }
  }
  if("DOSEU" %in% names(map_data)){
    if(map_data$DOSEU %in% names(merged_data)){
      if(length(na.omit(unique(data_data[, map_data$DOSEU]))[1]) > 1){
        warning("'DOSEU' value provided via 'map' has unit inconsistency in the the data provided via 'data'")
      }
    }
  }
  if("TOLDU" %in% names(map_data)){
    if(map_data$TOLDU %in% names(merged_data)){
      if(length(na.omit(unique(data_data[, map_data$TOLDU]))[1]) > 1){
        warning("'TOLDU' value provided via 'map' has unit inconsistency in the the data provided via 'data'")
      }
    }
  }
  if("TAUU" %in% names(map_data)){
    if(map_data$TAUU %in% names(merged_data)){
      if(length(na.omit(unique(data_data[, map_data$TAUU]))[1]) > 1){
        warning("'TAUU' value provided via 'map' has unit inconsistency in the the data provided via 'data'")
      }
    }
  }

  data_out <- NULL
  merged_data <- merged_data[order(merged_data[,map_data[[map_data$SDEID]]]),]
  
  if(isTRUE(return_partitioned_data_in) && !isTRUE(return_merged_data_in)){
    cat("No computation performed! Returning partitioned merged input data (based on 'data' and 'flag') as specified!", "\n")
    return(merged_data)
  }
  if(isTRUE(return_partitioned_data_in) && isTRUE(return_merged_data_in)){
    cat("No computation performed! Returning partitioned and merged input data (based on 'data' and 'flag') as specified!", "\n")
    results_list <- list()
    results_list$merged_data_in <- only_merged_data
    results_list$partitioned_data_in <- merged_data
    return(results_list)
  }
  if(is.null(map_data$PARAMETERDISPLAYLIST) || is.na(map_data$PARAMETERDISPLAYLIST)) { display_list <- model_parameters(model_regex) }
  else { display_list <- as.list(strsplit(map_data$PARAMETERDISPLAYLIST, ";")[[1]]) }
  
  if(parameterset=="PARAMETERDISPLAYLIST") { 
      parameter_list <- display_list
  }

  parameter_list <- as.list(parameter_list)
  
  if(toupper(map_data$MODEL) == 'M1' && toupper(map_data$DOSINGTYPE) == 'SD'){
    data_out <- run_M1_SD_computation(data = merged_data, map = map_data, method = method, model_regex = model_regex,
                                      parameter_list = parameter_list, return_list = return_list, raw_results_debug = raw_results_debug,
                                      optimize_kel_debug = optimize_kel_debug)
  } else if(toupper(map_data$MODEL) == 'M1' && toupper(map_data$DOSINGTYPE) == 'SS'){
    data_out <- run_M1_SS_computation(data = merged_data, map = map_data, method = method, model_regex = model_regex,
                                      parameter_list = parameter_list, return_list = return_list, raw_results_debug = raw_results_debug,
                                      optimize_kel_debug = optimize_kel_debug)
  } else if (toupper(map_data$MODEL) == 'M2' && toupper(map_data$DOSINGTYPE) == 'SD'){
    data_out <- run_M2_SD_computation(data = merged_data, map = map_data, method = method, model_regex = model_regex,
                                      parameter_list = parameter_list, return_list = return_list, raw_results_debug = raw_results_debug,
                                      optimize_kel_debug = optimize_kel_debug)
  } else if (toupper(map_data$MODEL) == 'M2' && toupper(map_data$DOSINGTYPE) == 'SS'){
    data_out <- run_M2_SS_computation(data = merged_data, map = map_data, method = method, model_regex = model_regex,
                                      parameter_list = parameter_list, return_list = return_list, raw_results_debug = raw_results_debug,
                                      optimize_kel_debug = optimize_kel_debug)
  } else if (toupper(map_data$MODEL) == 'M3' && toupper(map_data$DOSINGTYPE) == 'SD'){
    data_out <- run_M3_SD_computation(data = merged_data, map = map_data, method = method, model_regex = model_regex,
                                      parameter_list = parameter_list, return_list = return_list, raw_results_debug = raw_results_debug,
                                      optimize_kel_debug = optimize_kel_debug)
  } else if(toupper(map_data$MODEL) == 'M3' && toupper(map_data$DOSINGTYPE) == 'SS'){
    data_out <- run_M3_SS_computation(data = merged_data, map = map_data, method = method, model_regex = model_regex,
                                      parameter_list = parameter_list, return_list = return_list, raw_results_debug = raw_results_debug,
                                      optimize_kel_debug = optimize_kel_debug)
  } else if (toupper(map_data$MODEL) == 'M4' && toupper(map_data$DOSINGTYPE) == 'SD'){
    data_out <- run_M4_SD_computation(data = merged_data, map = map_data, method = method, model_regex = model_regex,
                                      parameter_list = parameter_list, return_list = return_list, raw_results_debug = raw_results_debug,
                                      optimize_kel_debug = optimize_kel_debug)
  } else if(toupper(map_data$MODEL) == 'M4' && toupper(map_data$DOSINGTYPE) == 'SS'){
    data_out <- run_M4_SS_computation(data = merged_data, map = map_data, method = method, model_regex = model_regex,
                                      parameter_list = parameter_list, return_list = return_list, raw_results_debug = raw_results_debug,
                                      optimize_kel_debug = optimize_kel_debug)
  }
  if(parameterset=="PARAMETERDISPLAYLIST") { 
      k <- grep("^CONC((TIME)*?)[0-9]*?$", names(data_out$data_out), ignore.case=TRUE, perl=TRUE)
      if(length(k)>0) {
        k <- names(data_out$data_out)[k]
        data_out$data_out <- data_out$data_out[,-match(k, names(data_out$data_out))]
      }
  }
  
  results_list <- list()
  if(optimize_kel){
    pkdata_order <- as.numeric(row.names(merged_data[order(merged_data[map_data$SDEID]),]))
    if("KEL" %in% parameter_list){
      results_list$data_out <- data_out$data_out
      results_list$flag_data <- flag_data[1:nrow(merged_data),]
      if(!is.null(data_out$optimized_kel_flag)){
        results_list$flag_data[pkdata_order,map_data$FLGEXKEL] <- data_out$optimized_kel_flag
      }
      results_list$est_data <- data_out$est_data
    } else {
      results_list$data_out <- data_out$data_out
      results_list$flag_data <- flag_data[1:nrow(merged_data),]
      if(!is.null(data_out$optimized_kel_flag)){
        results_list$flag_data[pkdata_order,map_data$FLGEXKEL] <- data_out$optimized_kel_flag
      }
      if(!all(unlist(lapply(data_out$est_data, length)) == 0)){
        results_list$est_data <- data_out$est_data 
      }
    }
  } else {
    if("KEL" %in% parameter_list){
      results_list$data_out <- data_out$data_out
      results_list$flag_data <- flag_data[1:nrow(merged_data),]
      results_list$est_data <- data_out$est_data
    } else {
      results_list$data_out <- data_out$data_out
      results_list$flag_data <- flag_data[1:nrow(merged_data),]
      if(!all(unlist(lapply(data_out$est_data, length)) == 0)){
        results_list$est_data <- data_out$est_data 
      }
    }
  }
  if(isTRUE(raw_results_debug)){
    results_list$raw_results <- data_out$raw_results
  }
  if(isTRUE(optimize_kel_debug)){
    results_list$optimize_kel_data <- data_out$optimize_kel_data
  }
  if(isTRUE(merged_data_in_debug)){
    results_list$merged_data_in <- only_merged_data
  }
  if(isTRUE(partitioned_data_in_debug)){
    results_list$partitioned_data_in <- merged_data
  }
  results_list$data_out <- lapply(results_list$data_out, function(x) {
      if(is.factor(x)) {
        x <- as.character(x)
      }
      return(x)
    }
  )
  results_list$data_out <- as.data.frame(results_list$data_out, stringsAsFactors=FALSE)
  
  elist <- c(map_data$FLGMERGE, map_data$SDEID,"TIME","CEST_KEL","CEST_INT","CEST_EXT","CEST_C0","CEST_TLAST")
  est_data <- flag_data[flag_data$FLGEXKEL==0, map_data$FLGMERGE, drop=FALSE] # Select rows to include in KEL computations to start with

  # Merge with concentration data
  vlist <- c(map_data$FLGMERGE, map_data$SDEID, map_data$TIME)
  est_data <- merge(x=est_data, y=data_data[,vlist], by=map_data$FLGMERGE, all.x=TRUE)
  # Rename selected TIME to "TIME"
  names(est_data)[match(map_data$TIME, names(est_data))] <- "TIME"
  # Add Placeholders for CEST_KEL, CEST_INT, CEST_EXT, CEST_C0, CEST_TLAST
  est_data$CEST_KEL <- rep(NA, nrow(est_data))
  est_data$CEST_INT <- rep(NA, nrow(est_data))
  est_data$CEST_EXT <- rep(NA, nrow(est_data))
  est_data$CEST_C0 <- rep(NA, nrow(est_data))
  est_data$CEST_TLAST <- rep(NA, nrow(est_data))
  # Reorder names
  names(est_data) <- elist

  # Add est_data to results_list
  if(is.null(results_list$est_data)){
    results_list$est_data <- est_data
  }
  return(results_list)
}


