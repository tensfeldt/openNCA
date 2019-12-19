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
run_computation <- function(data = NULL, map = NULL, flag = NULL, parameterset = NULL){
  function_name <- as.list(sys.call())[[1]]

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
### 2019-08-15/TGT/ Added following section to cover parameterset parameter/argument validation
  if(!is.null(parameterset)){
    if(grep("(PARAMETERLIST|PARAMETERDISPLAYLIST)", parameterset, ignore.case = TRUE, perl=TRUE)!=1) {
      stop("Invalid value provided for PARAMETERSET argument. This needs to be either 'PARAMETERLIST' or 'PARAMETERDISPLAYLIST'")
    }
  }

### 2019-08-23/TGT/ 
### THE FOLLOWING WILL NOT WORK IF TIME / TIMEU POINT TO DATA ITEMS DIRECTLY RATHER THAN ACT AS POINTERS TO THE NOMTIME/ACTTIME setting values
### THIS MUST BE REMEDIATED SINCE IT PREVENTS UNITS CONVERSION FROM WORKING RELIABLY
### 2019-09-03/the previous issue was addressed

### 2019-08-26/TGT/ added standardized "validate_timeconc_data" routine to resolve issue
###                 that was not handled if TIME is set to a value in the input dataset directly
### 2019-09-11/TGT/ Updated to update_mct_data directly to reduce code and stage FLGTIME updates of data
  map_data <- update_mct_data(map_data, data_data, flag_data, verbose=FALSE)

  k <- grep("DOSE", names(map_data), ignore.case=TRUE, perl=TRUE)

### 2019-09-26/TGT/ added standardized "validate_timeconc_data" routine to resolve issue
###                 that was not handled if TIME is set to a value in the input dataset directly
###  timeconcvalues <- validate_timeconc_data(map_data, data_data, verbose=FALSE)
###  print(timeconcvalues)
### timeconcvalues should all be column names/fields in data_data at this point, rather than pointers to other values in map_data

### 2019-09-11/TGT/ Maintain original values of TIME/TIMEU  
###  map_data$ORGTIME  <- map_data$TIME
### 2019-011/TGT/ Note if TIMEU doesn't exist in map_data, ORGTIMEU will not be created
###  map_data$ORGTIMEU <- map_data$TIMEU
  
### Update TIME/TIMEU with validated times/units
###  map_data$TIME     <- timeconcvalues$time
###  map_data$TIMEU    <- timeconcvalues$timeu
  
### Create Alternative TIME values if specified
###  if(parameter_required("^alttime$",  names(timeconcvalues))) { map_data$ALTTIME  <- timeconcvalues$alttime }
###  if(parameter_required("^alttimeu$", names(timeconcvalues))) { map_data$ALTTIMEU <- timeconcvalues$alttimeu }

### 2019-09-11/TGT/ Maintain original values of CONC/CONCU
###  map_data$ORGCONC  <- map_data$CONC
###  map_data$ORGCONCU <- map_data$CONCU

### Update CONC/CONCU with validated concs/units
###  map_data$CONC     <- timeconcvalues$conc
###  map_data$CONCU    <- timeconcvalues$concu

### For Interval Data set ENDTIME values/units
###  if(casefold(map_data$MODEL)=="m4") {
###      map_data$ENDTIME  <- timeconcvalues$endtime
###      map_data$ENDTIMEU <- timeconcvalues$endtimeu
###  }
  
### 2019-09-11/TGT/ Add FLGTIME column name in data_data to map_data to carry into Model method subroutines
###  flgtime <- "FLGTIME"
###  if(length(parameter_indices("^FLGTIME$", flag_data))==1) { flgtime <- flag_data[,parameter_indices("^FLGTIME$", flag_data)] }

  if(!(parameter_required("^SDEID$",names(data_data)))) {
      stop("Value for 'SDEID' provided via 'map' is not present in the dataset provided via 'data'")
  }

###  cat('map_data$TIME: ', map_data$TIME, ' map_data$TIMEU: ', map_data$TIMEU, '\n')
###  if(casefold(map_data$MODEL=="m4")) { cat('map_data$ENDTIME: ', map_data$ENDTIME, ' map_data$ENDTIMEU: ', map_data$ENDTIMEU, '\n') }
  
###  if(!("SDEID" %in% names(map_data) && "TIME" %in% names(map_data) && "CONC" %in% names(map_data))){
###    stop("Dataset provided via 'map' does not contain the required columns 'SDEID', 'TIME' and 'CONC'")
###  } else {
###    if(!(casefold(map_data$TIME) %in% c("nominal", "actual"))){
###      stop("'TIME' value provided via 'map' does not have valid value! Please provide either 'Nominal' or 'Actual'")
###    } else {
###      if(casefold(map_data$TIME) == 'nominal'){
###        map_data$TIME <- "NOMTIME"
###        map_data$TIMEU <- "NOMTIMEU"
###        if(toupper(map_data$MODEL) == 'M4'){
###          map_data$ENDTIME <- "NOMENDTIME"
###          map_data$ENDTIMEU <- "NOMENDTIMEU"
###        }
###      } else if(casefold(map_data$TIME) == 'actual'){
###        map_data$TIME <- "ACTTIME"
###        map_data$TIMEU <- "ACTTIMEU"
###        if(toupper(map_data$MODEL) == 'M4'){
###          map_data$ENDTIME <- "ACTENDTIME"
###          map_data$ENDTIMEU <- "ACTENDTIMEU"
###        }
###      }
###      if(!(map_data$TIME %in% names(map_data))){
###        stop("'TIME' value provided via 'map' is not present in 'map' dataset")
###      }
###    }
###  }
###  if(!(map_data$SDEID %in% names(data_data) && map_data[[map_data$TIME]] %in% names(data_data) && map_data$CONC %in% names(data_data))){
###    stop("Values for 'SDEID', 'TIME' and 'CONC' provided via 'map' are not present in the dataset provided via 'data'")
###  }
  
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
  
  merged_data <- merge(x = data_data, y = flag_data, by = map_data$FLGMERGE)
  colnames(merged_data) <- gsub('.x','.dataset',names(merged_data))
### 2019-09-09/TGT/ accept values of TAU/TOLD from flags
  colnames(merged_data) <- gsub('.y','',names(merged_data))
### 2019-08-27/TGT/ replace indirection of map_data[[map_data$TIME]] with map_data$TIME
  merged_data[,map_data$TIME] <- as.numeric(merged_data[,map_data$TIME])

### 2019-09-11/TGT/ Swap TIME with ALTTIME (and units) if FLGTIME available
  if(parameter_required("^FLGTIME$", names(map_data))) {
    if(parameter_required(map_data$FLGTIME, names(merged_data))) {
      merged_data[,map_data$TIME] <- ifelse(merged_data[,map_data$FLGTIME]!=map_data$ORGTIME, merged_data[,map_data$ALTTIME], merged_data[,map_data$TIME])
    }
  }

### 2019-09-09/TGT/ determine TAU/TOLD from FLAGS
  k <- grep("^(TAU)|(TOLD)(i)*?([0-9]*?)$", names(merged_data), ignore.case=TRUE, perl=TRUE)
###  if(length(k)>0) { print(names(merged_data)[k]) }
  
### 2019-08-17/TGT/ add maximum, configurable # of dosing intervals to the Model Configuration Template (MCT) Definition
  if(parameter_required("MAXDOSINGINTERVALS", names(map_data))) {
      maxdosingintervals <- map_data[["MAXDOSINGINTERVALS"]]
  } else if(parameter_required("DOSELIST", names(map_data))) { # read # of doses from MCT/map DOSELIST value if present
      maxdosingintervals <- length(unlist(strsplit(map_data$DOSELIST, ";")))
  } else { maxdosingintervals <- 5 } # default to 5
  
  if("MODEL" %in% names(map_data)){
    if(!(toupper(map_data$MODEL) %in% c("M1", "M2", "M3", "M4"))){
      if(identical(toupper(map_data$MODEL), "")){
        stop("'MODEL' value provided via 'map' is empty! Please provide a valid 'MODEL' parameter (M1, M2, M3, M4)!")
      } else {
        stop("'MODEL' value provided via 'map' is not valid! Please provide a valid 'MODEL' parameter (M1, M2, M3, M4)!")
      }
    }
  } else {
    stop("Dataset provided via 'map' does not contain the 'MODEL' column")
  }

### 2019-08-17/TGT/ Note that DOSE, TAU, TOLD values can appear in either the concentration DATASET or the FLAGS dataset
### if appear in the FLAGS dataset, these override what appears in the original input concentration dataset
### current behavior may not (hasn't been tested) be consistent with these requirements
### 2019-09-09/TGT/ Correction that DOSE cannot currently appear in FLAGS dataset
  
  if("DOSINGTYPE" %in% names(map_data)){
    if(!(toupper(map_data$DOSINGTYPE) %in% c("SD", "SS"))){
      if(identical(toupper(map_data$DOSINGTYPE), "")){
        stop("'DOSINGTYPE' value provided via 'map' is empty! Please provide a valid 'DOSINGTYPE' parameter (SD, SS)!")
      } else {
        stop("'DOSINGTYPE' value provided via 'map' is not valid! Please provide a valid 'DOSINGTYPE' parameter (SD, SS)!")
      }
    } else {
### 2019-08-17/TGT/ must create default UNIT DOSE==1 where no DOSE, DOSE1, DOSEi etc are available and provide notification that this is being done
### 2019-10-08/TGT/ the following is checked in validate_timeconc_data.R so is duplicated here and thus removed
###      if(all(c("DOSE1", "DOSE") %in% names(map_data))){
###        stop("Values provided  via 'map' contains duplicate required 'DOSE1' and 'DOSE' columns")
###      } else if(!("DOSE1" %in% names(map_data)) && !("DOSE" %in% names(map_data))){
###        stop("Dataset provided via 'map' does not contain the required 'DOSE1' column")
###      }
### 2019-10-16/TGT/ Remove - convert to DOSELIST from map
###      if("DOSE" %in% names(map_data)){
###        map_data[,'DOSE1'] <- map_data[,'DOSE']
###      }
###      if("DOSEU" %in% names(map_data)){
###        map_data[,'DOSE1U'] <- map_data[,'DOSEU']
###      }

### 2019-10-17/TGT/ Prepare IMPUTED Doses
      if(parameter_required("^IMPUTEDOSES$", names(map_data))) {
        imputeddoses <- strsplit(map_data$IMPUTEDOSES, ";")
        #print(imputeddoses)
        for(id in imputeddoses) {
          map_data[,id] <- id
          merged_data[,id] <- rep(1.0, nrow(merged_data))
        }
      }
      if(parameter_required("^IMPUTETAUS$", names(map_data))) {
        imputedtaus <- strsplit(map_data$IMPUTETAUS, ";")
        #print(imputedtaus)
        if("TAU1" %in% imputedtaus){
          #print("TAU present")
          for(i in 1:length(unique(merged_data[,map_data$SDEID]))){
            tmp_logic <- merged_data[,map_data$SDEID] == unique(merged_data[,map_data$SDEID])[i]
            tmp_df <- merged_data[tmp_logic,]
            tmp_df <- tmp_df[order(tmp_df[,map_data$TIME]),]
            tmp_df[,map_data$TIME] <- as.numeric(tmp_df[,map_data$TIME])
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
      if(parameter_required("^IMPUTETOLDS$", names(map_data))) {
        imputedtolds <- strsplit(map_data$IMPUTETOLDS, ";")
        #print(imputedtolds)
        if("TOLD1" %in% imputedtolds){
          #print("TOLD present")
          for(i in 1:length(unique(merged_data[,map_data$SDEID]))){
            tmp_logic <- merged_data[,map_data$SDEID] == unique(merged_data[,map_data$SDEID])[i]
            tmp_df <- merged_data[tmp_logic,]
            tmp_df <- tmp_df[order(tmp_df[,map_data$TIME]),]
            tmp_df[,map_data$TIME] <- as.numeric(tmp_df[,map_data$TIME])
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
        
      if(toupper(map_data$DOSINGTYPE) == 'SS'){
### 2019-08-17/TGT/ update coding to reflect maxdosingintervals setting
###        ss_dose <- c("DOSE1", "DOSE2", "DOSE3", "DOSE4", "DOSE5")
###        ss_tau <- c("TAU1", "TAU2","TAU3", "TAU4","TAU5")
###        ss_told <- c("TOLD1", "TOLD2", "TOLD3","TOLD4", "TOLD5")
        ss_dose <- paste0("DOSE", rep(1:maxdosingintervals))
        ss_tau  <- paste0("TAU" , rep(1:maxdosingintervals))
        ss_told <- paste0("TOLD", rep(1:maxdosingintervals))
          
        valid_dose <- NULL
        valid_tau <- NULL
        valid_told <- NULL
        if("DOSE" %in% names(map_data) || any(ss_dose %in% names(map_data) || ss_tau %in% names(map_data) || ss_told %in% names(map_data))){
          test_dose <- ifelse("DOSE" %in% names(map_data), !as.logical(is.na(map_data[,"DOSE"])), FALSE)
          if(test_dose || any(!as.logical(is.na(map_data[ss_dose[ss_dose %in% names(map_data)]]))) || any(!as.logical(is.na(map_data[ss_tau[ss_tau %in% names(map_data)]]))) || any(!as.logical(is.na(map_data[ss_told[ss_told %in% names(map_data)]])))){
            if(test_dose || any(!as.logical(is.na(map_data[ss_dose[ss_dose %in% names(map_data)]])))){
              tmp_dose <- map_data[ss_dose[ss_dose %in% names(map_data)]]
              if(ncol(tmp_dose) == 0){
                tmp_dose <- map_data['DOSE']
                map_data["DOSE1"] <- map_data['DOSE']
              }
              #print(tmp_dose)
              dose_logic <- unlist(lapply(tmp_dose, function(x) { return(is.na(x) || x == "")}))
              valid_dose <- names(tmp_dose[!as.logical(dose_logic)])
              #print(valid_dose)
            }
            if(any(!as.logical(is.na(map_data[ss_tau[ss_tau %in% names(map_data)]])))){
              tmp_tau <- map_data[ss_tau[ss_tau %in% names(map_data)]]
              #print(tmp_tau)
              tau_logic <- unlist(lapply(tmp_tau, function(x) { return(is.na(x) || x == "")}))
              valid_tau <- names(tmp_tau[!as.logical(tau_logic)])
              #print(valid_tau)
            }
            if(any(!as.logical(is.na(map_data[ss_told[ss_told %in% names(map_data)]])))){
              tmp_told <- map_data[ss_told[ss_told %in% names(map_data)]]
              #print(tmp_told)
              told_logic <- unlist(lapply(tmp_told, function(x) { return(is.na(x) || x == "")}))
              valid_told <- names(tmp_told[!as.logical(told_logic)])
              #print(valid_told)
            }
          } else {
            stop(paste("1 Unable to generate dosing interval for Steady State data! Values for", paste(ss_dose, collapse = ", "), "and", paste(ss_tau, collapse = ", "), "and", paste(ss_told, collapse = ", "), "provided via 'map' are not valid!"))
          }
###
###          cat("1: valid_tau: ", valid_tau, "valid_told: ", valid_told, "valid_dose: ", valid_dose, "\n")
###                print(head(merged_data))
###          cat("length(valid_dose) > 0 && length(valid_tau) > 0 && length(valid_told) > 0: ", length(valid_dose) > 0 && length(valid_tau) > 0 && length(valid_told) > 0, "\n")
          if(length(valid_dose) > 0 && length(valid_tau) > 0 && length(valid_told) > 0){
            if(!(length(valid_dose) == length(valid_tau) && length(valid_tau) == length(valid_told))){
              stop(paste("2 Unable to generate dosing interval for Steady State data! Please provide same number of interval values of data parameters, you provided:", paste(valid_dose, collapse = ", "), "and", paste(valid_tau, collapse = ", "), "and", paste(valid_told, collapse = ", "), "which are uneven!"))
            } else {
### 2019-08-13/TGT/ if there are the same number of values of valid_dose, valid_tau and valid_told then
###                  check whether any of these are missing from merged_data
###                cat("map_data[valid_dose] %in% names(merged_data): ", map_data[valid_dose] %in% names(merged_data), "\n")
###                cat("map_data[valid_tau] %in% names(merged_data): ", map_data[valid_tau] %in% names(merged_data), "\n")
###                cat("map_data[valid_told] %in% names(merged_data): ", map_data[valid_told] %in% names(merged_data), "\n")

###                print(names(map_data))
###                print(names(merged_data))
###                cat("print map_data[valid_dose] %in% names(merged_data): \n")
###                print(map_data[valid_dose] %in% names(merged_data))
              if(!(sum(map_data[valid_dose] %in% names(merged_data)) == sum(map_data[valid_tau] %in% names(merged_data)) && sum(map_data[valid_tau] %in% names(merged_data)) == sum(map_data[valid_told] %in% names(merged_data)))){
                missing_values <- ""
###                cat("map_data[valid_dose] %in% names(merged_data): ", map_data[valid_dose] %in% names(merged_data), "\n")
                k <- valid_dose[!map_data[valid_dose] %in% names(merged_data)]
###                print(k)
###                print(length(k))
###                valid_dose <- c("DOSE3", "DOSE4")
###                k <- grep(valid_dose, names(merged_data), ignore.case=TRUE, perl=TRUE)
###                cat("valid_dose: ", valid_dose, " k: ", k, " length(k): ", length(k), "\n")
                if(length(valid_dose[!map_data[valid_dose] %in% names(merged_data)]) > 0){
###
###                    print("dose!!!")
                  missing_values <- paste(missing_values, valid_dose[!map_data[valid_dose] %in% names(merged_data)])
                }
###                cat("map_data[valid_tau] %in% names(merged_data): ", map_data[valid_tau] %in% names(merged_data), "\n")
###                print(valid_tau[!map_data[valid_tau] %in% names(merged_data)])
                if(length(valid_tau[!map_data[valid_tau] %in% names(merged_data)]) > 0){
###
###                    print("tau!!!")
                  missing_values <- paste(missing_values, valid_tau[!map_data[valid_tau] %in% names(merged_data)])
                }
###
###                cat("map_data[valid_told] %in% names(merged_data): ", map_data[valid_told] %in% names(merged_data), "\n")
###                print(valid_told[!map_data[valid_told] %in% names(merged_data)])
                if(length(valid_told[!map_data[valid_told] %in% names(merged_data)]) > 0){
###
###                    print("told!!!")
                  missing_values <- paste(missing_values, valid_told[!map_data[valid_told] %in% names(merged_data)])
                }
                stop(paste("3 Unable to generate dosing interval for Steady State data! Values for", missing_values, "provided via 'map' are not present in the dataset provided via 'data'"))
              }
            }
          } else {
##            error_str <- "4 Unable to generate dosing interval for Steady State data! Please provide same number of interval values of data parameters, you provided:"
##            if(length(valid_dose) > 0){
##              error_str <- paste(error_str, paste(valid_dose, collapse = ", "))
##            }
##            if(length(valid_tau) > 0){
##              error_str <- paste(error_str, paste(valid_tau, collapse = ", "))
##            }
##            if(length(valid_told) > 0){
##              error_str <- paste(error_str, paste(valid_told, collapse = ", "))
##            }
##            error_str <- paste(error_str, "which is/are uneven!")
##            stop(error_str)
            if(length(valid_tau) > 0){
              map_data[,valid_tau] <- valid_tau
            }
            if(length(valid_told) > 0){
              map_data[,valid_told] <- valid_told
            }
            for(j in 1:length(unique(merged_data[,map_data$SDEID]))){
              tmp_df <- merged_data[merged_data[,map_data$SDEID] == unique(merged_data[,map_data$SDEID])[j],]
              tmp_df <- tmp_df[order(tmp_df[,map_data$TIME]),]
              print(tmp_df[,map_data$TIME])
              if(nrow(tmp_df) > 0){
                merged_data[merged_data[,map_data$SDEID] == unique(merged_data[,map_data$SDEID])[j],][,valid_told] <- tmp_df[1,map_data$TIME] 
                merged_data[merged_data[,map_data$SDEID] == unique(merged_data[,map_data$SDEID])[j],][,valid_tau] <- tmp_df[nrow(tmp_df),map_data$TIME] - tmp_df[1,map_data$TIME] 
              }
            }
          }

          intervals <- length(valid_dose)
###          cat('intervals: ', intervals, '\n')
          
          for(i in 1:intervals){
            if((("DOSE" %in% names(map_data)) || ss_dose[i] %in% names(map_data)) & ss_tau[i] %in% names(map_data) & ss_told[i] %in% names(map_data)) {
              if(ss_dose[i] %in% names(map_data)){
                if(!(map_data[,ss_dose[i]] %in% names(merged_data))){
                  stop(paste0("5 Unable to generate dosing interval for Steady State data! '", map_data[,ss_dose[i]], "' value for ", ss_dose[i], " is not present in the data"))
                }
              } else {
                if(!("DOSE" %in% names(map_data))){
                  stop(paste0("5 Unable to generate dosing interval for Steady State data! '", ss_dose[i], "' is not present in the 'map'"))
                }
              }
              if(!(map_data[,ss_tau[i]] %in% names(merged_data))){
                stop(paste0("6 Unable to generate dosing interval for Steady State data! '", map_data[,ss_tau[i]], "' value for ", ss_tau[i], " is not present in the data"))
              }
              if(!(map_data[,ss_told[i]] %in% names(merged_data))){
                stop(paste0("7 Unable to generate dosing interval for Steady State data! '", map_data[,ss_told[i]], "' value for ", ss_told[i], " is not present in the data"))
              }
              merged_data[c(paste0("DI", i, "F"))] <- NA
              #temp_data <- NA
              warning_generated <- FALSE
              for(j in 1:length(unique(merged_data[,map_data$SDEID]))){
                tmp_df <- merged_data[merged_data[,map_data$SDEID] == unique(merged_data[,map_data$SDEID])[j],]
                #print(unique(merged_data[,map_data$SDEID])[j])
                #tmp_rowId <- NA
                if(toupper(map_data$MODEL) == 'M4'){
                  tmp_df <- tmp_df[order(tmp_df[, map_data$TIME]),]
############################################################################################################
### 2019-09-14/TGT/ switch to NOMTIME, NOMENDTIME????????????????????????????????????? when testing M4SS - interval data
############################################################################################################
                  s_time <- tmp_df[, map_data$TIME]
                  e_time <- tmp_df[, map_data$ENDTIME]
###                  cat('s_time: ', s_time, ' e_time: ', e_time, '\n')
### 2019-08-27/TGT/ following no longer needed with validate_timeconc_data                    
###                  if(map_data$TIME == "NOMTIME" && map_data$ENDTIME == "NOMENDTIME"){
###                    if(!(map_data$NOMTIME %in% names(tmp_df))){
###                      stop("Value provided for 'NOMTIME' via 'map' does not exist in the data provided via 'data'")
###                    } else {
###                      tmp_df <- tmp_df[order(tmp_df[, map_data$NOMTIME]),]
###                      s_time <- tmp_df[, map_data$NOMTIME]
###                    }
###                    if(!(map_data$NOMTIME %in% names(tmp_df))){
###                      stop("Value provided for 'NOMENDTIME' via 'map' does not exist in the data provided via 'data'")
###                    } else {
###                      e_time <- tmp_df[, map_data$NOMENDTIME]
###                    }
###                  } else if(map_data$ENDTIME == "ACTTIME" && map_data$ENDTIME == "ACTENDTIME") {
###                    if(!(map_data$ACTTIME %in% names(tmp_df))){
###                      stop("Value provided for 'ACTTIME' via 'map' does not exist in the data provided via 'data'")
###                    } else {
###                      tmp_df <- tmp_df[order(tmp_df[, map_data$ACTTIME]),]
###                      s_time <- tmp_df[, map_data$ACTTIME]
###                    }
###                    if(!(map_data$ACTTIME %in% names(tmp_df))){
###                      stop("Value provided for 'ACTENDTIME' via 'map' does not exist in the data provided via 'data'")
###                    } else {
###                      e_time <- tmp_df[, map_data$ACTENDTIME]
###                    }
###                  } else {
###                    stop("Values provided for 'TIME' via 'map' does not match 'NOMTIME' or 'ACTTIME' and values provided for 'ENDTIME' via 'map' does not match 'NOMENDTIME' or 'ACTENDTIME'")
###                  }
                } else {
                      tmp_df <- tmp_df[order(tmp_df[, map_data$TIME]),]
############################################################################################################
### 2019-09-14/TGT/ use NOMTIME????????? - but does it perform all of the necessary interpolation/extrapolation to support this?
###                      time <- tmp_df[, map_data$TIME]
                      tmp_time <- tmp_df[, map_data$NOMTIME]
### 2019-08-27/TGT/ following no longer needed with validate_timeconc_data                    
###                  if(map_data$TIME == "NOMTIME"){
###                    if(!(map_data$NOMTIME %in% names(tmp_df))){
###                      stop("Value provided for 'NOMTIME' via 'map' does not exist in the data provided via 'data'")
###                    } else {
###                      tmp_df <- tmp_df[order(tmp_df[, map_data$NOMTIME]),]
###                      time <- tmp_df[, map_data$NOMTIME]
###                    }
###                  } else if(map_data$TIME == "ACTTIME") {
###                    if(!(map_data$ACTTIME %in% names(tmp_df))){
###                      stop("Value provided for 'ACTTIME' via 'map' does not exist in the data provided via 'data'")
###                    } else {
###                      tmp_df <- tmp_df[order(tmp_df[, map_data$ACTTIME]),]
###                      time <- tmp_df[, map_data$ACTTIME]
###                    }
###                  } else {
###                    stop("Values provided for 'TIME' via 'map' does not match 'NOMTIME' or 'ACTTIME'")
###                  }
                }
                #tmp_rowId <- as.integer(row.names(tmp_df))
                told <- unique(tmp_df[,as.character(map_data[,ss_told[i]])])[[1]]
                tau <- unique(tmp_df[,as.character(map_data[,ss_tau[i]])])[[1]]
###cat('i: ', i, ' told: ', told, ' tau: ', tau, '\n')
### 2019-09-05/TGT/ coerce tau/told to numeric
                if(!is.numeric(tau))  { tau  <- as.numeric(tau) }
                if(!is.numeric(told)) { told <- as.numeric(told) }
                
###                cat('tau: ', tau, ' class(tau): ', class(tau), ' told: ', told, ' class(told): ', class(told), '\n')
                if(is.na(told) || is.na(tau)){
                  merged_data[merged_data[,map_data$SDEID] == unique(merged_data[,map_data$SDEID])[j],][[c(paste0("DI", i, "F"))]] <- rep(0, nrow(tmp_df))
                } else {
                  if(toupper(map_data$MODEL) == 'M4'){
                    #print("M4")
                    #print(time)
                    told_i <- match(told, s_time)
                    tau_i <- match((tau+told), e_time)
                    #print(told)
                    #print(tau)
                    #print(told_i)
                    #print(tau_i)
                    if(is.na(told_i) || is.na(tau_i)){
                      if(is.na(told_i)){
                        if(casefold(map_data$ORGTIME)=='actual'){
                          told <- s_time[told < s_time][1]
                          told_i <- match(told, s_time)
                          tau_i <- match((tau+told), e_time)
                        } else {
                          stop("8 Unable to generate dosing interval for Steady State data! TOLD value not found in the provided TIME data")
                        }
                      }
                      if(is.na(tau_i)){
                        if(length(e_time[e_time < (tau+told)]) > 0){
                          tau <- e_time[e_time < (tau+told)][length(e_time[e_time < (tau+told)])]
                          tau_i <- match((tau+told), e_time)
                          #print('updated TAU')
                          #print(tau)
                          #print(tau_i)
                        } else {
                          stop("9 Unable to generate dosing interval for Steady State data! TAU value not found in the provided TIME data")
                        }
                      }
                    }
                    #start_i <-  1
                    #end_i <- nrow(tmp_df)

### 2019-08-27/TGT/ modify to map_data$TIME as no longer a pointer                      
###                    tmp_merged <-  merged_data[merged_data[,map_data$SDEID] == unique(merged_data[,map_data$SDEID])[j], map_data[, map_data$TIME]]
                    tmp_merged <-  merged_data[merged_data[,map_data$SDEID] == unique(merged_data[,map_data$SDEID])[j], map_data$TIME]
                    if(!is.na(tau_i) && !is.na(told_i)){
                      merged_data[merged_data[,map_data$SDEID] == unique(merged_data[,map_data$SDEID])[j],][order(tmp_merged),][[c(paste0("DI", i, "F"))]] <- c(rep(0, told_i-1), rep(1, (tau_i-told_i+1)), rep(0, (nrow(tmp_df) - tau_i)))
                    } else {
                      if(i == intervals){
                        if(is.na(told_i)){
                          merged_data[merged_data[,map_data$SDEID] == unique(merged_data[,map_data$SDEID])[j],][order(tmp_merged),][[c(paste0("DI", i, "F"))]] <- c(rep(0, nrow(tmp_df)))
                          if(!isTRUE(warning_generated)){
                            warning(paste0("Unable to generate dosing interval for Steady State data! TOLD value 'TOLD", i ,"' not found in the provided TIME data"))
                            warning_generated <- TRUE
                          }  
                        } else if(is.na(tau_i)){
                          tmp_tau_i <- which(e_time <= (tau+told))
                          if(length(tmp_tau_i) > 0){
                            tau_i <- tmp_tau_i[length(tmp_tau_i)] 
                            merged_data[merged_data[,map_data$SDEID] == unique(merged_data[,map_data$SDEID])[j],][order(tmp_merged),][[c(paste0("DI", i, "F"))]] <- c(rep(0, told_i-1), rep(1, (tau_i-told_i+1)), rep(0, (nrow(tmp_df) - tau_i)))
                          } else {
                            merged_data[merged_data[,map_data$SDEID] == unique(merged_data[,map_data$SDEID])[j],][order(tmp_merged),][[c(paste0("DI", i, "F"))]] <- c(rep(0, nrow(tmp_df)))
                            if(!isTRUE(warning_generated)){
                              warning(paste0("Unable to generate dosing interval for Steady State data! TAU value 'TAU", i ,"' not found in the provided TIME data"))
                              warning_generated <- TRUE
                            }
                          }
                        }
                      } else {
                        merged_data[merged_data[,map_data$SDEID] == unique(merged_data[,map_data$SDEID])[j],][order(tmp_merged),][[c(paste0("DI", i, "F"))]] <- c(rep(0, nrow(tmp_df)))
                        if(!isTRUE(warning_generated)){
                          warning(paste0("Unable to generate dosing interval for Steady State data! TAU value 'TAU", i ,"' or TOLD value 'TOLD", i ,"' not found in the provided TIME data"))
                          warning_generated <- TRUE
                        }   
                      }
                    }
                  } else {
                    #print(time)
### 2019-09-14/TGT/ need to match NOMINAL time for SS TOLD, TAU
###                      cat('i: ', i, ' told: ', told, ' tau: ', tau, ' tau+told: ', tau+told, ' nomtime: ', map_data$NOMTIME, ' acttime: ', map_data$ACTTIME, ' time: \n')
###                      print(time)
                    told_i <- match(told, tmp_time)
                    tau_i <- match((tau+told), tmp_time)
###                    cat('i: ', i, ' told: ', told, ' told_i: ', told_i, ' tau: ', tau, ' tau_i: ', tau_i, '\n')  
                    #print(told)
                    #print(tau)
                    #print(told_i)
                    #print(tau_i)
                    if(is.na(told_i) || is.na(tau_i)){
                      if(is.na(told_i)){
                        if(casefold(map_data$ORGTIME)=='actual'){
                          told <- tmp_time[told < tmp_time][1]
                          told_i <- match(told, tmp_time)
                          tau_i <- match((tau+told), tmp_time)
                        } else {
                          stop("10 Unable to generate dosing interval for Steady State data! TOLD value not found in the provided TIME data") 
                        }
                      }
                      if(is.na(tau_i)){
                        if(length(tmp_time[tmp_time < (tau+told)]) > 0){
                          tau <- tmp_time[tmp_time < (tau+told)][length(tmp_time[tmp_time < (tau+told)])]
                          tau_i <- match((tau+told), tmp_time)
                          #print('updated TAU')
                          #print(tau)
                          #print(tau_i)
                        } else {
                          stop("11 Unable to generate dosing interval for Steady State data! TAU value not found in the provided TIME data")
                        }
                      }
                    }
                    #start_i <-  1
                    #end_i <- nrow(tmp_df)

### 2019-08-27/TGT/ modify to map_data$TIME as no longer a pointer                      
###                    tmp_merged <-  merged_data[merged_data[,map_data$SDEID] == unique(merged_data[,map_data$SDEID])[j], map_data[, map_data$TIME]]
                    tmp_merged <-  merged_data[merged_data[,map_data$SDEID] == unique(merged_data[,map_data$SDEID])[j], map_data$TIME]
                    if(!is.na(tau_i) && !is.na(told_i)){
                      merged_data[merged_data[,map_data$SDEID] == unique(merged_data[,map_data$SDEID])[j],][order(tmp_merged),][[c(paste0("DI", i, "F"))]] <- c(rep(0, told_i-1), rep(1, (tau_i-told_i+1)), rep(0, (nrow(tmp_df) - tau_i)))
                    } else {
                      if(i == intervals){
                        if(is.na(told_i)){
                          merged_data[merged_data[,map_data$SDEID] == unique(merged_data[,map_data$SDEID])[j],][order(tmp_merged),][[c(paste0("DI", i, "F"))]] <- c(rep(0, nrow(tmp_df)))
                          if(!isTRUE(warning_generated)){
                            warning(paste0("Unable to generate dosing interval for Steady State data! TOLD value 'TOLD", i ,"' not found in the provided TIME data"))
                            warning_generated <- TRUE
                          }  
                        } else if(is.na(tau_i)){
                          tmp_tau_i <- which(tmp_time <= (tau+told))
                          if(length(tmp_tau_i) > 0){
                            tau_i <- tmp_tau_i[length(tmp_tau_i)] 
                            merged_data[merged_data[,map_data$SDEID] == unique(merged_data[,map_data$SDEID])[j],][order(tmp_merged),][[c(paste0("DI", i, "F"))]] <- c(rep(0, told_i-1), rep(1, (tau_i-told_i+1)), rep(0, (nrow(tmp_df) - tau_i)))
                          } else {
                            merged_data[merged_data[,map_data$SDEID] == unique(merged_data[,map_data$SDEID])[j],][order(tmp_merged),][[c(paste0("DI", i, "F"))]] <- c(rep(0, nrow(tmp_df)))
                            if(!isTRUE(warning_generated)){
                              warning(paste0("Unable to generate dosing interval for Steady State data! TAU value 'TAU", i ,"' not found in the provided TIME data"))
                              warning_generated <- TRUE
                            }
                          }
                        }
                      } else {
                        merged_data[merged_data[,map_data$SDEID] == unique(merged_data[,map_data$SDEID])[j],][order(tmp_merged),][[c(paste0("DI", i, "F"))]] <- c(rep(0, nrow(tmp_df)))
                        if(!isTRUE(warning_generated)){
                          warning(paste0("Unable to generate dosing interval for Steady State data! TAU value 'TAU", i ,"' or TOLD value 'TOLD", i ,"' not found in the provided TIME data"))
                          warning_generated <- TRUE
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        } else {
          merged_data[[c(paste0("DI", i, "F"))]] <- rep(1, nrow(merged_data))
          warning(paste0("Unable to generate dosing interval for Steady State data since no TAUx, TOLDx or DOSEx was provided! Using the entire profile as the dosing interval!"))
        }
### end DOSINGTYPE==SS
###        cat("2: valid_tau: ", valid_tau, "valid_told: ", valid_told, "valid_dose: ", valid_dose, " intervals: ", intervals, "\n")

      } else if(toupper(map_data$DOSINGTYPE) == 'SD') {
### need to make DOSE1 and DOSE aliases of each other. Also need to make DOSEU and DOSEUNI aliases of each other
###        if(!("DOSE1" %in% names(map_data))){
###          stop("Dataset provided via 'map' does not contain the required 'DOSE1' column")
###        } else {
###          if(!(map_data$DOSE1 %in% names(data_data))){
###            stop("'DOSE1' value provided via 'map' is not present in the dataset provided via 'data'")
###          }
###        }
      }
    }
  } else {
    stop("Dataset provided via 'map' does not contain the 'DOSINGTYPE' column")
  }

###
###  print(head(merged_data))
  
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
  #return(merged_data)

###
###  cat('merged_data names2:\n')
###  print(sort(names(merged_data)))
  
### 2019-09-01/TGT/ dependency_list is a list with named parameters that state explicit dependencies on other parameters
  dependency_list <- list()
  
### 2019-08-22/TGT/ Modify
###  CMAXiDN -> CMAXDNi
###  AUCLASTiDN -> AUCLASTDNi
###  AUCTAUiDN -> AUCTAUDNi
  
### 2019-08-30/TGT/ Remove "CMAXC"/"CMAXCi" since only valid for M2 Bolus administration applications
###  m1sd_param_list <- as.list(c("CMAXi", "CMINi", "CLASTi", "CMAXCi", "CMAXDNi", "TMAXi", "TMINi", "TLAST", "TLAG", "KEL", "CEST",
### 2019-09-01/TGT/ Change AUMCXPCTO to AUMCXPTO, AUMCXPCTP to AUMCXPTP
if(FALSE) {
  m1sd_param_list <- as.list(c("CMAXi", "CMINi", "CLASTi", "CMAXDNi", "TMAXi", "TMINi", "TLAST", "TLAG", "KEL", "CEST",
                               "KELTMLO", "KELTMHI", "KELNOPT", "KELRSQ", "KELRSQA", "THALF", "THALFF", "AUCALL", "AUCDN", "AUCLASTi",
                               "AUCLASTCi", "AUCLASTDNi", "AUMCLAST", "AUCT", "AUCT1_T2", "AUCINFO", "AUCINFP", "AUCINFOC", "AUCINFPC", "AUCINFODN",
                               "AUCINFPDN", "AUMCINFO", "AUMCINFP", "MRTLAST", "MRTEVIFOi", "MRTEVIFPi", "AUCXPCTO", "AUCXPCTP",
                               "AUMCXPTO", "AUMCXPTP", "CLO", "CLFO", "CLFOW", "CLFP", "CLFPW", "VZFO", "VZFOW", "VZFP", "VZFPW"))

### 2019-08-30/TGT/ Remove "CMAXC"/"CMAXCi" since only valid for M2 Bolus administration applications
###  m1ss_param_list <- as.list(c("CMAX", "CMAXi", "CMAXC", "CMAXCi", "CMAXDN", "CMAXDNi", "CMIN", "CMINi", "CLAST", "CLASTi", "TMAX",
  m1ss_param_list <- as.list(c("CMAX", "CMAXi", "CMAXDN", "CMAXDNi", "CMIN", "CMINi", "CLAST", "CLASTi", "TMAX",
                               "TMAXi", "TMIN", "TMINi", "TLAST", "TLASTi", "TLAG", "KEL", "KELC0", "KELTMLO", "KELTMHI", "KELNOPT", "KELRSQ",
                               "KELRSQA", "THALF", "THALFF", "LASTTIME", "LASTTIMEi", "AUCALL", "AUCDN", "AUCLAST", "AUCLASTC", "AUCLASTDN",
                               "AUCLASTi", "AUCLASTCi", "AUCLASTDNi", "AUMCLASTi", "AUCT", "AUCTDN", "AUCT1_T2", "AUCINFO", "AUCINFOi", "AUCINFOC",
                               "AUCINFODN", "CEST", "AUCINFP", "AUCINFPi", "AUCINFPC", "AUCINFPDN", "AUMCINFOi", "AUMCINFPi", "AUCTAUi", "AUCTAUDNi",
                               "AUMCTAUi", "MRTLAST", "MRTLASTi", "MRTEVIFOi", "MRTEVIFPi", "AUCXPCTO", "AUCXPCTOi", "AUCXPCTP", "AUCXPCTPi",
                               "AUMCXPTO", "AUMCXPTOi", "AUMCXPTP", "AUMCXPTPi", "CLFO", "CAVi", "CLFTAUi", "CLFTAUWi", "PTFi", "PTRi", "VZFTAUi",
                               "VZFTAUWi", "DIi", "TAUi", "TOLD", "DOSEi"))

  m2sd_param_list <- as.list(c("C0", "V0", "CMAXi", "CLASTi", "CMAXCi", "CMAXDNi", "TMAXi", "TLAST", "KEL", "CEST", 
                              "KELTMLO", "KELTMHI", "KELNOPT", "KELRSQ", "KELRSQA", "THALF", "THALFF", "AUCALL", "AUCDN", "AUCLASTi",
                              "AUCLASTCi", "AUCLASTDNi", "AUMCLAST", "AUCT", "AUCT1_T2", "AUCINFO", "AUCINFP", "AUCINFOC", "AUCINFPC", "AUCINFODN",
                              "AUCINFPDN", "AUMCINFO", "AUMCINFP", "MRTLAST", "MRTIVIFOi", "MRTIVIFPi", "AUCXPCTO", "AUCXPCTP",
                              "AUMCXPTO", "AUMCXPTP", "CLO", "CLOW", "CLP", "CLPW", "VZO", "VZOW", "VZP", "VZPW"))

  m2ss_param_list <- as.list(c("C0", "V0", "CMAX", "CMAXi", "CMAXC", "CMAXCi", "CMAXDN", "CMAXDNi", "CMIN", "CMINi", "CLAST", "CLASTi", "TMAX",
                               "TMAXi", "TMIN", "TMINi", "TLAST", "TLASTi", "KEL", "KELC0", "KELTMLO", "KELTMHI", "KELNOPT", "KELRSQ",
                               "KELRSQA", "THALF", "THALFF", "LASTTIME", "LASTTIMEi", "AUCALL", "AUCDN", "AUCLAST", "AUCLASTC", "AUCLASTDN",
                               "AUCLASTi", "AUCLASTCi", "AUCLASTDNi", "AUMCLASTi", "AUCT", "AUCTDN", "AUCT1_T2", "AUCINFO", "AUCINFOi", "AUCINFOC",
                               "AUCINFODN", "CEST", "AUCINFP", "AUCINFPi", "AUCINFPC", "AUCINFPDN", "AUMCINFOi", "AUMCINFPi", "AUCTAUi", "AUCTAUDNi",
                               "AUMCTAUi", "MRTLAST", "MRTLASTi", "MRTIVIFOi", "MRTIVIFPi", "AUCXPCTO", "AUCXPCTOi", "AUCXPCTP", "AUCXPCTPi",
                               "AUMCXPTO", "AUMCXPTOi", "AUMCXPTP", "AUMCXPTPi", "CLFO", "CAVi", "CLTAUi", "CLTAUWi", "PTFi", "PTRi", "VZO",
                               "VZP", "DIi", "TAUi", "TOLD", "DOSEi"))

### 2019-08-30/TGT/ Remove "CMAXC"/"CMAXCi" since only valid for M2 Bolus administration applications
###  m3sd_param_list <- as.list(c("CMAXi", "CLASTi", "CMAXCi", "CMAXDNi", "TMAXi", "TLAST", "KEL", "CEST", 
  m3sd_param_list <- as.list(c("CMAXi", "CLASTi", "CMAXDNi", "TMAXi", "TLAST", "KEL", "CEST", 
                              "KELTMLO", "KELTMHI", "KELNOPT", "KELRSQ", "KELRSQA", "THALF", "THALFF", "AUCALL", "AUCDN", "AUCLASTi",
                              "AUCLASTCi", "AUCLASTDNi", "AUMCLAST", "AUCT", "AUCT1_T2", "AUCINFO", "AUCINFP", "AUCINFOC", "AUCINFPC", "AUCINFODN",
                              "AUCINFPDN", "AUMCINFO", "AUMCINFP", "MRTLAST", "MRTIVIFOi", "MRTIVIFPi", "AUCXPCTO", "AUCXPCTP",
                              "AUMCXPTO", "AUMCXPTP", "CLO", "CLOW", "CLP", "CLPW", "VZO", "VZOW", "VZP", "VZPW"))

### 2019-08-30/TGT/ Remove "CMAXC"/"CMAXCi" since only valid for M2 Bolus administration applications
###  m3ss_param_list <- as.list(c("CMAX", "CMAXi", "CMAXC", "CMAXCi", "CMAXDN", "CMAXDNi", "CMIN", "CMINi", "CLAST", "CLASTi", "TMAX",
  m3ss_param_list <- as.list(c("CMAX", "CMAXi", "CMAXDN", "CMAXDNi", "CMIN", "CMINi", "CLAST", "CLASTi", "TMAX",
                               "TMAXi", "TMIN", "TMINi", "TLAST", "TLASTi", "KEL", "KELC0", "KELTMLO", "KELTMHI", "KELNOPT", "KELRSQ",
                               "KELRSQA", "THALF", "THALFF", "LASTTIME", "LASTTIMEi", "AUCALL", "AUCDN", "AUCLAST", "AUCLASTC", "AUCLASTDN",
                               "AUCLASTi", "AUCLASTCi", "AUCLASTDNi", "AUMCLASTi", "AUCT", "AUCTDN", "AUCT1_T2", "AUCINFO", "AUCINFOi", "AUCINFOC",
                               "AUCINFODN", "CEST", "AUCINFP", "AUCINFPi", "AUCINFPC", "AUCINFPDN", "AUMCINFOi", "AUMCINFPi", "AUCTAUi", "AUCTAUDNi",
                               "AUMCTAUi", "MRTLAST", "MRTLASTi", "MRTIVIFOi", "MRTIVIFPi", "AUCXPCTO", "AUCXPCTOi", "AUCXPCTP", "AUCXPCTPi",
                               "AUMCXPTO", "AUMCXPTOi", "AUMCXPTP", "AUMCXPTPi", "CLFO", "CAVi", "CLTAUi", "CLTAUWi", "PTFi", "PTRi", "VZO",
                               "VZP", "DIi", "TAUi", "TOLD", "DOSEi"))

  m4sd_param_list <- as.list(c("AET", "AETPCT", "AE", "AEPCT", "MAXRATEi", "TMAXRATEi", "RATELASTi", "MIDPTLASTi", "TLAG", "KEL", "KELTMLO", "KELTMHI",
                              "KELNOPT", "KELRSQ", "KELRSQA", "THALF", "THALFF", "AUCDN", "AURCALL", "AURCLAST", "AURCINFO", "AURCINFP", "AURCXPCTO", "AURCXPCTP",
                              "AURCT1_T2", "VOLSUM"))

  m4ss_param_list <- as.list(c("AET", "AETPCT", "MAXRATEi", "TMAXRATEi", "RATELASTi", "MIDPTLASTi", "DIi", "TAUi", "TOLD", "DOSEi",
                              "AE", "AEPCT", "TLAG", "KEL", "KELTMLO", "KELTMHI", "KELNOPT", "KELRSQ", "KELRSQA", "THALF", "THALFF",
                              "AUCDN", "AURCALL", "AURCLAST", "AURCINFO", "AURCINFP", "AURCXPCTO", "AURCXPCTP", "AURCT1_T2"))
}
  ### 2019-10-09/TGT/ Define model regular expression to determine associated model parameters
  model_regex <- paste0("^", map_data$MODEL, "(", map_data$DOSINGTYPE, ")*?$")

  parameter_list <- list()
  if(is.null(map_data$PARAMETERLIST)) { parameter_list <- model_parameters(model_regex) }
  else { parameter_list <- unlist(strsplit(map_data$PARAMETERLIST, ";")) }

#  xparameter_list <- model_parameters(model_regex)
#  print(xparameter_list)
  #yparameter_list <- unlist(strsplit(map_data$PARAMETERLIST, ";"))
  #print(yparameter_list)
  #k <- setdiff(yparameter_list, xparameter_list)
  #print(k)
  #j <- setdiff(xparameter_list, yparameter_list)
  #print(j)
#  stop("here")

if(FALSE) {  
###  if(!is.null(map_data$PARAMETERLIST) && !is.na(map_data$PARAMETERLIST)){
  if(!is.null(map_data$PARAMETERLIST) && !is.na(map_data$PARAMETERLIST)){
    parameter_list <- as.list(strsplit(map_data$PARAMETERLIST, ";")[[1]])

    param_check <- FALSE
    if(toupper(map_data$MODEL) == 'M1' && toupper(map_data$DOSINGTYPE) == 'SD'){
      param_check <- ifelse(sum(parameter_list %in% m1sd_param_list) > 0, TRUE, FALSE)
    } else if(toupper(map_data$MODEL) == 'M1' && toupper(map_data$DOSINGTYPE) == 'SS'){
      param_check <- ifelse(sum(parameter_list %in% m1ss_param_list) > 0, TRUE, FALSE)
    } else if (toupper(map_data$MODEL) == 'M2' && toupper(map_data$DOSINGTYPE) == 'SD'){
      param_check <- ifelse(sum(parameter_list %in% m2sd_param_list) > 0, TRUE, FALSE)
    } else if (toupper(map_data$MODEL) == 'M2' && toupper(map_data$DOSINGTYPE) == 'SS'){
      param_check <- ifelse(sum(parameter_list %in% m2ss_param_list) > 0, TRUE, FALSE)
    } else if (toupper(map_data$MODEL) == 'M3' && toupper(map_data$DOSINGTYPE) == 'SD'){
      param_check <- ifelse(sum(parameter_list %in% m3sd_param_list) > 0, TRUE, FALSE)
    } else if(toupper(map_data$MODEL) == 'M3' && toupper(map_data$DOSINGTYPE) == 'SS'){
      param_check <- ifelse(sum(parameter_list %in% m3ss_param_list) > 0, TRUE, FALSE)
    } else if (toupper(map_data$MODEL) == 'M4' && toupper(map_data$DOSINGTYPE) == 'SD'){
      param_check <- ifelse(sum(parameter_list %in% m4sd_param_list) > 0, TRUE, FALSE)
    } else if(toupper(map_data$MODEL) == 'M4' && toupper(map_data$DOSINGTYPE) == 'SS'){
      param_check <- ifelse(sum(parameter_list %in% m4ss_param_list) > 0, TRUE, FALSE)
    }
###
###    cat("parameter_list: \n")
###    print(unlist(parameter_list))
###    cat("param_check:\n")
###    print(param_check)
    
    if(length(parameter_list) == 0 || !param_check) {
      warning("'PARAMETERLIST' values provided via 'map' does not contain any values that are present in the input dataset! Using default parameters based on MODEL and DOSINGTYPE!")

      if(toupper(map_data$MODEL) == 'M1' && toupper(map_data$DOSINGTYPE) == 'SD'){
        parameter_list <- m1sd_param_list
      } else if(toupper(map_data$MODEL) == 'M1' && toupper(map_data$DOSINGTYPE) == 'SS'){
        parameter_list <- m1ss_param_list
      } else if (toupper(map_data$MODEL) == 'M2' && toupper(map_data$DOSINGTYPE) == 'SD'){
        parameter_list <- m2sd_param_list
      } else if (toupper(map_data$MODEL) == 'M2' && toupper(map_data$DOSINGTYPE) == 'SS'){
        parameter_list <- m2ss_param_list
      } else if (toupper(map_data$MODEL) == 'M3' && toupper(map_data$DOSINGTYPE) == 'SD'){
        parameter_list <- m3sd_param_list
      } else if(toupper(map_data$MODEL) == 'M3' && toupper(map_data$DOSINGTYPE) == 'SS'){
        parameter_list <- m3ss_param_list
      } else if (toupper(map_data$MODEL) == 'M4' && toupper(map_data$DOSINGTYPE) == 'SD'){
        parameter_list <- m4sd_param_list
      } else if(toupper(map_data$MODEL) == 'M4' && toupper(map_data$DOSINGTYPE) == 'SS'){
        parameter_list <- m4ss_param_list
      }
    }
  } else {
    if(toupper(map_data$MODEL) == 'M1' && toupper(map_data$DOSINGTYPE) == 'SD'){
      parameter_list <- m1sd_param_list
    } else if(toupper(map_data$MODEL) == 'M1' && toupper(map_data$DOSINGTYPE) == 'SS'){
      parameter_list <- m1ss_param_list
    } else if (toupper(map_data$MODEL) == 'M2' && toupper(map_data$DOSINGTYPE) == 'SD'){
      parameter_list <- m2sd_param_list
    } else if (toupper(map_data$MODEL) == 'M2' && toupper(map_data$DOSINGTYPE) == 'SS'){
      parameter_list <- m2ss_param_list
    } else if (toupper(map_data$MODEL) == 'M3' && toupper(map_data$DOSINGTYPE) == 'SD'){
      parameter_list <- m3sd_param_list
    } else if(toupper(map_data$MODEL) == 'M3' && toupper(map_data$DOSINGTYPE) == 'SS'){
      parameter_list <- m3ss_param_list
    } else if (toupper(map_data$MODEL) == 'M4' && toupper(map_data$DOSINGTYPE) == 'SD'){
      parameter_list <- m4sd_param_list
    } else if(toupper(map_data$MODEL) == 'M4' && toupper(map_data$DOSINGTYPE) == 'SS'){
      parameter_list <- m4ss_param_list
    }
  }

  if(toupper(map_data$MODEL) == 'M1' && toupper(map_data$DOSINGTYPE) == 'SD'){
    if(optimize_kel){
      if(!"TMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TMAX"
      }
      if(!"TLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TLAST"
      }
      if(!"CMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMAX"
      }
      if(!"CLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CLAST"
      }
      if(!"AUCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
      }
    }
    if("CMAXC" %in% parameter_list && (!"CMAX" %in% parameter_list || !"KEL" %in% parameter_list)) {
      if(!"CMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMAX"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
    }
    if("CMAXDN" %in% parameter_list && !"CMAX" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "CMAX"
    }
    if("AUCDN" %in% parameter_list && !"AUCALL" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "AUCALL"
    }
    if("AUCT" %in% parameter_list && !"TMAX" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "TMAX"
    }
    if("AUCT1_T2" %in% parameter_list && !"TMAXi" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "TMAXi"
    }

### 2019-08-23/TGT/ Just a note to interpret the following
###   here if AUCLASTCi is already in parameter_list, this adds AUCLASTi/KEL/TLAST
###   into the parameter_list as dependencies if not already there
    
    if("AUCLASTCi" %in% parameter_list && (!"AUCLASTi" %in% parameter_list || !"KEL" %in% parameter_list || !"TLAST" %in% parameter_list)){
        if(!"AUCLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLASTi"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"TLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TLAST"
      }
    }
    
    if("AUCLASTDNi" %in% parameter_list && !"AUCLASTi" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "AUCLASTi"
    }
    if("AUCINFOC" %in% parameter_list && (!"KEL" %in% parameter_list || !"AUCINFO" %in% parameter_list)){
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"AUCINFO" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFO"
      }
    }
    if("AUCINFODN" %in% parameter_list && !"AUCINFO" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "AUCINFO"
    }
    if("AUCINFPC" %in% parameter_list && (!"KEL" %in% parameter_list || !"AUCINFP" %in% parameter_list)){
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"AUCINFP" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFP"
      }
    }
    if("AUCINFPDN" %in% parameter_list && !"AUCINFP" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AUCINFP"
    }
    if("CLO" %in% parameter_list && !"AUCINFO" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AUCINFO"
    }
    if("CLFOW" %in% parameter_list && !"CLFO" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "CLFO"
    }
    if("CLFO" %in% parameter_list && !"AUCINFO" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AUCINFO"
    }
    if("CLFPW" %in% parameter_list && !"CLFP" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "CLFP"
    }
    if("CLFP" %in% parameter_list && !"AUCINFP" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AUCINFP"
    }
    if("VZFOW" %in% parameter_list && !"VZFO" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "VZFO"
    }
    if("VZFO" %in% parameter_list && (!"AUCINFO" %in% parameter_list || !"KEL" %in% parameter_list)) {
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"AUCINFO" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFO"
      }
    }
    if("VZFPW" %in% parameter_list && !"VZFP" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "VZFP"
    }
    if("VZFP" %in% parameter_list && (!"AUCINFP" %in% parameter_list || !"KEL" %in% parameter_list)) {
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"AUCINFP" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFP"
      }
    }
  } else if(toupper(map_data$MODEL) == 'M1' && toupper(map_data$DOSINGTYPE) == 'SS'){
    if(optimize_kel){
      if(!"TMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TMAX"
      }
      if(!"TLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TLAST"
      }
      if(!"CMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMAX"
      }
      if(!"CLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CLAST"
      }
      if(!"AUCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
      }
    }
    if("CMAXC" %in% parameter_list && (!"CMAX" %in% parameter_list || !"TMAX" %in% parameter_list || !"KEL" %in% parameter_list)){
      if(!"CMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMAX"
      }
      if(!"TMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TMAX"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
    }
    if("CMAXCi" %in% parameter_list && (!"CMAXi" %in% parameter_list || !"TMAXi" %in% parameter_list || !"KEL" %in% parameter_list)){
      if(!"CMAXi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMAXi"
      }
      if(!"TMAXi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TMAXi"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
    }
    if("CMAXDNi" %in% parameter_list && !"CMAXi" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "CMAXi"
    }
    if("AUCDN" %in% parameter_list && !"AUCALL" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "AUCALL"
    }
    if("AUCALL" %in% parameter_list && !"TMAXi" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "TMAXi"
    }
    if("MRTLAST" %in% parameter_list && (!"AUCLAST" %in% parameter_list || !"AUMCLAST" %in% parameter_list)){
      if(!"AUCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
      }
      if(!"AUMCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCLAST"
      }
    }
    if("AUCXPCTO" %in% parameter_list && (!"AUCINFO" %in% parameter_list || !"AUCLAST" %in% parameter_list)){
      if(!"AUCINFO" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFO"
      }
      if(!"AUCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
      }
    }
    if("AUCXPCTP" %in% parameter_list && (!"AUCINFP" %in% parameter_list || !"AUCLAST" %in% parameter_list)){
      if(!"AUCINFP" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFP"
      }
      if(!"AUCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
      }
    }
    if("AUCLASTC" %in% parameter_list && (!"AUCLAST" %in% parameter_list || !"TLAST" %in% parameter_list || !"KEL" %in% parameter_list)){
      if(!"AUCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
      }
      if(!"TLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TLAST"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
    }
    if("AUCLASTDN" %in% parameter_list && !"AUCLAST" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
    }
    if("AUCLAST" %in% parameter_list && (!"TMAX" %in% parameter_list || !"TLAST" %in% parameter_list)){
      if(!"TMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TMAX"
      }
      if(!"TLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TLAST"
      }
    }
    if("MRTEVIFOi" %in% parameter_list && (!"AUCINFOi" %in% parameter_list || !"AUCTAUi" %in% parameter_list || !"AUMCTAUi" %in% parameter_list)){
      if(!"AUCINFOi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFOi"
      }
      if(!"AUCTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
      }
      if(!"AUMCTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCTAUi"
      }
    }
    if("AUCXPCTOi" %in% parameter_list && (!"AUCINFOi" %in% parameter_list || !"AUCLASTi" %in% parameter_list)){
      if(!"AUCINFOi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFOi"
      }
      if(!"AUCLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLASTi"
      }
    }
    if("AUCINFOi" %in% parameter_list && (!"AUCLASTi" %in% parameter_list || !'CLASTi' %in% parameter_list || !"KEL" %in% parameter_list)) {
      if(!"AUCLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLASTi"
      }
      if(!"CLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CLASTi"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
    }
    if("MRTEVIFPi" %in% parameter_list && (!"AUCINFPi" %in% parameter_list || !"AUCTAUi" %in% parameter_list || !"AUMCTAUi" %in% parameter_list)){
      if(!"AUCINFPi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFPi"
      }
      if(!"AUCTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
      }
      if(!"AUMCTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCTAUi"
      }
    }
    if("AUCXPCTPi" %in% parameter_list && (!"AUCINFPi" %in% parameter_list || !"AUCLASTi" %in% parameter_list)){
      if(!"AUCINFPi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFPi"
      }
      if(!"AUCLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLASTi"
      }
    }
    if("AUCINFPi" %in% parameter_list && (!"CEST" %in% parameter_list || !"AUCLASTi" %in% parameter_list || !'TLASTi' %in% parameter_list || !"KEL" %in% parameter_list)) {
      if(!"CEST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CEST"
      }
      if(!"AUCLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLASTi"
      }
      if(!"TLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TLASTi"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
    }
    if("MRTLASTi" %in% parameter_list && (!"AUCLASTi" %in% parameter_list || !"AUMCLASTi" %in% parameter_list)){
      if(!"AUCLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLASTi"
      }
      if(!"AUMCLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCLASTi"
      }
    }
    if("AUCLASTCi" %in% parameter_list && (!"AUCLASTi" %in% parameter_list || !"TLASTi" %in% parameter_list || !"KEL" %in% parameter_list)){
      if(!"AUCLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLASTi"
      }
      if(!"TLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TLASTi"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
    }
    if("AUCLASTDNi" %in% parameter_list && !"AUCLASTi" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "AUCLASTi"
    }
    if("AUCLASTi" %in% parameter_list && (!"TMAXi" %in% parameter_list || !"TLASTi" %in% parameter_list)){
      if(!"TMAXi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TMAXi"
      }
      if(!"TLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TLASTi"
      }
    }
    if("AUCT" %in% parameter_list && !"TMAXi" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "TMAXi"
    }
    if("AUCT1_T2" %in% parameter_list && !"TMAXi" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "TMAXi"
    }
    if("AUCINFOC" %in% parameter_list && (!"KEL" %in% parameter_list || !"AUCINFO" %in% parameter_list)){
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"AUCINFO" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFO"
      }
    }
    if("AUCINFODN" %in% parameter_list && !"AUCINFO" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "AUCINFO"
    }
    if("CLFO" %in% parameter_list && !"AUCINFO" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AUCINFO"
    }
    if("AUCINFO" %in% parameter_list && (!"AUCLAST" %in% parameter_list || !'CLAST' %in% parameter_list || !"KEL" %in% parameter_list)) {
      if(!"AUCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
      }
      if(!"CLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CLAST"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
    }
    if("AUCINFPC" %in% parameter_list && (!"KEL" %in% parameter_list || !"AUCINFP" %in% parameter_list)){
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"AUCINFP" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFP"
      }
    }
    if("AUCINFPDN" %in% parameter_list && !"AUCINFP" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AUCINFP"
    }
    if("AUCINFP" %in% parameter_list && (!"CEST" %in% parameter_list || !"AUCLAST" %in% parameter_list || !'TLAST' %in% parameter_list || !"KEL" %in% parameter_list)) {
      if(!"CEST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CEST"
      }
      if(!"AUCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
      }
      if(!"TLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TLAST"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
    }
    if("AUCTAUDNi" %in% parameter_list && !"AUCTAUi" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
    }
    if("AUCTAUi" %in% parameter_list && !"TMAXi" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "TMAXi"
    }
    if("AUMCXPTO" %in% parameter_list && (!"AUMCINFO" %in% parameter_list || !"AUMCLAST" %in% parameter_list)){
      if(!"AUMCINFO" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCINFO"
      }
      if(!"AUMCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCLAST"
      }
    }
    if("AUMCXPTOi" %in% parameter_list && (!"AUMCINFOi" %in% parameter_list || !"AUMCLAST" %in% parameter_list)){
      if(!"AUMCINFOi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCINFOi"
      }
      if(!"AUMCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCLAST"
      }
    }
    if("AUMCXPTP" %in% parameter_list && (!"AUMCINFP" %in% parameter_list || !"AUMCLAST" %in% parameter_list)){
      if(!"AUMCINFP" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCINFP"
      }
      if(!"AUMCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCLAST"
      }
    }
    if("AUMCXPTPi" %in% parameter_list && (!"AUMCINFPi" %in% parameter_list || !"AUMCLAST" %in% parameter_list)){
      if(!"AUMCINFPi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCINFPi"
      }
      if(!"AUMCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCLAST"
      }
    }
    if("CEST" %in% parameter_list && (!"TLAST" %in% parameter_list || !"KEL" %in% parameter_list)){
      if(!"TLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TLAST"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
    }
    if("PTFi" %in% parameter_list && !"CMAXi" %in% parameter_list && !"CMINi" %in% parameter_list && !"CAVi" %in% parameter_list && "AUCTAUi" %in% parameter_list) {
      if(!"CMAXi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMAXi"
      }
      if(!"CMINi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMINi"
      }
      if(!"CAVi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CAVi"
      }
      if(!"AUCTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
      }
    }
    if("CAVi" %in% parameter_list && !"AUCTAUi" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
    }
    if("CLFTAUWi" %in% parameter_list && (!"CLFTAUi" %in% parameter_list || !"AUCTAUi" %in% parameter_list)) {
      if(!"CLFTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CLFTAUi"
      }
      if(!"AUCTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
      }
    }
    if("CLFTAUi" %in% parameter_list && !"AUCTAUi" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
    }
    if("PTRi" %in% parameter_list && (!"CMAXi" %in% parameter_list || !"CMINi" %in% parameter_list)) {
      if(!"CMAXi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMAXi"
      }
      if(!"CMINi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMINi"
      }
    }
    if("VZFTAUWi" %in% parameter_list && (!"VZFTAUi" %in% parameter_list || !"KEL" %in% parameter_list || !"AUCTAUi" %in% parameter_list)) {
      if(!"VZFTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "VZFTAUi"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"AUCTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
      }
    }
    if("VZFTAUi" %in% parameter_list && (!"KEL" %in% parameter_list || !"AUCTAUi" %in% parameter_list)) {
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"AUCTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
      }
    }
  } else if (toupper(map_data$MODEL) == 'M2' && toupper(map_data$DOSINGTYPE) == 'SD'){
    if(optimize_kel){
      if(!"TMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TMAX"
      }
      if(!"TLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TLAST"
      }
      if(!"CMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMAX"
      }
      if(!"CLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CLAST"
      }
      if(!"AUCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
      }
    }
    if("CMAXCi" %in% parameter_list && (!"CMAXi" %in% parameter_list || !"KEL" %in% parameter_list)) {
      if(!"CMAXi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMAXi"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
    }
    if("CMAXDNi" %in% parameter_list && !"CMAXi" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "CMAXi"
    }
    if("AUCDN" %in% parameter_list && !"AUCALL" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "AUCALL"
    }
    if("AUCT" %in% parameter_list && !"TMAXi" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "TMAXi"
    }
    if("AUCT1_T2" %in% parameter_list && !"TMAXi" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "TMAXi"
    }
    if("AUCLASTCi" %in% parameter_list && (!"AUCLASTi" %in% parameter_list || !"KEL" %in% parameter_list || !"TLAST" %in% parameter_list)){
      if(!"AUCLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLASTi"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"TLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TLAST"
      }
    }
    if("AUCLASTDNi" %in% parameter_list && !"AUCLASTi" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "AUCLASTi"
    }
    if("AUCINFOC" %in% parameter_list && (!"KEL" %in% parameter_list || !"AUCINFO" %in% parameter_list)){
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"AUCINFO" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFO"
      }
    }
    if("AUCINFODN" %in% parameter_list && !"AUCINFO" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "AUCINFO"
    }
    if("AUCINFPC" %in% parameter_list && (!"KEL" %in% parameter_list || !"AUCINFP" %in% parameter_list)){
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"AUCINFP" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFP"
      }
    }
    if("AUCINFPDN" %in% parameter_list && !"AUCINFP" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AUCINFP"
    }
    if("VZFP" %in% parameter_list && (!"AUCINFP" %in% parameter_list || !"KEL" %in% parameter_list)) {
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"AUCINFP" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFP"
      }
    }
    if("CAVi" %in% parameter_list && !"AUCTAUi" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
    }
    if("CLFTAUi" %in% parameter_list && !"AUCTAUi" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
    }
    if("CLFTAUWi" %in% parameter_list && (!"CLFTAUi" %in% parameter_list || !"AUCTAUi" %in% parameter_list)) {
      if(!"CLFTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CLFTAUi"
      }
      if(!"AUCTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
      }
    }
    if("CLOW" %in% parameter_list && !"CLO" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "CLO"
    }
    if("PTFi" %in% parameter_list && (!"CMAXi" %in% parameter_list || !"CMINi" %in% parameter_list || !"CAVi" %in% parameter_list || "AUCTAUi" %in% parameter_list)) {
      if(!"CMAXi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMAXi"
      }
      if(!"CMINi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMINi"
      }
      if(!"CAVi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CAVi"
      }
      if(!"AUCTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
      }
    }
    if("PTRi" %in% parameter_list && (!"CMAXi" %in% parameter_list || !"CMINi" %in% parameter_list)) {
      if(!"CMAXi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMAXi"
      }
      if(!"CMINi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMINi"
      }
    }
    if("VZFTAUi" %in% parameter_list && (!"KEL" %in% parameter_list || !"AUCTAUi" %in% parameter_list)) {
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"AUCTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
      }
    }
    if("VZFTAUWi" %in% parameter_list && (!"VZFTAUi" %in% parameter_list || !"KEL" %in% parameter_list || !"AUCTAUi" %in% parameter_list)) {
      if(!"VZFTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "VZFTAUi"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"AUCTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
      }
    }
  } else if (toupper(map_data$MODEL) == 'M2' && toupper(map_data$DOSINGTYPE) == 'SS'){
    if(optimize_kel){
      if(!"TMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TMAX"
      }
      if(!"TLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TLAST"
      }
      if(!"CMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMAX"
      }
      if(!"CLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CLAST"
      }
      if(!"AUCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
      }
    }
    if("CMAXC" %in% parameter_list && (!"CMAX" %in% parameter_list || !"TMAX" %in% parameter_list || !"KEL" %in% parameter_list)){
      if(!"CMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMAX"
      }
      if(!"TMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TMAX"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
    }
    if("CMAXCi" %in% parameter_list && (!"CMAXi" %in% parameter_list || !"TMAXi" %in% parameter_list || !"KEL" %in% parameter_list)){
      if(!"CMAXi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMAXi"
      }
      if(!"TMAXi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TMAXi"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
    }
    if("CMAXDNi" %in% parameter_list && !"CMAXi" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "CMAXi"
    }
    if("AUCDN" %in% parameter_list && !"AUCALL" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "AUCALL"
    }
    if("AUCALL" %in% parameter_list && !"TMAXi" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "TMAXi"
    }
    if("MRTLAST" %in% parameter_list && (!"AUCLAST" %in% parameter_list || !"AUMCLAST" %in% parameter_list)){
      if(!"AUCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
      }
      if(!"AUMCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCLAST"
      }
    }
    if("AUCXPCTO" %in% parameter_list && (!"AUCINFO" %in% parameter_list || !"AUCLAST" %in% parameter_list)){
      if(!"AUCINFO" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFO"
      }
      if(!"AUCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
      }
    }
    if("AUCXPCTP" %in% parameter_list && (!"AUCINFP" %in% parameter_list || !"AUCLAST" %in% parameter_list)){
      if(!"AUCINFP" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFP"
      }
      if(!"AUCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
      }
    }
    if("AUCLAST" %in% parameter_list && (!"TMAX" %in% parameter_list || !"TLAST" %in% parameter_list)){
      if(!"TMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TMAX"
      }
      if(!"TLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TLAST"
      }
    }
    if("AUCLASTDNi" %in% parameter_list && !"AUCLASTi" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "AUCLASTi"
    }
    if("MRTIVIFOi" %in% parameter_list && (!"AUCINFOi" %in% parameter_list || !"AUCTAUi" %in% parameter_list || !"AUMCTAUi" %in% parameter_list)){
      if(!"AUCINFOi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFOi"
      }
      if(!"AUCTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
      }
      if(!"AUMCTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCTAUi"
      }
    }
    if("AUCXPCTOi" %in% parameter_list && (!"AUCINFOi" %in% parameter_list || !"AUCLASTi" %in% parameter_list)){
      if(!"AUCINFOi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFOi"
      }
      if(!"AUCLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLASTi"
      }
    }
    if("VZO" %in% parameter_list && (!"KEL" %in% parameter_list || !"AUCINFOi" %in% parameter_list)) {
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"AUCINFOi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFOi"
      }
    }
    if("AUCINFOi" %in% parameter_list && (!"AUCLASTi" %in% parameter_list || !'CLASTi' %in% parameter_list || !"KEL" %in% parameter_list)) {
      if(!"AUCLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLASTi"
      }
      if(!"CLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CLASTi"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
    }
    if("MRTIVIFPi" %in% parameter_list && (!"AUCINFPi" %in% parameter_list || !"AUCTAUi" %in% parameter_list || !"AUMCTAUi" %in% parameter_list)){
      if(!"AUCINFPi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFPi"
      }
      if(!"AUCTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
      }
      if(!"AUMCTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCTAUi"
      }
    }
    if("AUCXPCTPi" %in% parameter_list && (!"AUCINFPi" %in% parameter_list || !"AUCLASTi" %in% parameter_list)){
      if(!"AUCINFPi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFPi"
      }
      if(!"AUCLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLASTi"
      }
    }
    if("VZP" %in% parameter_list && (!"KEL" %in% parameter_list || !"AUCINFPi" %in% parameter_list)) {
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"AUCINFPi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFPi"
      }
    }
    if("AUCINFPi" %in% parameter_list && (!"AUCLASTi" %in% parameter_list || !'TLASTi' %in% parameter_list || !"KEL" %in% parameter_list)) {
      if(!"AUCLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLASTi"
      }
      if(!"TLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TLASTi"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
    }
    if("MRTLASTi" %in% parameter_list && (!"AUCLASTi" %in% parameter_list || !"AUMCLASTi" %in% parameter_list)){
      if(!"AUCLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLASTi"
      }
      if(!"AUMCLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCLASTi"
      }
    }
    if("AUCLASTi" %in% parameter_list && (!"TMAXi" %in% parameter_list || !"TLASTi" %in% parameter_list)){
      if(!"TMAXi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TMAXi"
      }
      if(!"TLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TLASTi"
      }
    }
    if("AUCT" %in% parameter_list && !"TMAXi" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "TMAXi"
    }
    if("AUCT1_T2" %in% parameter_list && !"TMAXi" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "TMAXi"
    }
    if("AUCINFOC" %in% parameter_list && (!"KEL" %in% parameter_list || !"AUCINFO" %in% parameter_list)){
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"AUCINFO" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFO"
      }
    }
    if("AUCINFODN" %in% parameter_list && !"AUCINFO" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "AUCINFO"
    }
    if("AUCINFO" %in% parameter_list && (!"AUCLAST" %in% parameter_list || !'CLAST' %in% parameter_list || !"KEL" %in% parameter_list)) {
      if(!"AUCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
      }
      if(!"CLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CLAST"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
    }
    if("AUCINFPC" %in% parameter_list && (!"KEL" %in% parameter_list || !"AUCINFP" %in% parameter_list)){
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"AUCINFP" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFP"
      }
    }
    if("AUCINFPDN" %in% parameter_list && !"AUCINFP" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AUCINFP"
    }
    if("AUCINFP" %in% parameter_list && (!"AUCLAST" %in% parameter_list || !'TLAST' %in% parameter_list || !"KEL" %in% parameter_list)) {
      if(!"AUCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
      }
      if(!"TLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TLAST"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
    }
    if("AUCTAUDNi" %in% parameter_list && !"AUCTAUi" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
    }
    if("AUCTAUi" %in% parameter_list && !"TMAXi" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "TMAXi"
    }
    if("AUMCXPTO" %in% parameter_list && (!"AUMCINFO" %in% parameter_list || !"AUMCLAST" %in% parameter_list)){
      if(!"AUMCINFO" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCINFO"
      }
      if(!"AUMCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCLAST"
      }
    }
    if("AUMCXPTOi" %in% parameter_list && (!"AUMCINFOi" %in% parameter_list || !"AUMCLAST" %in% parameter_list)){
      if(!"AUMCINFOi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCINFOi"
      }
      if(!"AUMCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCLAST"
      }
    }
    if("AUMCXPTP" %in% parameter_list && (!"AUMCINFP" %in% parameter_list || !"AUMCLAST" %in% parameter_list)){
      if(!"AUMCINFP" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCINFP"
      }
      if(!"AUMCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCLAST"
      }
    }
    if("AUMCXPTPi" %in% parameter_list && (!"AUMCINFPi" %in% parameter_list || !"AUMCLAST" %in% parameter_list)){
      if(!"AUMCINFPi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCINFPi"
      }
      if(!"AUMCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCLAST"
      }
    }
    if("PTFi" %in% parameter_list && !"CMAXi" %in% parameter_list && !"CMINi" %in% parameter_list && !"CAVi" %in% parameter_list && "AUCTAUi" %in% parameter_list) {
      if(!"CMAXi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMAXi"
      }
      if(!"CMINi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMINi"
      }
      if(!"CAVi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CAVi"
      }
      if(!"AUCTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
      }
    }
    if("CAVi" %in% parameter_list && !"AUCTAUi" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
    }
    if("CLTAUWi" %in% parameter_list && (!"CLTAUi" %in% parameter_list || !"AUCTAUi" %in% parameter_list)) {
      if(!"CLTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CLTAUi"
      }
      if(!"AUCTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
      }
    }
    if("CLTAUi" %in% parameter_list && !"AUCTAUi" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
    }
    if("PTRi" %in% parameter_list && (!"CMAXi" %in% parameter_list || !"CMINi" %in% parameter_list)) {
      if(!"CMAXi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMAXi"
      }
      if(!"CMINi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMINi"
      }
    }
  } else if (toupper(map_data$MODEL) == 'M3' && toupper(map_data$DOSINGTYPE) == 'SD'){
    if(optimize_kel){
      if(!"TMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TMAX"
      }
      if(!"TLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TLAST"
      }
      if(!"CMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMAX"
      }
      if(!"CLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CLAST"
      }
      if(!"AUCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
      }
    }
    if("CMAXC" %in% parameter_list && (!"CMAX" %in% parameter_list || !"KEL" %in% parameter_list)) {
      if(!"CMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMAX"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
    }
    if("CMAXDN" %in% parameter_list && !"CMAX" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "CMAX"
    }
    if("AUCDN" %in% parameter_list && !"AUCALL" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "AUCALL"
    }
    if("AUCT" %in% parameter_list && !"TMAX" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "TMAX"
    }
    if("AUCT1_T2" %in% parameter_list && !"TMAX" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "TMAX"
    }
    if("AUCLASTCi" %in% parameter_list && (!"AUCLAST" %in% parameter_list || !"KEL" %in% parameter_list || !"TLAST" %in% parameter_list)){
      if(!"AUCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"TLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TLAST"
      }
    }
    if("AUCLASTDN" %in% parameter_list && !"AUCLAST" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
    }
    if("AUCINFOC" %in% parameter_list && (!"KEL" %in% parameter_list || !"AUCINFO" %in% parameter_list)){
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"AUCINFO" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFO"
      }
    }
    if("AUCINFODN" %in% parameter_list && !"AUCINFO" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "AUCINFO"
    }
    if("AUCINFPC" %in% parameter_list && (!"KEL" %in% parameter_list || !"AUCINFP" %in% parameter_list)){
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"AUCINFP" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFP"
      }
    }
    if("AUCINFPDN" %in% parameter_list && !"AUCINFP" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AUCINFP"
    }
    if("CLOW" %in% parameter_list && !"CLO" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "CLO"
    }
    if("CLO" %in% parameter_list && !"AUCINFO" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AUCINFO"
    }
    if("CLPW" %in% parameter_list && !"CLP" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "CLP"
    }
    if("CLP" %in% parameter_list && !"AUCINFP" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AUCINFP"
    }
    if("VZOW" %in% parameter_list && !"VZO" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "VZO"
    }
    if("VZO" %in% parameter_list && (!"AUCINFO" %in% parameter_list || !"KEL" %in% parameter_list)) {
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"AUCINFO" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFO"
      }
    }
    if("VZPW" %in% parameter_list && !"VZP" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "VZP"
    }
    if("VZP" %in% parameter_list && (!"AUCINFP" %in% parameter_list || !"KEL" %in% parameter_list)) {
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"AUCINFP" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFP"
      }
    }
  } else if(toupper(map_data$MODEL) == 'M3' && toupper(map_data$DOSINGTYPE) == 'SS'){
    if(optimize_kel){
      if(!"TMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TMAX"
      }
      if(!"TLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TLAST"
      }
      if(!"CMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMAX"
      }
      if(!"CLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CLAST"
      }
      if(!"AUCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
      }
    }
    if("CMAXC" %in% parameter_list && (!"CMAX" %in% parameter_list || !"TMAX" %in% parameter_list || !"KEL" %in% parameter_list)){
      if(!"CMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMAX"
      }
      if(!"TMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TMAX"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
    }
    if("CMAXCi" %in% parameter_list && (!"CMAXi" %in% parameter_list || !"TMAXi" %in% parameter_list || !"KEL" %in% parameter_list)){
      if(!"CMAXi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMAXi"
      }
      if(!"TMAXi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TMAXi"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
    }
    if("CMAXDNi" %in% parameter_list && !"CMAXi" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "CMAXi"
    }
    if("AUCDN" %in% parameter_list && !"AUCALL" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "AUCALL"
    }
    if("AUCALL" %in% parameter_list && !"TMAXi" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "TMAXi"
    }
    if("MRTLAST" %in% parameter_list && (!"AUCLAST" %in% parameter_list || !"AUMCLAST" %in% parameter_list)){
      if(!"AUCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
      }
      if(!"AUMCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCLAST"
      }
    }
    if("AUCXPCTO" %in% parameter_list && (!"AUCINFO" %in% parameter_list || !"AUCLAST" %in% parameter_list)){
      if(!"AUCINFO" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFO"
      }
      if(!"AUCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
      }
    }
    if("AUCXPCTP" %in% parameter_list && (!"AUCINFP" %in% parameter_list || !"AUCLAST" %in% parameter_list)){
      if(!"AUCINFP" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFP"
      }
      if(!"AUCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
      }
    }
    if("AUCLAST" %in% parameter_list && (!"TMAX" %in% parameter_list || !"TLAST" %in% parameter_list)){
      if(!"TMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TMAX"
      }
      if(!"TLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TLAST"
      }
    }
    if("AUCLASTDNi" %in% parameter_list && !"AUCLASTi" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "AUCLASTi"
    }
    if("MRTIVIFOi" %in% parameter_list && (!"AUCINFOi" %in% parameter_list || !"AUCTAUi" %in% parameter_list || !"AUMCTAUi" %in% parameter_list)){
      if(!"AUCINFOi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFOi"
      }
      if(!"AUCTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
      }
      if(!"AUMCTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCTAUi"
      }
    }
    if("AUCXPCTOi" %in% parameter_list && (!"AUCINFOi" %in% parameter_list || !"AUCLASTi" %in% parameter_list)){
      if(!"AUCINFOi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFOi"
      }
      if(!"AUCLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLASTi"
      }
    }
    if("VZO" %in% parameter_list && (!"KEL" %in% parameter_list || !"AUCINFOi" %in% parameter_list)) {
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"AUCINFOi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFOi"
      }
    }
    if("AUCINFOi" %in% parameter_list && (!"AUCLASTi" %in% parameter_list || !'CLASTi' %in% parameter_list || !"KEL" %in% parameter_list)) {
      if(!"AUCLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLASTi"
      }
      if(!"CLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CLASTi"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
    }
    if("MRTIVIFPi" %in% parameter_list && (!"AUCINFPi" %in% parameter_list || !"AUCTAUi" %in% parameter_list || !"AUMCTAUi" %in% parameter_list)){
      if(!"AUCINFPi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFPi"
      }
      if(!"AUCTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
      }
      if(!"AUMCTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCTAUi"
      }
    }
    if("AUCXPCTPi" %in% parameter_list && (!"AUCINFPi" %in% parameter_list || !"AUCLASTi" %in% parameter_list)){
      if(!"AUCINFPi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFPi"
      }
      if(!"AUCLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLASTi"
      }
    }
    if("VZP" %in% parameter_list && (!"KEL" %in% parameter_list || !"AUCINFPi" %in% parameter_list)) {
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"AUCINFPi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFPi"
      }
    }
    if("AUCINFPi" %in% parameter_list && (!"AUCLASTi" %in% parameter_list || !'TLASTi' %in% parameter_list || !"KEL" %in% parameter_list)) {
      if(!"AUCLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLASTi"
      }
      if(!"TLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TLASTi"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
    }
    if("MRTLASTi" %in% parameter_list && (!"AUCLASTi" %in% parameter_list || !"AUMCLASTi" %in% parameter_list)){
      if(!"AUCLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLASTi"
      }
      if(!"AUMCLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCLASTi"
      }
    }
    if("AUCLASTi" %in% parameter_list && (!"TMAXi" %in% parameter_list || !"TLASTi" %in% parameter_list)){
      if(!"TMAXi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TMAXi"
      }
      if(!"TLASTi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TLASTi"
      }
    }
    if("AUCT" %in% parameter_list && !"TMAXi" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "TMAXi"
    }
    if("AUCT1_T2" %in% parameter_list && !"TMAXi" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "TMAXi"
    }
    if("AUCINFOC" %in% parameter_list && (!"KEL" %in% parameter_list || !"AUCINFO" %in% parameter_list)){
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"AUCINFO" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFO"
      }
    }
    if("AUCINFODN" %in% parameter_list && !"AUCINFO" %in% parameter_list){
      parameter_list[[length(parameter_list)+1]] <- "AUCINFO"
    }
    if("AUCINFO" %in% parameter_list && (!"AUCLAST" %in% parameter_list || !'CLAST' %in% parameter_list || !"KEL" %in% parameter_list)) {
      if(!"AUCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
      }
      if(!"CLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CLAST"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
    }
    if("AUCINFPC" %in% parameter_list && (!"KEL" %in% parameter_list || !"AUCINFP" %in% parameter_list)){
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
      if(!"AUCINFP" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCINFP"
      }
    }
    if("AUCINFPDN" %in% parameter_list && !"AUCINFP" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AUCINFP"
    }
    if("AUCINFP" %in% parameter_list && (!"AUCLAST" %in% parameter_list || !'TLAST' %in% parameter_list || !"KEL" %in% parameter_list)) {
      if(!"AUCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
      }
      if(!"TLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TLAST"
      }
      if(!"KEL" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "KEL"
      }
    }
    if("AUCTAUDNi" %in% parameter_list && !"AUCTAUi" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
    }
    if("AUCTAUi" %in% parameter_list && !"TMAXi" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "TMAXi"
    }
    if("AUMCXPTO" %in% parameter_list && (!"AUMCINFO" %in% parameter_list || !"AUMCLAST" %in% parameter_list)){
      if(!"AUMCINFO" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCINFO"
      }
      if(!"AUMCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCLAST"
      }
    }
    if("AUMCXPTOi" %in% parameter_list && (!"AUMCINFOi" %in% parameter_list || !"AUMCLAST" %in% parameter_list)){
      if(!"AUMCINFOi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCINFOi"
      }
      if(!"AUMCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCLAST"
      }
    }
    if("AUMCXPTP" %in% parameter_list && (!"AUMCINFP" %in% parameter_list || !"AUMCLAST" %in% parameter_list)){
      if(!"AUMCINFP" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCINFP"
      }
      if(!"AUMCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCLAST"
      }
    }
    if("AUMCXPTPi" %in% parameter_list && (!"AUMCINFPi" %in% parameter_list || !"AUMCLAST" %in% parameter_list)){
      if(!"AUMCINFPi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCINFPi"
      }
      if(!"AUMCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUMCLAST"
      }
    }
    if("PTFi" %in% parameter_list && !"CMAXi" %in% parameter_list && !"CMINi" %in% parameter_list && !"CAVi" %in% parameter_list && "AUCTAUi" %in% parameter_list) {
      if(!"CMAXi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMAXi"
      }
      if(!"CMINi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMINi"
      }
      if(!"CAVi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CAVi"
      }
      if(!"AUCTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
      }
    }
    if("CAVi" %in% parameter_list && !"AUCTAUi" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
    }
    if("CLTAUWi" %in% parameter_list && (!"CLTAUi" %in% parameter_list || !"AUCTAUi" %in% parameter_list)) {
      if(!"CLTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CLTAUi"
      }
      if(!"AUCTAUi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
      }
    }
    if("CLTAUi" %in% parameter_list && !"AUCTAUi" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AUCTAUi"
    }
    if("PTRi" %in% parameter_list && (!"CMAXi" %in% parameter_list || !"CMINi" %in% parameter_list)) {
      if(!"CMAXi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMAXi"
      }
      if(!"CMINi" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMINi"
      }
    }
  } else if (toupper(map_data$MODEL) == 'M4' && toupper(map_data$DOSINGTYPE) == 'SD'){
    if(optimize_kel){
      if(!"TMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TMAX"
      }
      if(!"TLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TLAST"
      }
      if(!"CMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMAX"
      }
      if(!"CLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CLAST"
      }
      if(!"AUCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
      }
    }
    if("AEPCT" %in% parameter_list && !"AE" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AE"
    }
    if("AETPCT" %in% parameter_list && !"AET" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AET"
    }
  } else if(toupper(map_data$MODEL) == 'M4' && toupper(map_data$DOSINGTYPE) == 'SS'){
    if(optimize_kel){
      if(!"TMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TMAX"
      }
      if(!"TLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "TLAST"
      }
      if(!"CMAX" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CMAX"
      }
      if(!"CLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "CLAST"
      }
      if(!"AUCLAST" %in% parameter_list){
        parameter_list[[length(parameter_list)+1]] <- "AUCLAST"
      }
    }
    if("AEPCT" %in% parameter_list && !"AE" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AE"
    }
    if("AETPCT" %in% parameter_list && !"AET" %in% parameter_list) {
      parameter_list[[length(parameter_list)+1]] <- "AET"
    }
  }
}    
###
###  cat('MODEL: ', toupper(map_data$MODEL), ' DOSINGTYPE: ', toupper(map_data$DOSINGTYPE), ' parameter_list: \n')
###  print(unlist(parameter_list))
#  yparameter_list <- unlist(parameter_list)
#  print(yparameter_list)
#  k <- setdiff(yparameter_list, xparameter_list)
#  print(k)
#  j <- setdiff(xparameter_list, yparameter_list)
#  print(j)
###  stop("here")
  
  if("RETURNCOLS" %in% names(map_data)){
    if(!is.null(map_data$RETURNCOLS) && !is.na(map_data$RETURNCOLS) && map_data$RETURNCOLS != ""){
      return_list <- as.list(strsplit(map_data$RETURNCOLS, ";")[[1]])
    } else {
      return_list <- list()
### 2019-08-15/TGT/ Change wording
###      warning("'RETURNCOLS' values provided via 'map' is not used for this computation")
      warning("'RETURNCOLS' values provided via 'map' are not used for this computation")
    }
  } else {
    return_list <- list()
    warning("Dataset provided via 'map' does not contain the 'RETURNCOLS' column")
  }
  
  if(parameterset=="PARAMETERDISPLAYLIST") { 
    if("DATADISPLAYLIST" %in% names(map_data)){
      if(!is.null(map_data$DATADISPLAY) && !is.na(map_data$DATADISPLAYLIST) && map_data$DATADISPLAYLIST != ""){
        if(length(return_list) > 0){
          return_list <- as.list(c(strsplit(map_data$DATADISPLAYLIST, ";")[[1]], unlist(return_list)))
        } else {
          return_list <- as.list(strsplit(map_data$DATADISPLAYLIST, ";")[[1]])
        }
      } else {
        warning("'DATADISPLAYLIST' values provided via 'map' are not used for this computation")
      }
    } else {
      warning("Dataset provided via 'map' does not contain the 'DATADISPLAYLIST' column")
    }
  }

if(FALSE) {  
  if("PARAMETERDISPLAYLIST" %in% names(map_data)){
    if(!is.null(map_data$PARAMETERDISPLAYLIST) && !is.na(map_data$PARAMETERDISPLAYLIST)){
      display_list <- as.list(strsplit(map_data$PARAMETERDISPLAYLIST, ";")[[1]])
      display_check <- FALSE

      if(toupper(map_data$MODEL) == 'M1' && toupper(map_data$DOSINGTYPE) == 'SD'){
        display_check <- ifelse(sum(display_list %in% m1sd_param_list) > 0, TRUE, FALSE)
      } else if(toupper(map_data$MODEL) == 'M1' && toupper(map_data$DOSINGTYPE) == 'SS'){
        display_check <- ifelse(sum(display_list %in% m1ss_param_list) > 0, TRUE, FALSE)
      } else if (toupper(map_data$MODEL) == 'M2' && toupper(map_data$DOSINGTYPE) == 'SD'){
        display_check <- ifelse(sum(display_list %in% m2sd_param_list) > 0, TRUE, FALSE)
      } else if (toupper(map_data$MODEL) == 'M2' && toupper(map_data$DOSINGTYPE) == 'SS'){
        display_check <- ifelse(sum(display_list %in% m2ss_param_list) > 0, TRUE, FALSE)
      } else if (toupper(map_data$MODEL) == 'M3' && toupper(map_data$DOSINGTYPE) == 'SD'){
        display_check <- ifelse(sum(display_list %in% m3sd_param_list) > 0, TRUE, FALSE)
      } else if(toupper(map_data$MODEL) == 'M3' && toupper(map_data$DOSINGTYPE) == 'SS'){
        display_check <- ifelse(sum(display_list %in% m3ss_param_list) > 0, TRUE, FALSE)
      } else if (toupper(map_data$MODEL) == 'M4' && toupper(map_data$DOSINGTYPE) == 'SD'){
        display_check <- ifelse(sum(display_list %in% m4sd_param_list) > 0, TRUE, FALSE)
      } else if(toupper(map_data$MODEL) == 'M4' && toupper(map_data$DOSINGTYPE) == 'SS'){
        display_check <- ifelse(sum(display_list %in% m4ss_param_list) > 0, TRUE, FALSE)
      }

      if(!display_check){
        display_list <- list()
        warning("'PARAMETERDISPLAYLIST' values provided via 'map' does not contain any values that are present in the input dataset!")
      }
    } else {
      display_list <- list()
      warning("'PARAMETERDISPLAYLIST' values provided via 'map' is not used for this computation")
    }
  } else {
    display_list <- list()
    warning("Dataset provided via 'map' does not contain the 'PARAMETERDISPLAYLIST' column")
  }
}
  
### 2019-08-27/TGT/ need to update this since it's creating PKPTMSU rather than using the value of PKPTMU  
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

### 2019-08-27/TGT/ following no longer needed with validate_timeconc_data being used  
###  if(map_data$TIME == 'NOMTIME'){
###    map_data$TIME <- "Nominal"
###  } else if(map_data$TIME == 'ACTTIME'){
###    map_data$TIME <- "Actual"
###  }

### If PARAMETERDISPLAYLIST is defined in map, use that otherwise use the full model_parameters list
  if(is.null(map_data$PARAMETERDISPLAYLIST)) { display_list <- model_parameters(model_regex) }
  else { display_list <- as.list(strsplit(map_data$PARAMETERDISPLAYLIST, ";")[[1]]) }
  
### 2019-08-15/TGT/ Use parameterset value to direct which set of parameters will be produced
  if(parameterset=="PARAMETERDISPLAYLIST") { 
###      display_list <- display_list
      parameter_list <- display_list
  }

  parameter_list <- as.list(parameter_list)
###  print(parameter_list)
###  display_list <- parameter_list
  
  if(toupper(map_data$MODEL) == 'M1' && toupper(map_data$DOSINGTYPE) == 'SD'){
### Remove display_list, incorporate model_regex
###    data_out <- run_M1_SD_computation(data = merged_data, map = map_data, method = method,
###                                      parameter_list = parameter_list, display_list = display_list, return_list = return_list)
    data_out <- run_M1_SD_computation(data = merged_data, map = map_data, method = method, model_regex = model_regex,
                                      parameter_list = parameter_list, return_list = return_list)
  } else if(toupper(map_data$MODEL) == 'M1' && toupper(map_data$DOSINGTYPE) == 'SS'){
### Remove display_list, incorporate model_regex
###    data_out <- run_M1_SS_computation(data = merged_data, map = map_data, method = method,
###                                      parameter_list = parameter_list, display_list = display_list, return_list = return_list)
    data_out <- run_M1_SS_computation(data = merged_data, map = map_data, method = method, model_regex = model_regex,
                                      parameter_list = parameter_list, return_list = return_list)
  } else if (toupper(map_data$MODEL) == 'M2' && toupper(map_data$DOSINGTYPE) == 'SD'){
### Remove display_list, incorporate model_regex
###    data_out <- run_M2_SD_computation(data = merged_data, map = map_data, method = method
###                                      parameter_list = parameter_list, display_list = display_list, return_list = return_list)
    data_out <- run_M2_SD_computation(data = merged_data, map = map_data, method = method, model_regex = model_regex,
                                      parameter_list = parameter_list, return_list = return_list)
  } else if (toupper(map_data$MODEL) == 'M2' && toupper(map_data$DOSINGTYPE) == 'SS'){
### Remove display_list, incorporate model_regex
###    data_out <- run_M2_SS_computation(data = merged_data, map = map_data, method = method
###                                      parameter_list = parameter_list, display_list = display_list, return_list = return_list)
    data_out <- run_M2_SS_computation(data = merged_data, map = map_data, method = method, model_regex = model_regex,
                                      parameter_list = parameter_list, return_list = return_list)
  } else if (toupper(map_data$MODEL) == 'M3' && toupper(map_data$DOSINGTYPE) == 'SD'){
### Remove display_list, incorporate model_regex
###    data_out <- run_M3_SD_computation(data = merged_data, map = map_data, method = method
###                                      parameter_list = parameter_list, display_list = display_list, return_list = return_list)
    data_out <- run_M3_SD_computation(data = merged_data, map = map_data, method = method, model_regex = model_regex,
                                      parameter_list = parameter_list, return_list = return_list)
  } else if(toupper(map_data$MODEL) == 'M3' && toupper(map_data$DOSINGTYPE) == 'SS'){
### Remove display_list, incorporate model_regex
###    data_out <- run_M3_SS_computation(data = merged_data, map = map_data, method = method
###                                      parameter_list = parameter_list, display_list = display_list, return_list = return_list)
    data_out <- run_M3_SS_computation(data = merged_data, map = map_data, method = method, model_regex = model_regex,
                                      parameter_list = parameter_list, return_list = return_list)
  } else if (toupper(map_data$MODEL) == 'M4' && toupper(map_data$DOSINGTYPE) == 'SD'){
### Remove display_list, incorporate model_regex
###    data_out <- run_M4_SD_computation(data = merged_data, map = map_data, method = method
###                                      parameter_list = parameter_list, display_list = display_list, return_list = return_list)
    data_out <- run_M4_SD_computation(data = merged_data, map = map_data, method = method, model_regex = model_regex,
                                      parameter_list = parameter_list, return_list = return_list)
  } else if(toupper(map_data$MODEL) == 'M4' && toupper(map_data$DOSINGTYPE) == 'SS'){
### Remove display_list, incorporate model_regex
###    data_out <- run_M4_SS_computation(data = merged_data, map = map_data, method = method
###                                      parameter_list = parameter_list, display_list = display_list, return_list = return_list)
    data_out <- run_M4_SS_computation(data = merged_data, map = map_data, method = method, model_regex = model_regex,
                                      parameter_list = parameter_list, return_list = return_list)
  }
###  cat('in run_computation.r\n')
### 2019-09-19/TGT/ Remove CONC/CONCTIME from PARAMETERDISPLAYLIST Results
  if(parameterset=="PARAMETERDISPLAYLIST") { 
      k <- grep("^CONC((TIME)*?)[0-9]*?$", names(data_out$data_out), ignore.case=TRUE, perl=TRUE)
      if(length(k)>0) {
        k <- names(data_out$data_out)[k]
        data_out$data_out <- data_out$data_out[,-match(k, names(data_out$data_out))]
      }
  }
  
###  print(head(data_out))
  
  results_list <- list()
  if(optimize_kel && toupper(map_data$MODEL) != 'M4'){
    if("KEL" %in% parameter_list){
      results_list$data_out <- data_out$data_out
      results_list$flag_data <- flag_data
      if(!is.null(data_out$optimized_kel_flag)){
        results_list$flag_data[,map_data$FLGEXKEL] <- data_out$optimized_kel_flag
      }
      results_list$est_data <- data_out$est_data
    } else {
      results_list$data_out <- data_out$data_out
      results_list$flag_data <- flag_data
      if(!is.null(data_out$optimized_kel_flag)){
        results_list$flag_data[,map_data$FLGEXKEL] <- data_out$optimized_kel_flag
      }
    }
  } else {
    if("KEL" %in% parameter_list){
      results_list$data_out <- data_out$data_out
      results_list$flag_data <- flag_data
      results_list$est_data <- data_out$est_data
    } else {
      results_list$data_out <- data_out$data_out
      results_list$flag_data <- flag_data
    }
  }
###  print(head(results_list$data_out))

###  if(casefold(map_data$TIME) == "nominal") {
###    vartime <- map_data$NOMTIME
###  } else if(casefold(map_data$TIME) == "actual") {
###    vartime <- map_data$ACTTIME
###  } else {
###    vartime <- map_data$TIME
###  }

### 2019-09-19/TGT/ convert factors to character
  results_list$data_out <- lapply(results_list$data_out, function(x) {
      if(is.factor(x)) {
        x <- as.character(x)
      }
      return(x)
    }
  )
  results_list$data_out <- as.data.frame(results_list$data_out, stringsAsFactors=FALSE)
  
  elist <- c("PKDATAROWID","SDEID","TIME","CEST_KEL","CEST_INT","CEST_EXT","CEST_C0","CEST_TLAST")
  est_data <- flag_data[flag_data$FLGEXKEL==0,c("PKDATAROWID"), drop=FALSE] # Select rows to include in KEL computations to start with

  # Merge with concentration data
  vlist <- c("PKDATAROWID", "SDEID", map_data$TIME)
  est_data <- merge(x=est_data, y=data_data[,vlist], by="PKDATAROWID", all.x=TRUE)
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
###cat('data_out:\n')
###  print(head(results_list$data_out))
  return(results_list)
}


