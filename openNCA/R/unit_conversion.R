#' Run Computation
#'
#' This function will compute all the relevant parameters for based specified fields provided via map argument.\cr
#'
#' @param data The dataframe that contains the raw data
#' @param map The dataframe that contains the map data
#' @param result The dataframe that contains the computation engine result data
#'
#' @section Returns:
#' \strong{List} \cr
#' \itemize{
#'  \item data_out: Calculated default/specified M1SS Parameters
#'  \item result_data: Result Data used during calculation of parameters
#'  \item est_data: Calculated Estimated Parameters
#' }
#'
#' @examples
#'
#' @author
#' \itemize{
#'  \item \strong{Tom Tensfeldt, Pfizer Pharmacometrics & Rudraya Technical Team}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{thomas.g.tensfeldt@pfizer.com}
#' }
#' @export
unit_conversion <- function(data = NULL, map = NULL, result = NULL, unit_class = "ALL", verbose=TRUE){
  function_name <- as.list(sys.call())[[1]]

### remove this list...
###  TIMEUPARAM <- c("TOLDi", "TMAXi", "TMINi", "TLAST", "TLAG", "KELTMLO", "KELTMHI", "THALF", "MRTIVIFP", "MRTIVIFO",
###                  "MRTEVIFP", "MRTP", "MRTEVIFO", "MRTP", "MRTLAST", "TMAXRATEi", "MIDPTLASTi", "DOF")
###  AMOUNTUPARAM <- c("AT","AET","AE","AETAUi")
###  DOSEUPARAM  <- c("DOSEi", "DOSEC")
###  VOLUMEUPARAM <- c("VZFO", "VZFP", "VSSO", "VSSPi", "VZO", "VSP", "VOLSUM", "V0", "VZTAUi")
###  CONCUPARAM <- c("CMAXi", "CMAXCi", "CMAXiN", "CMINi", "CLASTi", "CTROUGHi", "CTROUGHENDi", "CAVi", "C0", "KELC0")
###  KELUPARAM	<- c("KEL")
###  CLUPARAM <- c("CLFO", "CLFP", "CLO", "CLP", "CLFTAUi", "CLR", "CLRTAUi", "CLRT", "CLRTAUi")
###  AUCUPARAM	<- c("AUCT", "AUCT1_T2", "AUCALL", "AUCLAST", "AUCLASTCi", "AUCINFO", "AUCINFOC", "AUCINFP", "AUCINFPC", "AUCTAUi")
###  AUMCUPARAM <- c("AUMCLAST", "AUMCINFO", "AUMCINFP", "AUMCTAUi")
###  AUCNORMUPARAM <- c("AUCTDN", "AUCLASTDNi", "AUCINFODN", "AUCINFPDN", "AUCTAUDNi")
###  AURCUPARAM <- c("AURCALL", "AURCLAST", "AURCINFO", "AURCINFP", "AURCT1_T2")
###  CONCNORMUPARAM  <- c("CMAXDN")
###  RATEUPARAM <- c("MAXRATEi", "RATELATEi", "RATEA", "RATEN")
###  VOLUMEWUPARAM <- c("VZFOW", "VZFPW", "VZFTAUWi", "VSSOW", "VSSPWi", "VZOW", "VZPW", "VZTAUW")
###  CLWUPARAM <- c("CLFOW", "CLFPW", "CLFTAUWi", "CLOW", "CLPW", "CLTAUWi")

###  u_class  <- c("TIMEU", "AMOUNTU", "DOSEU", "VOLUMEU", "CONCU", "ALL")
  u_class  <- c("TIMEU", "AMOUNTU", "DOSEU", "VOLUMEU", "CONCU", "KELU", "CLU", "AUCU", "AUMCU", "AUCNORMU", "AURCU", "CONCNORMU", "RATEU", "VOLUMEWU", "CLWU", "ALL")

### Elemental Unit Types
  units    <- c("HR", "MIN", "KG", "GM", "DPM", "ngeq",  "DG", "CG", "MG", "UG", "MCG", "NG",  "PG",  "FG", "KL", "L", "DL", "CL", "ML", "UL", "NL",  "PL",  "FL",        "")
  val      <- c(  60,     1,  1e3,    1,     1,      1,  1e-1, 1e-2, 1e-3, 1e-6,  1e-6, 1e-9, 1e-12, 1e-15,  1e3,   1, 1e-1, 1e-2, 1e-3, 1e-6, 1e-9, 1e-12, 1e-15,        NA)
  class    <- c( "T",   "T",  "M",  "M",   "M",    "M",   "M",  "M",  "M",  "M",   "M",  "M",   "M",   "M",  "V", "V",  "V",  "V",  "V",  "V",  "V",   "V",   "V", "MISSING")

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
  if(is.null(result)){
    stop("Please provide a valid path for the 'result' parameter")
  } else {
    if(is.data.frame(result)){
      result_data <- result
    } else {
      if(file.exists(result)){
        result_data <- read.csv(file = result)
      } else {
        stop("Invalid path provided for 'result'! Please provide a valid path for the 'result' parameter")
      }
    }
  }
  if(is.null(unit_class)){
    stop("Please provide a valid value for the 'unit_class' parameter")
  } else {
    unit_class <- toupper(as.character(unit_class))
    if(!unit_class %in% u_class){
      stop("Invalid value provided for 'unit_class'! Please provide a valid value for the 'unit_class' parameter")
    }
  }

  #----------------------------------------------------------------------------  
  #Unit Class 1: Time
  if(unit_class == "TIMEU" || unit_class == "ALL"){
###    TIMEUPARAM <- c()
### 2019-10-01/TGT/ should not modify input data
###    concIdx <- grep("CONCTIME[0-9]+", names(result_data))
###    if(length(concIdx) > 0) {
###      TIMEUPARAM <- c(TIMEUPARAM, names(result_data[concIdx]))
###    }
###    toldIdx <- grep("TOLD[0-9]+", names(result_data))
###    if(length(toldIdx) > 0) {
###      TIMEUPARAM <- c(TIMEUPARAM, names(result_data[toldIdx]))
###    }
###    tminIdx <- grep("TMIN[0-9]+", names(result_data))
###    if(length(tminIdx) > 0) {
###      TIMEUPARAM <- c(TIMEUPARAM, names(result_data[tminIdx]))
###    }
###    tmaxIdx <- grep("TMAX[0-9]+", names(result_data))
###    if(length(tmaxIdx) > 0) {
###      TIMEUPARAM <- c(TIMEUPARAM, names(result_data[tmaxIdx]))
###    }

###    timeparameters <- unitclass_parameters("TIMEU")
###    y <- names(parameter_required(paste0("^",timeparameters,"$"), names(result_data), simplify = FALSE))
###    TIMEUPARAM <- c(TIMEUPARAM, y)

    TIMEUPARAM <- parameters_by_class("TIMEU", names(result_data))
      
### 2019-08-30/TGT/ Change from indirect reference to direct name, "TIMEU"
###    if(paste0(map_data$TIME, "U") %in% names(map_data)){
    if(length(TIMEUPARAM)>0 && parameter_required("^TIMEU$", names(map_data))){
### 2019-08-30/TGT/ Change from indirect reference
###      if(map_data[, paste0(map_data$TIME, "U")] %in% names(data_data)){
      if(parameter_required(map_data$TIMEU, names(data_data))){
###        inputUnit1 <- unique(data_data[, map_data[, paste0(map_data$TIME, "U")]])[1]
        inputUnit1 <- as.character(unique(data_data[, map_data$TIMEU])[1])
        outputUnitLabel <- "TIMEOUTPUTUNIT"
        testunit <- is.element(outputUnitLabel, names(map))
        outputUnitFormat <- FALSE
        if(testunit) {
            outputUnit1 <- as.character(map_data[[outputUnitLabel]])
            outputUnitFormat <- length(outputUnit1)>0 & !is.na(outputUnit1)
        }
        testunit <- testunit && outputUnitFormat

### Added formattedinputUnit and formattedoutputUnit to simplify output
        formattedinputUnit   <- inputUnit1
        formattedoutputUnit  <- outputUnit1
          
        if(testunit){
          inputMatch1 <- match(inputUnit1, units, nomatch = 21)
          outputMatch1 <- match(outputUnit1, units, nomatch = 21)

          if(inputMatch1 != 21 && outputMatch1 != 21) {
            inputMScale1 <- val[inputMatch1]
            outputMScale1 <- val[outputMatch1]
            if(class[inputMatch1] == "T" && class[outputMatch1] == "T") {
              timeUScaler <- inputMScale1/outputMScale1
            } else {
              timeUScaler <- 1
              warning(paste0("'", map_data$TIMEU, "' and/or '", outputUnitLabel, "' value provided via 'map' is not accounted for unit conversion"))
            }
          } else {
            timeUScaler <- 1
            warning(paste0("'", map_data$TIMEU, "' and/or '", outputUnitLabel, "' value provided via 'map' is not valid for unit conversion"))
          }

          time_col <- names(result_data)[names(result_data) %in% TIMEUPARAM]
          result_data[time_col] <- result_data[time_col] * timeUScaler
###          result_data$TIMEU <- ifelse(timeUScaler == 1, inputUnit1, outputUnit1)
          result_data$TIMEU <- ifelse(timeUScaler == 1, as.character(formattedinputUnit), as.character(formattedoutputUnit))
          if(verbose) { cat(function_name, ': Unit Class 1 (Time) time_col: ', time_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', timeUScaler, '\n') }
        } else {
### retain original input unit
###          result_data$TIMEU <- inputUnit1
          result_data$TIMEU <- formattedinputUnit
###          if(!is.na(map_data$TIMEOUTPUTUNIT)){
          if(!outputUnitFormat){
            warning("'", outputUnitLabel, "' is not present in the proper form! Please try again using 'Time' format!")
          }
        }
      } else {
        result_data$TIMEU <- NA
        warning(paste0("'", map_data$TIMEU, "' value provided via 'map' is not present in the dataset provided via 'data'"))
      }
    } else {
      result_data$TIMEU <- NA
      warning(paste0("'", map_data$TIMEU, "' #1# is not present in the dataset provided via 'map'"))
    }
  }

  #----------------------------------------------------------------------------  
  #Unit Class 2: Amount
  if(unit_class == "AMOUNTU" || unit_class == "ALL"){
###    AMOUNTUPARAM <- c()
###    amtIdx <- grep("AMT.[0-9]+.00", names(result_data))
###    if(length(amtIdx) > 0) {
###      AMOUNTUPARAM <- c(AMOUNTUPARAM, names(result_data[amtIdx]))
###    }
###    aeIdx <- grep("AE.[0-9]+.00", names(result_data))
###    if(length(aeIdx) > 0) {
###      AMOUNTUPARAM <- c(AMOUNTUPARAM, names(result_data[aeIdx]))
###    }
###
###    amountparameters <- unitclass_parameters("AMOUNTU")
###    y <- names(parameter_required(paste0("^",amountparameters,"$"), names(result_data), simplify = FALSE))
###    AMOUNTUPARAM <- c(AMOUNTUPARAM, y)

    AMOUNTUPARAM <- parameters_by_class("AMOUNTU", names(result_data))

### 2019-08-30/TGT/ update to parameter_required
###    if("AMOUNTU" %in% names(map_data)){
    if(length(AMOUNTUPARAM)>0 && parameter_required("^AMOUNTU$",names(map_data)) && parameter_required("^CONCU$",names(map_data))){
      if(parameter_required(map_data$AMOUNTU, names(data_data))){
###        inputUnit2 <- unique(data_data[, map_data$AMOUNTU])[1]
###        inputUnit2 <- unique(data_data[, map_data$CONCU])[1]
        conc_unit_tmp <- unlist(strsplit(as.character(unique(data_data[, map_data$CONCU])), "/"))
        if(length(grep("/", as.character(unique(data_data[, map_data$CONCU])))) > 0){
          inputUnit2 <- as.character(conc_unit_tmp[1])
        } else {
          inputUnit2 <- NA
        }

###        outputUnit2 <- as.character(map_data$AMOUNTOUTPUTUNIT)

        outputUnitLabel <- "AMOUNTOUTPUTUNIT"
        testunit <- is.element(outputUnitLabel, names(map))
        outputUnitFormat <- FALSE
        if(testunit) {
            outputUnit2 <- as.character(map_data[[outputUnitLabel]])
            outputUnitFormat <- length(outputUnit2)>0 & !is.na(outputUnit2)
        }
        testunit <- testunit && outputUnitFormat

### Added formattedinputUnit and formattedoutputUnit to simplify output
        formattedinputUnit   <- inputUnit2
        formattedoutputUnit  <- outputUnit2

        if(testunit){
          inputMatch2 <- match(inputUnit2, units, nomatch = 21)
          outputMatch2 <- match(outputUnit2, units, nomatch = 21)

          if(inputMatch2 != 21 && outputMatch2 != 21) {
            inputMScale2 <- val[inputMatch2]
            outputMScale2 <- val[outputMatch2]
            if(class[inputMatch2] == "M" && class[outputMatch2] == "M") {
              amountUScaler <- inputMScale2/outputMScale2
            } else {
              amountUScaler <- 1
              warning("'AMOUNTU' and/or '", outputUnitLabel, "' value provided via 'map' is not accounted for unit conversion")
            }
          } else {
            amountUScaler <- 1
            warning("'AMOUNTU' and/or '", outputUnitLabel, "' value provided via 'map' is not valid for unit conversion")
          }
###cat('amountUScaler: ', amountUScaler, '\n')
          amount_col <- names(result_data)[names(result_data) %in% AMOUNTUPARAM]
          result_data[amount_col] <- result_data[amount_col] * amountUScaler
###          result_data$AMOUNTU <- ifelse(amountUScaler == 1, inputUnit2, outputUnit2)
          result_data$AMOUNTU <- ifelse(amountUScaler == 1, as.character(formattedinputUnit), as.character(formattedoutputUnit))
          if(verbose) { cat(function_name, ': Unit Class 2 (Amount) amount_col: ', amount_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', amountUScaler, '\n') }
        } else {
### retain original input unit
###          result_data$AMOUNTU <- inputUnit2
          result_data$AMOUNTU <- formattedinputUnit
          if(outputUnitFormat){
            warning("'", outputUnitLabel, "' is not present in the proper form! Please try again using 'Amount' format!")
          }
        }
      } else {
        result_data$AMOUNTU <- NA
        warning("'AMOUNTU' value provided via 'map' is not present in the dataset provided via 'data'")
      }
    } else {
      result_data$AMOUNTU <- NA
      warning("'AMOUNTU' #2# is not present in the dataset provided via 'map'")
    }
  }

  #----------------------------------------------------------------------------  
  #Unit Class 3: Dose Amount
  if(unit_class == "DOSEU" || unit_class == "ALL"){
###    doseIdx <- grep("DOSE[0-9]+", names(result_data))
###    doseuIdx <- grep("DOSE[0-9]+U", names(result_data))
###    doseIdx <- doseIdx[!doseIdx %in% doseuIdx]
###    if(length(doseIdx) > 0) {
###      DOSEUPARAM <- c(DOSEUPARAM, names(result_data[doseIdx]))
###    }
###    interval_len <- length(doseIdx)

    DOSEUPARAM <- parameters_by_class("DOSEU", names(result_data))

    if(length(DOSEUPARAM)>0 && parameter_required("^DOSELIST$", names(map_data)) && parameter_required("^DOSEULIST$", names(map_data))){
      doselist <- names(parameter_indices("^DOSELIST$", names(map_data), simplify=FALSE))
      dosevar <- unlist(strsplit(map_data[,doselist], ";"))
      doseulist <- names(parameter_indices("^DOSEULIST$", names(map_data), simplify=FALSE))
      doseuvar <- unlist(strsplit(map_data[,doseulist], ";"))
###      cat('dosevar:', dosevar, ' class(dosevar): ', class(dosevar), '\n')
###      cat('doseuvar:', doseuvar, ' class(doseuvar): ', class(doseuvar), '\n')

      interval_len <- length(dosevar)
###      cat(function_name, ': dosevar: ', ' interval_len: ', interval_len, '\n')
###      print(dosevar)
##      dosevar <- map[,dosevar]
##      interval_len <- length(doseIdx)
    
      if(interval_len > 1){
###        if(length(doseIdx) > 1) {
###          DOSEUPARAM <- c(DOSEUPARAM, names(result_data[doseIdx]))
###        }

        DOSEUPARAM <- c(DOSEUPARAM, dosevar)
        
###        for(i in 1:interval_len){
        for(i in 1:length(doseuvar)){
###   2019-08-30/TGT/ update to parameter_required
###          if(paste0("DOSE", i, "U") %in% names(map_data)) {
###          if(parameter_required(paste0("^DOSE", i, "U$"),names(map_data))) {
          if(parameter_required(paste0("^", doseuvar[i], "$"),names(map_data))) {
###          if(map_data[, paste0("DOSE", i, "U")] %in% names(data_data)){
###          if(parameter_required(map_data[, paste0("DOSE", i, "U")], names(data_data))) {
            if(parameter_required(map_data[, doseuvar[i]], names(data_data))) {
###              inputUnit3 <- unique(data_data[, map_data[, paste0("DOSE", i, "U")]])[1]
              inputUnit3 <- as.character(unique(data_data[, map_data[, doseuvar[i]]])[1])
###                cat('i: ', i, ' inputUnit3: ', inputUnit3, '\n')
#              outputUnit3 <- as.character(map_data$DOSEOUTPUTUNIT)

              outputUnitLabel <- "DOSEOUTPUTUNIT"
              testunit <- is.element(outputUnitLabel, names(map))
              outputUnitFormat <- FALSE
              if(testunit) {
                  outputUnit3 <- as.character(map_data[[outputUnitLabel]])
                  outputUnitFormat <- length(outputUnit3)>0 & !is.na(outputUnit3)
              }
              testunit <- testunit && outputUnitFormat
            
### Added formattedinputUnit and formattedoutputUnit to simplify output
              formattedinputUnit   <- inputUnit3
              formattedoutputUnit  <- outputUnit3
###            cat('formattedinputUnit: ', formattedinputUnit, ' formattedoutputUnit: ', formattedoutputUnit, '\n')
            
              if(testunit){
                inputMatch3 <- match(inputUnit3, units, nomatch = 21)
                outputMatch3 <- match(outputUnit3, units, nomatch = 21)
  
                if(inputMatch3 != 21 && outputMatch3 != 21) {
                  inputMScale3 <- val[inputMatch3]
                  outputMScale3 <- val[outputMatch3]
                  if(class[inputMatch3] == "M" && class[outputMatch3] == "M") {
                    doseUScaler <- inputMScale3/outputMScale3
                  } else {
                    doseUScaler <- 1
###                    warning(paste0("'", paste0("DOSE", i, "U"), "' and/or '", outputUnitLabel, "' value provided via 'map' is not accounted for unit conversion"))
                    warning(paste0("'", i, "' and/or '", outputUnitLabel, "' value provided via 'map' is not accounted for unit conversion"))
                  }
                } else {
                  doseUScaler <- 1
###                  warning(paste0("'", paste0("DOSE", i, "U"), "' and/or '", outputUnitLabel, "' value provided via 'map' is not valid for unit conversion"))
                  warning(paste0("'", i, "' and/or '", outputUnitLabel, "' value provided via 'map' is not valid for unit conversion"))
                }
  
###                dose_col <- names(result_data)[names(result_data) %in% c(paste0("DOSE", i), paste0("DOSEC", i))]
                dose_col <- names(result_data)[names(result_data) %in% dosevar[i]]
                result_data[dose_col] <- result_data[dose_col] * doseUScaler
###                result_data[paste0("DOSE", i, "U")] <- ifelse(doseUScaler == 1, inputUnit3, outputUnit3)
###                result_data[paste0("DOSE", i, "U")] <- ifelse(doseUScaler == 1, formattedinputUnit, formattedoutputUnit)
                result_data[doseuvar[i]] <- ifelse(doseUScaler == 1, as.character(formattedinputUnit), as.character(formattedoutputUnit))
                if(verbose) { cat(function_name, ': Unit Class 3 (Dose) dose_col: ', dose_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', doseUScaler, '\n') }
              } else {
###   retain original input unit
###                  result_data$DOSEU <- formattedinputUnit
###                  result_data[paste0("DOSE", i, "U")] <- formattedinputUnit
                  result_data[doseuvar[i]] <- formattedinputUnit
                  if(outputUnitFormat){
                    warning("'", outputUnitLabel, "' is not present in the proper form! Please try again using 'Dose Amount' format!")
                  }
               }
            } else {
              #result_data[paste0("DOSE", i, "U")] <- unique(data_data[, map_data[, paste0("DOSE", i, "U")]])[1]
###              result_data[paste0("DOSE", i, "U")] <- NA
              result_data[dosevar[i]] <- NA
###              warning(paste0("'", paste0("DOSE", i, "U"), "' value provided via 'map' is not present in the dataset provided via 'data'"))
              warning(paste0("'", doseuvar[i], "' value provided via 'map' is not present in the dataset provided via 'data'"))
            }
          } else {
            #result_data[paste0("DOSE", i, "U")] <- unique(data_data[, map_data[, paste0("DOSE", i, "U")]])[1]
###            result_data[paste0("DOSE", i, "U")] <- NA
            result_data[doseuvar[i]] <- NA
###            warning(paste0("'", paste0("DOSE", i, "U"), "' #3# is not present in the dataset provided via 'map'"))
            warning(paste0("'", doseuvar[i], "' #3# is not present in the dataset provided via 'map'"))
          }
        }
###   2019-08-12/TGT/ modify check (Not sure at this time why CONCOUTPUTUNIT is being checked here. Need to verify this
###        if(!is.na(map_data$CONCOUTPUTUNIT)){
        if(!is.element("CONCOUTPUTUNIT", names(map))){
          warning("'CONCOUTPUTUNIT' is not present in the proper form! Please try again using 'Amount/Volume' format!")
        }
      } else {
###   2019-08-30/TGT/ update to parameter_required
###        if("DOSE1U" %in% names(map_data)){
###   change to "DOSEU"
###          if(parameter_required("^DOSE1U$", names(map_data))){
###          if(parameter_required("^DOSEU$", names(map_data))){
          if(parameter_required(doseuvar, names(map_data))){
###          if(map_data$DOSE1U %in% names(data_data)){
###          if(parameter_required(map_data$DOSEU, names(data_data))){
          if(parameter_required(map_data[,doseuvar], names(data_data))){
###            inputUnit3 <- unique(data_data[, map_data$DOSEU])[1]
            inputUnit3 <- as.character(unique(data_data[, map_data[,doseuvar]])[1])
###            outputUnit3 <- as.character(map_data$DOSEOUTPUTUNIT)
  
            outputUnitLabel <- "DOSEOUTPUTUNIT"
            testunit <- is.element(outputUnitLabel, names(map))
            outputUnitFormat <- FALSE
            if(testunit) {
                outputUnit3 <- as.character(map_data[[outputUnitLabel]])
                outputUnitFormat <- length(outputUnit3)>0 & !is.na(outputUnit3)
            }
            testunit <- testunit && outputUnitFormat
              
###   Added formattedinputUnit and formattedoutputUnit to simplify output
            formattedinputUnit   <- inputUnit3
            formattedoutputUnit  <- outputUnit3
###            cat('formattedinputUnit: ', formattedinputUnit, ' formattedoutputUnit: ', formattedoutputUnit, '\n')
            
            if(testunit){
              inputMatch3 <- match(inputUnit3, units, nomatch = 21)
              outputMatch3 <- match(outputUnit3, units, nomatch = 21)
              if(inputMatch3 != 21 && outputMatch3 != 21) {
                inputMScale3 <- val[inputMatch3]
                outputMScale3 <- val[outputMatch3]
                if(class[inputMatch3] == "M" && class[outputMatch3] == "M") {
                  doseUScaler <- inputMScale3/outputMScale3
                } else {
                  doseUScaler <- 1
                  warning("'DOSEU' and/or '", outputUnitLabel, "' value provided via 'map' is not accounted for unit conversion")
                }
              } else {
                doseUScaler <- 1
                warning("'DOSEU' and/or '", outputUnitLabel, "' value provided via 'map' is not valid for unit conversion")
              }
  
###              dose_col <- names(result_data)[names(result_data) %in% c("DOSE1", "DOSEC")]
              dose_col <- names(result_data)[names(result_data) %in% DOSEUPARAM]
              result_data[dose_col] <- result_data[dose_col] * doseUScaler
###              result_data$DOSE1U <- ifelse(doseUScaler == 1, inputUnit3, outputUnit3)
              result_data$DOSEU <- ifelse(doseUScaler == 1, as.character(formattedinputUnit), as.character(formattedoutputUnit))
              if(verbose) { cat(function_name, ': Unit Class 3 (Dose) dose_col: ', dose_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', doseUScaler, '\n') }
            } else {
###   retain original input unit
###               result_data$DOSE1U <- inputUnit3
                result_data$DOSEU <- formattedinputUnit
              if(outputUnitFormat){
                warning("'", outputUnitLabel, "' is not present in the proper form! Please try again using 'Dose Amount' format!")
              }
            }
          } else {
            result_data$DOSEU <- NA
            warning("'DOSEU' value provided via 'map' is not present in the dataset provided via 'data'")
          }
        } else {
          result_data$DOSEU <- NA
          warning("'DOSEU' #4# is not present in the dataset provided via 'map'")
        }
      }
    }
  }

  #----------------------------------------------------------------------------  
  #Unit Class 4: Volume
  if(unit_class == "VOLUMEU" || unit_class == "ALL"){
###    VOLUMEUPARAM <- c()
###    vsspIdx <- grep("VSSP[0-9]+", names(result_data))
###    if(length(vsspIdx) > 0) {
###      VOLUMEUPARAM <- c(VOLUMEUPARAM, names(result_data[vsspIdx]))
###    }
###    vztauIdx <- grep("VZTAU[0-9]+", names(result_data))
###    if(length(vztauIdx) > 0) {
###      VOLUMEUPARAM <- c(VOLUMEUPARAM, names(result_data[vztauIdx]))
###    }
###
###    volumeparameters <- unitclass_parameters("VOLUMEU")
###    y <- names(parameter_required(paste0("^",volumeparameters,"$"), names(result_data), simplify = FALSE))
###    VOLUMEUPARAM <- c(VOLUMEUPARAM, y)

    VOLUMEUPARAM <- parameters_by_class("VOLUMEU", names(result_data))

### 2019-08-30/TGT/ update to parameter_required
###    if("CONCU" %in% names(map_data)){
    if(length(VOLUMEUPARAM)>0 && parameter_required("^CONCU$", names(map_data))){
      if(map_data$CONCU %in% names(data_data)){
        volume_unit_tmp <- unlist(strsplit(as.character(unique(data_data[, map_data$CONCU])[1]), "/"))
        inputUnit4 <- ifelse(length(grep("/", as.character(unique(data_data[, map_data$CONCU])[1]))) > 0, as.character(volume_unit_tmp[2]), NA)
###        outputUnit4 <- as.character(map_data$VOLUMEOUTPUTUNIT)

        outputUnitLabel <- "VOLUMEOUTPUTUNIT"
        testunit <- is.element(outputUnitLabel, names(map))
        outputUnitFormat <- FALSE
        if(testunit) {
            outputUnit4 <- as.character(map_data[[outputUnitLabel]])
            outputUnitFormat <- length(outputUnit4)>0 & !is.na(outputUnit4)
        }
        testunit <- testunit && outputUnitFormat
        
### Added formattedinputUnit and formattedoutputUnit to simplify output
        formattedinputUnit   <- inputUnit4
        formattedoutputUnit  <- outputUnit4
        
        if(testunit && (length(volume_unit_tmp) == 2)){
          inputMatch4 <- match(inputUnit4, units, nomatch = 21)
          outputMatch4 <- match(outputUnit4, units, nomatch = 21)

          if(inputMatch4 != 21 && outputMatch4 != 21) {
            inputMScale4 <- val[inputMatch4]
            outputMScale4 <- val[outputMatch4]
            if(class[inputMatch4] == "V" && class[outputMatch4] == "V") {
              volumeUScaler <- inputMScale4/outputMScale4
            } else {
              volumeUScaler <- 1
              warning("'CONCU' and/or 'VOLUMEOUTPUTUNIT' value provided via 'map' is not accounted for unit conversion")
            }
          } else {
            volumeUScaler <- 1
            warning("'CONCU' and/or 'VOLUMEOUTPUTUNIT' value provided via 'map' is not valid for unit conversion")
          }

          volume_col <- names(result_data)[names(result_data) %in% VOLUMEUPARAM]
          result_data[volume_col] <- result_data[volume_col] * volumeUScaler
###          result_data$VOLUMEU <- ifelse(volumeUScaler == 1, inputUnit4, outputUnit4)
          result_data$VOLUMEU <- ifelse(volumeUScaler == 1, as.character(formattedinputUnit), as.character(formattedoutputUnit))
          if(verbose) { cat(function_name, ': Unit Class 4 (Volume) volume_col: ', volume_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', volumeUScaler, '\n') }
        } else {
### retain original input unit
###          result_data$VOLUMEU <- inputUnit4
          result_data$VOLUMEU <- formattedinputUnit
          if(length(volume_unit_tmp) != 2){
            warning("Unit data provided via the 'CONCU' value provided via 'map' is not in the proper form! Please try again using 'Amount/Volume' format!")
          }
          if(outputUnitFormat){
            warning("'", outputUnitLabel, "' is not present in the proper form! Please try again using 'Volume' format!")
          }
        }
      } else {
        result_data$VOLUMEU <- NA
        warning("'CONCU' value provided via 'map' is not present in the dataset provided via 'data'")
      }
    } else {
      result_data$VOLUMEU <- NA
      warning("'CONCU' #5# is not present in the dataset provided via 'map'")
    }
  }

  #----------------------------------------------------------------------------  
  #Unit Class 5: Amount/Volume
  if(unit_class == "CONCU" || unit_class == "ALL"){
###    CONCUPARAM <- c()
###    cmaxIdx <- grep("CMAX[0-9]+", names(result_data))
###    if(length(cmaxIdx) > 0) {
###      CONCUPARAM <- c(CONCUPARAM, names(result_data[cmaxIdx]))
###    }
###    cmaxcIdx <- grep("CMAXC[0-9]+", names(result_data))
###    if(length(cmaxcIdx) > 0) {
###      CONCUPARAM <- c(CONCUPARAM, names(result_data[cmaxcIdx]))
###    }
###    cmaxnIdx <- grep("CMAX[0-9]+N", names(result_data))
###    if(length(cmaxnIdx) > 0) {
###      CONCUPARAM <- c(CONCUPARAM, names(result_data[cmaxnIdx]))
###    }
###    cminIdx <- grep("CMIN[0-9]+", names(result_data))
###    if(length(cminIdx) > 0) {
###      CONCUPARAM <- c(CONCUPARAM, names(result_data[cminIdx]))
###    }
###    clastIdx <- grep("CLAST[0-9]+", names(result_data))
###    if(length(clastIdx) > 0) {
###      CONCUPARAM <- c(CONCUPARAM, names(result_data[clastIdx]))
###    }
###    cthIdx <- grep("CTROUGH[0-9]+", names(result_data))
###    if(length(cthIdx) > 0) {
###      CONCUPARAM <- c(CONCUPARAM, names(result_data[cthIdx]))
###    }
###    cthendIdx <- grep("CTROUGHEND[0-9]+", names(result_data))
###    if(length(cthendIdx) > 0) {
###      CONCUPARAM <- c(CONCUPARAM, names(result_data[cthendIdx]))
###    }
###
###    concparameters <- unitclass_parameters("CONCU")
###    y <- names(parameter_required(paste0("^",concparameters,"$"), names(result_data), simplify = FALSE))
###    CONCUPARAM <- c(CONCUPARAM, y)

    CONCUPARAM <- parameters_by_class("CONCU", names(result_data))
    
### 2019-08-30/TGT/ update to parameter_required
###    if("CONCU" %in% names(map_data)){
      
    if(length(CONCUPARAM)>0 & parameter_required("^CONCU$", names(map_data))){

### 2019-08-30/TGT/ update to parameter_required
###      if(map_data$CONCU %in% names(data_data)){
      if(parameter_required(map_data$CONCU, names(data_data))){
        conc_unit_tmp <- unlist(strsplit(as.character(unique(data_data[, map_data$CONCU])), "/"))
        if(length(grep("/", as.character(unique(data_data[, map_data$CONCU])))) > 0){
          inputUnit5 <- as.character(conc_unit_tmp)
        } else {
          inputUnit5 <- NA
        }
        outputUnitLabel <- "CONCOUTPUTUNIT"
        testunit <- is.element(outputUnitLabel, names(map))
        outputUnitFormat <- FALSE
        if(testunit) {
            outputUnit5 <- as.character(map_data[[outputUnitLabel]])
            outputUnitFormat <- length(outputUnit5)>0 & !is.na(outputUnit5)
            if(outputUnitFormat) {
                if(length(grep("/", outputUnit5)) > 0) { outputUnit5 <- unlist(strsplit(outputUnit5, "/")) } else { outputUnitFormat <- FALSE }
            }
        }

        testunit <- testunit && outputUnitFormat
        if(testunit){
            testunit <- testunit && length(outputUnit5) == 2 && length(conc_unit_tmp) == 2
        }

### Added formattedinputUnit and formattedoutputUnit to simplify output
        formattedinputUnit   <- paste(inputUnit5,collapse="/")
        formattedoutputUnit  <- paste(outputUnit5,collapse="/")
###        cat("formattedinputUnit: ", formattedinputUnit, " formattedoutputUnit: ", formattedoutputUnit, "\n")
        
        concUScaler <- numeric()
        inputMatch5 <- numeric()
        outputMatch5 <- numeric()

        if(testunit){
          for(i in 1:2) {
            if(!all(is.na(outputUnit5))){
              inputMatch5[i] <- match(inputUnit5[i], units, nomatch = 21)
              outputMatch5[i] <- match(outputUnit5[i], units, nomatch = 21)
              if(inputMatch5[i] != 21 && outputMatch5[i] != 21) {
                inputMScale5 <- val[inputMatch5[i]]
                outputMScale5 <- val[outputMatch5[i]]
                if(class[inputMatch5[i]] == c("M", "V")[i] && class[outputMatch5[i]] == c("M", "V")[i]) {
                  concUScaler[i] <- inputMScale5/outputMScale5
                } else {
                  concUScaler[i] <- 1
                  warning("'CONCU' and/or '", outputUnitLabel, "' value provided via 'map' is not accounted for unit conversion")
                }
              } else {
                concUScaler[i] <- 1
                warning("'CONCU' and/or '", outputUnitLabel, "' value provided via 'map' is not valid for unit conversion")
              }
            }
          }
        }
        if(testunit) { testunit <- testunit && !all(is.na(outputUnit5)) }
        if(testunit){
          concUFinalScaler <- concUScaler[1]/concUScaler[2]
          conc_col <- names(result_data)[names(result_data) %in% CONCUPARAM]
          result_data[conc_col] <- result_data[conc_col] * concUFinalScaler
###          result_data$CONCU <- ifelse(concUFinalScaler == 1, as.character(unique(data_data[, map_data$CONCU])), as.character(map_data[[outputUnitLabel]]))
          result_data$CONCU <- ifelse(concUFinalScaler == 1, as.character(formattedinputUnit), as.character(formattedoutputUnit))
          if(verbose) { cat(function_name, ': Unit Class 5 (Amount/Volume) conc_col: ', conc_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', concUFinalScaler, '\n') }
        } else {
### retain original input unit
###          result_data$CONCU <- as.character(unique(data_data[, map_data$CONCU]))
          result_data$CONCU <- formattedinputUnit
          if(length(conc_unit_tmp) != 2){
            warning("Unit data provided via the 'CONCU' value provided via 'map' is not in the proper form! Please try again using 'Amount/Volume' format!")
          }
          if(outputUnitFormat){
            warning("'", outputUnitLabel, "' is not present in the proper form! Please try again using 'Amount/Volume' format!")
          }
        }
      } else {
        result_data$CONCU <- NA
        warning("'CONCU' value provided via 'map' is not present in the dataset provided via 'data'")
      }
    } else {
        if(parameter_required("^CONCU$", names(map_data)) && parameter_required(map_data$CONCU, names(data_data))) {
          result_data$CONCU <- unique(data_data[, map_data$CONCU])
        }
        else {
          result_data$CONCU <- NA
          warning("'CONCU' #6# is not present in the dataset provided via 'map'")
        }
    }
  }

  #----------------------------------------------------------------------------  
  #Unit Class 6: 1/Time
  if(unit_class == "KELU" || unit_class == "ALL"){
###    KELUPARAM <- c()
###
###    kelparameters <- unitclass_parameters("KELU")
###    y <- names(parameter_required(paste0("^",kelparameters,"$"), names(result_data), simplify = FALSE))
###    KELUPARAM <- c(KELUPARAM, y)
###    
    KELUPARAM <- parameters_by_class("KELU", names(result_data))

### 2018-09-23/TGT/ 
###    if(paste0(map_data$TIME, "U") %in% names(map_data)){
###       (casefold(map_data$TIME)=="nominal" && map_data$NOMTIMEU %in% names(data_data)) ||
###      (casefold(map_data$TIME)=="actual"  && map_data$ACTTIMEU %in% names(data_data))){
      if(length(KELUPARAM)>0 && (parameter_required(map_data$TIME, names(data_data)) &&
          parameter_required(map_data$TIMEU, names(data_data)))) {
### 2019-08-23/TGT/ unit_conversion.R wasn't updated to reflect that
###  mct$TIME points to Nominal/Actual which are pointers to 
###  mct$NOMTIME/mct$NOMTIMEU and mct$ACTTIME/ACTTIMEU
### Replacing
###       if(map_data[, paste0(map_data$TIME, "U")] %in% names(data_data)){
### with
###      if((casefold(map_data$TIME)=="nominal" && map_data$NOMTIMEU %in% names(data_data)) ||
###         (casefold(map_data$TIME)=="actual"  && map_data$ACTTIMEU %in% names(data_data))) {
### 2019-08-23/TGT/           
###        inputUnit6 <- ifelse(map_data[, paste0(map_data$TIME, "U")] %in% names(data_data), as.character(unique(data_data[, map_data[, paste0(map_data$TIME, "U")]]))[1], NA)
        inputUnit6 <- NA
###          if(casefold(map_data$TIME)=="nominal") { inputUnit6 <- data_data[,map_data$NOMTIMEU] }
###          if(casefold(map_data$TIME)=="actual")  { inputUnit6 <- data_data[,map_data$ACTTIMEU] }
        if(parameter_required(map_data$TIMEU, names(data_data))) { inputUnit6 <- as.character(unique(data_data[,map_data$TIMEU])) }  
          outputUnit6 <- ifelse(length(grep("/", map_data$KELOUTPUTUNIT)) > 0,
                         ifelse(as.character(unlist(strsplit(as.character(map_data$KELOUTPUTUNIT), "/")))[1] == "1",
                                as.character(unlist(strsplit(as.character(map_data$KELOUTPUTUNIT), "/")))[2], NA), NA)
#        outputUnit6 <- ifelse(length(grep("/", map_data$KELOUTPUTUNIT)) > 0,
#                       ifelse(as.character(unlist(strsplit(as.character(map_data$KELOUTPUTUNIT), "/")))[1] == "1",
#                              as.character(unlist(strsplit(as.character(map_data$KELOUTPUTUNIT), "/")))[2],
#                              NA),
#                       NA)

        outputUnitLabel <- "KELOUTPUTUNIT"
        testunit <- is.element(outputUnitLabel, names(map))
        outputUnitFormat <- FALSE

### Added formattedinputUnit and formattedoutputUnit to simplify output
        formattedinputUnit   <- paste("1",inputUnit6,sep="/")
        formattedoutputUnit  <- paste("1",outputUnit6,sep="/")

### 2019-08-30/TGT/ introduce a default numerator
        numerator <- "1"
        if(testunit) {
### 2019-08-30/TGT/
###            outputUnit6 <- as.character(map_data[[outputUnitLabel]])
            baseUnit6 <- outputUnit6 <- as.character(map_data[[outputUnitLabel]])
            outputUnitFormat <- length(outputUnit6)>0 & !is.na(outputUnit6)
            if(outputUnitFormat) { 
                if(length(grep("/", outputUnit6) > 0)) {
                  splitunit <- as.character(unlist(strsplit(outputUnit6, "/")))
                  outputUnit6 <- splitunit[2]
                  numerator <-   splitunit[1]
              }
              else { outputUnitFormat <- FALSE }
            }
        }
        testunit <- testunit && outputUnitFormat
            
        if(testunit){
          inputMatch6 <- match(inputUnit6, units, nomatch = 21)
          outputMatch6 <- match(outputUnit6, units, nomatch = 21)

          if(inputMatch6 != 21 && outputMatch6 != 21) {
            inputMScale6 <- val[inputMatch6]
            outputMScale6 <- val[outputMatch6]
            if(class[inputMatch6] == "T" && class[outputMatch6] == "T") {
              kelUScaler <- 1/(inputMScale6/outputMScale6)
            } else {
              kelUScaler <- 1
              warning(paste0("'", map_data$TIMEU, "' and/or '", outputUnitLabel, "' value provided via 'map' is not accounted for unit conversion"))
            }
          } else {
            kelUScaler <- 1
            warning(paste0("'", map_data$TIMEU, "' and/or '", outputUnitLabel, "' value provided via 'map' is not valid for unit conversion"))
          }

          kel_col <- names(result_data)[names(result_data) %in% KELUPARAM]
          result_data[kel_col] <- result_data[kel_col] * kelUScaler

### reform unit in proper form
          outputUnit6 <- paste0(numerator, "/", outputUnit6)
###          result_data$KELU <- ifelse(kelUScaler == 1, inputUnit6, outputUnit6)
###          result_data$KELU <- ifelse(kelUScaler == 1, rep(paste0(numerator,"/",unique(inputUnit6)),nrow(result_data)), rep(outputUnit6, nrow(result_data)))
          result_data$KELU <- ifelse(kelUScaler == 1, formattedinputUnit, formattedoutputUnit)
          if(verbose) { cat(function_name, ': Unit Class 6 (1/Time) kel_col: ', kel_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', kelUScaler, '\n') }
        } else {
### retain original input unit
###          result_data$KELU <- inputUnit6
###          result_data$KELU <- rep(paste0(numerator,"/",unique(inputUnit6)), nrow(result_data))
          result_data$KELU <- formattedinputUnit
          if(outputUnitFormat){
            warning("'", outputUnitLabel, "' is not present in the proper form! Please try again using '1/Time' format!")
          }
        }
###      } else {
###       result_data$KELU <- NA
###        warning(paste0("'", map_data$TIMEU, "' value provided via 'map' is not present in the dataset provided via 'data'"))
###      }
    } else {
      result_data$KELU <- NA
      warning(paste0("'", map_data$TIMEU, "' #7# is not present in the dataset provided via 'map'"))
    }
  }

  #----------------------------------------------------------------------------  
  #Unit Class 7: Volume/Time
  if(unit_class == "CLU" || unit_class == "ALL"){
###    CLUPARAM <- c()
###    clftauIdx <- grep("CLFTAU[0-9]+", names(result_data))
###    if(length(clftauIdx) > 0) {
###      CLUPARAM <- c(CLUPARAM, names(result_data[clftauIdx]))
###    }
###    clrtauIdx <- grep("CLRTAU[0-9]+", names(result_data))
###    if(length(clrtauIdx) > 0) {
###      CLUPARAM <- c(CLUPARAM, names(result_data[clrtauIdx]))
###    }
###    cltauIdx <- grep("CLTAU[0-9]+", names(result_data))
###    if(length(cltauIdx) > 0) {
###      CLUPARAM <- c(CLUPARAM, names(result_data[cltauIdx]))
###    }
###
###    clparameters <- unitclass_parameters("CLU")
###    y <- names(parameter_required(paste0("^",clparameters,"$"), names(result_data), simplify = FALSE))
###    CLUPARAM <- c(CLUPARAM, y)
    
    CLUPARAM <- parameters_by_class("CLU", names(result_data))

###      cat('CLUPARAM: ', CLUPARAM, '\n')
###      cat('map_data$TIMEU: ', map_data$TIMEU, '\n')
###      cat('map_data$TIMEU %in% names(map_data): ', map_data$TIMEU %in% names(map_data), '\n')
###      cat('map_data$CONCU: ', map_data$CONCU, '\n')
###      cat('"CONCU" %in% names(map_data): ', "CONCU" %in% names(map_data), '\n')
      
### 2019-10-01/TGT/ Change from indirect reference to direct name, "TIMEU", "CONCU"
###    if(map_data$TIMEU %in% names(map_data) && "CONCU" %in% names(map_data)){
    if(length(CLUPARAM)>0 && parameter_required("^TIMEU$", names(map_data)) && parameter_required("^CONCU$", names(map_data))){
###      if(map_data$TIMEU %in% names(data_data) && map_data$CONCU %in% names(data_data)){
      if(parameter_required(map_data$TIMEU, names(data_data)) && parameter_required(map_data$CONCU, names(data_data))){
        inputconcunit <- as.character(unique(data_data[, map_data$CONCU])[[1]])
        inputtimeunit <- as.character(unique(data_data[, map_data$TIMEU])[[1]])
        clu_unit_tmp <- unlist(strsplit(as.character(unique(data_data[, map_data$CONCU])[[1]]), "/"))
        inputUnit7 <- c(as.character(clu_unit_tmp[2]), as.character(unique(data_data[, map_data$TIMEU])[[1]]))
###        outputUnit7 <- unlist(strsplit(as.character(map_data$CLOUTPUTUNIT), "/")[[1]])

        outputUnitLabel <- "CLOUTPUTUNIT"
        testunit <- is.element(outputUnitLabel, names(map))
        outputUnitFormat <- FALSE
        if(testunit) {
            outputUnit7 <- as.character(map_data[[outputUnitLabel]])
            outputUnitFormat <- length(outputUnit7)>0 & !is.na(outputUnit7)
            if(outputUnitFormat) {
                if(length(grep("/", outputUnit7)) > 0) { outputUnit7 <- unlist(strsplit(outputUnit7, "/")) } else { outputUnitFormat <- FALSE }
            }
        }

        testunit <- testunit && outputUnitFormat
        if(testunit){
            testunit <- testunit && length(outputUnit7) == 2 && length(clu_unit_tmp) == 2
        }

        clUScaler <- numeric()
        inputMatch7 <- numeric()
        outputMatch7 <- numeric()

### Added formattedinputUnit and formattedoutputUnit to simplify output
        formattedinputUnit   <- paste(inputUnit7,collapse="/")
        formattedoutputUnit  <- paste(outputUnit7,collapse="/")
        
        if(testunit){
          for(i in 1:2) {
            if(outputUnitFormat){
              inputMatch7[i] <- match(inputUnit7[i], units, nomatch = 21)
              outputMatch7[i] <- match(outputUnit7[i], units, nomatch = 21)

              if(inputMatch7[i] != 21 && outputMatch7[i] != 21) {
                inputMScale7 <- val[inputMatch7[i]]
                outputMScale7 <- val[outputMatch7[i]]
                if(class[inputMatch7[i]] == c("V", "T")[i] && class[outputMatch7[i]] == c("V", "T")[i]) {
                  clUScaler[i] <- inputMScale7/outputMScale7
                } else {
                  clUScaler[i] <- 1
                  warning(paste0("'", map_data$TIMEU, "' and/or 'CONCU' and/or 'CONCOUTPUTUNIT' value provided via 'map' is not accounted for unit conversion"))
                }
              } else {
                clUScaler[i] <- 1
                warning(paste0("'", map_data$TIMEU, "' and/or 'CONCU' and/or 'CONCOUTPUTUNIT' value provided via 'map' is not valid for unit conversion"))
              }
            }
          }
        }
        if(testunit){
          clUFinalScaler <- clUScaler[1]/clUScaler[2]
          cl_col <- names(result_data)[names(result_data) %in% CLUPARAM]
          result_data[cl_col] <- result_data[cl_col] * clUFinalScaler
###          result_data$CLU <- ifelse(clUFinalScaler == 1, as.character(unique(data_data[, map_data$CONCU])[[1]]), as.character(map_data[[outputUnitLabel]]))
          result_data$CLU <- ifelse(clUFinalScaler == 1, formattedinputUnit, formattedoutputUnit)
          if(verbose) { cat(function_name, ': Unit Class 7 (Volume/Time) cl_col: ', cl_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', clUFinalScaler, '\n') }
        } else {
### retain original input unit
###          result_data$CLU <- as.character(unique(data_data[, map_data$CONCU])[[1]])
            result_data$CLU <- formattedinputUnit
            if(length(clu_unit_tmp) != 2){
            warning("Unit data provided via the 'CONCU' value provided via 'map' is not in the proper form! Please try again using 'Amount/Volume' format!")
          }
          if(outputUnitFormat){
            warning("'", outputUnitLabel, "' is not present in the proper form! Please try again using 'Volume/Time' format!")
          }
        }
      } else {
        result_data$CLU <- NA
        if(!(map_data$TIMEU %in% names(data_data) && map_data$CONCU %in% names(data_data))) {
          warning(paste0("'", map_data$TIMEU, "' and 'CONCU' values provided via 'map' #1# are not present in the dataset provided via 'data'"))
        } else if(!map_data$TIMEU %in% names(data_data)) {
          warning(paste0("'", map_data$TIMEU, "' value provided via 'map' is not present in the dataset provided via 'data'"))
        } else if(!map_data$CONCU %in% names(data_data)) {
          warning("'CONCU' value provided via 'map' is not present in the dataset provided via 'data'")
        }
      }
    } else {
      result_data$CLU <- NA
### 2019-08-30/TGT/ 
###      if(!(map_data$TIMEU %in% names(map_data) && "CONCU" %in% names(map_data))) {
      if(!(parameter_required(map_data$TIMEU, names(data_data)) && parameter_required(map_data$CONCU, names(data_data)))) {
        warning(paste0("'", map_data$TIMEU, "' and 'CONCU' #1# are not present in the dataset provided via 'map'"))
### 2019-08-30/TGT/ 
###      } else if(!map_data$TIMEU %in% names(map_data)) {
      } else if(!(parameter_required(map_data$TIMEU, names(data_data)))) {
        warning(paste0("'", map_data$TIMEU, "' value #8# is not present in the dataset provided via 'map'"))
###      } else if(!"CONCU" %in% names(map_data)) {
      } else if(!(parameter_required("^CONCU$", names(map_data)))) {
        warning("'CONCU' #9# is not present in the dataset provided via 'map'")
      }
    }
  }

  #----------------------------------------------------------------------------  
  #Unit Class 8: Amount.Time/Volume
  if(unit_class == "AUCU" || unit_class == "ALL"){
###    AUCUPARAM <- c()
###    auctIdx <- grep("AUC[0-9]+", names(result_data))
###    if(length(auctIdx) > 0) {
###      AUCUPARAM <- c(AUCUPARAM, names(result_data[auctIdx]))
###    }
###    auclastIdx <- grep("AUCLAST[0-9]+", names(result_data))
###    if(length(auclastIdx) > 0) {
###      AUCUPARAM <- c(AUCUPARAM, names(result_data[auclastIdx]))
###    }
###    auclastcIdx <- grep("AUCLASTC[0-9]+", names(result_data))
###    if(length(auclastcIdx) > 0) {
###      AUCUPARAM <- c(AUCUPARAM, names(result_data[auclastcIdx]))
###    }
###    auctauIdx <- grep("AUCTAU[0-9]+", names(result_data))
###    if(length(auctauIdx) > 0) {
###      AUCUPARAM <- c(AUCUPARAM, names(result_data[auctauIdx]))
###    }

###    aucparameters <- unitclass_parameters("AUCU")
###    y <- names(parameter_required(paste0("^",aucparameters,"$"), names(result_data), simplify = FALSE))
###    AUCUPARAM <- c(AUCUPARAM, y)
    
    AUCUPARAM <- parameters_by_class("AUCU", names(result_data))
    
###      cat('AUCUPARAM: ', AUCUPARAM, '\n')
###      cat('map_data$TIMEU: ', map_data$TIMEU, '\n')
###      cat('map_data$TIMEU %in% names(map_data): ', map_data$TIMEU %in% names(map_data), '\n')
###      cat('map_data$CONCU: ', map_data$CONCU, '\n')
###      cat('"CONCU" %in% names(map_data): ', "CONCU" %in% names(map_data), '\n')
      
### 2019-10-01/TGT/ Change from indirect reference to direct name, "TIMEU", "CONCU"
###    if(map_data$TIMEU %in% names(map_data) && "CONCU" %in% names(map_data)){
    if(length(AUCUPARAM)>0 && parameter_required("^TIMEU$", names(map_data)) && parameter_required("^CONCU$", names(map_data))){
###      if(map_data$TIMEU %in% names(data_data) && map_data$CONCU %in% names(data_data)){
      if(parameter_required(map_data$TIMEU, names(data_data)) && parameter_required(map_data$CONCU, names(data_data))){
###  cat('map_data$TIMEU: ', map_data$TIMEU, ' map_data$CONCU: ', map_data$CONCU, '\n')
        inputconcunit <- as.character(unique(data_data[, map_data$CONCU])[[1]])
        inputtimeunit <- as.character(unique(data_data[, map_data$TIMEU])[[1]])
        auc_unit_tmp <- unlist(strsplit(as.character(unique(data_data[, map_data$CONCU])[[1]]), "/"))
        inputUnit8 <- c(as.character(auc_unit_tmp[1]), as.character(unique(data_data[, map_data$TIMEU])[[1]]), as.character(auc_unit_tmp[2]))
        outputUnit8 <- unlist(strsplit(unlist(strsplit(as.character(map_data$AUCOUTPUTUNIT), "/")), "[.]"))
###cat('auc_unit_tmp: ', auc_unit_tmp, ' inputUnit8: ', inputUnit8, ' outputUnit8: ', outputUnit8, '\n')                
        outputUnitLabel <- "AUCOUTPUTUNIT"
        testunit <- is.element(outputUnitLabel, names(map))
        outputUnitFormat <- FALSE
        if(testunit) {
            outputUnit8 <- as.character(map_data[[outputUnitLabel]])
            outputUnitFormat <- length(outputUnit8)>0 & !is.na(outputUnit8)
            if(outputUnitFormat) {
                if(length(grep("/", outputUnit8)) > 0) { outputUnit8 <- unlist(strsplit(outputUnit8, "[./]", perl=TRUE)) } else { outputUnitFormat <- FALSE }
            }
        }
###cat("outputUnit8: ", outputUnit8, "\n")
        
        testunit <- testunit && outputUnitFormat
        if(testunit){
            testunit <- testunit && length(outputUnit8) == 3 && length(auc_unit_tmp) == 2
        }

### Added formattedinputUnit and formattedoutputUnit to simplify output
        formattedinputUnit   <- paste(c(paste(inputUnit8[1:2], collapse="."), inputUnit8[3]), collapse="/")
        formattedoutputUnit  <- paste(c(paste(outputUnit8[1:2], collapse="."), outputUnit8[3]), collapse="/")
          
        inputMatch8 <- numeric()
        outputMatch8 <- numeric()
        aucUScaler <- numeric()

        if(testunit){
          for(i in 1:3) {
            if(outputUnitFormat){
              inputMatch8[i] <- match(inputUnit8[i], units, nomatch = 21)
              outputMatch8[i] <- match(outputUnit8[i], units, nomatch = 21)

              if(inputMatch8[i] != 21 && outputMatch8[i] != 21) {
                inputMScale8 <- val[inputMatch8[i]]
                outputMScale8 <- val[outputMatch8[i]]
                if(class[inputMatch8[i]] == c("M", "T", "V")[i] && class[outputMatch8[i]] == c("M", "T", "V")[i]) {
                  aucUScaler[i] <- inputMScale8/outputMScale8
                } else {
                  aucUScaler[i] <- 1
                  warning(paste0("'", map_data$TIMEU, "' and/or 'CONCU' and/or '", outputUnitLabel, "' value provided via 'map' is not accounted for unit conversion"))
                }
              } else {
                aucUScaler[i] <- 1
                warning(paste0("'", map_data$TIMEU, "' and/or 'CONCU' and/or '", outputUnitLabel, "' value provided via 'map' is not valid for unit conversion"))
              }
            }
          }
        }
        if(testunit){
          aucUFinalScaler <- aucUScaler[1] * aucUScaler[2] / aucUScaler[3]
          auc_col <- names(result_data)[names(result_data) %in% AUCUPARAM]
          result_data[auc_col] <- result_data[auc_col] * aucUFinalScaler
###          result_data$AUCU <- ifelse(aucUFinalScaler == 1, as.character(paste0(auc_unit_tmp[1], ".", as.character(unique(data_data[, map_data$TIMEU])[[1]]), "/", auc_unit_tmp[2])), as.character(map_data[[outputUnitLabel]]))
          result_data$AUCU <- ifelse(aucUFinalScaler == 1, as.character(formattedinputUnit), as.character(formattedoutputUnit))
          if(verbose) { cat(function_name, ': Unit Class 8: (Amount.Time/Volume) auc_col: ', auc_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', aucUFinalScaler, '\n') }
        } else {
### retain original input unit
###          result_data$AUCU <- as.character(paste0(auc_unit_tmp[1], ".", as.character(unique(data_data[, map_data$TIMEU])[[1]]), "/", auc_unit_tmp[2]))
          result_data$AUCU <- formattedinputUnit
          if(length(auc_unit_tmp) != 2){
            warning("Unit data provided via the 'CONCU' value provided via 'map' is not in the proper form! Please try again using 'Amount/Volume' format!")
          }
          if(outputUnitFormat){
            warning("'", outputUnitLabel, "' is not present in the proper form! Please try again using 'Amount*Time/Volume' format!")
          }
        }
      } else {
        result_data$AUCU <- NA
        if(!(map_data$TIMEU %in% names(data_data) && map_data$CONCU %in% names(data_data))) {
          warning(paste0("'", map_data$TIMEU, "' and 'CONCU' values provided via 'map' #3# are not present in the dataset provided via 'data'"))
        } else if(!(parameter_required(map_data$TIMEU, names(data_data)))) {
          warning(paste0("'", map_data$TIMEU, "' value provided via 'map' is not present in the dataset provided via 'data'"))
        } else if(!(parameter_required(map_data$CONCU, names(data_data)))) {
          warning("'CONCU' value provided via 'map' is not present in the dataset provided via 'data'")
        }
      }
    } else {
      result_data$AUCU <- NA
### 2019-08-30/TGT/
###      if(!(map_data$TIMEU %in% names(map_data) && "CONCU" %in% names(map_data))) {
      if(!(parameter_required(map_data$TIMEU, names(data_data)) && parameter_required(map_data$CONCU, names(data_data)))) {
        warning(paste0("'", map_data$TIMEU, "' and 'CONCU' #4# are not present in the dataset provided via 'map'"))
###      } else if(!map_data$TIMEU %in% names(map_data)) {
      } else if(!(parameter_required(map_data$TIMEU,names(data_data)))) {
        warning(paste0("'", map_data$TIMEU, "' #10# is not present in the dataset provided via 'map'"))
###      } else if(!"CONCU" %in% names(map_data)) {
      } else if(!(parameter_required("^CONCU$", names(map_data)))) {
        warning("'CONCU' #11# is not present in the dataset provided via 'map'")
      }
    }
  }

  #----------------------------------------------------------------------------  
  #Unit Class 9: Amount.Time^2/Volume (Amount.Time.Time/Volume)
  if(unit_class == "AUMCU" || unit_class == "ALL"){
###    AUMCUPARAM <- c()
###    aumctauIdx <- grep("AUMCTAU[0-9]+", names(result_data))
###    if(length(aumctauIdx) > 0) {
###      AUMCUPARAM <- c(AUMCUPARAM, names(result_data[aumctauIdx]))
###    }
###
###    aumcparameters <- unitclass_parameters("AUMCU")
###    y <- names(parameter_required(paste0("^",aumcparameters,"$"), names(result_data), simplify = FALSE))
###    AUMCUPARAM <- c(AUMCUPARAM, y)
    
    AUMCUPARAM <- parameters_by_class("AUMCU", names(result_data))

###    cat('AUMCUPARAM: ', AUMCUPARAM, '\n')
###      cat('map_data$TIMEU: ', map_data$TIMEU, '\n')
###      cat('map_data$TIMEU %in% names(map_data): ', map_data$TIMEU %in% names(map_data), '\n')
###      cat('map_data$CONCU: ', map_data$CONCU, '\n')
###      cat('"CONCU" %in% names(map_data): ', "CONCU" %in% names(map_data), '\n')

### 2019-10-01/TGT/ Change from indirect reference to direct name, "TIMEU", "CONCU"
###    if(map_data$TIMEU %in% names(map_data) && "CONCU" %in% names(map_data)){
    if(length(AUMCUPARAM)>0 && parameter_required("^TIMEU$", names(map_data)) && parameter_required("^CONCU$", names(map_data))){
###      if(map_data$TIMEU %in% names(data_data) && map_data$CONCU %in% names(data_data)){
      if(parameter_required(map_data$TIMEU, names(data_data)) && parameter_required(map_data$CONCU, names(data_data))){
###  cat('map_data$TIMEU: ', map_data$TIMEU, ' map_data$CONCU: ', map_data$CONCU, '\n')
        inputconcunit <- as.character(unique(data_data[, map_data$CONCU])[[1]])
        inputtimeunit <- as.character(unique(data_data[, map_data$TIMEU])[[1]])
        aumc_unit_tmp <- unlist(strsplit(as.character(unique(data_data[, map_data$CONCU])[[1]]), "/"))
        aumc_unit_tmp2 <- as.character(unique(data_data[, map_data$TIMEU])[[1]])
        inputUnit9 <- c(as.character(aumc_unit_tmp[1]), aumc_unit_tmp2, aumc_unit_tmp2, as.character(aumc_unit_tmp[2]))
###        outputUnit9 <- unlist(strsplit(unlist(strsplit(as.character(map_data$AUMCOUTPUTUNIT), "/")), "[.]"))

        outputUnitLabel <- "AUMCOUTPUTUNIT"
        testunit <- is.element(outputUnitLabel, names(map))
        outputUnitFormat <- FALSE
        if(testunit) {
            outputUnit9 <- as.character(map_data[[outputUnitLabel]])
            outputUnitFormat <- length(outputUnit9)>0 & !is.na(outputUnit9)
            if(outputUnitFormat) {
                if(length(grep("/", outputUnit9)) > 0) { outputUnit9 <- unlist(strsplit(outputUnit9, "[./^]", perl=TRUE)) } else { outputUnitFormat <- FALSE }
            }
        }
        
        testunit <- testunit && outputUnitFormat
        if(testunit){
            testunit <- testunit && length(outputUnit9) == 4 && length(aumc_unit_tmp) == 2
        }

### Added formattedinputUnit and formattedoutputUnit to simplify output
        formattedinputUnit   <- paste(c(paste(inputUnit9[1:3],  collapse="."), inputUnit9[4]),  collapse="/")
        formattedoutputUnit  <- paste(c(paste(outputUnit9[1:3], collapse="."), outputUnit9[4]), collapse="/")

        inputMatch9 <- numeric()
        outputMatch9 <- numeric()
        aumcUScaler <- numeric()

        if(testunit){
          for(i in 1:4) {
            if(outputUnitFormat){
              inputMatch9[i] <- match(inputUnit9[i], units, nomatch = 21)
              outputMatch9[i] <- match(outputUnit9[i], units, nomatch = 21)

              if(inputMatch9[i] != 21 && outputMatch9[i] != 21) {
                inputMScale9 <- val[inputMatch9[i]]
                outputMScale9 <- val[outputMatch9[i]]
                if(class[inputMatch9[i]] == c("M", "T", "T", "V")[i] && class[outputMatch9[i]] == c("M", "T", "T", "V")[i]) {
                  aumcUScaler[i] <- inputMScale9/outputMScale9
                } else {
                  aumcUScaler[i] <- 1
                  warning(paste0("'", map_data$TIMEU, "' and/or 'CONCU' and/or '", outputUnitLabel, "' value provided via 'map' is not accounted for unit conversion"))
                }
              } else {
                aumcUScaler[i] <- 1
                warning(paste0("'", map_data$TIMEU, "' and/or 'CONCU' and/or '", outputUnitLabel, "' value provided via 'map' is not valid for unit conversion"))
              }
            }
          }
        }
        if(testunit){
          aumcUFinalScaler <- aumcUScaler[1] * aumcUScaler[2] * aumcUScaler[3] / aumcUScaler[4]
          aumc_col <- names(result_data)[names(result_data) %in% AUMCUPARAM]
          result_data[aumc_col] <- result_data[aumc_col] * aumcUFinalScaler
###          result_data$AUMCU <- ifelse(aumcUFinalScaler == 1, as.character(paste0(aumc_unit_tmp[1], ".", aumc_unit_tmp2, ".", aumc_unit_tmp2, "/", aumc_unit_tmp[2])), as.character(map_data[[outputUnitLabel]]))
          result_data$AUMCU <- ifelse(aumcUFinalScaler == 1, as.character(formattedinputUnit), as.character(formattedoutputUnit))
          if(verbose) { cat(function_name, ': Unit Class 9: (Amount.Time.Time/Volume) aumc_col: ', aumc_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', aumcUFinalScaler, '\n') }
        } else {
### retain original input unit
###          result_data$AUMCU <- as.character(paste0(aumc_unit_tmp[1], ".", aumc_unit_tmp2, ".", aumc_unit_tmp2, "/", aumc_unit_tmp[2]))
          result_data$AUMCU <- formattedinputUnit
          if(length(aumc_unit_tmp) != 2){
            warning("Unit data provided via the 'CONCU' value provided via 'map' is not in the proper form! Please try again using 'Amount/Volume' format!")
          }
          if(outputUnitFormat){
            warning("'", outputUnitLabel, "' is not present in the proper form! Please try again using 'Amount*Time^2/Volume' format!")
          }
        }
      } else {
        result_data$AUMCU <- NA
        if(!(map_data$TIMEU %in% names(data_data) && map_data$CONCU %in% names(data_data))) {
          warning(paste0("'", map_data$TIMEU, "' and 'CONCU' values provided via 'map' #5# are not present in the dataset provided via 'data'"))
        } else if(!map_data$TIMEU %in% names(data_data)) {
          warning(paste0("'", map_data$TIMEU, "' value provided via 'map' is not present in the dataset provided via 'data'"))
        } else if(!map_data$CONCU %in% names(data_data)) {
          warning("'CONCU' value provided via 'map' is not present in the dataset provided via 'data'")
        }
      }
    } else {
      result_data$AUMCU <- NA
### 2019-08-30/TGT/ 
###      if(!(map_data$TIMEU %in% names(map_data) && "CONCU" %in% names(map_data))) {
      if(!(parameter_required("^TIMEU$", names(map_data)) && parameter_required("^CONCU$", names(map_data)))) {
        warning(paste0("'", map_data$TIMEU, "' and 'CONCU' #6# are not present in the dataset provided via 'map'"))
      } else if(!(parameter_required("^TIMEU$", names(map_data)))) {
        warning(paste0("'", map_data$TIMEU, "' #13# is not present in the dataset provided via 'map'"))
      } else if(!"CONCU" %in% names(map_data)) {
        warning("'CONCU' #14# is not present in the dataset provided via 'map'")
      }
    }
  }

  #----------------------------------------------------------------------------  
  #Unit Class 10: [Amount.Time/Volume]/Amount
  if(unit_class == "AUCNORMU" || unit_class == "ALL"){
###    AUCNORMUPARAM <- c()
###    auclastdnIdx <- grep("AUCLAST[0-9]+DN", names(result_data))
###    if(length(auclastdnIdx) > 0) {
###      AUCNORMUPARAM <- c(AUCNORMUPARAM, names(result_data[auclastdnIdx]))
###    }
###    auctaudnIdx <- grep("AUCTAU[0-9]+DN", names(result_data))
###    if(length(auctaudnIdx) > 0) {
###      AUCNORMUPARAM <- c(AUCNORMUPARAM, names(result_data[auctaudnIdx]))
###    }
###
###    aucnormparameters <- unitclass_parameters("AUCNORMU")
###    y <- names(parameter_required(paste0("^",aucnormparameters,"$"), names(result_data), simplify = FALSE))
###    AUCNORMUPARAM <- c(AUCNORMUPARAM, y)
    
    AUCNORMUPARAM <- parameters_by_class("AUCNORMU", names(result_data))
    
###    cat('AUCNORMUPARAM: ', AUCNORMUPARAM, '\n')
###      cat('map_data$TIMEU: ', map_data$TIMEU, '\n')
###      cat('map_data$TIMEU %in% names(map_data): ', map_data$TIMEU %in% names(map_data), '\n')
###      cat('map_data$CONCU: ', map_data$CONCU, '\n')
###      cat('"CONCU" %in% names(map_data): ', "CONCU" %in% names(map_data), '\n')

###    if(map_data$TIMEU %in% names(map_data) && "CONCU" %in% names(map_data) && "DOSE1U" %in% names(map)){
###cat(parameter_required("^DOSE[0-9]*?U$", names(map)))
    if(length(AUCNORMUPARAM)>0 && parameter_required("^TIMEU$", names(map_data)) && parameter_required("^CONCU$", names(map_data)) && parameter_required("^DOSE[0-9]*?U$", names(map))){
###      if(map_data$TIMEU %in% names(data_data) && map_data$CONCU %in% names(data_data) && map_data$DOSE1U %in% names(data_data)){
      if(parameter_required(map_data$TIMEU, names(data_data)) && parameter_required(map_data$CONCU, names(data_data)) && parameter_required(map_data$DOSEU, names(data_data))){
        inputconcunit <- as.character(unique(data_data[, map_data$CONCU])[[1]])
        inputtimeunit <- as.character(unique(data_data[, map_data$TIMEU])[[1]])
        xdoseu <- unlist(strsplit(map_data$DOSEULIST, ";"))[1]
###        inputdoseunit <- as.character(unique(data_data[, map_data$DOSEU])[[1]])
        inputdoseunit <- as.character(unique(data_data[, map_data[,xdoseu]])[[1]])

        aucdn_unit_tmp <- unlist(strsplit(as.character(unique(data_data[, map_data$CONCU])[[1]]), "/"))
        aucdn_unit_tmp2 <- as.character(unique(data_data[, map_data$TIMEU])[[1]])
###        aucdn_unit_tmp3 <- as.character(unique(data_data[, map_data$DOSEU])[[1]])
        aucdn_unit_tmp3 <- as.character(unique(data_data[, map_data[,xdoseu]])[[1]])
###        cat('aucdn_unit_tmp3: ', aucdn_unit_tmp3, '\n')
        inputUnit10 <- c(as.character(aucdn_unit_tmp[1]), aucdn_unit_tmp2, as.character(aucdn_unit_tmp[2]), aucdn_unit_tmp3)
###        outputUnit10 <- unlist(strsplit(unlist(strsplit(as.character(map_data$AUCNORMOUTPUTUNIT), "/")), "[.]"))

        outputUnitLabel <- "AUCNORMOUTPUTUNIT"
        testunit <- is.element(outputUnitLabel, names(map))
        outputUnitFormat <- FALSE
        if(testunit) {
            outputUnit10 <- as.character(map_data[[outputUnitLabel]])
            outputUnitFormat <- length(outputUnit10)>0 & !is.na(outputUnit10)
            if(outputUnitFormat) {
                if(length(grep("/", outputUnit10)) > 0) { outputUnit10 <- unlist(strsplit(outputUnit10, "[./^]", perl=TRUE)) } else { outputUnitFormat <- FALSE }
            }
        }
###        cat('outputUnitLabel: ', outputUnitLabel, ' testunit: ', testunit, ' outputUnit10: ', outputUnit10, ' outputUnitFormat: ', outputUnitFormat, '\n')
        
        testunit <- testunit && outputUnitFormat
        if(testunit){
            testunit <- testunit && length(outputUnit10) == 4 && length(aucdn_unit_tmp) == 2
        }

### Added formattedinputUnit and formattedoutputUnit to simplify output
        formattedinputUnit   <- paste(c(paste(inputUnit10[1:2],  collapse="."), inputUnit10[3:4]),  collapse="/")
        formattedoutputUnit  <- paste(c(paste(outputUnit10[1:2], collapse="."), outputUnit10[3:4]), collapse="/")
        
        inputMatch10 <- numeric()
        outputMatch10 <- numeric()
        aucdnUScaler <- numeric()

        if(testunit){
          for(i in 1:4) {
            if(outputUnitFormat){
              inputMatch10[i] <- match(inputUnit10[i], units, nomatch = 21)
              outputMatch10[i] <- match(outputUnit10[i], units, nomatch = 21)

              if(inputMatch10[i] != 21 && outputMatch10[i] != 21) {
                inputMScale10 <- val[inputMatch10[i]]
                outputMScale10 <- val[outputMatch10[i]]
                if(class[inputMatch10[i]] == c("M", "T", "V", "M")[i] && class[outputMatch10[i]] == c("M", "T", "V", "M")[i]) {
                  aucdnUScaler[i] <- inputMScale10/outputMScale10
                } else {
                  aucdnUScaler[i] <- 1
                  warning(paste0("'", map_data$TIMEU, "' and/or 'CONCU' and/or '", outputUnitLabel, "' value provided via 'map' is not accounted for unit conversion"))
                }
              } else {
                aucdnUScaler[i] <- 1
                warning(paste0("'", map_data$TIMEU, "' and/or 'CONCU' and/or '", outputUnitLabel, "' value provided via 'map' is not valid for unit conversion"))
              }
            }
          }
        }
        if(testunit) {
          aucdnUFinalScaler <- (aucdnUScaler[1] * aucdnUScaler[2] / aucdnUScaler[3]) / aucdnUScaler[4]
          aucdn_col <- names(result_data)[names(result_data) %in% AUCNORMUPARAM]
          result_data[aucdn_col] <- result_data[aucdn_col] * aucdnUFinalScaler
###          result_data$AUCNORMU <- ifelse(aucdnUFinalScaler == 1, as.character(paste0(aucdn_unit_tmp[1], ".", aucdn_unit_tmp2, "/", aucdn_unit_tmp[2], "/", aucdn_unit_tmp3)), as.character(map_data[[outputUnitLabel]]))
          result_data$AUCNORMU <- ifelse(aucdnUFinalScaler == 1, as.character(formattedinputUnit), as.character(formattedoutputUnit))
          if(verbose) { cat(function_name, ': Unit Class 10: ([Amount.Time/Volume]/Amount) aucdn_col: ', aucdn_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', aucdnUFinalScaler, '\n') }
        } else {
### retain original input unit
###          result_data$AUCNORMU <- as.character(paste0(aucdn_unit_tmp[1], ".", aucdn_unit_tmp2, "/", aucdn_unit_tmp[2], "/", aucdn_unit_tmp3))
          result_data$AUCNORMU <- formattedinputUnit
          if(length(aucdn_unit_tmp) != 2){
            warning("Unit data provided via the 'CONCU' value provided via 'map' is not in the proper form! Please try again using 'Amount/Volume' format!")
          }
          if(outputUnitFormat){
            warning("'", outputUnitLabel, "' is not present in the proper form! Please try again using '[Amount*Time/Volume]/Amount' format!")
          }
        }
      } else {
          xdoseu <- unlist(strsplit(map_data$DOSEULIST, ";"))[1]
          result_data$AUCNORMU <- NA
###        if(!(map_data$TIMEU %in% names(data_data) && map_data$CONCU %in% names(data_data) && map_data$DOSEU %in% names(data_data))) {
        if(!(map_data$TIMEU %in% names(data_data) && map_data$CONCU %in% names(data_data) && map_data[,xdoseu] %in% names(data_data))) {
          warning(paste0("'", map_data$TIMEU, "', 'CONCU' and 'DOSEU' values provided via 'map' #7# are not present in the dataset provided via 'data'"))
        } else if(!(map_data$TIMEU %in% names(data_data) && map_data$CONCU %in% names(data_data))) {
          warning(paste0("'", map_data$TIMEU, "' and 'CONCU' values provided via 'map' is not present in the dataset provided via 'data'"))
        } else if(!(map_data$CONCU %in% names(data_data) && map_data$DOSEU %in% names(data_data))) {
          warning("'CONCU' and 'DOSEU' values provided via 'map' is not present in the dataset provided via 'data'")
        } else if(!(map_data$TIMEU %in% names(data_data) && map_data$DOSEU %in% names(data_data))) {
          warning(paste0("'", map_data$TIMEU, "' and 'DOSEU' values provided via 'map' is not present in the dataset provided via 'data'"))
        } else if(!map_data$TIMEU %in% names(data_data)) {
          warning(paste0("'", map_data$TIMEU, "' value provided via 'map' is not present in the dataset provided via 'data'"))
        } else if(!map_data$CONCU %in% names(data_data)) {
          warning("'CONCU' value provided via 'map' is not present in the dataset provided via 'data'")
        } else if(!map_data$DOSEU %in% names(data_data)) {
          warning("'DOSEU' value provided via 'map' is not present in the dataset provided via 'data'")
        }
      }
    } else {
          result_data$AUCNORMU <- NA
          xdoseu <- unlist(strsplit(map_data$DOSEULIST, ";"))[1]
###      if(!(map_data$TIMEU %in% names(map_data) && "CONCU" %in% names(map_data) && "DOSEU" %in% names(map_data))) {
      if(!(map_data$TIMEU %in% names(map_data) && "CONCU" %in% names(map_data) && xdoseu %in% names(map_data))) {
        warning(paste0("'", map_data$TIMEU, "', 'CONCU' and 'DOSEU' #8# are not present in the dataset provided via 'map'"))
      } else if(!(map_data$TIMEU %in% names(map_data) && "CONCU" %in% names(map_data))) {
        warning(paste0("'", map_data$TIMEU, "' and 'CONCU' #16# is not present in the dataset provided via 'map'"))
      } else if(!("CONCU" %in% names(map_data) && "DOSEU" %in% names(map_data))) {
        warning("'CONCU' and 'DOSEU' #17# is not present in the dataset provided via 'map'")
      } else if(!(map_data$TIMEU %in% names(map_data) && "DOSEU" %in% names(map_data))) {
        warning(paste0("'", map_data$TIMEU, "' and 'DOSEU' #18# is not present in the dataset provided via 'map'"))
      } else if(!map_data$TIMEU %in% names(map_data)) {
        warning(paste0("'", map_data$TIMEU, "' #19# is not present in the dataset provided via 'map'"))
      } else if(!"CONCU" %in% names(map_data)) {
        warning("'CONCU' #20# is not present in the dataset provided via 'map'")
      } else if(!"DOSEU" %in% names(map_data)) {
        warning("'DOSEU' #21# is not present in the dataset provided via 'map'")
      }
    }
  }

  #----------------------------------------------------------------------------  
  #Unit Class 11: [Volume.Amount]/Volume
  if(unit_class == "AURCU" || unit_class == "ALL"){
###    AURCUPARAM <- c()
###
###    aurcparameters <- unitclass_parameters("AURCU")
###    y <- names(parameter_required(paste0("^",aurcparameters,"$"), names(result_data), simplify = FALSE))
###    AURCUPARAM <- c(AURCUPARAM, y)

    AURCUPARAM <- parameters_by_class("AURCU", names(result_data))

###    if("CONCU" %in% names(map_data)){
    if(length(AURCUPARAM)>0 && parameter_required("^CONCU$", names(map_data))){
###      if(map_data$CONCU %in% names(data_data)){
      if(parameter_required(map_data$CONCU, names(data_data))){
        aurc_unit_tmp <- unlist(strsplit(as.character(unique(data_data[, map_data$CONCU])[[1]]), "/"))
        inputUnit11 <- c(as.character(aurc_unit_tmp[2]), as.character(aurc_unit_tmp[1]), as.character(aurc_unit_tmp[2]))
###        outputUnit11 <- unlist(strsplit(unlist(strsplit(as.character(map_data$AUROUTPUTUNIT), "/")), "[.]"))

        outputUnitLabel <- "AUROUTPUTUNIT"
        testunit <- is.element(outputUnitLabel, names(map))
        outputUnitFormat <- FALSE
        if(testunit) {
            outputUnit11 <- as.character(map_data[[outputUnitLabel]])
            outputUnitFormat <- length(outputUnit11)>0 & !is.na(outputUnit11)
            if(outputUnitFormat) {
                if(length(grep("/", outputUnit11)) > 0) { outputUnit11 <- unlist(strsplit(outputUnit11, "[./]", perl=TRUE)) } else { outputUnitFormat <- FALSE }
            }
        }

        testunit <- testunit && outputUnitFormat
        if(testunit){
            testunit <- testunit && length(outputUnit11) == 3 && length(aurc_unit_tmp) == 2
        }

### Added formattedinputUnit and formattedoutputUnit to simplify output
        formattedinputUnit   <- paste(c(paste(inputUnit11[1:2], collapse="."), inputUnit11[3]), collapse="/")
        formattedoutputUnit  <- paste(c(paste(outputUnit11[1:2], collapse="."), outputUnit11[3]), collapse="/")
          
        inputMatch11 <- numeric()
        outputMatch11 <- numeric()
        aurcUScaler <- numeric()

        if(testunit){
          for(i in 1:3) {
            if(outputUnitFormat){
              inputMatch11[i] <- match(inputUnit11[i], units, nomatch = 21)
              outputMatch11[i] <- match(outputUnit11[i], units, nomatch = 21)

              if(inputMatch11[i] != 21 && outputMatch11[i] != 21) {
                inputMScale11 <- val[inputMatch11[i]]
                outputMScale11 <- val[outputMatch11[i]]
                if(class[inputMatch11[i]] == c("V", "M", "V")[i] && class[outputMatch11[i]] == c("V", "M", "V")[i]) {
                  aurcUScaler[i] <- inputMScale11/outputMScale11
                } else {
                  aurcUScaler[i] <- 1
                  warning(paste0("'CONCU'  and/or '", outputUnitLabel, "' value provided via 'map' is not accounted for unit conversion"))
                }
              } else {
                aurcUScaler[i] <- 1
                warning(paste0("'CONCU'  and/or '", outputUnitLabel, "' value provided via 'map' is not valid for unit conversion"))
              }
            }
          }
        }
        if(testunit){
          aurcUFinalScaler <- aurcUScaler[1] * aurcUScaler[2] / aurcUScaler[3]
          aurc_col <- names(result_data)[names(result_data) %in% AURCUPARAM]
          result_data[aurc_col] <- result_data[aurc_col] * aurcUFinalScaler
###          result_data$AURCU <- ifelse(aurcUFinalScaler == 1, as.character(paste0(aurc_unit_tmp[2], ".", aurc_unit_tmp[1], "/", aurc_unit_tmp[2])), as.character(map_data[[outputUnitLabel]]))
          result_data$AURCU <- ifelse(aurcUFinalScaler == 1, as.character(formattedinputUnit), as.character(formattedoutputUnit))
          if(verbose) { cat(function_name, ': Unit Class 11: ([Volume.Amount]/Volume) aurc_col: ', aurc_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', aurcUFinalScaler, '\n') }
        } else {
### retain original input unit
###          result_data$AURCU <- as.character(paste0(aurc_unit_tmp[2], ".", aurc_unit_tmp[1], "/", aurc_unit_tmp[2]))
          result_data$AURCU <- formattedinputUnit
          if(length(aurc_unit_tmp) != 2){
            warning("Unit data provided via the 'CONCU' value provided via 'map' is not in the proper form! Please try again using 'Amount/Volume' format!")
          }
          if(outputUnitFormat){
            warning("'", outputUnitLabel, "' is not present in the proper form! Please try again using '[Volume*Amount]/Volume' format!")
          }
        }
      } else {
        result_data$AURCU <- NA
        if(!(map_data$TIMEU %in% names(data_data) && map_data$CONCU %in% names(data_data))) {
          warning(paste0("'", map_data$TIMEU, "' and 'CONCU' values provided via 'map' #9# are not present in the dataset provided via 'data'"))
        } else if(!map_data$TIMEU %in% names(data_data)) {
          warning(paste0("'", map_data$TIMEU, "' value provided via 'map' is not present in the dataset provided via 'data'"))
        } else if(!map_data$CONCU %in% names(data_data)) {
          warning("'CONCU' value provided via 'map' is not present in the dataset provided via 'data'")
        }
      }
    } else {
      result_data$AURCU <- NA
      if(!(map_data$TIMEU %in% names(map_data) && "CONCU" %in% names(map_data))) {
        warning(paste0("'", map_data$TIMEU, "' and 'CONCU' #10# are not present in the dataset provided via 'map'"))
      } else if(!map_data$TIMEU %in% names(map_data)) {
        warning(paste0("'", map_data$TIMEU, "' #22# is not present in the dataset provided via 'map'"))
      } else if(!"CONCU" %in% names(map_data)) {
        warning("'CONCU' #23# is not present in the dataset provided via 'map'")
      }
    }
  }

  #----------------------------------------------------------------------------  
  #Unit Class 12: [Amount/Volume]/Amount
  ### if SS dosing, First DOSE will drive NORMalization of CONC  
  if(unit_class == "CONCNORMU" || unit_class == "ALL"){
###    cmaxdnIdx <- grep("CMAX[0-9]+DN", names(result_data))
###    if(length(cmaxdnIdx) > 0) {
###      CONCNORMUPARAM <- c(CONCNORMUPARAM, names(result_data[cmaxdnIdx]))
###    }

    CONCNORMUPARAM <- parameters_by_class("CONCNORMU", names(result_data))

###    if("CONCU" %in% names(map_data) && "DOSE1U" %in% names(map)){
    if(length(CONCNORMUPARAM)>0 && parameter_required("^CONCU$", names(map_data)) && parameter_required("^DOSE(i{1}|[0-9]*?)U$", names(map))){
###      if(map_data$CONCU %in% names(data_data) && map_data$DOSE1U %in% names(data_data)){
      if(parameter_required(map_data$CONCU, names(data_data)) && parameter_required("^DOSE(i{1}|[0-9]*?)U$", names(map))){
        if(parameter_required(map_data[as.numeric(parameter_indices("^DOSE(i{1}|[0-9]*?)U$", names(map)))], names(data_data))){
          conc_unit_tmp <- unlist(strsplit(as.character(unique(data_data[, map_data$CONCU])[[1]]), "/"))
  ###        conc_unit_tmp2 <- as.character(unique(data_data[, map_data$DOSEU])[[1]])
          conc_unit_tmp2 <- as.character(unique(data_data[,map_data[,unlist(strsplit(map_data$DOSEULIST, ";"))[1]]]))
          inputUnit12 <- c(as.character(conc_unit_tmp[1]), as.character(conc_unit_tmp[2]), conc_unit_tmp2)
  ###        outputUnit12 <- unlist(strsplit(unlist(strsplit(as.character(map_data$CONCNORMOUTPUTUNIT), "/")), "[.]"))
  
          outputUnitLabel <- "CONCNORMOUTPUTUNIT"
          testunit <- is.element(outputUnitLabel, names(map))
          outputUnitFormat <- FALSE
          if(testunit) {
              outputUnit12 <- as.character(map_data[[outputUnitLabel]])
              outputUnitFormat <- length(outputUnit12)>0 & !is.na(outputUnit12)
              if(outputUnitFormat) {
                  if(length(grep("/", outputUnit12)) > 0) { outputUnit12 <- unlist(strsplit(outputUnit12, "[./]", perl=TRUE)) } else { outputUnitFormat <- FALSE }
              }
          }
  
          testunit <- testunit && outputUnitFormat
          if(testunit){
              testunit <- testunit && length(outputUnit12) == 3 && length(conc_unit_tmp) == 2
          }
          
  ### Added formattedinputUnit and formattedoutputUnit to simplify output
          formattedinputUnit   <- paste(c(paste(inputUnit12[1:2], collapse="/"), inputUnit12[3]), collapse="/")
          formattedoutputUnit  <- paste(c(paste(outputUnit12[1:2], collapse="/"), outputUnit12[3]), collapse="/")
          
          inputMatch12 <- numeric()
          outputMatch12 <- numeric()
          concdnUScaler <- numeric()
  
          if(testunit){
            for(i in 1:3) {
              if(outputUnitFormat){
                inputMatch12[i] <- match(inputUnit12[i], units, nomatch = 21)
                outputMatch12[i] <- match(outputUnit12[i], units, nomatch = 21)
  
                if(inputMatch12[i] != 21 && outputMatch12[i] != 21) {
                  inputMScale12 <- val[inputMatch12[i]]
                  outputMScale12 <- val[outputMatch12[i]]
                  if(class[inputMatch12[i]] == c("M", "V", "M")[i] && class[outputMatch12[i]] == c("M", "V", "M")[i]) {
                    concdnUScaler[i] <- inputMScale12/outputMScale12
                  } else {
                    concdnUScaler[i] <- 1
                    warning("'CONCU' and/or 'DOSEU' and/or '", outputUnitLabel, "' value provided via 'map' is not accounted for unit conversion")
                  }
                } else {
                  concdnUScaler[i] <- 1
                  warning("'CONCU' and/or 'DOSEU' and/or '", outputUnitLabel, "' value provided via 'map' is not valid for unit conversion")
                }
              }
            }
          }
          if(testunit){
            concdnUFinalScaler <- concdnUScaler[1] * concdnUScaler[2] / concdnUScaler[3]
            concdn_col <- names(result_data)[names(result_data) %in% CONCNORMUPARAM]
            result_data[concdn_col] <- result_data[concdn_col] * concdnUFinalScaler
  ###          result_data$CONCNORMU <- ifelse(concdnUFinalScaler == 1, as.character(paste0(conc_unit_tmp[1], "/", conc_unit_tmp[2], "/", conc_unit_tmp2)), as.character(map_data[[outputUnitLabel]]))
            result_data$CONCNORMU <- ifelse(concdnUFinalScaler == 1, as.character(formattedinputUnit), as.character(formattedoutputUnit))
            if(verbose) { cat(function_name, ': Unit Class 12: ([Amount/Volume]/Amount) concdn_col: ', concdn_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', concdnUFinalScaler, '\n') }
          } else {
  ### retain original input unit
  ####          result_data$CONCNORMU <- as.character(paste0(conc_unit_tmp[1], "/", conc_unit_tmp[2], "/", conc_unit_tmp2))
            result_data$CONCNORMU <- formattedinputUnit
            if(length(conc_unit_tmp) != 2){
              warning("Unit data provided via the 'CONCU' value provided via 'map' is not in the proper form! Please try again using 'Amount/Volume' format!")
            }
            if(outputUnitFormat){
              warning("'", outputUnitLabel, "' is not present in the proper form! Please try again using '[Amount/Volume]/Amount' format!")
            }
          }
        } else {
          result_data$CONCNORMU <- NA
          doseUnitLabel <- map_data[as.numeric(parameter_indices("^DOSE(i{1}|[0-9]*?)U$", names(map)))]
          warning(paste0("'", doseUnitLabel, "' value provided via 'map' is not present in the dataset provided via 'data'"))
        }
      } else {
        result_data$CONCNORMU <- NA
        warning("'CONCU' value provided via 'map' is not present in the dataset provided via 'data'")
      }
    } else {
      result_data$CONCNORMU <- NA
      warning("'CONCU' #24# is not present in the dataset provided via 'map'")
    }
  }

  #----------------------------------------------------------------------------  
  #Unit Class 13: Amount/Time
  if(unit_class == "RATEU" || unit_class == "ALL"){
###    RATEUPARAM <- c()
###
###    maxrateIdx <- grep("MAXRATE[0-9]+", names(result_data))
###    if(length(maxrateIdx) > 0) {
###      RATEUPARAM <- c(RATEUPARAM, names(result_data[maxrateIdx]))
###    }
###    ratelastIdx <- grep("RATELAST[0-9]+", names(result_data))
###    if(length(ratelastIdx) > 0) {
###      RATEUPARAM <- c(RATEUPARAM, names(result_data[ratelastIdx]))
###    }

    RATEUPARAM <- parameters_by_class("RATEU", names(result_data))
    
###    if(map_data$TIMEU %in% names(map_data) && "CONCU" %in% names(map_data)){
###    if(parameter_required("^TIMEU$", names(map_data)) && parameter_required("^CONCU$", names(map_data))){
    if(length(RATEUPARAM>0)) {
###      if(map_data$TIMEU %in% names(data_data) && map_data$CONCU %in% names(data_data)){
###      if(parameter_required("^TIMEU$", names(data_data)) && parameter_required("^CONCU$", names(data_data))){
        rate_unit_tmp <- unlist(strsplit(as.character(unique(data_data[, map_data$CONCU])[[1]]), "/"))
        inputUnit13 <- c(as.character(rate_unit_tmp[1]), as.character(unique(data_data[, map_data$TIMEU])[[1]]))
###        outputUnit13 <- unlist(strsplit(as.character(map_data$RATEOUTPUTUNIT), "/"))

        outputUnitLabel <- "RATEOUTPUTUNIT"
        testunit <- is.element(outputUnitLabel, names(map))
        outputUnitFormat <- FALSE
        if(testunit) {
            outputUnit13 <- as.character(map_data[[outputUnitLabel]])
            outputUnitFormat <- length(outputUnit13)>0 & !is.na(outputUnit13)
            if(outputUnitFormat) {
                if(length(grep("/", outputUnit13)) > 0) { outputUnit13 <- unlist(strsplit(outputUnit13, "[/]", perl=TRUE)) } else { outputUnitFormat <- FALSE }
            }
        }

        testunit <- testunit && outputUnitFormat
        if(testunit){
            testunit <- testunit && length(outputUnit13) == 2 && length(rate_unit_tmp) == 2
        }
        
### Added formattedinputUnit and formattedoutputUnit to simplify output
        formattedinputUnit   <- paste(inputUnit13, collapse="/")
        formattedoutputUnit  <- paste(outputUnit13, collapse="/")
        
        inputMatch13 <- numeric()
        outputMatch13 <- numeric()
        rateUScaler <- numeric()

        if(testunit){
          for(i in 1:2) {
            if(outputUnitFormat){
              inputMatch13[i] <- match(inputUnit13[i], units, nomatch = 21)
              outputMatch13[i] <- match(outputUnit13[i], units, nomatch = 21)

              if(inputMatch13[i] != 21 && outputMatch13[i] != 21) {
                inputMScale13 <- val[inputMatch13[i]]
                outputMScale13 <- val[outputMatch13[i]]
                if(class[inputMatch13[i]] == c("M", "T")[i] && class[outputMatch13[i]] == c("M", "T")[i]) {
                  rateUScaler[i] <- inputMScale13/outputMScale13
                } else {
                  rateUScaler[i] <- 1
                  warning(paste0("'", map_data$TIMEU, "' and/or 'CONCU' and/or '", outputUnitLabel, "' value provided via 'map' is not accounted for unit conversion"))
                }
              } else {
                rateUScaler[i] <- 1
                warning(paste0("'", map_data$TIMEU, "' and/or 'CONCU' and/or '", outputUnitLabel, "' value provided via 'map' is not valid for unit conversion"))
              }
            }
          }
        }
        if(testunit){
          rateUFinalScaler <- rateUScaler[1] / rateUScaler[2]
          rate_col <- names(result_data)[names(result_data) %in% RATEUPARAM]
          result_data[rate_col] <- result_data[rate_col] * rateUFinalScaler
###          result_data$RATEU <- ifelse(rateUFinalScaler == 1, as.character(paste0(rate_unit_tmp[1], "/", as.character(unique(data_data[, map_data$TIMEU])[[1]]))), as.character(map_data[[outputUnitLabel]]))
          result_data$RATEU <- ifelse(rateUFinalScaler == 1, as.character(formattedinputUnit), as.character(formattedoutputUnit))
          if(verbose) { cat(function_name, ': Unit Class 13: (Amount/Time) rate_col: ', rate_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', rateUFinalScaler, '\n') }
        } else {
### retain original input unit
###          result_data$RATEU <- as.character(paste0(rate_unit_tmp[1], "/", as.character(unique(data_data[, map_data$TIMEU])[[1]])))
          result_data$RATEU <- formattedinputUnit
          if(length(rate_unit_tmp) != 2){
            warning("Unit data provided via the 'CONCU' value provided via 'map' is not in the proper form! Please try again using 'Amount/Volume' format!")
          }
          if(outputUnitFormat){
            warning("'", outputUnitLabel, "' is not present in the proper form! Please try again using 'Amount/Time' format!")
          }
        }
###      } else {
###        result_data$RATEU <- NA
###        if(!(map_data$TIMEU %in% names(data_data) && map_data$CONCU %in% names(data_data))) {
###          warning(paste0("'", map_data$TIMEU, "' and 'CONCU' values provided via 'map' #11# are not present in the dataset provided via 'data'"))
###        } else if(!map_data$TIMEU %in% names(data_data)) {
###          warning(paste0("'", map_data$TIMEU, "' value provided via 'map' is not present in the dataset provided via 'data'"))
###        } else if(!map_data$CONCU %in% names(data_data)) {
###          warning("'CONCU' value provided via 'map' is not present in the dataset provided via 'data'")
###        }
###      }
    } else {
      result_data$RATEU <- NA
###   if(!(map_data$TIMEU %in% names(map_data) && "CONCU" %in% names(map_data))) {
###        warning(paste0("'", map_data$TIMEU, "' and 'CONCU' #12# are not present in the dataset provided via 'map'"))
###      } else if(!map_data$TIMEU %in% names(map_data)) {
###        warning(paste0("'", map_data$TIMEU, "' #25# is not present in the dataset provided via 'map'"))
###      } else if(!CONCU %in% names(map_data)) {
###        warning("'CONCU' #26# is not present in the dataset provided via 'map'")
###      }
    }
  }

  #----------------------------------------------------------------------------  
  #Unit Class 14: Volume/Body weight
  if(unit_class == "VOLUMEWU" || unit_class == "ALL"){
###    vzftauIdx <- grep("VZFTAUW[0-9]+", names(result_data))
###    if(length(vzftauIdx) > 0) {
###      VOLUMEWUPARAM <- c(VOLUMEWUPARAM, names(result_data[vzftauIdx]))
###    }
###    vsspwIdx <- grep("VSSPW[0-9]+", names(result_data))
###    if(length(vsspwIdx) > 0) {
###      VOLUMEWUPARAM <- c(VOLUMEWUPARAM, names(result_data[vsspwIdx]))
###    }

    VOLUMEWUPARAM <- parameters_by_class("VOLUMEWU", names(result_data))
    
###    if("CONCU" %in% names(map_data) && "NORMBSU" %in% names(map_data)){
    if(length(VOLUMEWUPARAM)>0 && parameter_required("^CONCU$", names(map_data)) && parameter_required("^NORMBSU$", names(map_data))){
###      if(map_data$CONCU %in% names(data_data) && map_data$NORMBSU %in% names(data_data)){
      if(parameter_required(map_data$CONCU, names(data_data)) && parameter_required(map_data$NORMBSU, names(data_data))){
        vwu_unit_tmp <- unlist(strsplit(as.character(unique(data_data[, map_data$CONCU])[[1]]), "/"))
        inputUnit14 <- c(as.character(vwu_unit_tmp[2]), as.character(unique(data_data[, map_data$NORMBSU])[1]))
###        outputUnit14 <- unlist(strsplit(as.character(map_data$VOLUMENORMOUTPUTUNIT), "/"))

        outputUnitLabel <- "VOLUMENORMOUTPUTUNIT"
        testunit <- is.element(outputUnitLabel, names(map))
        outputUnitFormat <- FALSE
        if(testunit) {
            outputUnit14 <- as.character(map_data[[outputUnitLabel]])
            outputUnitFormat <- length(outputUnit14)>0 & !is.na(outputUnit14)
            if(outputUnitFormat) {
                if(length(grep("/", outputUnit14)) > 0) { outputUnit14 <- unlist(strsplit(outputUnit14, "[/]", perl=TRUE)) } else { outputUnitFormat <- FALSE }
            }
        }

        testunit <- testunit && outputUnitFormat
        if(testunit){
            testunit <- testunit && length(outputUnit14) == 2 && length(vwu_unit_tmp) == 2
        }
        
### Added formattedinputUnit and formattedoutputUnit to simplify output
        formattedinputUnit   <- paste(inputUnit14, collapse="/")
        formattedoutputUnit  <- paste(outputUnit14, collapse="/")
        
        inputMatch14 <- numeric()
        outputMatch14 <- numeric()
        volumewUScaler <- numeric()

        if(testunit){
          for(i in 1:2) {
            if(outputUnitFormat){
              inputMatch14[i] <- match(inputUnit14[i], units, nomatch = 21)
              outputMatch14[i] <- match(outputUnit14[i], units, nomatch = 21)

              if(inputMatch14[i] != 21 && outputMatch14[i] != 21) {
                inputMScale14 <- val[inputMatch14[i]]
                outputMScale14 <- val[outputMatch14[i]]
                if(class[inputMatch14[i]] == c("V", "M")[i] && class[outputMatch14[i]] == c("V", "M")[i]) {
                  volumewUScaler[i] <- inputMScale14/outputMScale14
                } else {
                  volumewUScaler[i] <- 1
                  warning("'CONCU' and/or 'NORMBSU' and/or '", outputUnitLabel, "' value provided via 'map' is not accounted for unit conversion")
                }
              } else {
                volumewUScaler[i] <- 1
                warning("'CONCU' and/or 'NORMBSU' and/or '", outputUnitLabel, "' value provided via 'map' is not valid for unit conversion")
              }
            }
          }
        }
        if(testunit){
          volumewUFinalScaler <- volumewUScaler[1] / volumewUScaler[2]
          volumew_col <- names(result_data)[names(result_data) %in% VOLUMEWUPARAM]
          result_data[volumew_col] <- result_data[volumew_col] * volumewUFinalScaler
###          result_data$VOLUMEWU <- ifelse(volumewUFinalScaler == 1, as.character(paste0(vwu_unit_tmp[2], "/", as.character(unique(data_data[, map_data$NORMBSU])[1]))), as.character(map_data[[outputUnitLabel]]))
          result_data$VOLUMEWU <- ifelse(volumewUFinalScaler == 1, as.character(formattedinputUnit), as.character(formattedoutputUnit))
          if(verbose) { cat(function_name, ': Unit Class 14: (Volume/Body Weight) volumew_col: ', volumew_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', volumewUFinalScaler, '\n') }
        } else {
### retain original input unit
###          result_data$VOLUMEWU <- as.character(paste0(vwu_unit_tmp[2], "/", as.character(unique(data_data[, map_data$NORMBSU])[1])))
          result_data$VOLUMEWU <- formattedinputUnit
          if(length(vwu_unit_tmp) != 2){
            warning("Unit data provided via the 'CONCU' value provided via 'map' is not in the proper form! Please try again using 'Amount/Volume' format!")
          }
          if(outputUnitFormat){
            warning("'", outputUnitLabel, "' is not present in the proper form! Please try again using 'Volume/Body' format!")
          }
        }
      } else {
        result_data$VOLUMEWU <- NA
        if(!(map_data$CONCU %in% names(data_data) && map_data$NORMBSU %in% names(data_data))) {
          warning("'CONCU' and 'NORMBSU' value provided via 'map' #13# are not present in the dataset provided via 'data'")
        } else if(!map_data$CONCU %in% names(data_data)) {
          warning("'CONCU' value provided via 'map' is not present in the dataset provided via 'data'")
        } else if(!map_data$NORMBSU %in% names(data_data)) {
          warning("'NORMBSU' value provided via 'map' is not present in the dataset provided via 'data'")
        }
      }
    } else {
      result_data$VOLUMEWU <- NA
      if(!("CONCU" %in% names(map_data) && "NORMBSU" %in% names(map_data))) {
        warning("'CONCU' and 'NORMBSU' #14# are not present in the dataset provided via 'map'")
      } else if(!"CONCU" %in% names(map_data)) {
        warning("'CONCU' value provided via 'map' #27# is not present in the dataset provided via 'map'")
      } else if(!"NORMBSU" %in% names(map_data)) {
        warning("'NORMBSU' value provided via 'map' #28# is not present in the dataset provided via 'map'")
      }
    }
  }

  #----------------------------------------------------------------------------  
  #Unit Class 15: Volume/Time/Body weight
  if(unit_class == "CLWU" || unit_class == "ALL"){
###    clftauwIdx <- grep("CLFTAUW[0-9]+", names(result_data))
###    if(length(clftauwIdx) > 0) {
###      CLWUPARAM <- c(CLWUPARAM, names(result_data[clftauwIdx]))
###    }
###    cltauwIdx <- grep("CLTAUW[0-9]+", names(result_data))
###    if(length(cltauwIdx) > 0) {
###      CLWUPARAM <- c(CLWUPARAM, names(result_data[cltauwIdx]))
###    }

    CLWUPARAM <- parameters_by_class("CLWU", names(result_data))

###    if("CONCU" %in% names(map_data) && map_data$TIMEU %in% names(map_data) && "NORMBSU" %in% names(map_data)){
    if(length(CLWUPARAM)>0 && parameter_required("^CONCU$", names(map_data)) && parameter_required("^TIMEU$", names(map_data)) && parameter_required("^NORMBSU$", names(map_data))){
###      if(map_data$CONCU %in% names(data_data) && map_data$TIMEU %in% names(data_data) && map_data$NORMBSU %in% names(data_data)){
      if(parameter_required(map_data$CONCU, names(data_data)) && parameter_required(map_data$TIMEU, names(data_data)) && parameter_required(map_data$NORMBSU, names(data_data))){
        clwu_unit_tmp <- unlist(strsplit(as.character(unique(data_data[, map_data$CONCU])[[1]]), "/"))
        inputUnit15 <- c(as.character(clwu_unit_tmp[2]), as.character(unique(data_data[, map_data$TIMEU])[[1]]), as.character(unique(data_data[, map_data$NORMBSU])[1]))
###        outputUnit15 <- unlist(strsplit(as.character(map_data$CLNORMOUTPUTUNIT), "/"))

        outputUnitLabel <- "CLNORMOUTPUTUNIT"
        testunit <- is.element(outputUnitLabel, names(map))
        outputUnitFormat <- FALSE
        if(testunit) {
            outputUnit15 <- as.character(map_data[[outputUnitLabel]])
            outputUnitFormat <- length(outputUnit15)>0 & !is.na(outputUnit15)
            if(outputUnitFormat) {
                if(length(grep("/", outputUnit15)) > 0) { outputUnit15 <- unlist(strsplit(outputUnit15, "[/]", perl=TRUE)) } else { outputUnitFormat <- FALSE }
            }
        }

        testunit <- testunit && outputUnitFormat
        if(testunit){
            testunit <- testunit && length(outputUnit15) == 3 && length(clwu_unit_tmp) == 2
        }
        
### Added formattedinputUnit and formattedoutputUnit to simplify output
        formattedinputUnit   <- paste(inputUnit15, collapse="/")
        formattedoutputUnit  <- paste(outputUnit15, collapse="/")

        inputMatch15 <- numeric()
        outputMatch15 <- numeric()
        clwUScaler <- numeric()

        if(testunit){
          for(i in 1:3) {
            if(outputUnitFormat){
              inputMatch15[i] <- match(inputUnit15[i], units, nomatch = 21)
              outputMatch15[i] <- match(outputUnit15[i], units, nomatch = 21)

              if(inputMatch15[i] != 21 && outputMatch15[i] != 21) {
                inputMScale15 <- val[inputMatch15[i]]
                outputMScale15 <- val[outputMatch15[i]]
                if(class[inputMatch15[i]] == c("V", "T", "M")[i] && class[outputMatch15[i]] == c("V", "T", "M")[i]) {
                  clwUScaler[i] <- inputMScale15/outputMScale15
                } else {
                  clwUScaler[i] <- 1
                  warning(paste0("'CONCU' and/or '", map_data$TIMEU, " and/or 'NORMBSU' and/or '", outputUnitFormat, "' value provided via 'map' is not accounted for unit conversion"))
                }
              } else {
                clwUScaler[i] <- 1
                warning(paste0("'CONCU' and/or '", map_data$TIMEU, " and/or 'NORMBSU' and/or '", outputUnitFormat, "' value provided via 'map' is not valid for unit conversion"))
              }
            }
          }
        }
        if(testunit){
          clwUFinalScaler <- clwUScaler[1] / clwUScaler[2] / clwUScaler[3]
          clw_col <- names(result_data)[names(result_data) %in% CLWUPARAM]
          result_data[clw_col] <- result_data[clw_col] * clwUFinalScaler
###          result_data$CLWU <- ifelse(clwUFinalScaler == 1, as.character(paste0(clwu_unit_tmp[2], "/", as.character(unique(data_data[, map_data$TIMEU])[[1]]), "/", as.character(unique(data_data[, map_data$NORMBSU])[1]))), as.character(map_data[[outputUnitLabel]]))
          result_data$CLWU <- ifelse(clwUFinalScaler == 1, as.character(formattedinputUnit), as.character(formattedoutputUnit))
          if(verbose) { cat(function_name, ': Unit Class 15: (Volume/Time/Body Weight) clw_col: ', clw_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', clwUFinalScaler, '\n') }
        } else {
### retain original input unit
###          result_data$CLWU <- as.character(paste0(clwu_unit_tmp[2], "/", as.character(unique(data_data[, map_data$TIMEU])[[1]]), "/", as.character(unique(data_data[, map_data$NORMBSU])[1])))
          result_data$CLWU <- formattedinputUnit
          if(length(clwu_unit_tmp) != 2){
            warning("Unit data provided via the 'CONCU' value provided via 'map' is not in the proper form! Please try again using 'Amount/Volume' format!")
          }
          if(outputUnitFormat){
            warning("'", outputUnitLabel, "' is not present in the proper form! Please try again using 'Volume/Time/Body' format!")
          }
        }
      } else {
        result_data$CLWU <- NA
        if(!(map_data$CONCU %in% names(data_data) && map_data$TIMEU %in% names(data_data) && map_data$NORMBSU %in% names(data_data))) {
          warning(paste0("'CONCU', '", map_data$TIMEU, " and 'NORMBSU' value provided via 'map' #15# are not present in the dataset provided via 'data'"))
        } else if(!(map_data$CONCU %in% names(data_data) && map_data$TIMEU %in% names(data_data))) {
          warning(paste0("'CONCU' and '", map_data$TIMEU, "' value provided via 'map' #16#  are not present in the dataset provided via 'data'"))
        } else if(!(map_data$TIMEU %in% names(data_data) && map_data$NORMBSU %in% names(data_data))) {
          warning(paste0("'", map_data$TIMEU, " and 'NORMBSU' value provided via 'map' #17# are not present in the dataset provided via 'data'"))
        } else if(!(map_data$CONCU %in% names(data_data) && map_data$NORMBSU %in% names(data_data))) {
          warning("'CONCU' and 'NORMBSU' value provided via 'map' #18# are not present in the dataset provided via 'data'")
        } else if(!map_data$CONCU %in% names(data_data)) {
          warning("'CONCU' value provided via 'map' is not present in the dataset provided via 'data'")
        } else if(!map_data$TIMEU %in% names(data_data)) {
          warning(paste0("'", map_data$TIMEU, " value provided via 'map' is not present in the dataset provided via 'data'"))
        } else if(!map_data$NORMBSU %in% names(data_data)) {
          warning("'NORMBSU' value provided via 'map' is not present in the dataset provided via 'data'")
        }
      }
    } else {
      result_data$CLWU <- NA
      if(!("CONCU" %in% names(map_data) && map_data$TIMEU %in% names(map_data) && "NORMBSU" %in% names(data_data))) {
        warning(paste0("'CONCU', '", map_data$TIMEU, " and 'NORMBSU' #19# are not present in the dataset provided via 'map'"))
      } else if(!("CONCU" %in% names(map_data) && map_data$TIMEU %in% names(map_data))) {
        warning(paste0("'CONCU' and '", map_data$TIMEU, "' #20# are not present in the dataset provided via 'map'"))
      } else if(!(map_data$TIMEU %in% names(map_data) && "NORMBSU" %in% names(map_data))) {
        warning(paste0("'", map_data$TIMEU, " and 'NORMBSU' #21# are not present in the dataset provided via 'map'"))
      } else if(!("CONCU" %in% names(map_data) && "NORMBSU" %in% names(map_data))) {
        warning("'CONCU' and 'NORMBSU' #22# are not present in the dataset provided via 'map'")
      } else if(!"CONCU" %in% names(map_data)) {
        warning("'CONCU' #29# is not present in the dataset provided via 'map'")
      } else if(!map_data$TIMEU %in% names(map_data)) {
        warning(paste0("'", map_data$TIMEU, " #30# is not present in the dataset provided via 'map'"))
      } else if(!"NORMBSU" %in% names(map_data)) {
        warning("'NORMBSU' #31# is not present in the dataset provided via 'map'")
      }
    }
  }
  return(result_data)
}
