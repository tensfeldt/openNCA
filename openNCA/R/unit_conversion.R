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

  u_class  <- c("TIMEU", "AMOUNTU", "DOSEU", "VOLUMEU", "CONCU", "KELU", "CLU", "AUCU", "AUMCU", "AUCNORMU", "AURCU", "CONCNORMU", "RATEU", "VOLUMEWU", "CLWU", "ALL")
  units    <- c("H", "HR", "MIN", "LB", "KG", "GM", "DPM", "ngeq",  "DG", "CG", "MG", "UG", "MCG", "NG",  "PG",  "FG", "KL", "L", "DL", "CL", "ML", "UL", "NL",  "PL",  "FL",        "")
  val      <- c(  60, 60,     1,  453.592, 1e3,    1,     1,      1,  1e-1, 1e-2, 1e-3, 1e-6,  1e-6, 1e-9, 1e-12, 1e-15,  1e3,   1, 1e-1, 1e-2, 1e-3, 1e-6, 1e-9, 1e-12, 1e-15,        NA)
  class    <- c( "T", "T",   "T",  "M", "M",  "M",   "M",    "M",   "M",  "M",  "M",  "M",   "M",  "M",   "M",   "M",  "V", "V",  "V",  "V",  "V",  "V",  "V",   "V",   "V", "MISSING")

  dose_numerator <- c("DPM", "ngeq")
  dose_denominator <- c("KM2", "M2", "DM2", "CM2", "MM2", "UM2", "NM2",  "PM2",  "FM2")
  dose_ignore_params <- c('CLFO','CLFP','CLFTAUi','CLP','CLO','CLTAUi','VSSP','VSSPi','VSSO','VSSOi','VZFO','VZFP','VZFTAUi','VZO','VZP','VZTAUi','AEPCT','AETAUPTi','AETPCT')
  
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
  
  #This should reprsent the 'NA' value in array 'val'
  missing_idx <- length(val)

  #----------------------------------------------------------------------------  
  #Unit Class 1: Time
  if(unit_class == "TIMEU" || unit_class == "ALL"){
    TIMEUPARAM <- parameters_by_class("TIMEU", names(result_data))
      
    if(length(TIMEUPARAM)>0 && parameter_required("^TIMEU$", names(map_data))){
      if(parameter_required(map_data$TIMEU, names(data_data))){
        inputUnit1 <- as.character(unique(data_data[, map_data$TIMEU])[1])
        outputUnitLabel <- "TIMEOUTPUTUNIT"
        testunit <- is.element(outputUnitLabel, names(map))
        outputUnitFormat <- FALSE
        outputUnit1 <- NA
        
        if(testunit) {
          outputUnit1 <- as.character(map_data[[outputUnitLabel]])
          outputUnitFormat <- length(outputUnit1)>0 & isTRUE(!is.na(outputUnit1))
        } 
        testunit <- testunit && outputUnitFormat

        formattedinputUnit   <- inputUnit1
        formattedoutputUnit  <- outputUnit1
          
        if(testunit){
          inputMatch1 <- match(inputUnit1, units, nomatch = missing_idx)
          outputMatch1 <- match(outputUnit1, units, nomatch = missing_idx)

          if(inputMatch1 != missing_idx && outputMatch1 != missing_idx) {
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
          result_data$TIMEU <- ifelse(timeUScaler == 1 & as.character(formattedinputUnit) == as.character(formattedoutputUnit), as.character(formattedinputUnit), as.character(formattedoutputUnit))
          if(verbose) { cat(function_name, ': Unit Class 1 (Time) time_col: ', time_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', timeUScaler, '\n') }
        } else {
          result_data$TIMEU <- formattedinputUnit
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
    AMOUNTUPARAM <- parameters_by_class("AMOUNTU", names(result_data))

    if(length(AMOUNTUPARAM)>0 && parameter_required("^AMOUNTU$",names(map_data)) && parameter_required("^CONCU$",names(map_data))){
      if(parameter_required(map_data$AMOUNTU, names(data_data))){
        if(!is.null(map_data$CONCU)){
          if(!is.null(unique(data_data[, map_data$CONCU])[1])){
            conc_unit_tmp <- as.character(unlist(strsplit(as.character(unique(data_data[, map_data$CONCU])[1]), "/"))) 
          } else {
            conc_unit_tmp <- NA
          }
        } else {
          conc_unit_tmp <- NA
        }
        if(length(grep("/", as.character(unique(data_data[, map_data$CONCU])))) > 0){
          inputUnit2 <- as.character(conc_unit_tmp[1])
        } else {
          inputUnit2 <- NA
        }

        outputUnitLabel <- "AMOUNTOUTPUTUNIT"
        testunit <- is.element(outputUnitLabel, names(map))
        outputUnitFormat <- FALSE
        outputUnit2 <- NA
        
        if(testunit) {
            outputUnit2 <- as.character(map_data[[outputUnitLabel]])
            outputUnitFormat <- length(outputUnit2)>0 & isTRUE(!is.na(outputUnit2))
        }
        testunit <- testunit && outputUnitFormat

        formattedinputUnit   <- inputUnit2
        formattedoutputUnit  <- outputUnit2

        if(testunit){
          inputMatch2 <- match(inputUnit2, units, nomatch = missing_idx)
          outputMatch2 <- match(outputUnit2, units, nomatch = missing_idx)

          if(inputMatch2 != missing_idx && outputMatch2 != missing_idx) {
            inputMScale2 <- val[inputMatch2]
            outputMScale2 <- val[outputMatch2]
            if(class[inputMatch2] == "M" && class[outputMatch2] == "M") {
              amountUScaler <- ifelse(isTRUE(inputUnit2 == "DPM" || outputUnit2 == "DPM"), 1, inputMScale2/outputMScale2)
            } else {
              amountUScaler <- 1
              warning("'AMOUNTU' and/or '", outputUnitLabel, "' value provided via 'map' is not accounted for unit conversion")
            }
          } else {
            amountUScaler <- 1
            warning("'AMOUNTU' and/or '", outputUnitLabel, "' value provided via 'map' is not valid for unit conversion")
          }
          amount_col <- names(result_data)[names(result_data) %in% AMOUNTUPARAM]
          result_data[amount_col] <- result_data[amount_col] * amountUScaler
          result_data$AMOUNTU <- ifelse(amountUScaler == 1 & as.character(formattedinputUnit) == as.character(formattedoutputUnit), as.character(formattedinputUnit), as.character(formattedoutputUnit))
          if(verbose) { cat(function_name, ': Unit Class 2 (Amount) amount_col: ', amount_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', amountUScaler, '\n') }
        } else {
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
    DOSEUPARAM <- parameters_by_class("DOSEU", names(result_data))

    if(length(DOSEUPARAM)>0 && parameter_required("^DOSELIST$", names(map_data)) && parameter_required("^DOSEULIST$", names(map_data))){
      doselist <- names(parameter_indices("^DOSELIST$", names(map_data), simplify=FALSE))
      dosevar <- ifelse(!is.null(doselist), ifelse(!is.null(map_data[,doselist]), unlist(strsplit(map_data[,doselist], ";")), NA), NA)
      doseulist <- names(parameter_indices("^DOSEULIST$", names(map_data), simplify=FALSE))
      doseuvar <- ifelse(!is.null(doseulist), ifelse(!is.null(map_data[,doselist]), unlist(strsplit(map_data[,doseulist], ";")), NA), NA)

      interval_len <- length(dosevar)
      if(interval_len > 1){
        DOSEUPARAM <- c(DOSEUPARAM, dosevar)
        
        for(i in 1:length(doseuvar)){
          if(parameter_required(paste0("^", doseuvar[i], "$"),names(map_data))) {
            if(parameter_required(map_data[, doseuvar[i]], names(data_data))) {
              #FEEDBACK: Update DOSEU conversion to account for invalid input units and
              #update parameteres where applicable, Based of 3.9a scope item (Based on tc1582_M1SD)
              #Also based of 3.11 scope item (Based on tc1661_M1SD)
              if(length(unique(data_data[, as.character(map_data[,doseuvar])])) == 1){
                if(length(grep("/", as.character(unique(data_data[, as.character(map_data[,doseuvar])])[1])) > 0)){
                  inputUnit3 <- as.character(unlist(strsplit(as.character(unique(data_data[, as.character(map_data[,doseuvar[i]])])[1]), "/")))
                  dose_mass_input <- TRUE
                  dose_mass_output <- FALSE
                } else {
                  inputUnit3 <- as.character(unique(data_data[, map_data[, doseuvar[i]]])[1])
                  dose_mass_input <- FALSE
                  dose_mass_output <- FALSE
                }
              } else {
                inputUnit3 <- ""
                dose_mass_input <- FALSE
                dose_mass_output <- FALSE
              }
              
              outputUnitLabel <- "DOSEOUTPUTUNIT"
              testunit <- is.element(outputUnitLabel, names(map))
              outputUnitFormat <- FALSE
              outputUnit3 <- NA
              
              if(testunit) {
                  outputUnit3 <- as.character(map_data[[outputUnitLabel]])
                  outputUnitFormat <- length(outputUnit3)>0 & isTRUE(!is.na(outputUnit3))
                  if(outputUnitFormat && isTRUE(dose_mass_input)) {
                    if(length(grep("/", outputUnit3)) > 0) { 
                      outputUnit3 <- unlist(strsplit(outputUnit3, "/"))
                      dose_mass_output <- TRUE
                    } else { 
                      outputUnitFormat <- FALSE
                    }
                  }
              }
              testunit <- testunit && outputUnitFormat
            
              if(isTRUE(dose_mass_input)) { 
                formattedinputUnit   <- paste(inputUnit3,collapse="/")
              } else {
                formattedinputUnit   <- inputUnit3
              }
              if(isTRUE(dose_mass_output)) { 
                formattedoutputUnit   <- paste(outputUnit3,collapse="/")
              } else {
                formattedoutputUnit  <- outputUnit3
              }
            
              valid_input_unit <- TRUE
              valid_output_unit <- TRUE
              if(testunit){
                if(isTRUE(dose_mass_input) && isTRUE(dose_mass_output)){
                  result_data[,names(result_data) %in% dose_ignore_params] <- NA
                  
                  doseUScaler <- numeric()
                  inputMatch3 <- numeric()
                  outputMatch3 <- numeric()
                  DPMcheck <- FALSE
                  
                  for(i in 1:2) {
                    if(!all(is.na(outputUnit3))){
                      inputMatch3[i] <- match(inputUnit3[i], units, nomatch = missing_idx)
                      outputMatch3[i] <- match(outputUnit3[i], units, nomatch = missing_idx)
                      if(inputMatch3[i] != missing_idx && outputMatch3[i] != missing_idx) {
                        inputMScale3 <- val[inputMatch3[i]]
                        outputMScale3 <- val[outputMatch3[i]]
                        if(class[inputMatch3[i]] == c("M", "M")[i] && class[outputMatch3[i]] == c("M", "M")[i]) {
                          if(class[inputMatch3[i]] == "M" && class[outputMatch3[i]] == "M"){
                            if(isTRUE(inputUnit3[i] == "DPM" || outputUnit3[i] == "DPM")){
                              doseUScaler[i] <- 1
                              DPMcheck <- TRUE
                            } else {
                              doseUScaler[i] <- inputMScale3/outputMScale3
                            }
                          } else {
                            doseUScaler[i] <- inputMScale3/outputMScale3
                          }
                        } else if(i == 2 && isTRUE(DPMcheck) && class[inputMatch3[i]] == c("M", "V")[i] && class[outputMatch3[i]] == c("M", "V")[i]) {
                          doseUScaler[i] <- inputMScale3/outputMScale3
                        } else if(i == 2 && inputUnit3[i] %in% dose_denominator && outputUnit3[i] %in% dose_denominator) {
                          doseUScaler[i] <- 1
                          if(inputUnit3[i] != outputUnit3[i]){
                            warning("'", inputUnit3[i], "' to '", outputUnit3[i], "' DOSEU conversion is not accounted for unit conversion")
                          }
                        } else {
                          doseUScaler[i] <- 1
                          warning("'DOSEU' and/or '", outputUnitLabel, "' value provided via 'map' is not accounted for unit conversion")
                        }
                      } else {
                        if(inputMatch3[i] == missing_idx){
                          valid_input_unit <- FALSE
                        }
                        if(outputMatch3[i] == missing_idx){
                          valid_output_unit <- FALSE
                        }
                        doseUScaler[i] <- 1
                        warning("'DOSEU' and/or '", outputUnitLabel, "' value provided via 'map' is not valid for unit conversion")
                      }
                    } else {
                      valid_output_unit <- FALSE
                    }
                  }
                  if(testunit) { testunit <- testunit && valid_input_unit && valid_output_unit }
                  if(testunit){
                    doseUFinalScaler <- doseUScaler[1]/doseUScaler[2]
                    doseUScaler <- doseUFinalScaler
                  } else {
                    result_data$DOSEU <- formattedinputUnit
                    if(length(inputUnit3) != 2){
                      warning("Unit data provided via the 'DOSEU' value provided via 'map' is not in the proper form! Please try again using 'Amount' or 'Amount/Amount' format!")
                    }
                    if(outputUnitFormat){
                      warning("'", outputUnitLabel, "' is not present in the proper form! Please try again using 'Amount' or 'Amount/Amount' format!")
                    }
                  }
                } else if(!isTRUE(dose_mass_input) && !isTRUE(dose_mass_output)){
                  inputMatch3 <- match(inputUnit3, units, nomatch = missing_idx)
                  outputMatch3 <- match(outputUnit3, units, nomatch = missing_idx)
                  if(inputMatch3 != missing_idx && outputMatch3 != missing_idx) {
                    inputMScale3 <- val[inputMatch3]
                    outputMScale3 <- val[outputMatch3]
                    if(class[inputMatch3] == "M" && class[outputMatch3] == "M") {
                      doseUScaler <- ifelse(isTRUE(inputUnit3 == "DPM" || outputUnit3 == "DPM"), 1, inputMScale3/outputMScale3)
                    } else {
                      doseUScaler <- 1
                      warning("'DOSEU' and/or '", outputUnitLabel, "' value provided via 'map' is not accounted for unit conversion")
                    }
                  } else {
                    if(inputMatch3 == missing_idx){
                      valid_input_unit <- FALSE
                    }
                    if(outputMatch3 == missing_idx){
                      valid_output_unit <- FALSE
                    }
                    doseUScaler <- 1
                    warning("'DOSEU' and/or '", outputUnitLabel, "' value provided via 'map' is not valid for unit conversion")
                  }
                } else {
                  doseUScaler <- 1
                  warning("'DOSEU' and/or '", outputUnitLabel, "' value provided via 'map' is not valid for unit conversion")
                }
                if(testunit) { testunit <- testunit && valid_input_unit && valid_output_unit }
                if(testunit){
                  dose_col <- names(result_data)[names(result_data) %in% dosevar[i]]
                  result_data[dose_col] <- result_data[dose_col] * doseUScaler
                  result_data[doseuvar[i]] <- ifelse(doseUScaler == 1 & as.character(formattedinputUnit) == as.character(formattedoutputUnit), as.character(formattedinputUnit), as.character(formattedoutputUnit))
                }
                if(!valid_input_unit || !valid_output_unit){
                  dose_dep_col <- c(grep("^DOSE", names(result_data)), grep("DN$", names(result_data)))
                  result_data[,dose_dep_col] <- NA
                }
                if(verbose && testunit) { cat(function_name, ': Unit Class 3 (Dose) dose_col: ', dose_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', doseUScaler, '\n') }
              } else {
                result_data[doseuvar[i]] <- formattedinputUnit
                if(outputUnitFormat){
                  warning("'", outputUnitLabel, "' is not present in the proper form! Please try again using 'Dose Amount' format!")
                }
             }
            } else {
              result_data[dosevar[i]] <- NA
              warning(paste0("'", doseuvar[i], "' value provided via 'map' is not present in the dataset provided via 'data'"))
            }
          } else {
            result_data[doseuvar[i]] <- NA
            warning(paste0("'", doseuvar[i], "' #3# is not present in the dataset provided via 'map'"))
          }
        }
        if(!is.element("CONCOUTPUTUNIT", names(map))){
          warning("'CONCOUTPUTUNIT' is not present in the proper form! Please try again using 'Amount/Volume' format!")
        }
      } else {
        if(parameter_required(doseuvar, names(map_data))){
          if(parameter_required(map_data[,doseuvar], names(data_data))){
            if(length(unique(data_data[, as.character(map_data[,doseuvar])])) == 1){
              if(length(grep("/", as.character(unique(data_data[, as.character(map_data[,doseuvar])])[1])) > 0)){
                inputUnit3 <- as.character(unlist(strsplit(as.character(unique(data_data[, as.character(map_data[,doseuvar])])[1]), "/")))
                dose_mass_input <- TRUE
                dose_mass_output <- FALSE
              } else {
                inputUnit3 <- as.character(unique(data_data[, as.character(map_data[,doseuvar])])[1])
                dose_mass_input <- FALSE
                dose_mass_output <- FALSE
              }
            } else {
              inputUnit3 <- ""
              dose_mass_input <- FALSE
              dose_mass_output <- FALSE
            }
            outputUnitLabel <- "DOSEOUTPUTUNIT"
            testunit <- is.element(outputUnitLabel, names(map))
            outputUnitFormat <- FALSE
            outputUnit3 <- ""

            if(testunit) {
              outputUnit3 <- as.character(map_data[[outputUnitLabel]])
              outputUnitFormat <- length(outputUnit3)>0 & isTRUE(!is.na(outputUnit3))
              if(outputUnitFormat && isTRUE(dose_mass_input)) {
                if(length(grep("/", outputUnit3)) > 0) { 
                  outputUnit3 <- unlist(strsplit(outputUnit3, "/"))
                  dose_mass_output <- TRUE
                } else { 
                  outputUnitFormat <- FALSE
                }
              }
            }
            testunit <- testunit && outputUnitFormat
              
            if(isTRUE(dose_mass_input)) { 
              formattedinputUnit   <- paste(inputUnit3,collapse="/")
            } else {
              formattedinputUnit   <- inputUnit3
            }
            if(isTRUE(dose_mass_output)) { 
              formattedoutputUnit   <- paste(outputUnit3,collapse="/")
            } else {
              formattedoutputUnit  <- outputUnit3
            }
            
            valid_input_unit <- TRUE
            valid_output_unit <- TRUE
            if(testunit){
              if(isTRUE(dose_mass_input) && isTRUE(dose_mass_output)){
                result_data[,names(result_data) %in% dose_ignore_params] <- NA
                
                doseUScaler <- numeric()
                inputMatch3 <- numeric()
                outputMatch3 <- numeric()
                DPMcheck <- FALSE
                
                for(i in 1:2) {
                  if(!all(is.na(outputUnit3))){
                    inputMatch3[i] <- match(inputUnit3[i], units, nomatch = missing_idx)
                    outputMatch3[i] <- match(outputUnit3[i], units, nomatch = missing_idx)
                    if(inputMatch3[i] != missing_idx && outputMatch3[i] != missing_idx) {
                      inputMScale3 <- val[inputMatch3[i]]
                      outputMScale3 <- val[outputMatch3[i]]
                      if(class[inputMatch3[i]] == c("M", "M")[i] && class[outputMatch3[i]] == c("M", "M")[i]) {
                        if(class[inputMatch3[i]] == "M" && class[outputMatch3[i]] == "M"){
                          if(isTRUE(inputUnit3[i] == "DPM" || outputUnit3[i] == "DPM")){
                            doseUScaler[i] <- 1
                            DPMcheck <- TRUE
                          } else {
                            doseUScaler[i] <- inputMScale3/outputMScale3
                          }
                        } else {
                          doseUScaler[i] <- inputMScale3/outputMScale3
                        }
                      } else if(i == 2 && isTRUE(DPMcheck) && class[inputMatch3[i]] == c("M", "V")[i] && class[outputMatch3[i]] == c("M", "V")[i]) {
                        doseUScaler[i] <- inputMScale3/outputMScale3
                      } else if(i == 2 && inputUnit3[i] %in% dose_denominator && outputUnit3[i] %in% dose_denominator) {
                        doseUScaler[i] <- 1
                        if(inputUnit3[i] != outputUnit3[i]){
                          warning("'", inputUnit3[i], "' to '", outputUnit3[i], "' DOSEU conversion is not accounted for unit conversion")
                        }
                      } else {
                        doseUScaler[i] <- 1
                        warning("'DOSEU' and/or '", outputUnitLabel, "' value provided via 'map' is not accounted for unit conversion")
                      }
                    } else {
                      if(inputMatch3[i] == missing_idx){
                        valid_input_unit <- FALSE
                      }
                      if(outputMatch3[i] == missing_idx){
                        valid_output_unit <- FALSE
                      }
                      doseUScaler[i] <- 1
                      warning("'DOSEU' and/or '", outputUnitLabel, "' value provided via 'map' is not valid for unit conversion")
                    }
                  } else {
                    valid_output_unit <- FALSE
                  }
                }
                if(testunit) { testunit <- testunit && valid_input_unit && valid_output_unit }
                if(testunit){
                  doseUFinalScaler <- doseUScaler[1]/doseUScaler[2]
                  doseUScaler <- doseUFinalScaler
                } else {
                  result_data$DOSEU <- formattedinputUnit
                  if(length(inputUnit3) != 2){
                    warning("Unit data provided via the 'DOSEU' value provided via 'map' is not in the proper form! Please try again using 'Amount' or 'Amount/Amount' format!")
                  }
                  if(outputUnitFormat){
                    warning("'", outputUnitLabel, "' is not present in the proper form! Please try again using 'Amount' or 'Amount/Amount' format!")
                  }
                }
              } else if(!isTRUE(dose_mass_input) && !isTRUE(dose_mass_output)){
                inputMatch3 <- match(inputUnit3, units, nomatch = missing_idx)
                outputMatch3 <- match(outputUnit3, units, nomatch = missing_idx)
                if(inputMatch3 != missing_idx && outputMatch3 != missing_idx) {
                  inputMScale3 <- val[inputMatch3]
                  outputMScale3 <- val[outputMatch3]
                  if(class[inputMatch3] == "M" && class[outputMatch3] == "M") {
                    doseUScaler <- ifelse(isTRUE(inputUnit3 == "DPM" || outputUnit3 == "DPM"), 1, inputMScale3/outputMScale3)
                  } else {
                    doseUScaler <- 1
                    warning("'DOSEU' and/or '", outputUnitLabel, "' value provided via 'map' is not accounted for unit conversion")
                  }
                } else {
                  if(inputMatch3 == missing_idx){
                    valid_input_unit <- FALSE
                  }
                  if(outputMatch3 == missing_idx){
                    valid_output_unit <- FALSE
                  }
                  doseUScaler <- 1
                  warning("'DOSEU' and/or '", outputUnitLabel, "' value provided via 'map' is not valid for unit conversion")
                }
              } else {
                doseUScaler <- 1
                warning("'DOSEU' and/or '", outputUnitLabel, "' value provided via 'map' is not valid for unit conversion")
              }
              if(testunit) { testunit <- testunit && valid_input_unit && valid_output_unit }
              if(testunit){
                dose_col <- names(result_data)[names(result_data) %in% DOSEUPARAM]
                result_data[dose_col] <- result_data[dose_col] * doseUScaler
                result_data$DOSEU <- ifelse(doseUScaler == 1 & as.character(formattedinputUnit) == as.character(formattedoutputUnit), as.character(formattedinputUnit), as.character(formattedoutputUnit))
              }
              if(!valid_input_unit || !valid_output_unit){
                dose_dep_col <- c(grep("^DOSE", names(result_data)), grep("DN$", names(result_data)))
                result_data[,dose_dep_col] <- NA
              }
              if(verbose && testunit) { cat(function_name, ': Unit Class 3 (Dose) dose_col: ', dose_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', doseUScaler, '\n') }
            } else {
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
    VOLUMEUPARAM <- parameters_by_class("VOLUMEU", names(result_data))

    if(length(VOLUMEUPARAM)>0 && parameter_required("^CONCU$", names(map_data))){
      if(map_data$CONCU %in% names(data_data)){
        if(!is.null(map_data$CONCU)){
          if(!is.null(unique(data_data[, map_data$CONCU])[1])){
            volume_unit_tmp <- as.character(unlist(strsplit(as.character(unique(data_data[, map_data$CONCU])[1]), "/"))) 
          } else {
            volume_unit_tmp <- NA
          }
        } else {
          volume_unit_tmp <- NA
        }
        inputUnit4 <- ifelse(length(as.character(volume_unit_tmp)) == 2, as.character(volume_unit_tmp[2]), NA)

        outputUnitLabel <- "VOLUMEOUTPUTUNIT"
        testunit <- is.element(outputUnitLabel, names(map))
        outputUnitFormat <- FALSE
        outputUnit4 <- NA
        
        if(testunit) {
            outputUnit4 <- as.character(map_data[[outputUnitLabel]])
            outputUnitFormat <- length(outputUnit4)>0 & isTRUE(!is.na(outputUnit4))
        }
        testunit <- testunit && outputUnitFormat

        formattedinputUnit   <- inputUnit4
        formattedoutputUnit  <- outputUnit4

        if(testunit && (length(volume_unit_tmp) == 2)){
          inputMatch4 <- match(inputUnit4, units, nomatch = missing_idx)
          outputMatch4 <- match(outputUnit4, units, nomatch = missing_idx)

          if(inputMatch4 != missing_idx && outputMatch4 != missing_idx) {
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
          result_data$VOLUMEU <- ifelse(volumeUScaler == 1 & as.character(formattedinputUnit) == as.character(formattedoutputUnit), as.character(formattedinputUnit), as.character(formattedoutputUnit))
          if(verbose) { cat(function_name, ': Unit Class 4 (Volume) volume_col: ', volume_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', volumeUScaler, '\n') }
        } else {
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
        warning(paste0("'", map_data$CONCU, " value provided via 'map' is not present in the dataset provided via 'data'"))
      }
    } else {
      result_data$VOLUMEU <- NA
      warning(paste0("'", map_data$CONCU, "' #5# is not present in the dataset provided via 'map'"))
    }
  }

  #----------------------------------------------------------------------------  
  #Unit Class 5: Amount/Volume
  if(unit_class == "CONCU" || unit_class == "ALL"){
    CONCUPARAM <- parameters_by_class("CONCU", names(result_data))
      
    if(length(CONCUPARAM)>0 & parameter_required("^CONCU$", names(map_data))){
      if(parameter_required(map_data$CONCU, names(data_data))){
        if(!is.null(map_data$CONCU)){
          if(!is.null(unique(data_data[, map_data$CONCU])[1])){
            conc_unit_tmp <- as.character(unlist(strsplit(as.character(unique(data_data[, map_data$CONCU])[1]), "/"))) 
          } else {
            conc_unit_tmp <- NA
          }
        } else {
          conc_unit_tmp <- NA
        }
        if(length(grep("/", as.character(unique(data_data[, map_data$CONCU])))) > 0){
          inputUnit5 <- as.character(conc_unit_tmp)
        } else {
          inputUnit5 <- NA
        }
        outputUnitLabel <- "CONCOUTPUTUNIT"
        testunit <- is.element(outputUnitLabel, names(map))
        outputUnitFormat <- FALSE
        outputUnit5 <- NA
        
        if(testunit) {
            outputUnit5 <- as.character(map_data[[outputUnitLabel]])
            outputUnitFormat <- length(outputUnit5)>0 & isTRUE(!is.na(outputUnit5))
            if(outputUnitFormat) {
                if(length(grep("/", outputUnit5)) > 0) { outputUnit5 <- unlist(strsplit(outputUnit5, "/")) } else { outputUnitFormat <- FALSE }
            }
        }

        testunit <- testunit && outputUnitFormat
        if(testunit){
            testunit <- testunit && length(outputUnit5) == 2 && length(conc_unit_tmp) == 2
        }

        formattedinputUnit   <- paste(inputUnit5,collapse="/")
        formattedoutputUnit  <- paste(outputUnit5,collapse="/")
        
        concUScaler <- numeric()
        inputMatch5 <- numeric()
        outputMatch5 <- numeric()

        if(testunit){
          for(i in 1:2) {
            if(!all(is.na(outputUnit5))){
              inputMatch5[i] <- match(inputUnit5[i], units, nomatch = missing_idx)
              outputMatch5[i] <- match(outputUnit5[i], units, nomatch = missing_idx)
              if(inputMatch5[i] != missing_idx && outputMatch5[i] != missing_idx) {
                inputMScale5 <- val[inputMatch5[i]]
                outputMScale5 <- val[outputMatch5[i]]
                if(class[inputMatch5[i]] == c("M", "V")[i] && class[outputMatch5[i]] == c("M", "V")[i]) {
                  if(class[inputMatch5[i]] == "M" && class[outputMatch5[i]] == "M"){
                    concUScaler[i] <- ifelse(isTRUE(inputUnit5[i] == "DPM" || outputUnit5[i] == "DPM"), 1, inputMScale5/outputMScale5)
                  } else {
                    concUScaler[i] <- inputMScale5/outputMScale5
                  }
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
          result_data$CONCU <- ifelse(concUFinalScaler == 1 & as.character(formattedinputUnit) == as.character(formattedoutputUnit), as.character(formattedinputUnit), as.character(formattedoutputUnit))
          if(verbose) { cat(function_name, ': Unit Class 5 (Amount/Volume) conc_col: ', conc_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', concUFinalScaler, '\n') }
        } else {
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
        warning(paste0("'", map_data$CONCU, "' value provided via 'map' is not present in the dataset provided via 'data'"))
      }
    } else {
        if(parameter_required("^CONCU$", names(map_data)) && parameter_required(map_data$CONCU, names(data_data))) {
          result_data$CONCU <- unique(data_data[, map_data$CONCU])
        }
        else {
          result_data$CONCU <- NA
          warning(paste0("'", map_data$CONCU, "' #6# is not present in the dataset provided via 'map'"))
        }
    }
  }

  #----------------------------------------------------------------------------  
  #Unit Class 6: 1/Time
  if(unit_class == "KELU" || unit_class == "ALL"){
    KELUPARAM <- parameters_by_class("KELU", names(result_data))

      if(length(KELUPARAM)>0 && (parameter_required(map_data$TIME, names(data_data)) &&
          parameter_required(map_data$TIMEU, names(data_data)))) {
        inputUnit6 <- NA
        outputUnit6 <- NA
        if(parameter_required(map_data$TIMEU, names(data_data))) { inputUnit6 <- as.character(unique(data_data[,map_data$TIMEU])) }  
          outputUnit6 <- ifelse(!is.null(map_data$KELOUTPUTUNIT), ifelse(length(grep("/", map_data$KELOUTPUTUNIT)) > 0,
                         ifelse(as.character(unlist(strsplit(as.character(map_data$KELOUTPUTUNIT), "/")))[1] == "1",
                                as.character(unlist(strsplit(as.character(map_data$KELOUTPUTUNIT), "/")))[2], NA), NA), NA)

        outputUnitLabel <- "KELOUTPUTUNIT"
        testunit <- is.element(outputUnitLabel, names(map))
        outputUnitFormat <- FALSE
        formattedinputUnit   <- paste("1",inputUnit6,sep="/")
        formattedoutputUnit  <- paste("1",outputUnit6,sep="/")

        numerator <- "1"
        if(testunit) {
            baseUnit6 <- outputUnit6 <- as.character(map_data[[outputUnitLabel]])
            outputUnitFormat <- length(outputUnit6)>0 & isTRUE(!is.na(outputUnit6))
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
          inputMatch6 <- match(inputUnit6, units, nomatch = missing_idx)
          outputMatch6 <- match(outputUnit6, units, nomatch = missing_idx)

          if(inputMatch6 != missing_idx && outputMatch6 != missing_idx) {
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

          outputUnit6 <- paste0(numerator, "/", outputUnit6)
          result_data$KELU <- ifelse(kelUScaler == 1 & as.character(formattedinputUnit) == as.character(formattedoutputUnit), formattedinputUnit, formattedoutputUnit)
          if(verbose) { cat(function_name, ': Unit Class 6 (1/Time) kel_col: ', kel_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', kelUScaler, '\n') }
        } else {
          result_data$KELU <- formattedinputUnit
          if(outputUnitFormat){
            warning("'", outputUnitLabel, "' is not present in the proper form! Please try again using '1/Time' format!")
          }
        }
    } else {
      result_data$KELU <- NA
      warning(paste0("'", map_data$TIMEU, "' #7# is not present in the dataset provided via 'map'"))
    }
  }

  #----------------------------------------------------------------------------  
  #Unit Class 7: Volume/Time
  if(unit_class == "CLU" || unit_class == "ALL"){
    CLUPARAM <- parameters_by_class("CLU", names(result_data))

    if(length(CLUPARAM)>0 && parameter_required("^TIMEU$", names(map_data)) && parameter_required("^CONCU$", names(map_data))){
      if(parameter_required(map_data$TIMEU, names(data_data)) && parameter_required(map_data$CONCU, names(data_data))){
        inputconcunit <- as.character(unique(data_data[, map_data$CONCU])[[1]])
        inputtimeunit <- as.character(unique(data_data[, map_data$TIMEU])[[1]])
        if(!is.null(map_data$CONCU)){
          if(!is.null(unique(data_data[, map_data$CONCU])[1])){
            clu_unit_tmp <- as.character(unlist(strsplit(as.character(unique(data_data[, map_data$CONCU])[1]), "/"))) 
          } else {
            clu_unit_tmp <- NA
          }
        } else {
          clu_unit_tmp <- NA
        }
        inputUnit7 <- c(as.character(clu_unit_tmp[2]), as.character(unique(data_data[, map_data$TIMEU])[[1]]))

        outputUnitLabel <- "CLOUTPUTUNIT"
        testunit <- is.element(outputUnitLabel, names(map))
        outputUnitFormat <- FALSE
        outputUnit7 <- NA
      
        if(testunit) {
            outputUnit7 <- as.character(map_data[[outputUnitLabel]])
            outputUnitFormat <- length(outputUnit7)>0 & isTRUE(!is.na(outputUnit7))
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

        formattedinputUnit   <- paste(inputUnit7,collapse="/")
        formattedoutputUnit  <- paste(outputUnit7,collapse="/")
        
        if(testunit){
          for(i in 1:2) {
            if(outputUnitFormat){
              inputMatch7[i] <- match(inputUnit7[i], units, nomatch = missing_idx)
              outputMatch7[i] <- match(outputUnit7[i], units, nomatch = missing_idx)

              if(inputMatch7[i] != missing_idx && outputMatch7[i] != missing_idx) {
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
          result_data$CLU <- ifelse(clUFinalScaler == 1 & as.character(formattedinputUnit) == as.character(formattedoutputUnit), formattedinputUnit, formattedoutputUnit)
          if(verbose) { cat(function_name, ': Unit Class 7 (Volume/Time) cl_col: ', cl_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', clUFinalScaler, '\n') }
        } else {
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
          warning(paste0("'", map_data$TIMEU, "' and '", map_data$CONCU, "' values provided via 'map' #1# are not present in the dataset provided via 'data'"))
        } else if(!map_data$TIMEU %in% names(data_data)) {
          warning(paste0("'", map_data$TIMEU, "' value provided via 'map' is not present in the dataset provided via 'data'"))
        } else if(!map_data$CONCU %in% names(data_data)) {
          warning(paste0("'", map_data$CONCU, "' value provided via 'map' is not present in the dataset provided via 'data'"))
        }
      }
    } else {
      result_data$CLU <- NA
      if(!(parameter_required(map_data$TIMEU, names(data_data)) && parameter_required(map_data$CONCU, names(data_data)))) {
        warning(paste0("'", map_data$TIMEU, "' and '", map_data$CONCU, "' #1# are not present in the dataset provided via 'map'"))
      } else if(!(parameter_required(map_data$TIMEU, names(data_data)))) {
        warning(paste0("'", map_data$TIMEU, "' value #8# is not present in the dataset provided via 'map'"))
      } else if(!(parameter_required("^CONCU$", names(map_data)))) {
        warning(paste0("'", map_data$CONCU, "' #9# is not present in the dataset provided via 'map'"))
      }
    }
  }

  #----------------------------------------------------------------------------  
  #Unit Class 8: Amount.Time/Volume
  if(unit_class == "AUCU" || unit_class == "ALL"){
    AUCUPARAM <- parameters_by_class("AUCU", names(result_data))
    
    if(length(AUCUPARAM)>0 && parameter_required("^TIMEU$", names(map_data)) && parameter_required("^CONCU$", names(map_data))){
      if(parameter_required(map_data$TIMEU, names(data_data)) && parameter_required(map_data$CONCU, names(data_data))){
        inputconcunit <- as.character(unique(data_data[, map_data$CONCU])[[1]])
        inputtimeunit <- as.character(unique(data_data[, map_data$TIMEU])[[1]])
        
        if(!is.null(map_data$CONCU)){
          if(!is.null(unique(data_data[, map_data$CONCU])[1])){
            auc_unit_tmp <- as.character(unlist(strsplit(as.character(unique(data_data[, map_data$CONCU])[1]), "/"))) 
          } else {
            auc_unit_tmp <- NA
          }
        } else {
          auc_unit_tmp <- NA
        }
        
        inputUnit8 <- c(as.character(auc_unit_tmp[1]), as.character(unique(data_data[, map_data$TIMEU])[[1]]), as.character(auc_unit_tmp[2]))
        outputUnit8 <- ifelse(!is.null(map_data$AUCOUTPUTUNIT), unlist(strsplit(unlist(strsplit(as.character(map_data$AUCOUTPUTUNIT), "/")), "[.]")), NA)          
        outputUnitLabel <- "AUCOUTPUTUNIT"
        testunit <- is.element(outputUnitLabel, names(map))
        outputUnitFormat <- FALSE
        outputUnit8 <- NA
        
        if(testunit) {
            outputUnit8 <- as.character(map_data[[outputUnitLabel]])
            outputUnitFormat <- length(outputUnit8)>0 & isTRUE(!is.na(outputUnit8))
            if(outputUnitFormat) {
                if(length(grep("/", outputUnit8)) > 0) { outputUnit8 <- unlist(strsplit(outputUnit8, "[./]", perl=TRUE)) } else { outputUnitFormat <- FALSE }
            }
        }
        
        testunit <- testunit && outputUnitFormat
        if(testunit){
            testunit <- testunit && length(outputUnit8) == 3 && length(auc_unit_tmp) == 2
        }

        formattedinputUnit   <- paste(c(paste(inputUnit8[1:2], collapse="."), inputUnit8[3]), collapse="/")
        formattedoutputUnit  <- paste(c(paste(outputUnit8[1:2], collapse="."), outputUnit8[3]), collapse="/")
          
        inputMatch8 <- numeric()
        outputMatch8 <- numeric()
        aucUScaler <- numeric()

        if(testunit){
          for(i in 1:3) {
            if(outputUnitFormat){
              inputMatch8[i] <- match(inputUnit8[i], units, nomatch = missing_idx)
              outputMatch8[i] <- match(outputUnit8[i], units, nomatch = missing_idx)

              if(inputMatch8[i] != missing_idx && outputMatch8[i] != missing_idx) {
                inputMScale8 <- val[inputMatch8[i]]
                outputMScale8 <- val[outputMatch8[i]]
                if(class[inputMatch8[i]] == c("M", "T", "V")[i] && class[outputMatch8[i]] == c("M", "T", "V")[i]) {
                  if(class[inputMatch8[i]] == "M" && class[outputMatch8[i]] == "M") {
                    aucUScaler[i] <- ifelse(isTRUE(inputUnit8[i] == "DPM" || outputUnit8[i] == "DPM"), 1, inputMScale8/outputMScale8)
                  } else {
                    aucUScaler[i] <- inputMScale8/outputMScale8
                  }
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
          result_data$AUCU <- ifelse(aucUFinalScaler == 1 & as.character(formattedinputUnit) == as.character(formattedoutputUnit), as.character(formattedinputUnit), as.character(formattedoutputUnit))
          if(verbose) { cat(function_name, ': Unit Class 8: (Amount.Time/Volume) auc_col: ', auc_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', aucUFinalScaler, '\n') }
        } else {
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
          warning(paste0("'", map_data$TIMEU, "' and '", map_data$CONCU, "' values provided via 'map' #3# are not present in the dataset provided via 'data'"))
        } else if(!(parameter_required(map_data$TIMEU, names(data_data)))) {
          warning(paste0("'", map_data$TIMEU, "' value provided via 'map' is not present in the dataset provided via 'data'"))
        } else if(!(parameter_required(map_data$CONCU, names(data_data)))) {
          warning(paste0("'", map_data$CONCU, "' value provided via 'map' is not present in the dataset provided via 'data'"))
        }
      }
    } else {
      result_data$AUCU <- NA
      if(!(parameter_required(map_data$TIMEU, names(data_data)) && parameter_required(map_data$CONCU, names(data_data)))) {
        warning(paste0("'", map_data$TIMEU, "' and '", map_data$CONCU, "' #4# are not present in the dataset provided via 'map'"))
      } else if(!(parameter_required(map_data$TIMEU,names(data_data)))) {
        warning(paste0("'", map_data$TIMEU, "' #10# is not present in the dataset provided via 'map'"))
      } else if(!(parameter_required("^CONCU$", names(map_data)))) {
        warning(paste0("'", map_data$CONCU, "' #11# is not present in the dataset provided via 'map'"))
      }
    }
  }

  #----------------------------------------------------------------------------  
  #Unit Class 9: Amount.Time^2/Volume (Amount.Time.Time/Volume)
  if(unit_class == "AUMCU" || unit_class == "ALL"){
    AUMCUPARAM <- parameters_by_class("AUMCU", names(result_data))

    if(length(AUMCUPARAM)>0 && parameter_required("^TIMEU$", names(map_data)) && parameter_required("^CONCU$", names(map_data))){
      if(parameter_required(map_data$TIMEU, names(data_data)) && parameter_required(map_data$CONCU, names(data_data))){
        inputconcunit <- as.character(unique(data_data[, map_data$CONCU])[[1]])
        inputtimeunit <- as.character(unique(data_data[, map_data$TIMEU])[[1]])
        
        if(!is.null(map_data$CONCU)){
          if(!is.null(unique(data_data[, map_data$CONCU])[1])){
            aumc_unit_tmp <- as.character(unlist(strsplit(as.character(unique(data_data[, map_data$CONCU])[1]), "/"))) 
          } else {
            aumc_unit_tmp <- NA
          }
        } else {
          aumc_unit_tmp <- NA
        }
        aumc_unit_tmp2 <- as.character(unique(data_data[, map_data$TIMEU])[[1]])
        inputUnit9 <- c(as.character(aumc_unit_tmp[1]), aumc_unit_tmp2, aumc_unit_tmp2, as.character(aumc_unit_tmp[2]))

        outputUnitLabel <- "AUMCOUTPUTUNIT"
        testunit <- is.element(outputUnitLabel, names(map))
        outputUnitFormat <- FALSE
        outputUnit9 <- NA
        
        if(testunit) {
            outputUnit9 <- as.character(map_data[[outputUnitLabel]])
            outputUnitFormat <- length(outputUnit9)>0 & isTRUE(!is.na(outputUnit9))
            if(outputUnitFormat) {
                if(length(grep("/", outputUnit9)) > 0) { outputUnit9 <- unlist(strsplit(outputUnit9, "[./^]", perl=TRUE)) } else { outputUnitFormat <- FALSE }
            }
        }
        
        testunit <- testunit && outputUnitFormat
        if(testunit){
            testunit <- testunit && length(outputUnit9) == 4 && length(aumc_unit_tmp) == 2
        }

        formattedinputUnit   <- paste(c(paste(inputUnit9[1:3],  collapse="."), inputUnit9[4]),  collapse="/")
        formattedoutputUnit  <- paste(c(paste(outputUnit9[1:3], collapse="."), outputUnit9[4]), collapse="/")

        inputMatch9 <- numeric()
        outputMatch9 <- numeric()
        aumcUScaler <- numeric()

        if(testunit){
          for(i in 1:4) {
            if(outputUnitFormat){
              inputMatch9[i] <- match(inputUnit9[i], units, nomatch = missing_idx)
              outputMatch9[i] <- match(outputUnit9[i], units, nomatch = missing_idx)

              if(inputMatch9[i] != missing_idx && outputMatch9[i] != missing_idx) {
                inputMScale9 <- val[inputMatch9[i]]
                outputMScale9 <- val[outputMatch9[i]]
                if(class[inputMatch9[i]] == c("M", "T", "T", "V")[i] && class[outputMatch9[i]] == c("M", "T", "T", "V")[i]) {
                  if(class[inputMatch9[i]] == "M" && class[outputMatch9[i]] == "M") {
                    aumcUScaler[i] <- ifelse(isTRUE(inputUnit9[i] == "DPM" || outputUnit9[i] == "DPM"), 1, inputMScale9/outputMScale9)
                  } else {
                    aumcUScaler[i] <- inputMScale9/outputMScale9
                  }
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
          result_data$AUMCU <- ifelse(aumcUFinalScaler == 1 & as.character(formattedinputUnit) == as.character(formattedoutputUnit), as.character(formattedinputUnit), as.character(formattedoutputUnit))
          if(verbose) { cat(function_name, ': Unit Class 9: (Amount.Time.Time/Volume) aumc_col: ', aumc_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', aumcUFinalScaler, '\n') }
        } else {
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
          warning(paste0("'", map_data$TIMEU, "' and '", map_data$CONCU, "' values provided via 'map' #5# are not present in the dataset provided via 'data'"))
        } else if(!map_data$TIMEU %in% names(data_data)) {
          warning(paste0("'", map_data$TIMEU, "' value provided via 'map' is not present in the dataset provided via 'data'"))
        } else if(!map_data$CONCU %in% names(data_data)) {
          warning(paste0("'", map_data$CONCU, "' value provided via 'map' is not present in the dataset provided via 'data'"))
        }
      }
    } else {
      result_data$AUMCU <- NA
      if(!(parameter_required("^TIMEU$", names(map_data)) && parameter_required("^CONCU$", names(map_data)))) {
        warning(paste0("'", map_data$TIMEU, "' and '", map_data$CONCU, "' #6# are not present in the dataset provided via 'map'"))
      } else if(!(parameter_required("^TIMEU$", names(map_data)))) {
        warning(paste0("'", map_data$TIMEU, "' #13# is not present in the dataset provided via 'map'"))
      } else if(!"CONCU" %in% names(map_data)) {
        warning(paste0("'", map_data$CONCU, "' #14# is not present in the dataset provided via 'map'"))
      }
    }
  }

  #----------------------------------------------------------------------------  
  #Unit Class 10: [Amount.Time/Volume]/Amount
  if(unit_class == "AUCNORMU" || unit_class == "ALL"){
    AUCNORMUPARAM <- parameters_by_class("AUCNORMU", names(result_data))
    
    if(length(AUCNORMUPARAM)>0 && parameter_required("^TIMEU$", names(map_data)) && parameter_required("^CONCU$", names(map_data)) && parameter_required("^DOSE[0-9]*?U$", names(map))){
      if(parameter_required(map_data$TIMEU, names(data_data)) && parameter_required(map_data$CONCU, names(data_data)) && parameter_required("^DOSE(i{1}|[0-9]*?)U$", names(map))){
        inputconcunit <- as.character(unique(data_data[, map_data$CONCU])[[1]])
        inputtimeunit <- as.character(unique(data_data[, map_data$TIMEU])[[1]])
        
        if(!is.null(map_data$CONCU)){
          if(!is.null(unique(data_data[, map_data$CONCU])[1])){
            aucdn_unit_tmp <- as.character(unlist(strsplit(as.character(unique(data_data[, map_data$CONCU])[1]), "/"))) 
          } else {
            aucdn_unit_tmp <- NA
          }
        } else {
          aucdn_unit_tmp <- NA
        }
        aucdn_unit_tmp2 <- as.character(unique(data_data[, map_data$TIMEU])[[1]])
        aucdn_unit_tmp3 <- as.character(unique(data_data[, map_data[,unlist(strsplit(map_data$DOSEULIST, ";"))[1]]]))
        inputUnit10 <- c(as.character(aucdn_unit_tmp[1]), aucdn_unit_tmp2, as.character(aucdn_unit_tmp[2]), aucdn_unit_tmp3)

        outputUnitLabel <- "AUCNORMOUTPUTUNIT"
        testunit <- is.element(outputUnitLabel, names(map))
        outputUnitFormat <- FALSE
        outputUnit10 <- NA
        
        if(testunit) {
            outputUnit10 <- as.character(map_data[[outputUnitLabel]])
            outputUnitFormat <- length(outputUnit10)>0 & isTRUE(!is.na(outputUnit10))
            if(outputUnitFormat) {
                if(length(grep("/", outputUnit10)) > 0) { outputUnit10 <- unlist(strsplit(outputUnit10, "[./^]", perl=TRUE)) } else { outputUnitFormat <- FALSE }
            }
        }
        testunit <- testunit && outputUnitFormat
        if(testunit){
            testunit <- testunit && length(outputUnit10) == 4 && length(aucdn_unit_tmp) == 2
        }

        formattedinputUnit   <- paste(c(paste(inputUnit10[1:2],  collapse="."), inputUnit10[3:4]),  collapse="/")
        formattedoutputUnit  <- paste(c(paste(outputUnit10[1:2], collapse="."), outputUnit10[3:4]), collapse="/")
        
        inputMatch10 <- numeric()
        outputMatch10 <- numeric()
        aucdnUScaler <- numeric()

        if(testunit){
          for(i in 1:4) {
            if(outputUnitFormat){
              inputMatch10[i] <- match(inputUnit10[i], units, nomatch = missing_idx)
              outputMatch10[i] <- match(outputUnit10[i], units, nomatch = missing_idx)

              if(inputMatch10[i] != missing_idx && outputMatch10[i] != missing_idx) {
                inputMScale10 <- val[inputMatch10[i]]
                outputMScale10 <- val[outputMatch10[i]]
                if(class[inputMatch10[i]] == c("M", "T", "V", "M")[i] && class[outputMatch10[i]] == c("M", "T", "V", "M")[i]) {
                  if(class[inputMatch10[i]] == "M" && class[outputMatch10[i]] == "M") {
                    aucdnUScaler[i] <- ifelse(isTRUE(inputUnit10[i] == "DPM" || outputUnit10[i] == "DPM"), 1, inputMScale10/outputMScale10)
                  } else {
                    aucdnUScaler[i] <- inputMScale10/outputMScale10
                  }
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
          result_data$AUCNORMU <- ifelse(aucdnUFinalScaler == 1 & as.character(formattedinputUnit) == as.character(formattedoutputUnit), as.character(formattedinputUnit), as.character(formattedoutputUnit))
          if(verbose) { cat(function_name, ': Unit Class 10: ([Amount.Time/Volume]/Amount) aucdn_col: ', aucdn_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', aucdnUFinalScaler, '\n') }
        } else {
          result_data$AUCNORMU <- formattedinputUnit
          if(length(aucdn_unit_tmp) != 2){
            warning("Unit data provided via the 'CONCU' value provided via 'map' is not in the proper form! Please try again using 'Amount/Volume' format!")
          }
          if(outputUnitFormat){
            warning("'", outputUnitLabel, "' is not present in the proper form! Please try again using '[Amount*Time/Volume]/Amount' format!")
          }
        }
      } else {
        xdoseu <- ifelse(!is.null(map_data$DOSEULIST), unlist(strsplit(map_data$DOSEULIST, ";"))[1], NA)
        result_data$AUCNORMU <- NA
        if(!(map_data$TIMEU %in% names(data_data) && map_data$CONCU %in% names(data_data) && map_data[,xdoseu] %in% names(data_data))) {
          warning(paste0("'", map_data$TIMEU, "', '", map_data$CONCU, "' and '", map_data[,xdoseu], "' values provided via 'map' #7# are not present in the dataset provided via 'data'"))
        } else if(!(map_data$TIMEU %in% names(data_data) && map_data$CONCU %in% names(data_data))) {
          warning(paste0("'", map_data$TIMEU, "' and '", map_data$CONCU, "' values provided via 'map' is not present in the dataset provided via 'data'"))
        } else if(!(map_data$CONCU %in% names(data_data) && map_data$DOSEU %in% names(data_data))) {
          warning(paste0("'", map_data$CONCU, "' and '", map_data[,xdoseu], "' values provided via 'map' is not present in the dataset provided via 'data'"))
        } else if(!(map_data$TIMEU %in% names(data_data) && map_data$DOSEU %in% names(data_data))) {
          warning(paste0("'", map_data$TIMEU, "' and '", map_data[,xdoseu], "' values provided via 'map' is not present in the dataset provided via 'data'"))
        } else if(!map_data$TIMEU %in% names(data_data)) {
          warning(paste0("'", map_data$TIMEU, "' value provided via 'map' is not present in the dataset provided via 'data'"))
        } else if(!map_data$CONCU %in% names(data_data)) {
          warning(paste0("'", map_data$CONCU, "' value provided via 'map' is not present in the dataset provided via 'data'"))
        } else if(!map_data$DOSEU %in% names(data_data)) {
          warning(paste0("'", map_data[,xdoseu], "' value provided via 'map' is not present in the dataset provided via 'data'"))
        }
      }
    } else {
      result_data$AUCNORMU <- NA
      xdoseu <- ifelse(!is.null(map_data$DOSEULIST), unlist(strsplit(map_data$DOSEULIST, ";"))[1], NA)
      if(!(map_data$TIMEU %in% names(map_data) && "CONCU" %in% names(map_data) && xdoseu %in% names(map_data))) {
        warning(paste0("'", map_data$TIMEU, "', '", map_data$CONCU, "' and '", map_data[,xdoseu], "' #8# are not present in the dataset provided via 'map'"))
      } else if(!(map_data$TIMEU %in% names(map_data) && "CONCU" %in% names(map_data))) {
        warning(paste0("'", map_data$TIMEU, "' and '", map_data$CONCU, "' #16# is not present in the dataset provided via 'map'"))
      } else if(!("CONCU" %in% names(map_data) && "DOSEU" %in% names(map_data))) {
        warning(paste0("'", map_data$CONCU, "' and '", map_data[,xdoseu], "' #17# is not present in the dataset provided via 'map'"))
      } else if(!(map_data$TIMEU %in% names(map_data) && "DOSEU" %in% names(map_data))) {
        warning(paste0("'", map_data$TIMEU, "' and '", map_data[,xdoseu], "' #18# is not present in the dataset provided via 'map'"))
      } else if(!map_data$TIMEU %in% names(map_data)) {
        warning(paste0("'", map_data$TIMEU, "' #19# is not present in the dataset provided via 'map'"))
      } else if(!"CONCU" %in% names(map_data)) {
        warning(paste0("'", map_data$CONCU, "' #20# is not present in the dataset provided via 'map'"))
      } else if(!"DOSEU" %in% names(map_data)) {
        warning(paste0("'", map_data[,xdoseu], "' #21# is not present in the dataset provided via 'map'"))
      }
    }
  }

  #----------------------------------------------------------------------------  
  #Unit Class 11: [Volume.Amount]/Volume
  if(unit_class == "AURCU" || unit_class == "ALL"){
    AURCUPARAM <- parameters_by_class("AURCU", names(result_data))

    if(length(AURCUPARAM)>0 && parameter_required("^CONCU$", names(map_data))){
      if(parameter_required(map_data$CONCU, names(data_data))){
        if(!is.null(map_data$CONCU)){
          if(!is.null(unique(data_data[, map_data$CONCU])[1])){
            aurc_unit_tmp <- as.character(unlist(strsplit(as.character(unique(data_data[, map_data$CONCU])[1]), "/"))) 
          } else {
            aurc_unit_tmp <- NA
          }
        } else {
          aurc_unit_tmp <- NA
        }
        inputUnit11 <- c(as.character(aurc_unit_tmp[2]), as.character(aurc_unit_tmp[1]), as.character(aurc_unit_tmp[2]))

        outputUnitLabel <- "AUROUTPUTUNIT"
        testunit <- is.element(outputUnitLabel, names(map))
        outputUnitFormat <- FALSE
        outputUnit11 <- NA
        
        if(testunit) {
            outputUnit11 <- as.character(map_data[[outputUnitLabel]])
            outputUnitFormat <- length(outputUnit11)>0 & isTRUE(!is.na(outputUnit11))
            if(outputUnitFormat) {
                if(length(grep("/", outputUnit11)) > 0) { outputUnit11 <- unlist(strsplit(outputUnit11, "[./]", perl=TRUE)) } else { outputUnitFormat <- FALSE }
            }
        }

        testunit <- testunit && outputUnitFormat
        if(testunit){
            testunit <- testunit && length(outputUnit11) == 3 && length(aurc_unit_tmp) == 2
        }

        formattedinputUnit   <- paste(c(paste(inputUnit11[1:2], collapse="."), inputUnit11[3]), collapse="/")
        formattedoutputUnit  <- paste(c(paste(outputUnit11[1:2], collapse="."), outputUnit11[3]), collapse="/")
          
        inputMatch11 <- numeric()
        outputMatch11 <- numeric()
        aurcUScaler <- numeric()

        if(testunit){
          for(i in 1:3) {
            if(outputUnitFormat){
              inputMatch11[i] <- match(inputUnit11[i], units, nomatch = missing_idx)
              outputMatch11[i] <- match(outputUnit11[i], units, nomatch = missing_idx)

              if(inputMatch11[i] != missing_idx && outputMatch11[i] != missing_idx) {
                inputMScale11 <- val[inputMatch11[i]]
                outputMScale11 <- val[outputMatch11[i]]
                if(class[inputMatch11[i]] == c("V", "M", "V")[i] && class[outputMatch11[i]] == c("V", "M", "V")[i]) {
                  if(class[inputMatch11[i]] == "M" && class[outputMatch11[i]] == "M") {
                    aurcUScaler[i] <- ifelse(isTRUE(inputUnit11[i] == "DPM" || outputUnit11[i] == "DPM"), 1, inputMScale11/outputMScale11)
                  } else {
                    aurcUScaler[i] <- inputMScale11/outputMScale11
                  }
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
          result_data$AURCU <- ifelse(aurcUFinalScaler == 1 & as.character(formattedinputUnit) == as.character(formattedoutputUnit), as.character(formattedinputUnit), as.character(formattedoutputUnit))
          if(verbose) { cat(function_name, ': Unit Class 11: ([Volume.Amount]/Volume) aurc_col: ', aurc_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', aurcUFinalScaler, '\n') }
        } else {
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
          warning(paste0("'", map_data$TIMEU, "' and '", map_data$CONCU, "' values provided via 'map' #9# are not present in the dataset provided via 'data'"))
        } else if(!map_data$TIMEU %in% names(data_data)) {
          warning(paste0("'", map_data$TIMEU, "' value provided via 'map' is not present in the dataset provided via 'data'"))
        } else if(!map_data$CONCU %in% names(data_data)) {
          warning(paste0("'", map_data$CONCU, "' value provided via 'map' is not present in the dataset provided via 'data'"))
        }
      }
    } else {
      result_data$AURCU <- NA
      if(!(map_data$TIMEU %in% names(map_data) && "CONCU" %in% names(map_data))) {
        warning(paste0("'", map_data$TIMEU, "' and '", map_data$CONCU, "' #10# are not present in the dataset provided via 'map'"))
      } else if(!map_data$TIMEU %in% names(map_data)) {
        warning(paste0("'", map_data$TIMEU, "' #22# is not present in the dataset provided via 'map'"))
      } else if(!"CONCU" %in% names(map_data)) {
        warning(paste0("'", map_data$CONCU, "' #23# is not present in the dataset provided via 'map'"))
      }
    }
  }

  #----------------------------------------------------------------------------  
  #Unit Class 12: [Amount/Volume]/Amount
  if(unit_class == "CONCNORMU" || unit_class == "ALL"){
    CONCNORMUPARAM <- parameters_by_class("CONCNORMU", names(result_data))

    if(length(CONCNORMUPARAM)>0 && parameter_required("^CONCU$", names(map_data)) && parameter_required("^DOSE(i{1}|[0-9]*?)U$", names(map))){
      if(parameter_required(map_data$CONCU, names(data_data)) && parameter_required("^DOSE(i{1}|[0-9]*?)U$", names(map))){
        if(parameter_required(map_data[as.numeric(parameter_indices("^DOSE(i{1}|[0-9]*?)U$", names(map)))], names(data_data))){
          if(!is.null(map_data$CONCU)){
            if(!is.null(unique(data_data[, map_data$CONCU])[1])){
              conc_unit_tmp <- as.character(unlist(strsplit(as.character(unique(data_data[, map_data$CONCU])[1]), "/"))) 
            } else {
              conc_unit_tmp <- NA
            }
          } else {
            conc_unit_tmp <- NA
          }
          conc_unit_tmp2 <- as.character(unique(data_data[,map_data[,unlist(strsplit(map_data$DOSEULIST, ";"))[1]]]))
          inputUnit12 <- c(as.character(conc_unit_tmp[1]), as.character(conc_unit_tmp[2]), conc_unit_tmp2)
  
          outputUnitLabel <- "CONCNORMOUTPUTUNIT"
          testunit <- is.element(outputUnitLabel, names(map))
          outputUnitFormat <- FALSE
          outputUnit12 <- NA
          
          if(testunit) {
              outputUnit12 <- as.character(map_data[[outputUnitLabel]])
              outputUnitFormat <- length(outputUnit12)>0 & isTRUE(!is.na(outputUnit12))
              if(outputUnitFormat) {
                  if(length(grep("/", outputUnit12)) > 0) { outputUnit12 <- unlist(strsplit(outputUnit12, "[./]", perl=TRUE)) } else { outputUnitFormat <- FALSE }
              }
          }
  
          testunit <- testunit && outputUnitFormat
          if(testunit){
              testunit <- testunit && length(outputUnit12) == 3 && length(conc_unit_tmp) == 2
          }
          
          formattedinputUnit   <- paste(c(paste(inputUnit12[1:2], collapse="/"), inputUnit12[3]), collapse="/")
          formattedoutputUnit  <- paste(c(paste(outputUnit12[1:2], collapse="/"), outputUnit12[3]), collapse="/")
          
          inputMatch12 <- numeric()
          outputMatch12 <- numeric()
          concdnUScaler <- numeric()
  
          if(testunit){
            for(i in 1:3) {
              if(outputUnitFormat){
                inputMatch12[i] <- match(inputUnit12[i], units, nomatch = missing_idx)
                outputMatch12[i] <- match(outputUnit12[i], units, nomatch = missing_idx)
  
                if(inputMatch12[i] != missing_idx && outputMatch12[i] != missing_idx) {
                  inputMScale12 <- val[inputMatch12[i]]
                  outputMScale12 <- val[outputMatch12[i]]
                  if(class[inputMatch12[i]] == c("M", "V", "M")[i] && class[outputMatch12[i]] == c("M", "V", "M")[i]) {
                    if(class[inputMatch12[i]] == "M" && class[outputMatch12[i]] == "M") {
                      concdnUScaler[i] <- ifelse(isTRUE(inputUnit12[i] == "DPM" || outputUnit12[i] == "DPM"), 1, inputMScale12/outputMScale12)
                    } else {
                      concdnUScaler[i] <- inputMScale12/outputMScale12
                    }
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
            concdnUFinalScaler <- concdnUScaler[1] / concdnUScaler[2] / concdnUScaler[3]
            concdn_col <- names(result_data)[names(result_data) %in% CONCNORMUPARAM]
            result_data[concdn_col] <- result_data[concdn_col] * concdnUFinalScaler
            result_data$CONCNORMU <- ifelse(concdnUFinalScaler == 1 & as.character(formattedinputUnit) == as.character(formattedoutputUnit), as.character(formattedinputUnit), as.character(formattedoutputUnit))
            if(verbose) { cat(function_name, ': Unit Class 12: ([Amount/Volume]/Amount) concdn_col: ', concdn_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', concdnUFinalScaler, '\n') }
          } else {
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
    RATEUPARAM <- parameters_by_class("RATEU", names(result_data))
    
    if(length(RATEUPARAM>0)) {
        if(!is.null(map_data$CONCU)){
          if(!is.null(unique(data_data[, map_data$CONCU])[1])){
            rate_unit_tmp <- as.character(unlist(strsplit(as.character(unique(data_data[, map_data$CONCU])[1]), "/"))) 
          } else {
            rate_unit_tmp <- NA
          }
        } else {
          rate_unit_tmp <- NA
        }
        inputUnit13 <- c(as.character(rate_unit_tmp[1]), as.character(unique(data_data[, map_data$TIMEU])[[1]]))

        outputUnitLabel <- "RATEOUTPUTUNIT"
        testunit <- is.element(outputUnitLabel, names(map))
        outputUnitFormat <- FALSE
        outputUnit13 <- NA
        
        if(testunit) {
            outputUnit13 <- as.character(map_data[[outputUnitLabel]])
            outputUnitFormat <- length(outputUnit13)>0 & isTRUE(!is.na(outputUnit13))
            if(outputUnitFormat) {
                if(length(grep("/", outputUnit13)) > 0) { outputUnit13 <- unlist(strsplit(outputUnit13, "[/]", perl=TRUE)) } else { outputUnitFormat <- FALSE }
            }
        }

        testunit <- testunit && outputUnitFormat
        if(testunit){
            testunit <- testunit && length(outputUnit13) == 2 && length(rate_unit_tmp) == 2
        }
        
        formattedinputUnit   <- paste(inputUnit13, collapse="/")
        formattedoutputUnit  <- paste(outputUnit13, collapse="/")
        
        inputMatch13 <- numeric()
        outputMatch13 <- numeric()
        rateUScaler <- numeric()

        if(testunit){
          for(i in 1:2) {
            if(outputUnitFormat){
              inputMatch13[i] <- match(inputUnit13[i], units, nomatch = missing_idx)
              outputMatch13[i] <- match(outputUnit13[i], units, nomatch = missing_idx)

              if(inputMatch13[i] != missing_idx && outputMatch13[i] != missing_idx) {
                inputMScale13 <- val[inputMatch13[i]]
                outputMScale13 <- val[outputMatch13[i]]
                if(class[inputMatch13[i]] == c("M", "T")[i] && class[outputMatch13[i]] == c("M", "T")[i]) {
                  if(class[inputMatch13[i]] == "M" && class[outputMatch13[i]] == "M") {
                    rateUScaler[i] <- ifelse(isTRUE(inputUnit13[i] == "DPM" || outputUnit13[i] == "DPM"), 1, inputMScale13/outputMScale13)
                  } else {
                    rateUScaler[i] <- inputMScale13/outputMScale13
                  }
                } else {
                  rateUScaler[i] <- 1
                  warning(paste0("'", map_data$TIMEU, "' and/or '", map_data$CONCU, "' and/or '", outputUnitLabel, "' value provided via 'map' is not accounted for unit conversion"))
                }
              } else {
                rateUScaler[i] <- 1
                warning(paste0("'", map_data$TIMEU, "' and/or '", map_data$CONCU, "' and/or '", outputUnitLabel, "' value provided via 'map' is not valid for unit conversion"))
              }
            }
          }
        }
        if(testunit){
          rateUFinalScaler <- rateUScaler[1] / rateUScaler[2]
          rate_col <- names(result_data)[names(result_data) %in% RATEUPARAM]
          result_data[rate_col] <- result_data[rate_col] * rateUFinalScaler
          result_data$RATEU <- ifelse(rateUFinalScaler == 1 & as.character(formattedinputUnit) == as.character(formattedoutputUnit), as.character(formattedinputUnit), as.character(formattedoutputUnit))
          if(verbose) { cat(function_name, ': Unit Class 13: (Amount/Time) rate_col: ', rate_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', rateUFinalScaler, '\n') }
        } else {
          result_data$RATEU <- formattedinputUnit
          if(length(rate_unit_tmp) != 2){
            warning("Unit data provided via the 'CONCU' value provided via 'map' is not in the proper form! Please try again using 'Amount/Volume' format!")
          }
          if(outputUnitFormat){
            warning("'", outputUnitLabel, "' is not present in the proper form! Please try again using 'Amount/Time' format!")
          }
        }
    } else {
      result_data$RATEU <- NA
    }
  }

  #----------------------------------------------------------------------------  
  #Unit Class 14: Volume/Body weight
  if(unit_class == "VOLUMEWU" || unit_class == "ALL"){
    VOLUMEWUPARAM <- parameters_by_class("VOLUMEWU", names(result_data))
    
    if(length(VOLUMEWUPARAM)>0 && parameter_required("^CONCU$", names(map_data)) && parameter_required("^NORMBSU$", names(map_data))){
      if(parameter_required(map_data$CONCU, names(data_data)) && parameter_required(map_data$NORMBSU, names(data_data))){
        if(!is.null(map_data$CONCU)){
          if(!is.null(unique(data_data[, map_data$CONCU])[1])){
            vwu_unit_tmp <- as.character(unlist(strsplit(as.character(unique(data_data[, map_data$CONCU])[1]), "/"))) 
          } else {
            vwu_unit_tmp <- NA
          }
        } else {
          vwu_unit_tmp <- NA
        }
        inputUnit14 <- c(as.character(vwu_unit_tmp[2]), as.character(unique(data_data[, map_data$NORMBSU])[1]))

        outputUnitLabel <- "VOLUMENORMOUTPUTUNIT"
        testunit <- is.element(outputUnitLabel, names(map))
        outputUnitFormat <- FALSE
        outputUnit14 <- NA
        
        if(testunit) {
          outputUnit14 <- as.character(map_data[[outputUnitLabel]])
          outputUnitFormat <- length(outputUnit14)>0 & isTRUE(!is.na(outputUnit14))
          if(outputUnitFormat) {
              if(length(grep("/", outputUnit14)) > 0) { outputUnit14 <- unlist(strsplit(outputUnit14, "[/]", perl=TRUE)) } else { outputUnitFormat <- FALSE }
          }
        }

        testunit <- testunit && outputUnitFormat
        if(testunit){
          testunit <- testunit && length(outputUnit14) == 2 && length(vwu_unit_tmp) == 2
        }
        
        formattedinputUnit   <- paste(inputUnit14, collapse="/")
        formattedoutputUnit  <- paste(outputUnit14, collapse="/")
        
        inputMatch14 <- numeric()
        outputMatch14 <- numeric()
        volumewUScaler <- numeric()

        valid_input_unit <- TRUE
        valid_output_unit <- TRUE
        if(testunit){
          for(i in 1:2) {
            if(outputUnitFormat){
              inputMatch14[i] <- match(inputUnit14[i], units, nomatch = missing_idx)
              outputMatch14[i] <- match(outputUnit14[i], units, nomatch = missing_idx)

              if(inputMatch14[i] != missing_idx && outputMatch14[i] != missing_idx) {
                inputMScale14 <- val[inputMatch14[i]]
                outputMScale14 <- val[outputMatch14[i]]
                if(class[inputMatch14[i]] == c("V", "M")[i] && class[outputMatch14[i]] == c("V", "M")[i]) {
                  if(class[inputMatch14[i]] == "M" && class[outputMatch14[i]] == "M") {
                    volumewUScaler[i] <- ifelse(isTRUE(inputUnit14[i] == "DPM" || outputUnit14[i] == "DPM"), 1, inputMScale14/outputMScale14)
                  } else {
                    volumewUScaler[i] <- inputMScale14/outputMScale14
                  }
                } else {
                  volumewUScaler[i] <- 1
                  warning(paste0("'", map_data$CONCU, "' and/or '", map_data$NORMBSU, "' and/or '", outputUnitLabel, "' value provided via 'map' is not accounted for unit conversion"))
                }
              } else {
                if(inputMatch14[i] == missing_idx){
                  valid_input_unit <- FALSE
                }
                if(outputMatch14[i] == missing_idx){
                  valid_output_unit <- FALSE
                }
                volumewUScaler[i] <- 1
                warning(paste0("'", map_data$CONCU, "' and/or '", map_data$NORMBSU, "' and/or '", outputUnitLabel, "' value provided via 'map' is not valid for unit conversion"))
              }
            } else {
              valid_output_unit <- FALSE
            }
          }
        }
        if(testunit) { testunit <- testunit && valid_input_unit && valid_output_unit }
        if(testunit){
          volumewUFinalScaler <- volumewUScaler[1] / volumewUScaler[2]
          volumew_col <- names(result_data)[names(result_data) %in% VOLUMEWUPARAM]
          result_data[volumew_col] <- result_data[volumew_col] * volumewUFinalScaler
          result_data$VOLUMEWU <- ifelse(volumewUFinalScaler == 1 & as.character(formattedinputUnit) == as.character(formattedoutputUnit), as.character(formattedinputUnit), as.character(formattedoutputUnit))
          if(verbose) { cat(function_name, ': Unit Class 14: (Volume/Body Weight) volumew_col: ', volumew_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', volumewUFinalScaler, '\n') }
        } else {
          result_data$VOLUMEWU <- formattedinputUnit
          if(!valid_input_unit || !valid_output_unit){
            if(length(VOLUMEWUPARAM) > 0){
              result_data[,names(result_data) %in% VOLUMEWUPARAM] <- NA
            }
          }
          if(length(vwu_unit_tmp) != 2){
            warning("Unit data provided via the 'CONCU' value provided via 'map' is not in the proper form! Please try again using 'Amount/Volume' format!")
          }
          if(outputUnitFormat){
            warning("'", outputUnitLabel, "' is not present in the proper form! Please try again using 'Volume/Body' format!")
          }
        }
      } else {
        result_data$VOLUMEWU <- NA
        if(length(VOLUMEWUPARAM) > 0){
          result_data[,names(result_data) %in% VOLUMEWUPARAM] <- NA
        }
        if(!(map_data$CONCU %in% names(data_data) && map_data$NORMBSU %in% names(data_data))) {
          warning(paste0("'", map_data$CONCU, "' and '", map_data$NORMBSU, "' value provided via 'map' #13# are not present in the dataset provided via 'data'"))
        } else if(!map_data$CONCU %in% names(data_data)) {
          warning(paste0("'", map_data$CONCU, "' value provided via 'map' is not present in the dataset provided via 'data'"))
        } else if(!map_data$NORMBSU %in% names(data_data)) {
          warning(paste0("'", map_data$NORMBSU, "' value provided via 'map' is not present in the dataset provided via 'data'"))
        }
      }
    } else {
      result_data$VOLUMEWU <- NA
      if(length(VOLUMEWUPARAM) > 0){
        result_data[,names(result_data) %in% VOLUMEWUPARAM] <- NA
      }
      if(!("CONCU" %in% names(map_data) && "NORMBSU" %in% names(map_data))) {
        warning(paste0("'", map_data$CONCU, "' and '", map_data$NORMBSU, "' #14# are not present in the dataset provided via 'map'"))
      } else if(!"CONCU" %in% names(map_data)) {
        warning(paste0("'", map_data$CONCU, "' value provided via 'map' #27# is not present in the dataset provided via 'map'"))
      } else if(!"NORMBSU" %in% names(map_data)) {
        warning(paste0("'", map_data$NORMBSU, "' value provided via 'map' #28# is not present in the dataset provided via 'map'"))
      }
    }
  }

  #----------------------------------------------------------------------------  
  #Unit Class 15: Volume/Time/Body weight
  if(unit_class == "CLWU" || unit_class == "ALL"){
    CLWUPARAM <- parameters_by_class("CLWU", names(result_data))
    
    if(length(CLWUPARAM)>0 && parameter_required("^CONCU$", names(map_data)) && parameter_required("^TIMEU$", names(map_data)) && parameter_required("^NORMBSU$", names(map_data))){
      if(parameter_required(map_data$CONCU, names(data_data)) && parameter_required(map_data$TIMEU, names(data_data)) && parameter_required(map_data$NORMBSU, names(data_data))){
        if(!is.null(map_data$CONCU)){
          if(!is.null(unique(data_data[, map_data$CONCU])[1])){
            clwu_unit_tmp <- as.character(unlist(strsplit(as.character(unique(data_data[, map_data$CONCU])[1]), "/"))) 
          } else {
            clwu_unit_tmp <- NA
          }
        } else {
          clwu_unit_tmp <- NA
        }
        inputUnit15 <- c(as.character(clwu_unit_tmp[2]), as.character(unique(data_data[, map_data$TIMEU])[[1]]), as.character(unique(data_data[, map_data$NORMBSU])[1]))

        outputUnitLabel <- "CLNORMOUTPUTUNIT"
        testunit <- is.element(outputUnitLabel, names(map))
        outputUnitFormat <- FALSE
        outputUnit15 <- NA
        
        if(testunit) {
            outputUnit15 <- as.character(map_data[[outputUnitLabel]])
            outputUnitFormat <- length(outputUnit15)>0 & isTRUE(!is.na(outputUnit15))
            if(outputUnitFormat) {
                if(length(grep("/", outputUnit15)) > 0) { outputUnit15 <- unlist(strsplit(outputUnit15, "[/]", perl=TRUE)) } else { outputUnitFormat <- FALSE }
            }
        }

        testunit <- testunit && outputUnitFormat
        if(testunit){
            testunit <- testunit && length(outputUnit15) == 3 && length(clwu_unit_tmp) == 2
        }
      
        formattedinputUnit   <- paste(inputUnit15, collapse="/")
        formattedoutputUnit  <- paste(outputUnit15, collapse="/")

        inputMatch15 <- numeric()
        outputMatch15 <- numeric()
        clwUScaler <- numeric()

        valid_input_unit <- TRUE
        valid_output_unit <- TRUE
        if(testunit){
          for(i in 1:3) {
            if(outputUnitFormat){
              inputMatch15[i] <- match(inputUnit15[i], units, nomatch = missing_idx)
              outputMatch15[i] <- match(outputUnit15[i], units, nomatch = missing_idx)

              if(inputMatch15[i] != missing_idx && outputMatch15[i] != missing_idx) {
                inputMScale15 <- val[inputMatch15[i]]
                outputMScale15 <- val[outputMatch15[i]]
                if(class[inputMatch15[i]] == c("V", "T", "M")[i] && class[outputMatch15[i]] == c("V", "T", "M")[i]) {
                  if(class[inputMatch15[i]] == "M" && class[outputMatch15[i]] == "M") {
                    clwUScaler[i] <- ifelse(isTRUE(inputUnit15[i] == "DPM" || outputUnit15[i] == "DPM"), 1, inputMScale15/outputMScale15)
                  } else {
                    clwUScaler[i] <- inputMScale15/outputMScale15
                  }
                } else {
                  clwUScaler[i] <- 1
                  warning(paste0("'CONCU' and/or '", map_data$TIMEU, " and/or 'NORMBSU' and/or '", outputUnitFormat, "' value provided via 'map' is not accounted for unit conversion"))
                }
              } else {
                if(inputMatch15[i] == missing_idx){
                  valid_input_unit <- FALSE
                }
                if(outputMatch15[i] == missing_idx){
                  valid_output_unit <- FALSE
                }
                clwUScaler[i] <- 1
                warning(paste0("'CONCU' and/or '", map_data$TIMEU, " and/or 'NORMBSU' and/or '", outputUnitFormat, "' value provided via 'map' is not valid for unit conversion"))
              }
            } else {
              valid_output_unit <- FALSE
            }
          }
        }
        if(testunit) { testunit <- testunit && valid_input_unit && valid_output_unit }
        if(testunit){
          clwUFinalScaler <- clwUScaler[1] / clwUScaler[2] / clwUScaler[3]
          clw_col <- names(result_data)[names(result_data) %in% CLWUPARAM]
          result_data[clw_col] <- result_data[clw_col] * clwUFinalScaler
          result_data$CLWU <- ifelse(clwUFinalScaler == 1 & as.character(formattedinputUnit) == as.character(formattedoutputUnit), as.character(formattedinputUnit), as.character(formattedoutputUnit))
          if(verbose) { cat(function_name, ': Unit Class 15: (Volume/Time/Body Weight) clw_col: ', clw_col, ' parameters are scaled from ', formattedinputUnit, ' to ', formattedoutputUnit, ' via scaling factor: ', clwUFinalScaler, '\n') }
        } else {
          result_data$CLWU <- formattedinputUnit
          if(length(VOLUMEWUPARAM) > 0){
            result_data[,names(result_data) %in% VOLUMEWUPARAM] <- NA
          }
          if(length(clwu_unit_tmp) != 2){
            warning("Unit data provided via the 'CONCU' value provided via 'map' is not in the proper form! Please try again using 'Amount/Volume' format!")
          }
          if(outputUnitFormat){
            warning("'", outputUnitLabel, "' is not present in the proper form! Please try again using 'Volume/Time/Body' format!")
          }
        }
      } else {
        result_data$CLWU <- NA
        if(!valid_input_unit || !valid_output_unit){
          if(length(CLWUPARAM) > 0){
            result_data[,names(result_data) %in% CLWUPARAM] <- NA
          }
        }
        if(!(map_data$CONCU %in% names(data_data) && map_data$TIMEU %in% names(data_data) && map_data$NORMBSU %in% names(data_data))) {
          warning(paste0("'", map_data$CONCU, "', '", map_data$TIMEU, " and '", map_data$NORMBSU, "' value provided via 'map' #15# are not present in the dataset provided via 'data'"))
        } else if(!(map_data$CONCU %in% names(data_data) && map_data$TIMEU %in% names(data_data))) {
          warning(paste0("'", map_data$CONCU, "' and '", map_data$TIMEU, "' value provided via 'map' #16#  are not present in the dataset provided via 'data'"))
        } else if(!(map_data$TIMEU %in% names(data_data) && map_data$NORMBSU %in% names(data_data))) {
          warning(paste0("'", map_data$TIMEU, " and '", map_data$NORMBSU, "' value provided via 'map' #17# are not present in the dataset provided via 'data'"))
        } else if(!(map_data$CONCU %in% names(data_data) && map_data$NORMBSU %in% names(data_data))) {
          warning(paste0("'", map_data$CONCU, "' and '", map_data$NORMBSU, "' value provided via 'map' #18# are not present in the dataset provided via 'data'"))
        } else if(!map_data$CONCU %in% names(data_data)) {
          warning(paste0("'", map_data$CONCU, "' value provided via 'map' is not present in the dataset provided via 'data'"))
        } else if(!map_data$TIMEU %in% names(data_data)) {
          warning(paste0("'", map_data$TIMEU, " value provided via 'map' is not present in the dataset provided via 'data'"))
        } else if(!map_data$NORMBSU %in% names(data_data)) {
          warning(paste0("'", map_data$NORMBSU, "' value provided via 'map' is not present in the dataset provided via 'data'"))
        }
      }
    } else {
      result_data$CLWU <- NA
      if(length(CLWUPARAM) > 0){
        result_data[,names(result_data) %in% CLWUPARAM] <- NA
      }
      if(!("CONCU" %in% names(map_data) && map_data$TIMEU %in% names(map_data) && "NORMBSU" %in% names(data_data))) {
        warning(paste0("'", map_data$CONCU, "', '", map_data$TIMEU, " and '", map_data$NORMBSU, "' #19# are not present in the dataset provided via 'map'"))
      } else if(!("CONCU" %in% names(map_data) && map_data$TIMEU %in% names(map_data))) {
        warning(paste0("'", map_data$CONCU, "' and '", map_data$TIMEU, "' #20# are not present in the dataset provided via 'map'"))
      } else if(!(map_data$TIMEU %in% names(map_data) && "NORMBSU" %in% names(map_data))) {
        warning(paste0("'", map_data$TIMEU, " and '", map_data$NORMBSU, "' #21# are not present in the dataset provided via 'map'"))
      } else if(!("CONCU" %in% names(map_data) && "NORMBSU" %in% names(map_data))) {
        warning(paste0("'", map_data$CONCU, "' and '", map_data$NORMBSU, "' #22# are not present in the dataset provided via 'map'"))
      } else if(!"CONCU" %in% names(map_data)) {
        warning(paste0("'", map_data$CONCU, "' #29# is not present in the dataset provided via 'map'"))
      } else if(!map_data$TIMEU %in% names(map_data)) {
        warning(paste0("'", map_data$TIMEU, " #30# is not present in the dataset provided via 'map'"))
      } else if(!"NORMBSU" %in% names(map_data)) {
        warning(paste0("'", map_data$NORMBSU, "' #31# is not present in the dataset provided via 'map'"))
      }
    }
  }
  return(result_data)
}
