#' validate_timeconc_data validates that time, concentration, and dose data were passed via data and map/MCT and are available
#'
#' validate_timeconc_data checks that map$TIME variable either points to a valid variable in data or is nominal/actual selection.
#' map$TIME is either a value of "Nominal" or "Actual" or it points directly to a column/data field in the data dataframe.
#' if map$TIME=="Nominal" then map$NOMTIME/map$NOMTIMEU will point to the column/data field in the data dataframe for TIME
#' if map$TIME=="Actual"  then map$ACTTIME/map$ACTTIMEU will point to the column/data field in the data dataframe for TIME
#' if map$TIME points directly to a column/data field in the data dataframe, then map$TIMEU is an required field in map that
#'  points to avalud column/data field in the data dataframe for the units of time.
#'
#' @param data The dataframe that contains the raw data
#' @param map The dataframe that contains the map data
#' @param flag The dataframe that contains the flag data
#' @param verbose logs selected time/concentration values and units to stdout
#' 
#' @section Returns:
#' list with names time, timeu, conc, concu
#'
#' @examples
#'
#' @author
#' \itemize{
#'  \item \strong{Thomas Tensfeldt, Pfizer}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{thomas.g.tensfeldt@pfizer.com}
#' }
#'
#' @export
validate_timeconc_data <- function(map, data, flag, verbose=FALSE) {
    results <- list()
    time.pointer  <- NULL
    timeu.pointer <- NULL
    nomtime <- NULL
    nomtimeu <- NULL
    acttime <- NULL
    acttimeu <- NULL
    vtime <- NULL
    vtimeu <- NULL
    atime <- NULL
    atimeu <- NULL
    vconc <- NULL
    vconcu <- NULL
    vdose <- NULL
    vdoseu <- NULL
    if(casefold(map$MODEL)=="m4") { 
      nomendtime <- NULL
      nomendtimeu <- NULL
      actendtime <- NULL
      actendtimeu <- NULL
      vendtime <- NULL
      vendtimeu <- NULL
    }
    
    ### Check if TIME and CONC appear in map/mct
    if( !(parameter_required("^TIME$", names(map)) && parameter_required("^CONC$", names(map))) ) {
        stop("Dataset provided via 'map' does not contain the required columns 'TIME' and 'CONC'")
    }

    ### Check if both DOSE and DOSE1 are defined in map/mct
    if(parameter_required("^DOSE$", names(map)) && parameter_required("^DOSE1$", names(map))) {
        stop("Only define either 'DOSE' or 'DOSE1' in model configuration template, not both!")
    }
    
    time.pointer  <- map$TIME
    flgm4 <- casefold(map$MODEL)=="m4"

##### 2019-08-27/TGT/ ----------- NEED tO ADD PROCESSING OF dosing_intervals
### and associated mapping to field names for
### TAU (TAUi)
### TOLD
### DOSE
### and pass back the # of intervals, the names of TAU/TOLD/DOSE for each interval in the data
### and resolve any synonyms like TAU=TAU1 when only 1 dosing interval is present
    
    ### if map$TIME points directly to a column in the input dataset, then that column/datafield is used as the TIME basis as a default
    ### if that is the case then time units must appear in the map/MCT pointing to a column in the input dataset
    if(parameter_required(time.pointer, names(data))) {
        ### then check if TIMEU exists in map/mct
        if(parameter_required("^TIMEU$", names(map))) {
            timeu.pointer <- map$TIMEU
        }
        else {
            msg <- paste0("map/MCT (Model Configuration Template) value of TIME points to data field: ", time.pointer, " but TIMEU is missing from map/MCT")
            stop(msg)
        }
    }

    if(verbose) { cat("time.pointer: ", time.pointer, " timeu.pointer: ", timeu.pointer, "\n") }

    if(parameter_required("^NOMTIME$",  names(map))) { nomtime   <- map$NOMTIME  }
    if(parameter_required("^NOMTIMEU$", names(map))) { nomtimeu  <- map$NOMTIMEU }
    if(parameter_required("^ACTTIME$",  names(map))) { acttime   <- map$ACTTIME  }
    if(parameter_required("^ACTTIMEU$", names(map))) { acttimeu  <- map$ACTTIMEU }
    if(flgm4) {
        if(parameter_required("^NOMENDTIME$",  names(map))) { nomendtime   <- map$NOMENDTIME  }
        if(parameter_required("^NOMENDTIMEU$", names(map))) { nomendtimeu  <- map$NOMENDTIMEU }
        if(parameter_required("^ACTENDTIME$",  names(map))) { actendtime   <- map$ACTENDTIME  }
        if(parameter_required("^ACTENDTIMEU$", names(map))) { actendtimeu  <- map$ACTENDTIMEU }
    }
    if(parameter_required("^AMOUNT$",  names(map))) { results[["samplevolume"]]  <- samplevolume  <- map$AMOUNT }
    if(parameter_required("^AMOUNTU$", names(map))) { results[["samplevolumeu"]] <- samplevolumeu <- map$AMOUNTU }

    if(verbose) {
        cat('nomtime: ', nomtime, ' nomtimeu: ', nomtimeu, ' acttime: ', acttime, ' acttimeu: ', acttimeu, '\n')
        if(flgm4) { cat('nomendtime: ', nomendtime, ' nomendtimeu: ', nomendtimeu, ' actendtime: ', actendtime, ' actendtimeu: ', actendtimeu, '\n') }
    }
    
    ### 2019-09-11/TGT/ note that if map$TIME points directly to a dataset column, there is
    ### currently no option for an alternative TIME basis if FLGTIME is being used as well in the flags dataset
    if(parameter_required(time.pointer, names(data)) && parameter_required(timeu.pointer, names(data))) {
        vtime  <- map$TIME
        vtimeu <- map$TIMEU
        if(flgm4) {
            vendtime  <- map$ENDTIME
            vendtimeu <- map$ENDTIMEU
         }
    }
    else if (casefold(time.pointer)=='nominal' && !(parameter_required(nomtime, names(data)) && parameter_required(nomtimeu, names(data)))) {
      stop("Nominal Time: '", nomtime, "' and Nominal Time Unit: '", nomtimeu, "' isn't present in input dataset\n")
    }
    else if (casefold(time.pointer)=='actual' && !(parameter_required(acttime, names(data)) && parameter_required(acttimeu, names(data)))) {
      stop("Actual Time: '", acttime, "' and Actual Time Unit: '", acttimeu, "' isn't present in input dataset\n")
    }
    else if (flgm4 && casefold(time.pointer)=='nominal' && !(parameter_required(nomendtime, names(data)) && parameter_required(nomendtimeu, names(data)))) {
      stop("Nominal End Time: '", nomendtime, "' and Nominal End Time Unit: '", nomendtimeu, "' isn't present in input dataset\n")
    }
    else if (flgm4 && casefold(time.pointer)=='actual' && !(parameter_required(actendtime, names(data)) && parameter_required(actendtimeu, names(data)))) {
      stop("Actual End Time: '", actendtime, "' and Actual End Time Unit: '", actendtimeu, "' isn't present in input dataset\n")
    }
    ### else use the columns pointed to by NOMTIME/NOMTIMEU if map$TIME is "nominal"
    else if (casefold(time.pointer)=='nominal' && parameter_required(nomtime, names(data)) && parameter_required(nomtimeu, names(data))) {
        vtime  <- nomtime
        vtimeu <- nomtimeu
        # 2019-09-11/TGT/ Set "alternative" times to nominal, i.e. actual
        if (parameter_required(acttime, names(data)) && parameter_required(acttimeu, names(data))) {
            atime  <- acttime
            atimeu <- acttimeu
        }
        
        if(flgm4) {
            vendtime <- nomendtime
            vendtimeu <- nomendtimeu
        }
    }
    ### else use the columns pointed to by ACTTIME/ACTTIMEU if map$TIME is "actual"
    else if (casefold(time.pointer)=='actual' && parameter_required(acttime, names(data)) && parameter_required(acttimeu, names(data))) {
        vtime  <- acttime
        vtimeu <- acttimeu
        # 2019-09-11/TGT/ Set "alternative" times to actual, i.e. nominal
        if (parameter_required(nomtime, names(data)) && parameter_required(nomtimeu, names(data))) {
            atime  <- nomtime
            atimeu <- nomtimeu
        }
        if(flgm4) {
            vendtime <- actendtime
            vendtimeu <- actendtimeu
        }
    }

    ### Check CONC/CONCU entries are available in the MCT/map and that their encoded values appear in the dataset
    if(parameter_required("^CONC$",  names(map))) { vconc   <- map$CONC  }
    if(parameter_required("^CONCU$", names(map))) { vconcu  <- map$CONCU }

    if(!(parameter_required(vconc, names(data)) && parameter_required(vconcu, names(data)))) {
      stop("Concentration: '", vconc, "' or Concentration Unit: '", vconcu, "' aren't present in input dataset\n")
    }

    ### Check if DOSE is in map/mct
    missing_dose_names <- c()
    if(parameter_required("^DOSE(i{1}|[0-9]*?)$", names(map))) {
        vdose <- parameter_indices("^DOSE(i{1}|[0-9]*?)$", names(map))
        vdose <- names(vdose)

        ### Check that doses appear in concentration dataset
        ###  any missing dose definitions in MCP will be treated as UNIT DOSES
        k <- parameter_indices(paste0("^",map[,vdose],"$"), names(data), simplify=FALSE)
        ### vdosedata is a boolean indicating whether the values of vdose appear in the input concentration dataset or not        
        vdosedata <- is.element(map[,vdose], names(k))
        missing_dose_names <- vdose[!vdosedata]
        if(length(missing_dose_names)>0) {
            cat(missing_dose_names, " as defined in 'map', do not appear in input concentration dataset", "\n")
            cat("assuming unit dose amounts for: ", missing_dose_names, "\n")
            warning("assuming unit dose amounts for each dose")
        }
    } else {
      if(isTRUE(casefold(map$DOSINGTYPE) == "ss")){
        vdose <- "DOSE1"
        missing_dose_names <- "DOSE1"
      } else {
        vdose <- "DOSE"
        missing_dose_names <- "DOSE"
      }
      vdosedata <- FALSE
      warning("No dose information provided in 'map'. Assuming unit dose amount for a single dose.")
    }
    
    ### Assumption is that there is one source of units for dosing regardless the # of doses supporting each individual profile
    ###   i.e. there is one unit for all of the doses defined in the map
    ### missing dosing unit is considered a warning
    if(parameter_required("^DOSE(i{1}|[0-9]*?)U$", names(map))) {
        vdoseu <- parameter_indices("^DOSE(i{1}|[0-9]*?)U$", names(map))
        vdoseu <- names(vdoseu)

        ### Check that dose units appear in concentration dataset
        ###  any missing dose unit definitions in MCP will be treated as UNIT DOSES
        k <- parameter_indices(paste0("^",map[,vdoseu],"$"), names(data), simplify=FALSE)
        ### vdoseudata is a boolean indicating whether the values of vdoseu appear in the input concentration dataset or not        
        vdoseudata <- is.element(map[,vdoseu], names(k))
        if(length(vdose) > length(vdoseu)){
          for(i in (length(vdoseu)+1):length(vdose)){
            tmp_vdoseu <- paste0(vdose[i],"U")
            vdoseu <- c(vdoseu, tmp_vdoseu)
            vdoseudata <- c(vdoseudata, FALSE)
          }
        }
        missing_doseu_names <- vdoseu[!vdoseudata]
        if(length(missing_doseu_names)>0) {
            cat(missing_doseu_names, " as defined in 'map', do not appear in input concentration dataset", "\n")
###            cat("assuming unit dose amounts for: ", missing_dose_names, "\n")
            warning("Dosing unit: '", vdoseu[!vdoseudata], "' isn't present in input concentration dataset\n")
        }
    }
    else {
        vdoseu <- "DOSEU"
        vdoseudata <- FALSE
        missing_doseu_names <- "DOSEU"
        warning("No dose unit information provided in 'map'.")
    }
    
    missing_tau_names <- c()
    if(parameter_required("^TAU(i{1}|[0-9]*?)$", names(map))) {
      vtau <- parameter_indices("^TAU(i{1}|[0-9]*?)$", names(map))
      vtau <- names(vtau)

      k0 <- parameter_indices(paste0("^",map[,vtau],"$"), names(flag), simplify=FALSE)
      vtaudata <- is.element(map[,vtau], names(k0))
      missing_tau_names <- vtau[!vtaudata]
      ##if(length(missing_tau_names)>0) {
      ##  cat(missing_tau_names, " as defined in 'map', do not appear in input concentration flag dataset", "\n")
      ##}
      if(!all(as.logical(vtaudata))){
        ### Check that taus appear in concentration dataset
        ###  any missing tau definitions in MCP will be treated as NA TAUS
        k <- parameter_indices(paste0("^",map[,vtau],"$"), names(data), simplify=FALSE)
        ### vtaudata is a boolean indicating whether the values of vtau appear in the input concentration dataset or not        
        vtaudata <- is.element(map[,vtau], names(k))
        missing_tau_names <- vtau[!vtaudata]
        if(length(missing_tau_names)>0) {
          cat(missing_tau_names, " as defined in 'map', do not appear in input concentration dataset", "\n")
        }
      } else {
        
      }
    } else {
      if(isTRUE(casefold(map$DOSINGTYPE) == "ss")){
        vtau <- "TAU1"
        missing_tau_names <- "TAU1"
      } else {
        vtau <- "TAU"
        missing_tau_names <- "TAU"
      }
      vtaudata <- FALSE
      warning("No tau information provided in 'map'.")
    }
    
    missing_told_names <- c()
    if(parameter_required("^TOLD(i{1}|[0-9]*?)$", names(map))) {
      vtold <- parameter_indices("^TOLD(i{1}|[0-9]*?)$", names(map))
      vtold <- names(vtold)
      
      k0 <- parameter_indices(paste0("^",map[,vtold],"$"), names(flag), simplify=FALSE)
      vtolddata <- is.element(map[,vtold], names(k0))
      missing_told_names <- vtold[!vtolddata]
      ##if(length(missing_told_names)>0) {
      ##  cat(missing_told_names, " as defined in 'map', do not appear in input concentration flag dataset", "\n")
      ##}
      if(!all(as.logical(vtolddata))){
        ### Check that tolds appear in concentration dataset
        ###  any missing dose definitions in MCP will be treated as NA TOLDS
        k <- parameter_indices(paste0("^",map[,vtold],"$"), names(data), simplify=FALSE)
        ### vtolddata is a boolean indicating whether the values of vtold appear in the input concentration dataset or not        
        vtolddata <- is.element(map[,vtold], names(k))
        missing_told_names <- vtold[!vtolddata]
        if(length(missing_told_names)>0) {
          cat(missing_told_names, " as defined in 'map', do not appear in input concentration dataset", "\n")
        }
      }
    } else {
      if(isTRUE(casefold(map$DOSINGTYPE) == "ss")){
        vtold <- "TOLD1"
        missing_told_names <- "TOLD1"
      } else {
        vtold <- "TOLD"
        missing_told_names <- "TOLD"
      }
      vtolddata <- FALSE
      warning("No told information provided in 'map'.")
    }
        
###    if(parameter_required("^DOSEU$", names(map))) { vdoseu <- map$DOSEU }
###    else {
###        vdoseu <- ""
###        warning("No dosing unit provided in 'map'!")
###    }

###    if (!(parameter_required(vdoseu, names(data)))) {
###            warning("Dosing unit: '", vdoseu, "' isn't present in input concentration dataset\n")
###    }
    
    if(verbose) { cat('vtime: ', vtime, ' vtimeu: ', vtimeu, 'atime: ', atime, ' atimeu: ', atimeu, ' vconc: ', vconc, ' vconcu' , vconcu, '\n') }
    if(verbose && flgm4) { cat('vendtime: ', vendtime, ' vendtimeu: ', vendtimeu, '\n') }
    if(verbose) { cat('vdose: ', vdose, ' vdoseu: ', vdoseu, '\n') }

    results[["time"]]     <- vtime
    results[["timeu"]]    <- vtimeu
    results[["alttime"]]  <- atime
    results[["alttimeu"]] <- atimeu
    results[["conc"]]     <- vconc
    results[["concu"]]    <- vconcu
    if(flgm4) {
        results[["endtime"]]  <- vendtime
        results[["endtimeu"]] <- vendtimeu
    }
    results[["dose"]]        <- vdose
    results[["dosedata"]]    <- vdosedata
    results[["imputedoses"]] <- missing_dose_names
    results[["doseu"]]       <- vdoseu
    results[["doseudata"]]   <- vdoseudata
    results[["tau"]]         <- vtau
    results[["taudata"]]     <- vtaudata
    results[["imputetaus"]]  <- missing_tau_names
    results[["told"]]        <- vtold
    results[["tolddata"]]    <- vtolddata
    results[["imputetolds"]] <- missing_told_names
    
    return(results)
}
