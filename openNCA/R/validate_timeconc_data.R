#' validate_timeconc_data validates that time, concentration, and dose data were passed via data and map/MCT and are available
#'
#' validate_timeconc_data checks that map$TIME variable either points to a valid variable in data or is nominal/actual selection.
#' map$TIME is either a value of "Nominal" or "Actual" or it points directly to a column/data field in the data dataframe.
#' if map$TIME=="Nominal" then map$NOMTIME/map$NOMTIMEU will point to the column/data field in the data dataframe for TIME
#' if map$TIME=="Actual"  then map$ACTTIME/map$ACTTIMEU will point to the column/data field in the data dataframe for TIME
#' if map$TIME points directly to a column/data field in the data dataframe, then map$TIMEU is an required field in map that
#'  points to avalud column/data field in the data dataframe for the units of time.
#'
#' @param map Model Configuration Template (MCT)
#' @param data input concentration dataset
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
validate_timeconc_data <- function(map, data, verbose=FALSE) {
    results <- list()
    time.pointer  <- NULL
    timeu.pointer <- NULL

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
    if(parameter_required("^SAMPLEVOLUME$",  names(map))) { results[["samplevolume"]]  <- samplevolume  <- map$SAMPLEVOLUME }
    if(parameter_required("^SAMPLEVOLUMEU$", names(map))) { results[["samplevolumeu"]] <- samplevolumeu <- map$SAMPLEVOLUMEU }

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
    else if (!(parameter_required(nomtime, names(data)) && parameter_required(nomtimeu, names(data)))) {
        stop("Nominal Time: '", nomtime, "' and Nominal Time Unit: '", nomtimeu, "' isn't present in input dataset\n")
    }
    else if (!(parameter_required(acttime, names(data)) && parameter_required(acttimeu, names(data)))) {
        stop("Actual Time: '", acttime, "' and Actual Time Unit: '", acttimeu, "' isn't present in input dataset\n")
    }
    else if (flgm4) {
        if (!(parameter_required(nomendtime, names(data)) && parameter_required(nomendtimeu, names(data)))) {
            stop("Nominal End Time: '", nomendtime, "' and Nominal End Time Unit: '", nomendtimeu, "' isn't present in input dataset\n")
        }
        else if (!(parameter_required(actendtime, names(data)) && parameter_required(actendtimeu, names(data)))) {
            stop("Actual End Time: '", actendtime, "' and Actual End Time Unit: '", actendtimeu, "' isn't present in input dataset\n")
        }
    }

    ### Check CONC/CONCU entries are available in the MCT/map and that their encoded values appear in the dataset
    if(parameter_required("^CONC$",  names(map))) { vconc   <- map$CONC  }
    if(parameter_required("^CONCU$", names(map))) { vconcu  <- map$CONCU }

    if (!(parameter_required(vconc, names(data)) && parameter_required(vconcu, names(data)))) {
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
    }
    else {
        vdose <- "DOSE"
        vdosedata <- FALSE
        missing_dose_names <- "DOSE"
        warning("No dose information provided in 'map'. Assuming unit dose amount for a single dose.")
    }
    
    ### Assumption is that there is one source of units for dosing regardless the # of doses supporting each individual profile
    ###   i.e. there is one unit for all of the doses defined in the map
    ### missing dosing unit is considered a warning
    if(parameter_required("^DOSEU$", names(map))) { vdoseu <- map$DOSEU }
    else {
        vdoseu <- ""
        warning("No dosing unit provided in 'map'!")
    }

    if (!(parameter_required(vdoseu, names(data)))) {
            warning("Dosing unit: '", vdoseu, "' isn't present in input concentration dataset\n")
    }
    
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
    results[["dose"]]     <- vdose
    results[["dosedata"]] <- vdosedata
    results[["doseu"]]    <- vdoseu
    results[["imputedoses"]] <- missing_dose_names
    
    return(results)
}
