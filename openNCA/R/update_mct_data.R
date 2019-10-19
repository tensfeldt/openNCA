#' update_mct_data updates time, alternative time and concentration data
#'
#' update_mct_data executes validate_timeconc_data and updates Model Configuration Template/MCT/map values to align with
#' selected TIME, alternative TIME, CONC and associated units.
#'
#' that map$TIME variable either points to a valid variable in data or is nominal/actual selection.
#' map$TIME is either a value of "Nominal" or "Actual" or it points directly to a column/data field in the data dataframe.
#' if map$TIME=="Nominal" then map$NOMTIME/map$NOMTIMEU will point to the column/data field in the data dataframe for TIME
#' if map$TIME=="Actual"  then map$ACTTIME/map$ACTTIMEU will point to the column/data field in the data dataframe for TIME
#' if map$TIME points directly to a column/data field in the data dataframe, then map$TIMEU is an required field in map that
#'  points to avalud column/data field in the data dataframe for the units of time.
#'
#' @param map Model Configuration Template (MCT)
#' @param data input concentration dataset
#' @param flags input flags dataset
#' @param verbose logs selected time/concentration values and units to stdout
#' 
#' @section Returns:
#' updated Model Confuguration Template/MCT/map
#'
#' @return map
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
update_mct_data <- function(map, data, flag, verbose=FALSE) {
    function_name <- as.list(sys.call())[[1]]

    ### call timeconcvalues to determine TIME/CONC values provided
    timeconcvalues <- validate_timeconc_data(map, data, verbose=verbose)
    if(verbose) { cat(function_name, ' timeconcvalues: \n'); print(timeconcvalues) }
    ### timeconcvalues should all be column names/fields in data at this point, rather than pointers to other values in map

    ### maintain original values of TIME/TIMEU  
    if(parameter_required("^TIME$", names(map)))  { map$ORGTIME  <- map$TIME  } else { map$ORGTIME  <- NA }
    ### Note if TIMEU doesn't exist in map, ORGTIMEU will not be created
    if(parameter_required("^TIMEU$", names(map))) { map$ORGTIMEU <- map$TIMEU } else { map$ORGTIMEU <- NA }
  
    ### Update TIME/TIMEU with validated times/units
    map$TIME     <- timeconcvalues$time
    map$TIMEU    <- timeconcvalues$timeu
  
    ### Create Alternative TIME values if specified
    if(parameter_required("^alttime$",  names(timeconcvalues))) { map$ALTTIME  <- timeconcvalues$alttime }
    if(parameter_required("^alttimeu$", names(timeconcvalues))) { map$ALTTIMEU <- timeconcvalues$alttimeu }

    ### maintain original values of CONC/CONCU
    if(parameter_required("^CONC$", names(map)))  { map$ORGCONC  <- map$CONC   } else { map$ORGCONC  <- NA }
    if(parameter_required("^CONCU$", names(map))) { map$ORGCONCU <- map$CONCU  } else { map$ORGCONCU <- NA }

    ### Update CONC/CONCU with validated concs/units
    map$CONC     <- timeconcvalues$conc
    map$CONCU    <- timeconcvalues$concu

    ### For Interval Data set ENDTIME values/units
    if(casefold(map$MODEL)=="m4") {
        map$ENDTIME  <- timeconcvalues$endtime
        map$ENDTIMEU <- timeconcvalues$endtimeu
    }

    ### DOSES to impute to unit dose
    if(parameter_required("imputedoses", names(timeconcvalues))) {
        map$IMPUTEDOSES <- paste(timeconcvalues$imputedoses, collapse=";") 
###        cat(function_name, ' timeconcvalues$imputedoses: ', timeconcvalues$imputedoses, ' str(timeconcvalues$imputedoses): ', str(timeconcvalues$imputedoses), "map$IMPUTEDOSES: ", map$IMPUTEDOSES, '\n')
   }

    ### DOSE information
    if(parameter_required("^DOSE(i{1}|[0-9]*?)$", names(map)))  {
        for(i in 1:length(timeconcvalues$dose)) { map[,paste0("ORGDOSE",i)]  <- timeconcvalues$dose[i] }
        map$DOSELIST <- paste(timeconcvalues$dose, collapse=";")
    }
    else { map$ORGDOSE  <- NA }

    ### DOSEU information
    if(parameter_required("^DOSEU$", names(map))) { map$ORGDOSEU <- timeconcvalues$doseu } else { map$ORGDOSEU <- NA }

    ### create FLGTIME column name in map to map into flags and data once data and flags are merged
    flgtime <- "FLGTIME"
    if(length(parameter_indices("^FLGTIME$", flag))==1) { flgtime <- flag[,parameter_indices("^FLGTIME$", flag)] }
    map$FLGTIME <- flgtime

    ### create FLGMCTUPDATE to indicate that timeconc data was already validated and MCT/MAP updated
    ### This can be tested in the Model/Method codes to prevent re-execution of this code
    map$FLGMCTUPDATE <- TRUE

    return(map)
}
