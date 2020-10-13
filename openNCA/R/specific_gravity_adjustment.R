#' Adjust Urinary Amounts in weight units to volume units
#'
#' @details
#' If urine amounts are reported in weight units, apply specific gravity correction to convert to volume units.
#' The standard correction utilized is 1.020 g/mL = specific gravity or urine. 
#'
#' Default Human Urine Specific Gravity Value obtained from Pagana, KD. Mosbys
#' Diagnostic and Laboratory Test Reference (Eighth Edition). 2007, Page 968.
#' The range for adult is 1.005-1.030, with a median derived value of 1.020 g/mL.
#'
#' @param amt_units The units for each sample amount measurement
#' @param specific_gravity - human urine specific gravity, default to 1.020 g/mL
#' @param map Model Configuration Template (MCT/map) can include a user specified correct for specific
#'            gravity or a data column that provides that information for a specific subject/experimental unit.
#' @param verbose The value that will print out additional details (logical value)
#' 
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item sga: specific gravity adjustment multiplier
#' }
#'
#' @return ssga: specific gravity adjustment multiplier
#' 
#' @examples
#' specific_gravity_adjustment(amt_units="mL")
#' #[1] 1
#' 
#' specific_gravity_adjustment(amt_units="ml")
#' #[1] 1
#' 
#' specific_gravity_adjustment(amt_units="L")
#' #[1] 1
#' 
#' specific_gravity_adjustment(amt_units="mg")
#' #[1] 1.02
#' 
#' specific_gravity_adjustment(amt_units="GM")
#' #[1] 1.02
#' 
#' is.element("SPECIFIC_GRAVITY", names(mct))
#' #[1] FALSE
#' specific_gravity_adjustment(amt_units="GM", map=mct)
#' #[1] 1.02
#'
#' mct$SPECIFIC_GRAVITY <- 1.03
#' specific_gravity_adjustment(amt_units="mL", map=mct)
#' #[1] 1
#' specific_gravity_adjustment(amt_units="gm", map=mct)
#' #[1] 1.03
#'
#' @author
#' \itemize{
#'  \item \strong{Thomas Tensfeldt, Pfizer, Inc}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{thomas.g.tensfeldt@pfizer.com}
#' }
#' @export
specific_gravity_adjustment <- function(amt_units, specific_gravity = 1.02, map = NULL, verbose=FALSE){
  function_name <- as.list(sys.call())[[1]]

  if(is.null(amt_units)) {
      msg <- cat(function_name, ": sample or volume units are missing!")
      warning(msg)
  }
  
  if(!is.null(map)){
    if(is.data.frame(map)){
      map_data <- as.data.frame(lapply(map, as.character), stringsAsFactors = FALSE)
    } else {
      stop("Invalid data frame provided for 'map'! Please provide a valid data frame")
    }
    if(parameter_required("specific_gravity", names(map_data))) {
        specific_gravity = as.numeric(map_data$SPECIFIC_GRAVITY)
    }
  }
  
  if(verbose) { cat('specific_gravity: ', specific_gravity, ' class(specific_gravity): ', class(specific_gravity), '\n') }
  
  wt_units <- c("kg", "gm", "dg", "cg", "mg", "ug", "ng", "pg", "fg", "")
  v_units <- c("kl", "l", "dl", "cl", "ml", "ul", "nl", "pl", "fl", "")

  sga <- NA
  unit <- tolower(unique(amt_units))
  if(unit %in% v_units) {
    sga <- 1
  } else if(unit %in% wt_units) {
    sga <- specific_gravity
  } else {
    msg <- cat(function_name, ": units not recognized!")
    warning(msg)
  }

  return(sga)
}
