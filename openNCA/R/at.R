#' Amount of Drug in the body at time T, where T can have a numerical value.
#'
#' @details
#' \strong{Model M4} \cr
#' Use the appropriate equation to calculate the amount of drug recovered unchanged in the urine at each time post-dose \cr \cr
#' \strong{Equation} \cr
#' If urine is reported as volume then the following equation should be used: \cr
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{at_1.png} \cr
#'  }
#'  \eqn{V = urine volume at each time post-dose of time interval} \cr
#'  \eqn{C = urince concentration at each time post-dose of time interval} \cr
#' }
#' If urine is reported as weight, then system automatically will apply specific gravity corrections where 1.020a g/mL is the approximate specific gravity of urine. \cr
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{at_2.png} \cr
#'  }
#'  \eqn{W = urine weight at each time post-dose of time interval} \cr
#'  \eqn{C = urince concentration at each time post-dose of time interval} \cr
#'  \eqn{1.020 g/ml = specify gravity of urine} \cr
#' }
#'
#' @param conc The concentration data (given in a vector form)
#' @param amt The amount data (given in a vector form) either represents the volume or weight
#' @param time The time data (given in a vector form)
#' @param amt_units The units data for the amount (given in a vector form)
#' @param map The dataframe that contains the map data
#'  
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item AT: Amount of Drug in the body
#' }
#'
#' @examples
#' ##########
#' ## Data ##
#' ################################################################
#' ##  SID  ##  TIME  ##   CONC   ##   AMOUNT  ##  AMOUNT UNITS  ##
#' ################################################################
#' ##   30  ##    0   ##   2.89   ##    210    ##       GM       ##
#' ##   30  ##    1   ##   2.49   ##    198    ##       GM       ##
#' ##   30  ##    2   ##   2.47   ##    190    ##       GM       ##
#' ################################################################
#'
#' #Data mentioned will be used for the following example
#'
#' #at()
#' #Error in at: 'time', 'conc', 'amt', 'amt_units' vectors are NULL
#'
#' conc_vector <- c(2.89, 2.49, 2.47)
#' time_vector <- c(0, 1, 2)
#' amt_vector <- c(210, 198, 190)
#' amt_units_vector <- c("GM", "GM", "GM")
#'
#' at(conc = conc_vector, time = time_vector, amt = amt_vector, amt_units = amt_units_vector)
#' #595.0000 483.35219 460.0980
#'
#' ############
#' ## Data 2 ##
#' ################################################################
#' ##  SID  ##  TIME  ##   CONC   ##   AMOUNT  ##  AMOUNT UNITS  ##
#' ################################################################
#' ##   31  ##    0   ##      0   ##    210    ##       GM       ##
#' ##   31  ##    1   ##      0   ##    198    ##       GM       ##
#' ##   31  ##    2   ##      0   ##    190    ##       GM       ##
#' ################################################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(0, 0, 0)
#' time_vector <- c(0, 1, 2)
#' amt_vector <- c(210, 198, 190)
#' amt_units_vector <- c("GM", "GM", "GM")
#'
#' at(conc = conc_vector, time = time_vector, amt = amt_vector, amt_units = amt_units_vector)
#' #0 0 0
#'
#' ############
#' ## Data 3 ##
#' ################################################################
#' ##  SID  ##  TIME  ##   CONC   ##   AMOUNT  ##  AMOUNT UNITS  ##
#' ################################################################
#' ##   32  ##    0   ##   1.19   ##    120    ##       GM       ##
#' ##   32  ##    1   ##   1.23   ##    145    ##       GM       ##
#' ##   32  ##    2   ##   1.34   ##    160    ##       GM       ##
#' ##   32  ## "None" ##   1.32   ##    NA     ##       GM       ##
#' ################################################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(1.19, 1.23, 1.34, 1.32)
#' time_vector <- c(0, 1, 2, "None")
#' amt_vector <- c(120, 145, 160, NA)
#' amt_units_vector <- c("GM", "GM", "GM", "GM")
#'
#' at(conc = conc_vector, time = time_vector, amt = amt_vector, amt_units = amt_units_vector)
#' #NA
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
at <- function(conc = NULL, amt = NULL, time = NULL, amt_units = NULL, map = NULL){
  function_name <- as.list(sys.call())[[1]]

  if(is.null(time) || is.null(conc) || is.null(amt) || is.null(amt_units)){
    count <- 0
    err <- ""
    if(is.null(time)){
      err <- paste0(err, "'time'")
      count <- count + 1
    }
    if(is.null(conc)){
      err <- paste0(err, ifelse(is.null(time), ", ", ""), "'conc'")
      count <- count + 1
    }
    if(is.null(amt)){
      err <- paste0(err, ifelse((is.null(time) || is.null(conc)), ", ", ""), "'amt'")
      count <- count + 1
    }
    if(is.null(amt_units)){
      err <- paste0(err, ifelse((is.null(time) || is.null(conc) || is.null(amt)), ", ", ""), "'amt_units'")
      count <- count + 1
    }
    if(count > 1){
      stop(paste0("Error in at: ", err, " vectors are NULL"))
    } else {
      stop(paste0("Error in at: ", err, " vector is NULL"))
    }
  } else if(all(is.na(time))) { # 2019-11-24/RD/
    return(rep(NA, length(conc[!is.na(time)])))
  } else if(all(is.na(conc))) { # 2019-11-24/RD/
    return(rep(NA, length(conc[!is.na(time)])))
  } else if(all(is.na(amt))) { # 2019-11-24/RD/
    return(rep(NA, length(conc[!is.na(time)])))
  }

  if(any(is.na(amt)) || any(is.na(conc))) {
    a_t <- rep(NA, length(conc[!is.na(time)]))
  } else {
### 2019-10-03/TGT/ remove units processing and replace with specific_gravity_adjustment() call      
###    wt_units <- c("kg", "gm", "dg", "cg", "mg", "ug", "ng", "pg", "fg", "")
###    v_units <- c("kl", "l", "dl", "cl", "ml", "ul", "nl", "pl", "fl", "")
###
###    unit <- tolower(unique(amt_units))
###    if(unit %in% v_units) {
###      a_t <- conc[!is.na(time)] * amt[!is.na(time)]
###    } else if(unit %in% wt_units) {
###      a_t <- conc[!is.na(time)] * (amt[!is.na(time)]/1.020)
      a_t <- conc[!is.na(time)] * (amt[!is.na(time)]/specific_gravity_adjustment(amt_units = amt_units, map=map))
###    } else {
###      a_t <- NA
###    }
  }

  return(a_t)
}
