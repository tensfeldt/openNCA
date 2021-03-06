
#' Cumulative Sum of Urine Volumes
#'
#' @details
#' \strong{Model M4} \cr
#' Computed the cumulative sum of volume amounts for interval data such as that arising from urine samples. 
#' \strong{Equation} \cr
#'
#' #' If urine is reported as weight, then system automatically will apply specific gravity corrections where 1.020a g/mL is the approximate specific gravity of urine. \cr
#' \enumerate{
#'  \tabular{rl}{
#'   \tab \figure{at_2.png} \cr
#'  }
#'  \eqn{W = urine weight at each time post-dose of time interval} \cr
#'  \eqn{C = urine concentration at each time post-dose of time interval} \cr
#'  \eqn{1.020 g/ml = specify gravity of urine} \cr
#' }
#'
#' Default Human Urine Specific Gravity Value obtained from Pagana, KD. Mosbys
#' Diagnostic and Laboratory Test Reference (Eighth Edition). 2007, Page 968.
#' The range for adult is 1.005-1.030, with a median derived value of 1.020 g/mL.
#'
#' @param vol The volume data with one value/sample collection interval (given in a vector form) 
#' @param volu The units for each volume measurement
#' @param na.rm Pass nm.rm to sum function, true by default
#' @param specific_gravity - human urine specific gravity, default to 1.020 g/mL
#' 
#' @section Returns:
#' \strong{Value} \cr
#' \itemize{
#'  \item VOLSUM: Cumulative Volume from all sample intervals in the profile
#' }
#'
#' @examples
#' ##########
#' ## Data ##
#' ################################################################
#' ##  SID  ##  START TIME  ##   CONC   ##   AMOUNT  ##  AMOUNT UNITS  ##
#' ################################################################
#' ##   30  ##    0         
#' ##   30  ##    1         
#' ##   30  ##    2         
#' ################################################################
#'
#' ######################################################################
#' # SID # START TIME # END TIME # MIDPT # SAMPLEVOLUME # SAMPLEVOLUMEU #
#' # 30  #      0     #    0     #   0   #       0      #       ML      #
#' # 30  #      0     #    4     #   2   #     200      #       ML      #
#' # 30  #      4     #    8     #   6   #     350      #       ML      #
#' # 30  #      8     #   12     #  10   #     300      #       ML      #
#' # 30  #     12     #   16     #  14   #     275      #       ML      #
#' # 30  #     16     #   20     #  18   #     250      #       ML      #
#' # 30  #     20     #   24     #  22   #     200      #       ML      #
#' ######################################################################
#' 
#' #Data above will be used for the following example
#'
#' #vol_sum()
#' #Error in at: 'vol', 'volu' vectors are NULL
#'
#' vol_vector  <- c(   0,  200,  350,  300,  275,  250,  200)
#' volu_vector <- c("ML", "ML", "ML", "ML", "ML", "ML", "ML")
#'
#' vol_sum(vol = vol_vector, volu = volu_vector)
#' #1575
#'
#' @author
#' \itemize{
#'  \item \strong{Thomas Tensfeldt, Pfizer, Inc}
#'  \item website: \url{www.pfizer.com}
#'  \item email: \url{thomas.g.tensfeldt@pfizer.com}
#' }
#' @export
vol_sum <- function(vol = NULL, volu = NULL, na.rm=TRUE, specific_gravity=1.020){
  function_name <- as.list(sys.call())[[1]]

  if(all(is.na(vol))) {
    volsum_result <- NA
  } else {
    wt_units <- c("kg", "gm", "dg", "cg", "mg", "ug", "ng", "pg", "fg", "")
    v_units <- c("kl", "l", "dl", "cl", "ml", "ul", "nl", "pl", "fl", "")

    unit <- tolower(unique(volu))
    if(unit %in% v_units) {
      volsum_result <- sum(vol, na.rm=na.rm)
    } else if(unit %in% wt_units) {
      volsum_result <- sum(vol, na.rm=na.rm)/specific_gravity
    } else {
      volsum_result <- NA
    }
  }
### 2019-08-30/TGT/ need to return unit as well via a list but this will require
### changes in the calling routine to accommodate a new parameter
  return(volsum_result)
}
