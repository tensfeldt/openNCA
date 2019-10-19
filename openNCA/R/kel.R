#' Terminal or Elimination phase rate constant
#'
#' This function gets the terminal or elimination phase rate constant, which is described as the absolute
#' value of the slope of a least-squares linear regression during the terminal phase of the natural-logarithm
#' (ln) transformed concentration-time profile. The function also gets the the intercept on natural log of
#' plasma drug concentration axis (KELC0), the corresponding values of initial timepoint used in the calculation
#' of kel (KELTMLO), final timepoint used in the calculation of kel (KELTMHI), number of timepoints used in
#' the calculation of kel (KELNOPT), the terminal phase half-life (THALF), and the terminal phase half-life flag (THALFF).
#'
#' @aliases kelc0 keltmlo keltmhi kelnopt thalf thalff
#'
#' @details
#' \strong{First calculate slope} \cr
#' \figure{kel_eq1.png} \cr
#' \figure{kel_eq2.png} \cr
#' \figure{kel_eq3.png} \cr
#' \figure{kel_eq0.png} \cr
#' \eqn{S = Slope} \cr
#' \eqn{b = KELC0 (the y intercept)} \cr
#' \eqn{C_{i} = i^{th} concentration}{Ci = ith concentration} \cr
#' \eqn{T_{i} = i^{th} time}{Ti = ith time} \cr
#' \eqn{n = the number of data points} \cr \cr
#' \strong{Terminal log-linear phase} = The phase in the natural-log transformed concentration-time profile selected
#' by user at the time of primary parameter computaitons via interactive concentration-time plots(refer to KEL
#' tab on the front end) \cr \cr
#' Set time of last dose TDOSE to 0 and shift T accordingly in calculation of KEL. This impacts the C0 the intercept
#' used in other paramerter calculations AUCINF \cr \cr
#' \strong{Equation for KEL}\cr \eqn{KEL = S x -1} \cr \cr
#' \strong{Terminal Phase Half-Life Equation} \cr
#' \figure{thalf.png} \cr
#' \eqn{KEL = Terminal Rate Constant} \cr \cr
#' \strong{Terminal Phase Half-Life Flag (THALFF)} \cr
#' If \cr
#' \figure{thalff.png} \cr
#' Then:    Value = 0 \cr
#' Else:    Value = 1 \cr \cr
#' \eqn{KELTMHI = Final timepoint used for KEL} \cr
#' \eqn{KELTMLO = Initial timepoint used for KEL} \cr
#' \eqn{THALF = Terminal Phase Half-Life} \cr
#' \eqn{SPANRATIOCRIT = Spanratio criteria used for THALFF flag} \cr
#'
#' @section Note:
#' If concentrations are less than or equal to zero or 'NA' for a data point, that data point is not used
#' in calculations. There needs to be atleast 2 data points in the data. If KEL is less than 0 then the function
#' will return NA. \cr
#' The function will ensure that the concentration data and the time data provided are numeric vectors and
#' are also of the same length. \cr
#'
#' @param conc The concentration data (given in a vector form)
#' @param time The time data (given in a vector form)
#' @param exflag The exclude flag data (given in a numeric vector)
#' @param spanratio The spanratio criteria value used to calculate flag for THALF (numeric value)
#'
#' @section Returns:
#' \strong{Vector} \cr
#' \itemize{
#'  \item KEL: maximum observed concentration
#'  \item KELCO: the intercept on natural log of plasma drug concentration axis (y-intercept)
#'  \item KELTMLO: intial timepoint used in the calculation of KEL
#'  \item KELTMHI: final timepoint used in the calculation of KEL
#'  \item KELNOPT: number of time points used in the calculation of KEL
#'  \item THALF: terminal phase half-life
#' }
#' @section Additional Details:
#' If KEL returns NA then TMHI and TMLO both are set to NA as well. If TMLO is greater
#' than or equal to TMHI, then TMLO is set to NA. If TMHI is less than or equal to TMLO, then TMHI is set
#' to NA. If KEL returns NA then THALF is set to NA. If THALF is less than or equal to zero, then it is
#' set to NA
#'
#' @examples
#' ##########
#' ## Data ##
#' #################################
#' ##  SID  ##  TIME  ##   CONC   ##
#' #################################
#' ##   30  ##    0   ##   2.89   ##
#' ##   30  ##    1   ##   2.49   ##
#' ##   30  ##    2   ##   2.47   ##
#' #################################
#'
#' #Data mentioned will be used for the following example
#'
#' kel()
#' #Error in kel: 'conc' and 'time' vectors are NULL
#'
#' conc_vector <- c(2.89, 2.49, 2.47)
#' time_vector <- c(0, 1, 2)
#'
#' kel(conc = conc_vector, time = time_vector)
#' #        KEL      KELC0    KELTMLO    KELTMHI    KELNOPT      THALF  THALFF
#' # 0.07851918 1.03777163 0.00000000 2.00000000 3.00000000 8.82774397      NA
#'
#' ############
#' ## Data 2 ##
#' #################################
#' ##  SID  ##  TIME  ##   CONC   ##
#' #################################
#' ##   31  ##    0   ##      0   ##
#' ##   31  ##    1   ##      0   ##
#' ##   31  ##    2   ##      0   ##
#' #################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(0, 0, 0)
#' time_vector <- c(0, 1, 2)
#'
#' kel(conc = conc_vector, time = time_vector)
#' # KEL KELC0 KELTMLO KELTMHI KELNOPT   THALF  THALFF
#' #  NA    NA      NA      NA       0      NA      NA
#'
#' ############
#' ## Data 3 ##
#' #################################
#' ##  SID  ##  TIME  ##   CONC   ##
#' #################################
#' ##   32  ##    0   ##   2.19   ##
#' ##   32  ##    1   ##   2.23   ##
#' ##   32  ##    2   ##   2.04   ##
#' ##   32  ##    3   ##   NA     ##
#' #################################
#'
#' #Data mentioned will be used for the following example
#'
#' conc_vector <- c(2.19, 2.23, 2.04, NA)
#' time_vector <- c(0, 1, 2, 3)
#'
#' kel(conc = conc_vector, time = time_vector, spanratio = 2)
#' #        KEL       KELC0     KELTMLO     KELTMHI     KELNOPT       THALF  THALFF
#' # 0.03547587  0.80176018  0.00000000  2.00000000  3.00000000 19.53855451       1
#'
#' @author
#' \itemize{
#'  \item \strong{Rudraya Technical Team}
#'  \item website: \url{www.rudraya.com}
#'  \item email: \url{support@rudraya.com}
#' }
#' @export
kel <- function(conc = NULL, time = NULL, exflag = NULL, spanratio = NULL){
  kel_val <- c(NA, NA, NA, NA, 0, NA, NA)
  names(kel_val) <- c("KEL", "KELC0", "KELTMLO", "KELTMHI", "KELNOPT", "THALF", "THALFF")

  if(is.null(conc) && is.null(time)){
    stop("Error in kel: 'conc' and 'time' vectors are NULL")
  } else if(is.null(conc)) {
    stop("Error in kel: 'conc' vector is NULL")
  } else if(is.null(time)) {
    stop("Error in kel: 'time' vectors is NULL")
  } else if(all(is.na(time))) { # 2019-09-13/TGT/
      return(kel_val)
  } else if(all(is.na(conc))) { # 2019-09-13/TGT/
      return(kel_val)
  }

  if(!(is.numeric(conc) && is.vector(conc)) ){
    stop("Error in kel: 'conc' is not a numeric vector")
  }
  if(!(is.numeric(time) && is.vector(time)) ){
    stop("Error in kel: 'time' is not a numeric vector")
  }
  if(length(time) != length(conc) ){
    stop("Error in kel: length of 'time' and 'conc' vectors are not equal")
  }
  if(!is.null(exflag)){
    if(!(is.logical(exflag) || is.numeric(exflag))){
      stop("Error in kel: 'exflag' is not a logical vector")
    }
  }

  #Formatting data to remove any NA or less than 0 concentration values and corresponding time values
  if(!is.null(exflag) && (length(time) == length(exflag))) {
    exflag <- !as.logical(exflag)
    time <- time[exflag]
    conc <- conc[exflag]
  }
  time <- time[!is.na(conc)]
  conc <- conc[!is.na(conc)]
  time <- time[conc > 0]
  conc <- conc[conc > 0]
  
### 2019-09-13/TGT/ don't need the following check 

  if(length(time) < 2 || length(conc) < 2){
    kel_val <- c(NA, NA, NA, NA, 0, NA, NA)
    names(kel_val) <- c("KEL", "KELC0", "KELTMLO", "KELTMHI", "KELNOPT", "THALF", "THALFF")
  } else {
    kel_tmlo <- min(time, na.rm = TRUE)
    kel_tmhi <- max(time, na.rm = TRUE)
    kel_nopt <- length(time)
    if(kel_tmhi == kel_tmlo){
      kel_tmlo <- NA
      kel_tmhi <- NA
    }
    if(kel_tmlo > kel_tmhi){
      kel_tmlo <- NA
    }
    if(kel_tmhi < kel_tmlo){
      kel_tmhi <- NA
    }
    time_val <- (sum(time, na.rm = TRUE)/length(time))
    #print(paste0("Time VAL:", time_val))
    #print(paste0("Time:", time))
    conc_val <- (sum(sapply(conc, function(x) {
      return(log(x))
    }))/length(conc))
    #print(paste0("Concentration VAL:", conc_val))
    #print(paste0("Concentration:", conc))
    conc_ln <- sapply(conc, function(x) {
      return(log(x))
    })
    #print(paste0("ln(Concentration):", conc_ln))
    tmp_numer <- sum(sapply(time, function(x){
      index <- which(time %in% x)
      if(length(index) > 1){
        stop("Error in kel: unable to calcualte due to duplicate entires in 'time' vector")
      } else if(length(index) < 1){
        stop("Error in kel: unable to calcualte due to value not present in 'time' vector")
      } else {
        return(x*conc_ln[index])
      }
    }))
    #print(paste0("Tmp Numer:", tmp_numer))
    slope_numer <- (tmp_numer - ((length(time) * time_val * conc_val)))
    #print(paste0("Slope Numer:", slope_numer))
    tmp_denom <- sum(sapply(time, function(x){
      return(x*x)
    }))
    #print(paste0("Tmp Denom:", tmp_denom))
    slope_denom <- (tmp_denom - ((length(time))*(time_val * time_val)))
    #print(paste0("Slope Denom:", slope_denom))
    if((slope_numer == 0 && slope_denom == 0) || is.na(slope_numer) || is.na(slope_denom)){
      slope = 0
    } else {
      slope = slope_numer/slope_denom
    }
    #print(paste0("Slope:", slope))
    kel <- (slope*(-1))
    #print(paste0("Kel:", round(kel, 4)))
    if(kel == 0){
      thalf <- NA
    } else {
      thalf <- log(2)/kel
      if(thalf <= 0){
        thalf <- NA
      }
    }
    #print(paste0("Thalf:", thalf))
    if(suppressWarnings(is.na(thalf) || is.na(kel_tmlo) || is.na(kel_tmhi) || is.na(spanratio) || is.null(spanratio) || !is.numeric(spanratio))){
      thalff <- NA
    } else {
      thalff <- ifelse(((kel_tmhi - kel_tmlo)/thalf) >= spanratio, 0, 1)
    }
    kelc0 <- (((conc_val * tmp_denom) - (time_val * tmp_numer))/(slope_denom))
    kelc0 <- exp(kelc0)
    #print(paste0("KelC0:", kelc0))

    if(kel <= 0){
      kel_val <- c(NA, NA, NA, NA, 0, NA, NA)
      names(kel_val) <- c("KEL", "KELC0", "KELTMLO", "KELTMHI", "KELNOPT", "THALF", "THALFF")
    } else {
      kel_val <- c(kel, kelc0, kel_tmlo, kel_tmhi, kel_nopt, thalf, thalff)
      names(kel_val) <- c("KEL", "KELC0", "KELTMLO", "KELTMHI", "KELNOPT", "THALF", "THALFF")
    }
  }

  return(kel_val)
}
