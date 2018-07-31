#' Run M3 SS Computation 
#'
#' This function will compute all the relevant parameters for a M3 model Stedy State (SS).\cr
#' 
#' @details
#' \strong{Methods:} You can use the following methods to calculate AUC: \cr
#' \enumerate{
#'  \item \strong{1: Linear-Log Trapazoidal Rule}(default method): The linear method is used up to Tmax (the 
#'  first occurance of Cmax) and the log trapezoidal method is used for the remainder of the profile. If
#'  Ci or Ci+1 is 0 then the linear trapezoidal rule is used.
#'  \item \strong{2: Linear Trapazoidal Rule}: The linear method is used for the entire profile.
#'  \item \strong{3: Log Trapazoidal Rule}: The log trapezoidal method is used for the entire profile. If
#'  Ci or Ci+1 is 0 then the linear trapezoidal rule is used.
#'  \item \strong{4: Linear Up - Log Down Trapazoidal Rule}: Linear trapezoidal while the concentrations
#'  are increasing and log trapezoidal while the concentration are decreasing, the assessment is made on
#'  a step basis for each portion of the profile i.e. t1 to t2. If Ci or Ci+1 is 0 then the linear 
#'  trapezoidal rule is used.
#' }
#' You can specify the options to subset the list of parameters that are returned: \cr
#' \strong{Return List options} \cr  
#' \enumerate{
#'  \item \strong{cmax}: Refer to \code{\link{cmax}} for more details
#'  \item \strong{cmin}: Refer to \code{\link{cmin}} for more details
#'  \item \strong{clast}: Refer to \code{\link{clast}} for more details
#'  \item \strong{cmax_c}: Refer to \code{\link{cmaxc}} for more details
#'  \item \strong{cmax_dn}: Refer to \code{\link{cmax_dn}} for more details
#'  \item \strong{tmax}: Refer to \code{\link{tmax}} for more details
#'  \item \strong{tmin}: Refer to \code{\link{tmin}} for more details
#'  \item \strong{tlast}: Refer to \code{\link{tlast}} for more details
#'  \item \strong{tlag}: Refer to \code{\link{tlag}} for more details
#'  \item \strong{kel}: Refer to \code{\link{kel}} for more details
#'  \item \strong{kelr}: Refer to \code{\link{kel_r}} for more details
#'  \item \strong{lasttime}: Refer to \code{\link{lasttime}} for more details
#'  \item \strong{auc_dn}: Refer to \code{\link{auc_dn}} for more details
#'  \item \strong{auc_all}: Refer to \code{\link{auc_all}} for more details
#'  \item \strong{auc_last}: Refer to \code{\link{auc_last}} for more details
#'  \item \strong{auc_last_c}: Refer to \code{\link{auc_lastc}} for more details
#'  \item \strong{auc_last_dn}: Refer to \code{\link{auc_dn}} for more details
#'  \item \strong{aumc_last}: Refer to \code{\link{aumc_last}} for more details
#'  \item \strong{auc_t1_t2}: Refer to \code{\link{auc_t1_t2}} for more details
#'  \item \strong{auc_inf_o}: Refer to \code{\link{auc_inf_o}} for more details
#'  \item \strong{auc_inf_o_c}: Refer to \code{\link{auc_inf_oc}} for more details
#'  \item \strong{auc_inf_p}: Refer to \code{\link{auc_inf_p}} for more details
#'  \item \strong{auc_inf_p_c}: Refer to \code{\link{auc_inf_pc}} for more details
#'  \item \strong{auc_inf_o_dn}: Refer to \code{\link{auc_dn}} for more details
#'  \item \strong{auc_inf_p_dn}: Refer to \code{\link{auc_dn}} for more details
#'  \item \strong{aumc_inf_o}: Refer to \code{\link{aumc_inf_o}} for more details
#'  \item \strong{aumc_inf_p}: Refer to \code{\link{aumc_inf_p}} for more details
#'  \item \strong{mrt_last}: Refer to \code{\link{mrt_last}} for more details
#'  \item \strong{mrto}: Refer to \code{\link{mrt_evif_o}} for more details
#'  \item \strong{mrtp}: Refer to \code{\link{mrt_evif_p}} for more details
#'  \item \strong{auc_xpct_o}: Refer to \code{\link{auc_XpctO}} for more details
#'  \item \strong{auc_xpct_p}: Refer to \code{\link{auc_XpctP}} for more details
#'  \item \strong{aumc_xpct_o}: Refer to \code{\link{aumc_XpctO}} for more details
#'  \item \strong{aumc_xpct_p}: Refer to \code{\link{aumc_XpctP}} for more details
#'  \item \strong{clo}: Refer to \code{\link{clo}} for more details
#'  \item \strong{clfo}: Refer to \code{\link{clfo}} for more details
#'  \item \strong{clfow}: Refer to \code{\link{clfow}} for more details
#'  \item \strong{clfp}: Refer to \code{\link{clfp}} for more details
#'  \item \strong{clfpw}: Refer to \code{\link{clfpw}} for more details
#'  \item \strong{vzfo}: Refer to \code{\link{vzfo}} for more details
#'  \item \strong{vzfow}: Refer to \code{\link{vzfow}} for more details
#'  \item \strong{vzfp}: Refer to \code{\link{vzfp}} for more details
#'  \item \strong{vzfpw}: Refer to \code{\link{vzfpw}} for more details
#' }
#' 
#' @section Note:
#' By default all the return list options are selected and calculated
#' 
#' @param data The dataframe that contians the raw data
#' @param map The dataframe that contians the map data 
#' @param method The AUC method to use 
#' @param model This is the model type
#' @param parameter This is either single dose (SD) or steady state (SS)
#' @param return The list of parameters to return (by defualt it is empty, which means it will retunr all parameters)
#' 
#' @section Returns:
#' \strong{Dataset} \cr 
#' 
#' @examples 
#' ##########
#' ## Data ##
#' ########################################
#' ##  SID  ##  TIME  ##  RESULT  ## ... ##
#' ########################################
#' ##   30  ##    0   ##   2.89   ##     ##
#' ##   30  ##    1   ##   2.49   ##     ##
#' ##   30  ##    2   ##   2.47   ##     ##
#' ##   31  ##    0   ##      0   ##     ##
#' ##   31  ##    1   ##   1.00   ##     ##
#' ##   31  ##    2   ##      0   ##     ##
#' ##   32  ##    0   ##   1.19   ##     ##
#' ##   32  ##    1   ##   1.23   ##     ##
#' ##   32  ##    2   ##   1.34   ##     ##
#' ##   32  ##    4   ##   1.32   ##     ##
#' ########################################
#' 
#' data <- data.frame(
#'     SID = ...,
#'     TIME = ...,
#'     RESULT = ...
#' )
#' #Same data as above, just represented as a dataframe
#' 
#' #########
#' ## Map ## 
#' ##################################################
#' ##  PARAMETER  ##  MODEL  ##  DOSINGTYPE ## ... ##
#' ##################################################
#' ##    VALUE    ##    M1   ##     SD      ##     ##
#' ##################################################
#' 
#' map <- data.frame(...)
#' #Same map as above, just represented as a dataframe
#' 
#' run_computation(data = data, map = map)
#' #Generates the M1 SD computation results
#' 
#' run_computation(data = data, map = map, flag = flag)
#' #Generates the M1 SD computation results
#' 
#' @author
#' \itemize{
#'  \item Kevin McConnell
#' }
#' @export
run_M3_SS_computation <- function(data = NULL, map = NULL, method = 1, model = "M3", parameter = "SS", return = list()){
  if(is.null(data)){
    stop("Please provide a valid path for the 'data' parameter")
  } else {
    if(is.data.frame(data)){
      data_data <- data
    } else {
      stop("Invalid data frame provided for 'data'! Please provide a valid data frame")
    }
  }
  if(is.null(map)){
    stop("Please provide a valid path for the 'map' parameter")
  } else {
    if(is.data.frame(map)){
      map_data <- as.data.frame(lapply(map, as.character), stringsAsFactors = FALSE)
    } else {
      stop("Invalid data frame provided for 'map'! Please provide a valid data frame")
    }
  }
  if(!("SDEID" %in% names(map_data) && "NOMTIME" %in% names(map_data) && "CONC" %in% names(map_data))){
    stop("Dataset provided via 'map' does not contain the required columns")
  }
  if(!(map_data$SDEID %in% names(data_data) && map_data$NOMTIME %in% names(data_data) && map_data$CONC %in% names(data_data))){
    stop("Values provided via 'map' are not present in the dataset provided via 'data'")
  }
  
  computation_list <- list("cmax", "cmin", "clast", "cmax_c", "cmax_dn", "tmax", "tmin", "tlast", "tlag", "kel", 
                           "kelr", "lasttime", "auc_all", "auc_dn", "auc_last", "auc_last_c", "auc_last_dn", "aumc_last", 
                           "auc_t1_t2", "auc_inf_o", "auc_inf_o_c", "auc_inf_p", "auc_inf_p_c", "auc_inf_o_dn", 
                           "auc_inf_p_dn", "aumc_inf_p", "aumc_inf_p", "mrt_last", "mrto", "mrtp", "auc_xpct_o", 
                           "auc_xcpt_p", "aumc_xpct_o", "aumc_xpct_p", "clo", "clfo", "clfow", "clfp", "clfpw", 
                           "vzfo", "vzfow", "vzfp", "vzfpw")
  
  ss_dose <- c("DI1F", "DI2F", "DI3F", "DI4F", "DI5F")

  if(tolower(return) == 'all' ||  (typeof(return) == 'list' && length(return) == 0)){
    if(any(ss_dose %in% names(data_data))){
      di_col <- sum(ss_dose %in% names(data_data))
    } else {
      stop("Unable to find dosing interval for Stedy State data! Please provide a valid 'data' parameter")
    }
    
    auc_col <- length(unique(data_data[,map_data$NOMTIME]))-1
    col <- 20 + 2*auc_col + 23*di_col + 1 
    computation_df <- data.frame(matrix(ncol = col, nrow = 0)) 
    
    names(computation_df) <- c("SDEID", rep(paste0("CMAX",1:di_col)), rep(paste0("CMIN",1:di_col)), rep(paste0("CLAST",1:di_col)),
                               rep(paste0("CMAXDN",1:di_col)), rep(paste0("TMAX",1:di_col)), rep(paste0("TMIN",1:di_col)), rep(paste0("TLAST",1:di_col)), 
                               "KEL", "KELTMLO", "KELTHMI", "KELNOPT", "KELR", "KELRSQ", "KELRSQA", "THALF", "LASTTIME", rep(paste0("DI",1:di_col)), rep(paste0("TAU",1:di_col)),
                               rep(paste0("TOLD",1:di_col)), rep(paste0("DOSE",1:di_col)), "AUCALL", rep(paste0("AUCDN",1:di_col)), rep(paste0("AUCLAST",1:di_col)), rep(paste0("AUCLASTDN",1:di_col)), 
                               rep(paste0("AUC",1:auc_col)), rep(paste0("AUCINT",1:auc_col)), "AUCINFO", "AUCINFP", "AUCINFOC", "AUCINFPC", rep(paste0("AUCINFODN",1:di_col)), 
                               rep(paste0("AUCINFPDN",1:di_col)), rep(paste0("AUCTAU",1:di_col)), rep(paste0("AUCTAUDN",1:di_col)), "MRTO", "MRTP", "AUCXPCTO", 
                               "AUCXPCTP", rep(paste0("VZFP",1:di_col)), rep(paste0("CAV",1:di_col)), rep(paste0("PTF",1:di_col)), rep(paste0("PTR",1:di_col)),
                               rep(paste0("VZO",1:di_col)), rep(paste0("VZP",1:di_col)))
    
    for(i in 1:length(unique(data_data[,map_data$SDEID]))){
      c_max <- list()
      c_min <- list()
      c_last <- list()
      cmaxdn <- list()
      t_max <- list()
      t_min <- list()
      t_last <- list()
      aucinfo_dn <- list()
      aucinfp_dn <- list()
      aucdn <- list()
      auclast <- list()
      auclastdn <- list()
      tau <- list()
      told <- list()
      vzf_p <- list() 
      auctau <- list()
      auctaudn <- list()
      ca_v <- list()
      di <- list()
      dose <- list()
      pt_f <- list()
      pt_r <- list()
      vz_o <- list()
      vz_p <- list()
      
      tmp_df <- data_data[data_data[,map_data$SDEID] == unique(data_data[,map_data$SDEID])[i],]
    
      aucall <- auc_all(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME], method = method)
      aucinf_o <- auc_inf_o(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME], method = method)
      aucinf_p <- auc_inf_p(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME], method = method)
      c_0 <- c0(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME])
      kel_v <- kel(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME])
      kelr_v <- kel_r(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME])
      last_time <- lasttime(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME])
      
      for(d in 1:di_col){
        tmp_di_df <- tmp_df[tmp_df[c(paste0("DI", d, "F"))] == 1,]
        tmp_dose <- tmp_di_df[, as.character(map_data[c(paste0("DOSE",d))])][1]

        c_max[[d]] <- cmax(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$NOMTIME])
        c_min[[d]] <- cmin(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$NOMTIME])
        c_last[[d]] <- clast(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$NOMTIME])
        cmaxdn[[d]] <- cmax_dn(cmax = c_max[[d]], dose = tmp_dose)
        t_max[[d]] <- tmax(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$NOMTIME])
        t_min[[d]] <- tmin(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$NOMTIME])
        t_last[[d]] <- tlast(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$NOMTIME])
        #c_max_c <- cmaxc(kel = kel_v[["KEL"]], cmax = c_max, c0 = c_0, tmax = t_max)
        aucinfo_dn[[d]] <- auc_dn(auc = aucinf_o, dose = tmp_dose)
        aucinfp_dn[[d]] <- auc_dn(auc = aucinf_p, dose = tmp_dose)
        auctau[[d]] <- auc_all(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$NOMTIME], method = method)
        auctaudn[[d]] <- auc_dn(auc = auctau[[d]], dose = tmp_dose)
        aucdn[[d]] <- auc_dn(auc = aucall, dose = tmp_dose)
        auclast[[d]] <- auc_last(conc = tmp_di_df[,map_data$CONC], time = tmp_di_df[,map_data$NOMTIME], method = method)
        auclastdn[[d]] <- auc_dn(auc = auclast[[d]], dose = tmp_dose)
        tau[[d]] <- tmp_di_df[, as.character(map_data[c(paste0("TAU",d))])][1]
        told[[d]] <- tmp_di_df[, as.character(map_data[c(paste0("TOLD",d))])][1]
        dose[[d]] <- tmp_dose
        vzf_p[[d]] <- vzfp(kel = kel_v[["KEL"]], aucinfp = aucinf_p, dose = tmp_dose)
        ca_v[[d]] <- cav(auctau = auctau[[d]], tau = tau[[d]])
        pt_f[[d]] <- ptf(cmax = c_max[[d]], cmin = c_min[[d]], cav = ca_v[[d]])
        pt_r[[d]] <- ptr(cmax = c_max[[d]], cmin = c_min[[d]])
        vz_o[[d]] <- vzo(kel = kel_v[["KEL"]], aucinfo = aucinf_o, dose = tmp_dose)
        vz_p[[d]] <- vzp(kel = kel_v[["KEL"]], aucinfp = aucinf_p, dose = tmp_dose)
        
        if(is.na(tmp_di_df[,map_data$NOMTIME][1])){
          di[[d]] <- NA
        } else {
          di[[d]] <- paste0(tmp_di_df[,map_data$NOMTIME][1], "-", tmp_di_df[,map_data$NOMTIME][nrow(tmp_di_df)])
        }
      }
      #c_max_c <- cmaxc(kel = kel_v[["KEL"]], cmax = c_max, c0 = c_0, tmax = t_max)
      #auclast_c <- auc_lastc(kel = kel_v[["KEL"]], auclast = auclast, c0 = c_0, tlast = t_last)
      
      auct <- NULL
      auc_int <- NULL
      for(t in 2:length(unique(data_data[,map_data$NOMTIME]))){
        time <- sort(unique(data_data[,map_data$NOMTIME]))
        if(time[t] %in% sort(tmp_df[,map_data$NOMTIME])) {
          tmp <- auc_t1_t2(conc = tmp_df[,map_data$CONC], time = na.omit(tmp_df[,map_data$NOMTIME]), t1 = time[1], t2 = time[t], method = method)
          tmp_int <- paste0(time[1], "_", time[t])
        } else {
          tmp <- NA
          tmp_int <- paste0(time[1], "_", time[t])
        }
        
        if(is.null(auc_t)){
          auct <- tmp 
          auc_int <- tmp_int
        } else {
          auct <- c(auct, tmp)
          auc_int <- c(auc_int, tmp_int)
        }
      }
      if(length(auct) < auc_col) {
        auct <- c(auct, rep(NA, (auc_col - length(auct))))
      }
      if(length(auc_int) < auc_col) {
        auc_int <- c(auc_int, rep(NA, (auc_col - length(auc_int))))
      }
      
      aucinf_oc <- auc_inf_oc(kel = kel_v[["KEL"]], aucinfo = aucinf_o, c0 = c_0)
      aucinf_pc <- auc_inf_pc(kel = kel_v[["KEL"]], aucinfp = aucinf_p, c0 = c_0)
      mrto <- mrt_ivif_o(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME], method = method, model = model, parameter = parameter)
      mrtp <- mrt_ivif_p(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME], method = method, model = model, parameter = parameter)
      aucxpcto <- auc_XpctO(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME], method = method)
      aucxpctp <- auc_XpctP(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME], method = method)
      
      computation_df[i,] <- c(unique(data_data[,map_data$SDEID])[i], unlist(c_max), unlist(c_min), unlist(c_last),
                              unlist(cmaxdn), unlist(t_max), unlist(t_min), unlist(t_last), kel_v[["KEL"]], kel_v[["KELTMLO"]], 
                              kel_v[["KELTMHI"]], kel_v[["KELNOPT"]], kelr_v[["KELR"]], kelr_v[["KELRSQ"]], kelr_v[["KELRSQA"]], 
                              kel_v[["THALF"]], last_time, unlist(di), unlist(tau), unlist(told), unlist(dose), aucall, unlist(aucdn), unlist(auclast), 
                              unlist(auclastdn), auct, auc_int, aucinf_o, aucinf_p, aucinf_oc, aucinf_pc, unlist(aucinfo_dn), 
                              unlist(aucinfp_dn), unlist(auctau), unlist(auctaudn), mrto, mrtp, aucxpcto, aucxpctp, unlist(vzf_p),
                              unlist(ca_v), unlist(pt_f), unlist(pt_r), unlist(vz_o), unlist(vz_p))
    }
  } else {
    if(typeof(return) == "list" && !any(!return %in% computation_list)) {
      count <- 1
      col_names <- c("SDEID")
      count <- count + 1
      
      if("cmax" %in% return){
        col_names[count] <- "CMAX"
        count <- count + 1
      }
      if("cmin" %in% return){
        col_names[count] <- "CMIN"
        count <- count + 1
      }
      if("clast" %in% return){
        col_names[count] <- "CLAST"
        count <- count + 1
      }
      if("cmax_c" %in% return){
        if("cmax" %in% return){
          col_names[count] <- "CMAXC"
          count <- count + 1
        }
      }
      if("cmax_dn" %in% return){
        if("cmax" %in% return){
          col_names[count] <- "CMAXDN"
          count <- count + 1
        }
      }
      if("tmax" %in% return){
        col_names[count] <- "TMAX"
        count <- count + 1
      }
      if("tmin" %in% return){
        col_names[count] <- "TMIN"
        count <- count + 1
      }
      if("tlast" %in% return){
        col_names[count] <- "TLAST"
        count <- count + 1
      }
      if("tlag" %in% return){
        col_names[count] <- "TLAG"
        count <- count + 1
      }
      if("kel" %in% return){
        col_names[count] <- "KEL"
        col_names[count+1] <- "KELTMLO"
        col_names[count+2] <- "KELTMHI"
        col_names[count+3] <- "KELNOPT"
        if("kelr" %in% return){
          col_names[count+7] <- "THALF"
        } else {
          col_names[count+4] <- "THALF" 
        }
        count <- count + 4
      }
      if("kelr" %in% return){
        col_names[count] <- "KELR"
        col_names[count+1] <- "KELRSQ"
        col_names[count+2] <- "KELRSQA"
        if("kel" %in% return){
          count <- count + 4
        } else {
          count <- count + 3
        }
      }
      if("lasttime" %in% return){
        col_names[count] <- "LASTTIME"
        count <- count + 1
      }
      if("auc_all" %in% return){
        col_names[count] <- "AUCALL"
        count <- count + 1
      }
      if("auc_dn" %in% return){
        col_names[count] <- "AUCDN"
        count <- count + 1
      }
      if("auc_last" %in% return){
        col_names[count] <- "AUCLAST"
        count <- count + 1
      }
      if("auc_last_c" %in% return){
        col_names[count] <- "AUCLASTC"
        count <- count + 1
      }
      if("auc_last_dn" %in% return){
        col_names[count] <- "AUCLASTDN"
        count <- count + 1
      }
      if("aumc_last" %in% return){
        col_names[count] <- "AUMCLAST"
        count <- count + 1
      }
      if("auc_t1_t2" %in% return){
        auc_col <- length(unique(data_data[,map_data$NOMTIME]))-1
        for(x in 1:auc_col){
          col_names[count] <- paste0("AUC",x)
          col_names[count+auc_col] <- paste0("AUCINT",x)
          count <- count + 1
        }
        count <- count + auc_col
      } 
      if("auc_inf_o" %in% return){
        col_names[count] <- "AUCINFO"
        count <- count + 1
      }
      if("auc_inf_p" %in% return){
        col_names[count] <- "AUCINFP"
        count <- count + 1
      }
      if("auc_inf_o_c" %in% return){
        col_names[count] <- "AUCINFOC"
        count <- count + 1
      }
      if("auc_inf_p_c" %in% return){
        col_names[count] <- "AUCINFPC"
        count <- count + 1
      }
      if("auc_inf_o_dn" %in% return){
        col_names[count] <- "AUCINFODN"
        count <- count + 1
      }
      if("auc_inf_p_dn" %in% return){
        col_names[count] <- "AUCINFPDN"
        count <- count + 1
      }
      if("aumc_inf_o" %in% return){
        col_names[count] <- "AUMCINFO"
        count <- count + 1
      }
      if("aumc_inf_p" %in% return){
        col_names[count] <- "AUMCINFP"
        count <- count + 1
      }
      if("mrt_last" %in% return){
        col_names[count] <- "MRTLAST"
        count <- count + 1
      }
      if("mrto" %in% return){
        col_names[count] <- "MRTO"
        count <- count + 1
      }
      if("mrtp" %in% return){
        col_names[count] <- "MRTP"
        count <- count + 1
      }
      if("auc_xpct_o" %in% return){
        col_names[count] <- "AUCXPCTO"
        count <- count + 1
      }
      if("auc_xpct_p" %in% return){
        col_names[count] <- "AUCXPCTP"
        count <- count + 1
      }
      if("aumc_xpct_o" %in% return){
        col_names[count] <- "AUMCXPCTO"
        count <- count + 1
      }
      if("aumc_xpct_p" %in% return){
        col_names[count] <- "AUMCXPCTP"
        count <- count + 1
      }
      if("clo" %in% return){
        col_names[count] <- "CLO"
        count <- count + 1
      }
      if("clfo" %in% return){
        col_names[count] <- "CLFO"
        count <- count + 1
      }
      if("clfow" %in% return){
        col_names[count] <- "CLFOW"
        count <- count + 1
      }
      if("clfp" %in% return){
        col_names[count] <- "CLFP"
        count <- count + 1
      }
      if("clfpw" %in% return){
        col_names[count] <- "CLFPW"
        count <- count + 1
      }
      if("vzfo" %in% return){
        col_names[count] <- "VZFO"
        count <- count + 1
      }
      if("vzfow" %in% return){
        col_names[count] <- "VZFOW"
        count <- count + 1
      }
      if("vzfp" %in% return){
        col_names[count] <- "VZFP"
        count <- count + 1
      }
      if("vzfpw" %in% return){
        col_names[count] <- "VZFPW"
        count <- count + 1
      }
      
      computation_df <- data.frame(matrix(ncol = count-1, nrow = 0)) 
      names(computation_df) <- col_names
      c_0 <- c0(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME])
      
      for(i in 1:length(unique(data_data[,map_data$SDEID]))){
        tmp_df <- data_data[data_data[,map_data$SDEID] == unique(data_data[,map_data$SDEID])[i],]
        row_data <- c(unique(data_data[,map_data$SDEID])[i])
        
        if("cmax" %in% return){
          c_max <- cmax(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME])
          row_data <- append(row_data, c_max)
        }
        if("cmin" %in% return){
          c_min <- cmin(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME])
          row_data <- append(row_data, c_min)
        }
        if("clast" %in% return){
          c_last <- clast(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME])
          row_data <- append(row_data, c_last)
        }
        if("cmax_dn" %in% return){
          if("cmax" %in% return){
            cmaxdn <- cmax_dn(cmax = c_max, dose = tmp_df[,map_data$DOSE][i])
            row_data <- append(row_data, cmaxdn)
          }
        }
        if("tmax" %in% return){
          t_max <- tmax(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME])
          row_data <- append(row_data, t_max)
        }
        if("tmin" %in% return){
          t_min <- tmin(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME])
          row_data <- append(row_data, t_min)
        }
        if("tlast" %in% return){
          t_last <- tlast(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME])
          row_data <- append(row_data, t_last)
        }
        if("tlag" %in% return){
          t_lag <- tlag(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME])
          row_data <- append(row_data, t_lag)
        }
        if("kel" %in% return){
          kel_v <- kel(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME])
          row_data <- append(row_data, kel_v[["KEL"]])
          row_data <- append(row_data, kel_v[["KELTMLO"]])
          row_data <- append(row_data, kel_v[["KELTMHI"]])
          row_data <- append(row_data, kel_v[["KELNOPT"]])
        }
        if("kelr" %in% return){
          kelr_v <- kel_r(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME])
          row_data <- append(row_data, kelr_v[["KELR"]])
          row_data <- append(row_data, kelr_v[["KELRSQ"]])
          row_data <- append(row_data, kelr_v[["KELRSQA"]])
        }
        if("kel" %in% return){
          row_data <- append(row_data, kel_v[["THALF"]])
        }
        if("lasttime" %in% return){
          last_time <- lasttime(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME])
          row_data <- append(row_data, last_time)
        }
        if("cmax_c" %in% return){
          if("cmax" %in% return){
            c_max_c <- cmaxc(kel = kel_v[["KEL"]], cmax = c_max, c0 = c_0, tmax = t_max)
            row_data <- append(row_data, cmaxdn)
          }
        }
        if("auc_all" %in% return){
          aucall <- auc_all(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME], method = method)
          row_data <- append(row_data, aucall)
        }
        if("auc_dn" %in% return){
          if("auc_all" %in% return){
            aucdn <- auc_dn(auc = aucall, dose = tmp_df[,map_data$DOSE][i])
            row_data <- append(row_data, aucdn)
          }
        }
        if("auc_last" %in% return){
          auclast <- auc_last(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME], method = method)
          row_data <- append(row_data, auclast)
        }
        if("auc_last_c" %in% return){
          auclast_c <- auc_lastc(kel = kel_v[["KEL"]], auclast = auclast, c0 = c_0, tlast = t_last)
          row_data <- append(row_data, auclast_c)
        }
        if("auc_last_c" %in% return){
          if("auc_last" %in% return){
            auclastdn <- auc_dn(auc = auclast, dose = tmp_df[,map_data$DOSE][i])
            row_data <- append(row_data, auclastdn)
          }
        }
        if("auc_last_dn" %in% return){
          if("auc_last" %in% return){
            auclastdn <- auc_dn(auc = auclast, dose = tmp_df[,map_data$DOSE][i])
            row_data <- append(row_data, auclastdn)
          }
        }
        if("aumc_last" %in% return){
          aumclast <- aumc_last(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME], method = method)
          row_data <- append(row_data, aumclast)
        }
        if("auc_t1_t2" %in% return){
          auct <- NULL
          auc_int <- NULL
          for(t in 2:length(unique(tmp_df[,map_data$NOMTIME]))){
            tmp <- auc_t1_t2(conc = tmp_df[,map_data$CONC], time = na.omit(tmp_df[,map_data$NOMTIME]), t1 = tmp_df[,map_data$NOMTIME][1], t2 = tmp_df[,map_data$NOMTIME][t], method = method)
            tmp_int <- paste0(unique(data_data[,map_data$NOMTIME])[1], "_", unique(data_data[,map_data$NOMTIME])[t])
            
            if(is.null(auc_t)){
              auct <- tmp
              auc_int <- tmp_int
            } else {
              auct <- c(auct, tmp)
              auc_int <- c(auc_int, tmp_int)
            }
          }
          if(length(auct) < auc_col) {
            auct <- c(auct, rep(NA, (auc_col - length(auct))))
          }
          if(length(auc_int) < auc_col) {
            auc_int <- c(auc_int, rep(NA, (auc_col - length(auc_int))))
          }
          row_data <- append(row_data, auct)
          row_data <- append(row_data, auc_int)
        }
        
        if("auc_inf_o" %in% return){
          aucinf_o <- auc_inf_o(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME], method = method)
          row_data <- append(row_data, aucinf_o)
        }
        if("auc_inf_p" %in% return){
          aucinf_p <- auc_inf_p(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME], method = method)
          row_data <- append(row_data, aucinf_p)
        }
        if("auc_inf_o_c" %in% return){
          if("auc_inf_o" %in% return){
            aucinf_oc <- auc_inf_oc(kel = kel_v[["KEL"]], aucinfo = aucinf_o, c0 = c_0)
            row_data <- append(row_data, aucinf_oc)
          }
        }
        if("auc_inf_p_c" %in% return){
          if("auc_inf_p" %in% return){
            aucinf_pc <- auc_inf_pc(kel = kel_v[["KEL"]], aucinfp = aucinf_p, c0 = c_0)
            row_data <- append(row_data, aucinf_pc)
          }
        }
        if("auc_inf_o_dn" %in% return){
          if("auc_inf_o" %in% return){
            aucinfo_dn <- auc_dn(auc = aucinf_o, dose = tmp_df[,map_data$DOSE][i])
            row_data <- append(row_data, aucinfo_dn)
          }
        }
        if("auc_inf_p_dn" %in% return){
          if("auc_inf_p" %in% return){
            aucinfp_dn <- auc_dn(auc = aucinf_p, dose = tmp_df[,map_data$DOSE][i])
            row_data <- append(row_data, aucinfp_dn)
          }
        }
        if("aumc_inf_o" %in% return){
          aumcinf_o <- aumc_inf_o(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME], method = method)
          row_data <- append(row_data, aumcinf_o)
        }
        if("aumc_inf_p" %in% return){
          aumcinf_p <- aumc_inf_p(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME], method = method)
          row_data <- append(row_data, aumcinf_p)
        }
        if("mrt_last" %in% return){
          mrtlast <- mrt_last(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME], method = method, model = model)
          row_data <- append(row_data, mrtlast)
        }
        if("mrto" %in% return){
          mrto <- mrt_evif_o(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME], method = method, parameter = parameter)
          row_data <- append(row_data, mrto)
        }
        if("mrtp" %in% return){
          mrtp <- mrt_evif_p(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME], method = method, parameter = parameter)
          row_data <- append(row_data, mrtp)
        }
        if("auc_xpct_o" %in% return){
          aucxpcto <- auc_XpctO(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME], method = method)
          row_data <- append(row_data, aucxpcto)
        }
        if("auc_xpct_p" %in% return){
          aucxpctp <- auc_XpctP(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME], method = method)
          row_data <- append(row_data, aucxpctp)
        }
        if("aumc_xpct_o" %in% return){
          aumcxpcto <- aumc_XpctO(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME], method = method)
          row_data <- append(row_data, aumcxpcto)
        }
        if("aumc_xpct_p" %in% return){
          aumcxpctp <- aumc_XpctP(conc = tmp_df[,map_data$CONC], time = tmp_df[,map_data$NOMTIME], method = method)
          row_data <- append(row_data, aumcxpctp)
        }
        if("clo" %in% return){
          cl_o <- clo(aucinfo = aucinf_o, dose = tmp_df[,map_data$DOSE][i])
          row_data <- append(row_data, cl_o)
        }
        if("clfo" %in% return){
          clf_o <- clfo(aucinfo = aucinf_o, dose = tmp_df[,map_data$DOSE][i])
          row_data <- append(row_data, clf_o)
        }
        if("clfow" %in% return){
          clf_ow <- clfow(clfo = clf_o, normbs = tmp_df[,map_data$NORMBS][i])
          row_data <- append(row_data, clf_ow)
        }
        if("clfp" %in% return){
          clf_p <- clfp(aucinfp = aucinf_p, dose = tmp_df[,map_data$DOSE][i])
          row_data <- append(row_data, clf_p)
        }
        if("clfpw" %in% return){
          clf_pw <- clfpw(clfp = clf_p, normbs = tmp_df[,map_data$NORMBS][i])
          row_data <- append(row_data, clf_pw)
        }
        if("vzfo" %in% return){
          vzf_o <- vzfo(kel = kel_v, aucinfo = aucinf_o, dose = tmp_df[,map_data$DOSE][i])
          row_data <- append(row_data, vzf_o)
        }
        if("vzfow" %in% return){
          vzf_ow <- vzfow(vzfo = vzf_o, normbs = tmp_df[,map_data$NORMBS][i])
          row_data <- append(row_data, vzf_ow)
        }
        if("vzfp" %in% return){
          vzf_p <- vzfp(kel = kel_v, aucinfp = aucinf_p, dose = tmp_df[,map_data$DOSE][i])
          row_data <- append(row_data, vzf_p)
        }
        if("vzfpw" %in% return){
          vzf_pw <- vzfpw(vzfp = vzf_p, normbs = tmp_df[,map_data$NORMBS][i])
          row_data <- append(row_data, vzf_pw)
        }

        computation_df[i,] <- row_data
      }
    } else {
      stop("Values provided via 'return' are not valid or cannot be calculated! Please provide valid inputs")
    }
  }
  
  return(computation_df)
}

