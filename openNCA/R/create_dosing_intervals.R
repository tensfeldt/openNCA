#' create_dosing_intervals updates concentration data with dosing intervals
#'
#' create_dosing_intervals updates concentration data with dosing intevals values to align with
#' selected TIME, DOSINGTYPE, TOLDi and TAUi values.
#'
#' that map$TIME variable either points to a valid variable in data or is nominal/actual selection.
#' map$TIME is either a value of "Nominal" or "Actual" or it points directly to a column/data field in the data dataframe.
#' if map$TIME=="Nominal" then map$NOMTIME/map$NOMTIMEU will point to the column/data field in the data dataframe for TIME
#' if map$TIME=="Actual"  then map$ACTTIME/map$ACTTIMEU will point to the column/data field in the data dataframe for TIME
#' if map$TIME=="Nominal"  then map$NOMTIME will be used to create the dosing intevals with respect to TOLDi and TAUi
#' if map$TIME=="Actual"  then map$NOMTIME will be used to create the dosing intevals with respect to TOLDi and TAUi
#'
#' @param data input concentration dataset
#' @param map Model Configuration Template (MCT)
#' @param flags input flags dataset
#' @param maxdosingintervals number of maximum dosing intervals
#' 
#' @section Returns:
#' updated concentration data
#'
#' @return data
#' 
#' @examples
#' No appropriate examples
#'
#' @export
create_dosing_intervals <- function(data, map, flag, maxdosingintervals) {
  if(toupper(map$DOSINGTYPE) == 'SS'){
    ss_dose <- paste0("DOSE", rep(1:maxdosingintervals))
    ss_tau  <- paste0("TAU" , rep(1:maxdosingintervals))
    ss_told <- paste0("TOLD", rep(1:maxdosingintervals))
    
    valid_dose <- NULL
    valid_tau <- NULL
    valid_told <- NULL
    if("DOSE" %in% names(map) || any(ss_dose %in% names(map) || ss_tau %in% names(map) || ss_told %in% names(map))){
      test_dose <- ifelse("DOSE" %in% names(map), !as.logical(is.na(map[,"DOSE"])), FALSE)
      if(test_dose || any(!as.logical(is.na(map[ss_dose[ss_dose %in% names(map)]]))) || any(!as.logical(is.na(map[ss_tau[ss_tau %in% names(map)]]))) || any(!as.logical(is.na(map[ss_told[ss_told %in% names(map)]])))){
        if(test_dose || any(!as.logical(is.na(map[ss_dose[ss_dose %in% names(map)]])))){
          tmp_dose <- map[ss_dose[ss_dose %in% names(map)]]
          if(ncol(tmp_dose) == 0){
            tmp_dose <- map['DOSE']
            map["DOSE1"] <- map['DOSE']
          }
          #print(tmp_dose)
          dose_logic <- unlist(lapply(tmp_dose, function(x) { return(is.na(x) || x == "")}))
          valid_dose <- names(tmp_dose[!as.logical(dose_logic)])
          #print(valid_dose)
        }
        if(any(!as.logical(is.na(map[ss_tau[ss_tau %in% names(map)]])))){
          tmp_tau <- map[ss_tau[ss_tau %in% names(map)]]
          #print(tmp_tau)
          tau_logic <- unlist(lapply(tmp_tau, function(x) { return(is.na(x) || x == "")}))
          valid_tau <- names(tmp_tau[!as.logical(tau_logic)])
          #print(valid_tau)
        }
        if(any(!as.logical(is.na(map[ss_told[ss_told %in% names(map)]])))){
          tmp_told <- map[ss_told[ss_told %in% names(map)]]
          #print(tmp_told)
          told_logic <- unlist(lapply(tmp_told, function(x) { return(is.na(x) || x == "")}))
          valid_told <- names(tmp_told[!as.logical(told_logic)])
          #print(valid_told)
        }
      } else {
        stop(paste("1 Unable to generate dosing interval for Steady State data! Values for", paste(ss_dose, collapse = ", "), "and", paste(ss_tau, collapse = ", "), "and", paste(ss_told, collapse = ", "), "provided via 'map' are not valid!"))
      }
      if(length(valid_dose) > 0 && length(valid_tau) > 0 && length(valid_told) > 0){
        if(!(length(valid_dose) == length(valid_tau) && length(valid_tau) == length(valid_told))){
          stop(paste("2 Unable to generate dosing interval for Steady State data! Please provide same number of interval values of data parameters, you provided:", paste(valid_dose, collapse = ", "), "and", paste(valid_tau, collapse = ", "), "and", paste(valid_told, collapse = ", "), "which are uneven!"))
        } else {
          if(!(sum(map[valid_dose] %in% names(data)) == sum(map[valid_tau] %in% names(data)) && sum(map[valid_tau] %in% names(data)) == sum(map[valid_told] %in% names(data)))){
            missing_values <- ""
            k <- valid_dose[!map[valid_dose] %in% names(data)]
            if(length(valid_dose[!map[valid_dose] %in% names(data)]) > 0){
              missing_values <- paste(missing_values, valid_dose[!map[valid_dose] %in% names(data)])
            }
            if(length(valid_tau[!map[valid_tau] %in% names(data)]) > 0){
              missing_values <- paste(missing_values, valid_tau[!map[valid_tau] %in% names(data)])
            }
            if(length(valid_told[!map[valid_told] %in% names(data)]) > 0){
              missing_values <- paste(missing_values, valid_told[!map[valid_told] %in% names(data)])
            }
            stop(paste("3 Unable to generate dosing interval for Steady State data! Values for", missing_values, "provided via 'map' are not present in the dataset provided via 'data'"))
          }
        }
      } else {
        if(length(valid_tau) > 0){
          map[,valid_tau] <- valid_tau
        }
        if(length(valid_told) > 0){
          map[,valid_told] <- valid_told
        }
        for(j in 1:length(unique(data[,map$SDEID]))){
          tmp_df <- data[data[,map$SDEID] == unique(data[,map$SDEID])[j],]
          tmp_df <- tmp_df[order(tmp_df[,map$TIME]),]
          if(nrow(tmp_df) > 0){
            data[data[,map$SDEID] == unique(data[,map$SDEID])[j],][,valid_told] <- tmp_df[1,map$TIME] 
            data[data[,map$SDEID] == unique(data[,map$SDEID])[j],][,valid_tau] <- tmp_df[nrow(tmp_df),map$TIME] - tmp_df[1,map$TIME] 
          }
        }
      }
      
      intervals <- length(valid_dose)
      prev_told <- list()
      prev_tau <- list()
      for(i in 1:intervals){
        if((isTRUE("DOSE" %in% names(map)) || isTRUE(ss_dose[i] %in% names(map))) & isTRUE(ss_tau[i] %in% names(map)) & isTRUE(ss_told[i] %in% names(map))) {
          if(ss_dose[i] %in% names(map)){
            if(!(map[,ss_dose[i]] %in% names(data))){
              stop(paste0("5 Unable to generate dosing interval for Steady State data! '", map[,ss_dose[i]], "' value for ", ss_dose[i], " is not present in the data"))
            }
          } else {
            if(!("DOSE" %in% names(map))){
              stop(paste0("5 Unable to generate dosing interval for Steady State data! '", ss_dose[i], "' is not present in the 'map'"))
            }
          }
          if(!(map[,ss_tau[i]] %in% names(data))){
            stop(paste0("6 Unable to generate dosing interval for Steady State data! '", map[,ss_tau[i]], "' value for ", ss_tau[i], " is not present in the data"))
          }
          if(!(map[,ss_told[i]] %in% names(data))){
            stop(paste0("7 Unable to generate dosing interval for Steady State data! '", map[,ss_told[i]], "' value for ", ss_told[i], " is not present in the data"))
          }
          data[c(paste0("DI", i, "F"))] <- NA
          warning_generated <- FALSE
          for(j in 1:length(unique(data[,map$SDEID]))){
            tmp_df <- data[data[,map$SDEID] == unique(data[,map$SDEID])[j],]
            if(toupper(map$MODEL) == 'M4'){
              tmp_df <- tmp_df[order(tmp_df[, map$TIME]),]
              s_time <- tmp_df[, map$TIME]
              e_time <- tmp_df[, map$ENDTIME]
            } else {
              tmp_df <- tmp_df[order(tmp_df[, map$TIME]),]
              tmp_time <- tmp_df[, map$NOMTIME]
            }
            told <- unique(tmp_df[,as.character(map[,ss_told[i]])])[[1]]
            tau <- unique(tmp_df[,as.character(map[,ss_tau[i]])])[[1]]
            if(!is.numeric(tau))  { tau  <- as.numeric(tau) }
            if(!is.numeric(told)) { told <- as.numeric(told) }
            if(i > 1){
              for(k in 1:(i-1)){
                if(!is.na(told) && !is.na(prev_tau[[j]][k])) {
                  if(told < prev_tau[[j]][k]){
                    if(i == 2){
                      interval_text <- paste0(i,"nd")
                    } else if(i == 3){
                      interval_text <- paste0(i,"rd")
                    } else {
                      interval_text <- paste0(i,"th")
                    }
                    warning(paste0("Unable to generate ", interval_text," dosing interval for Steady State data because it overlaps previous intervals for SDEID: '", unique(data[,map$SDEID])[j],"'!"))
                    told <- NA
                    tau <- NA
                  }
                }
              }
              if(!is.na(tau) && isTRUE(tau == 0)){
                if(i == 2){
                  interval_text <- paste0(i,"nd")
                } else if(i == 3){
                  interval_text <- paste0(i,"rd")
                } else {
                  interval_text <- paste0(i,"th")
                }
                warning(paste0("Unable to generate ", interval_text," dosing interval for Steady State data because TAU is zero for SDEID: '", unique(data[,map$SDEID])[j],"'!"))
                told <- NA
                tau <- NA
              }
            }
            if(i > 1){
              prev_told[[j]] <- c(prev_told[[j]], told)
              prev_tau[[j]] <- c(prev_tau[[j]], tau)
            } else {
              prev_told[[j]] <- told
              prev_tau[[j]] <- tau
            }
            
            if(is.na(told) || is.na(tau)){
              data[data[,map$SDEID] == unique(data[,map$SDEID])[j],][[c(paste0("DI", i, "F"))]] <- rep(0, nrow(tmp_df))
            } else {
              if(toupper(map$MODEL) == 'M4'){
                told_i <- match(told, s_time)
                tau_i <- match((tau+told), e_time)
                
                if(is.na(told_i) || is.na(tau_i)){
                  if(is.na(told_i)){
                    if(casefold(map$ORGTIME)=='actual'){
                      told <- s_time[told < s_time][1]
                      told_i <- match(told, s_time)
                      tau_i <- match((tau+told), e_time)
                    } else {
                      stop("8 Unable to generate dosing interval for Steady State data! TOLD value not found in the provided TIME data")
                    }
                  }
                  if(is.na(tau_i)){
                    if(length(e_time[e_time < (tau+told)]) > 0 || length(e_time) == 1){
                      if(length(e_time[e_time < (tau+told)]) > 0){
                        tau <- e_time[e_time < (tau+told)][length(e_time[e_time < (tau+told)])]
                      } else if(length(e_time) == 1){
                        tau <- e_time[1]
                      }
                      tau_i <- match((tau+told), e_time)
                    } else {
                      stop("9 Unable to generate dosing interval for Steady State data! TAU value not found in the provided TIME data")
                    }
                  }
                }
                tmp_merged <-  data[data[,map$SDEID] == unique(data[,map$SDEID])[j], map$TIME]
                if(!is.na(tau_i) && !is.na(told_i)){
                  data[data[,map$SDEID] == unique(data[,map$SDEID])[j],][order(tmp_merged),][[c(paste0("DI", i, "F"))]] <- c(rep(0, told_i-1), rep(1, (tau_i-told_i+1)), rep(0, (nrow(tmp_df) - tau_i)))
                } else {
                  if(i == intervals){
                    if(is.na(told_i)){
                      data[data[,map$SDEID] == unique(data[,map$SDEID])[j],][order(tmp_merged),][[c(paste0("DI", i, "F"))]] <- c(rep(0, nrow(tmp_df)))
                      if(!isTRUE(warning_generated)){
                        warning(paste0("Unable to generate dosing interval for Steady State data! TOLD value 'TOLD", i ,"' not found in the provided TIME data"))
                        warning_generated <- TRUE
                      }  
                    } else if(is.na(tau_i)){
                      tmp_tau_i <- which(e_time <= (tau+told))
                      if(length(tmp_tau_i) > 0){
                        tau_i <- tmp_tau_i[length(tmp_tau_i)] 
                        data[data[,map$SDEID] == unique(data[,map$SDEID])[j],][order(tmp_merged),][[c(paste0("DI", i, "F"))]] <- c(rep(0, told_i-1), rep(1, (tau_i-told_i+1)), rep(0, (nrow(tmp_df) - tau_i)))
                      } else {
                        data[data[,map$SDEID] == unique(data[,map$SDEID])[j],][order(tmp_merged),][[c(paste0("DI", i, "F"))]] <- c(rep(0, nrow(tmp_df)))
                        if(!isTRUE(warning_generated)){
                          warning(paste0("Unable to generate dosing interval for Steady State data! TAU value 'TAU", i ,"' not found in the provided TIME data"))
                          warning_generated <- TRUE
                        }
                      }
                    }
                  } else {
                    data[data[,map$SDEID] == unique(data[,map$SDEID])[j],][order(tmp_merged),][[c(paste0("DI", i, "F"))]] <- c(rep(0, nrow(tmp_df)))
                    if(!isTRUE(warning_generated)){
                      warning(paste0("Unable to generate dosing interval for Steady State data! TAU value 'TAU", i ,"' or TOLD value 'TOLD", i ,"' not found in the provided TIME data"))
                      warning_generated <- TRUE
                    }   
                  }
                }
              } else {
                told_i <- match(told, tmp_time)
                tau_i <- match((tau+told), tmp_time)
                if(is.na(told_i) || is.na(tau_i)){
                  if(is.na(told_i)){
                    told <- tmp_time[told < tmp_time][1]
                    told_i <- match(told, tmp_time)
                    tau_i <- match((tau+told), tmp_time)
                  }
                  if(is.na(tau_i)){
                    if(length(tmp_time[tmp_time < (tau+told)]) > 0){
                      tau <- tmp_time[tmp_time < (tau+told)][length(tmp_time[tmp_time < (tau+told)])]
                      tau_i <- match(tau, tmp_time)
                    } else {
                      stop("10 Unable to generate dosing interval for Steady State data! TAU value not found in the provided TIME data")
                    }
                  }
                }
                tmp_merged <-  data[data[,map$SDEID] == unique(data[,map$SDEID])[j], map$TIME]
                if(!is.na(tau_i) && !is.na(told_i)){
                  data[data[,map$SDEID] == unique(data[,map$SDEID])[j],][order(tmp_merged),][[c(paste0("DI", i, "F"))]] <- c(rep(0, told_i-1), rep(1, (tau_i-told_i+1)), rep(0, (nrow(tmp_df) - tau_i)))
                } else {
                  if(i == intervals){
                    if(is.na(told_i)){
                      data[data[,map$SDEID] == unique(data[,map$SDEID])[j],][order(tmp_merged),][[c(paste0("DI", i, "F"))]] <- c(rep(0, nrow(tmp_df)))
                      if(!isTRUE(warning_generated)){
                        warning(paste0("Unable to generate dosing interval for Steady State data! TOLD value 'TOLD", i ,"' not found in the provided TIME data"))
                        warning_generated <- TRUE
                      }  
                    } else if(is.na(tau_i)){
                      tmp_tau_i <- which(tmp_time <= (tau+told))
                      if(length(tmp_tau_i) > 0){
                        tau_i <- tmp_tau_i[length(tmp_tau_i)] 
                        data[data[,map$SDEID] == unique(data[,map$SDEID])[j],][order(tmp_merged),][[c(paste0("DI", i, "F"))]] <- c(rep(0, told_i-1), rep(1, (tau_i-told_i+1)), rep(0, (nrow(tmp_df) - tau_i)))
                      } else {
                        data[data[,map$SDEID] == unique(data[,map$SDEID])[j],][order(tmp_merged),][[c(paste0("DI", i, "F"))]] <- c(rep(0, nrow(tmp_df)))
                        if(!isTRUE(warning_generated)){
                          warning(paste0("Unable to generate dosing interval for Steady State data! TAU value 'TAU", i ,"' not found in the provided TIME data"))
                          warning_generated <- TRUE
                        }
                      }
                    }
                  } else {
                    data[data[,map$SDEID] == unique(data[,map$SDEID])[j],][order(tmp_merged),][[c(paste0("DI", i, "F"))]] <- c(rep(0, nrow(tmp_df)))
                    if(!isTRUE(warning_generated)){
                      warning(paste0("Unable to generate dosing interval for Steady State data! TAU value 'TAU", i ,"' or TOLD value 'TOLD", i ,"' not found in the provided TIME data"))
                      warning_generated <- TRUE
                    }
                  }
                }
              }
            }
          }
        }
      }
    } else {
      data[[c(paste0("DI", i, "F"))]] <- rep(1, nrow(data))
      warning(paste0("Unable to generate dosing interval for Steady State data since no TAUi, TOLDi or DOSEi was provided! Using the entire profile as the dosing interval!"))
    }
  } 
  return(data)
}
