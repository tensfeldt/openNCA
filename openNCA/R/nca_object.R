#' Load data files into the NCA Environment
#'
#' This function loads a mapping file with specified paramenters and data for NCA computation. 
#' It assumes that the content gives an accurate representation of the sample and dose route 
#' in order to provide the most accurate results. You can also provide a flag data, but it is not 
#' necessary.
#' 
#' @details 
#' Dataset must contain a 'SID', 'TIME', 'RESULT' (case sensitive), if it doesn't then the 
#' user must provide the column names corresponding to these in the dataset passed as the arguement. 
#' By default, it will assume the columns names in the data set provided are 'SID', 'TIME', 'RESULT'. \cr 
#' 
#' For the units arguemnts, only provide the column name if the units are specified in a seperate 
#' column of the dataset. If the units are witin the same column, then they will be accounted for 
#' and seperated in its own column. Keep in mind, if there is a seperate column for units and 
#' the dataset also contains its own set of units, then the colummn with the seperate units will 
#' be overriden.\cr
#' 
#' Columns of 'DOSE' and 'MATRIX' are optional. If Matrix column is provided within the argument
#' then the data will be subsetted based on the model selection with its respective matrix. \cr
#' 
#' Primary parameter computation with NCA will be grouped in the following six PK NCA models: \cr
#' \figure{model.png}
#'
#' @param data The path for data that will be used for computation (usually .csv) or a data frame object
#' @param sid The column name that corresponds to the subject's identification number
#' @param time The column name that corresponds to the time of the results
#' @param result The column name that corresponds to the results (concentration)
#' @param result_units The column name that corresponds to the units of the result (if applicable)
#' @param dose The column name that corresponds to the dose amount (if applicable)
#' @param dose_units The column name that corresponds to the units of the dose amount (if applicable)
#' @param omit The value for omitting NA's from the data set (default set to FALSE)
#' @param model_num The precomputed models that inbedded in the package
#' @param matrix The column name that corresponds to the sample that is being tested (if applicable)
#' @section Returns:
#' \strong{Dataset} \cr 
#' \itemize{
#'  \item data: parsed data from the file
#'  \item model: details of the model (mentioned in 'Details') \itemize{
#'    \item model number
#'    \item matrix 
#'    \item dose route 
#'  }
#' }
#' @examples
#' 
#' ########## 
#' ## Data ##
#' ######################################################
#' ##  SID  ##  TIME  ##  RESULT  ## ... other columns ##
#' ######################################################
#' ##   30  ##    0   ##   2.89   ##                   ##
#' ##   30  ##    1   ##   2.49   ##                   ##
#' ##   30  ##    2   ##   2.47   ##                   ##
#' ##   31  ##    0   ##      0   ##                   ##
#' ##   31  ##    1   ##      0   ##                   ##
#' ##   31  ##    2   ##      0   ##                   ##
#' ##   32  ##    0   ##   1.19   ##                   ##
#' ##   32  ##    1   ##   1.23   ##                   ##
#' ##   32  ##    2   ##   1.34   ##                   ##
#' ##   32  ##    4   ##    BLQ   ##                   ##
#' ######################################################
#' 
#' data <- data.frame(
#'     SID = ...,
#'     TIME = ...,
#'     RESULT = ...
#' )
#' #Same data as above, just represented as a dataframe
#' 
#' model()   
#' #No data found! Please provide a dataset!
#' 
#' model("~/data.csv")  
#' #Creates an NCA object with data represented in 'data' above
#' 
#' ########### 
#' ## Data2 ##
#' ######################################################
#' ##  ID   ##  TIME  ##   CONC   ## ... other columns ##
#' ######################################################
#' ##   30  ##    0   ##   2.89   ##                   ##
#' ##   30  ##    1   ##   2.49   ##                   ##
#' ##   30  ##    2   ##   2.47   ##                   ##
#' ##   31  ##    0   ##      0   ##                   ##
#' ##   31  ##    1   ##      0   ##                   ##
#' ##   31  ##    2   ##      0   ##                   ##
#' ##   32  ##    0   ##   1.19   ##                   ##
#' ##   32  ##    1   ##   1.23   ##                   ##
#' ##   32  ##    2   ##   1.34   ##                   ##
#' ##   32  ##    4   ##    BLQ   ##                   ##
#' ######################################################
#' 
#' data2 <- data.frame(
#'     ID = ...,
#'     TIME = ...,
#'     CONC = ...
#' )
#' #Same data as above, just represented as a dataframe
#' 
#' model("~/data2.csv")  
#' #Column name for sid not found! Please provide a valid sid column name!
#' 
#' model("~/data2.csv", sid = "ID")
#' #Column name for result not found! Please provide a valid result column name!
#' 
#' model("~/data2.csv", sid = "ID", result = "CONC")
#' #$data
#' #   SID   TIME   RESULT  DOSE 
#' #1   30      0     2.89     1
#' #2   30      1     2.49     1
#' #3   30      2     2.47     1
#' #4   31      0        0     1
#' #5   31      1        0     1
#' #6   31      2        0     1
#' #7   32      0     1.19     1
#' #8   32      1     1.23     1 
#' #9   32      2     1.34     1
#' #10  32      4       NA     1
#' 
#' model("~/data2.csv", sid = "ID", result = "CONC", omit = TRUE)
#' #$data
#' #   SID   TIME   RESULT  DOSE
#' #1   30      0     2.89     1
#' #2   30      1     2.49     1 
#' #3   30      2     2.47     1
#' #4   31      0        0     1  
#' #5   31      1        0     1
#' #6   31      2        0     1
#' #7   32      0     1.19     1
#' #8   32      1     1.23     1
#' #9   32      2     1.34     1
#' #10  32      4        0     1
#' 
#' model("~/data2.csv", sid = "ID", result = "CONC", omit = TRUE, model_num = "M7")
#' #Invalid Model Number! Please enter a valid model number!
#' 
#' ########### 
#' ## Data3 ##
#' ####################################################################################
#' ##  ID   ##  TIME  ##   CONC       ## ... other columns ##  DOSE  ##  DOSE_UNITS  ##
#' ####################################################################################
#' ##   30  ##    0   ##   2.89ng/ml  ##                   ##   1    ##      ng      ##
#' ##   30  ##    1   ##   2.49ng/ml  ##                   ##   1    ##      ng      ##  
#' ##   30  ##    2   ##   2.47ng/ml  ##                   ##   1    ##      ng      ##
#' ##   31  ##    0   ##      0g/ml   ##                   ##   1    ##      g       ##
#' ##   31  ##    1   ##      0g/ml   ##                   ##   1    ##      g       ##
#' ##   31  ##    2   ##      0g/ml   ##                   ##   1    ##      g       ##
#' ##   32  ##    0   ##   1.19ng/ml  ##                   ##   1    ##      ng      ##
#' ##   32  ##    1   ##   1.23ng/ml  ##                   ##   1    ##      ng      ##
#' ##   32  ##    2   ##   1.34ng/ml  ##                   ##   1    ##      ng      ##
#' ##   32  ##    4   ##    BLQ       ##                   ##   1    ##      ng      ##
#' ####################################################################################
#' 
#' data3 <- data.frame(
#'     ID = ...,
#'     TIME = ...,
#'     CONC = ...
#' )
#' #Same data as above, just represented as a dataframe
#' 
#' model("~/data3.csv", sid = "ID", result = "CONC", dose = "DOSE", dose_units = "DOSE_UNITS", omit = TRUE)
#' #$data
#' #   SID   TIME   RESULT  RESUL_U DOSE DOSE_U
#' #1   30      0     2.89    ng/ml    1     ng
#' #2   30      1     2.49    ng/ml    1     ng 
#' #3   30      2     2.47    ng/ml    1     ng
#' #4   31      0        0     g/ml    1      g
#' #5   31      1        0     g/ml    1      g
#' #6   31      2        0     g/ml    1      g
#' #7   32      0     1.19    ng/ml    1     ng
#' #8   32      1     1.23    ng/ml    1     ng
#' #9   32      2     1.34    ng/ml    1     ng
#' #10  32      4        0    ng/ml    1     ng
#' 
#' ##############
#' ## nca_data ##
#' ##############
#' 
#' model(nca_data)
#' #Column name for sid not found! Please provide a valid sid column name!
#' 
#' model(nca_data, sid = "Subject")
#' #Column name for time not found! Please provide a valid time column name!
#' 
#' model(nca_data, sid = "Subject", time = "Ntime", result = "Conc")
#' #$data
#' #    ProfileID SID Gender Age AgeU   Wt WtU Treatment Dose Matrix  TIME NtimeU  Time TimeU RESULT ConcU DataExclude AUCExclude KELExclude SUMExclude
#' #1           1   1      M  35   Yr 79.6  Kg         A 4.02 Plasma  0.00     Hr  0.00    Hr   0.74  MG/L           0          0          1          0
#' #2           1   1      M  35   Yr 79.6  Kg         A 4.02 Plasma  0.25     Hr  0.25    Hr   2.84  MG/L           0          0          1          0
#' #3           1   1      M  35   Yr 79.6  Kg         A 4.02 Plasma  0.50     Hr  0.57    Hr   6.57  MG/L           0          0          1          0
#' #4           1   1      M  35   Yr 79.6  Kg         A 4.02 Plasma  1.00     Hr  1.12    Hr  10.50  MG/L           0          0          1          0
#' #5           1   1      M  35   Yr 79.6  Kg         A 4.02 Plasma  2.00     Hr  2.02    Hr   9.66  MG/L           0          0          0          0
#' #6           1   1      M  35   Yr 79.6  Kg         A 4.02 Plasma  4.00     Hr  3.82    Hr   8.58  MG/L           0          0          0          0
#' #7           1   1      M  35   Yr 79.6  Kg         A 4.02 Plasma  5.00     Hr  5.10    Hr   8.36  MG/L           0          0          0          0
#' #8           1   1      M  35   Yr 79.6  Kg         A 4.02 Plasma  7.00     Hr  7.03    Hr   7.47  MG/L           0          0          0          0
#' #9           1   1      M  35   Yr 79.6  Kg         A 4.02 Plasma  9.00     Hr  9.05    Hr   6.89  MG/L           0          0          0          0
#' #10          1   1      M  35   Yr 79.6  Kg         A 4.02 Plasma 12.00     Hr 12.12    Hr   5.94  MG/L           0          0          0          0
#' #11          1   1      M  35   Yr 79.6  Kg         A 4.02 Plasma 24.00     Hr 24.37    Hr   3.28  MG/L           0          0          0          0
#' #12          2   2      M  37   Yr 72.4  Kg         A 4.40 Plasma  0.00     Hr  0.00    Hr   0.00  MG/L           0          0          1          0
#' #13          2   2      M  37   Yr 72.4  Kg         A 4.40 Plasma  0.25     Hr  0.25    Hr   1.72  MG/L           0          0          1          0
#' #14          2   2      M  37   Yr 72.4  Kg         A 4.40 Plasma  0.50     Hr  0.50    Hr   7.91  MG/L           0          0          1          0
#' #15          2   2      M  37   Yr 72.4  Kg         A 4.40 Plasma  1.00     Hr  1.00    Hr   8.31  MG/L           0          0          1          0
#' #16          2   2      M  37   Yr 72.4  Kg         A 4.40 Plasma  2.00     Hr  2.00    Hr   8.33  MG/L           0          0          0          0
#' #17          2   2      M  37   Yr 72.4  Kg         A 4.40 Plasma  4.00     Hr  4.00    Hr   6.85  MG/L           0          0          0          0
#' #18          2   2      M  37   Yr 72.4  Kg         A 4.40 Plasma  5.00     Hr  5.00    Hr   6.08  MG/L           0          0          0          0
#' #19          2   2      M  37   Yr 72.4  Kg         A 4.40 Plasma  7.00     Hr  7.00    Hr   5.40  MG/L           0          0          0          0
#' #20          2   2      M  37   Yr 72.4  Kg         A 4.40 Plasma  9.00     Hr  9.00    Hr   4.55  MG/L           0          0          0          0
#' #21          2   2      M  37   Yr 72.4  Kg         A 4.40 Plasma 12.00     Hr 12.00    Hr   3.01  MG/L           0          0          0          0
#' #22          2   2      M  37   Yr 72.4  Kg         A 4.40 Plasma 24.00     Hr 24.00    Hr   0.90  MG/L           0          0          0          0
#' #23          3   3      M  36   Yr 70.5  Kg         A 4.53 Plasma  0.00     Hr  0.00    Hr   0.00  MG/L           0          0          1          0
#' #24          3   3      M  36   Yr 70.5  Kg         A 4.53 Plasma  0.25     Hr  0.27    Hr   4.40  MG/L           0          0          1          0
#' #25          3   3      M  36   Yr 70.5  Kg         A 4.53 Plasma  0.50     Hr  0.58    Hr   6.90  MG/L           0          0          1          0
#' #26          3   3      M  36   Yr 70.5  Kg         A 4.53 Plasma  1.00     Hr  1.02    Hr   8.20  MG/L           0          0          1          0
#' #27          3   3      M  36   Yr 70.5  Kg         A 4.53 Plasma  2.00     Hr  2.02    Hr   7.80  MG/L           0          0          0          0
#' #28          3   3      M  36   Yr 70.5  Kg         A 4.53 Plasma  4.00     Hr  3.62    Hr   7.50  MG/L           0          0          0          0
#' #29          3   3      M  36   Yr 70.5  Kg         A 4.53 Plasma  5.00     Hr  5.08    Hr   6.20  MG/L           0          0          0          0
#' #30          3   3      M  36   Yr 70.5  Kg         A 4.53 Plasma  7.00     Hr  7.07    Hr   5.30  MG/L           0          0          0          0
#' #31          3   3      M  36   Yr 70.5  Kg         A 4.53 Plasma  9.00     Hr  9.00    Hr   4.90  MG/L           0          0          0          0
#' #32          3   3      M  36   Yr 70.5  Kg         A 4.53 Plasma 12.00     Hr 12.15    Hr   3.70  MG/L           0          0          0          0
#' #33          3   3      M  36   Yr 70.5  Kg         A 4.53 Plasma 24.00     Hr 24.17    Hr   1.05  MG/L           0          0          0          0
#' #... reached max.print
#' 
#' @author
#' \itemize{
#'  \item Ronish Desai: \email{rbdesai@@rudraya.com} 
#' }
#' Other contributors: \cr
#' \itemize{
#'  \item Rudraya Corporation: \url{http://rudraya.com}
#' }
#' \figure{rudraya.png} 
#' @export
model <- function(data = NULL, sid = NULL, time = NULL, result = NULL, result_units = NULL, dose = NULL, dose_units = NULL, omit = FALSE, model_num = "M1", matrix = NULL){
  if(is.null(data)){
    stop("No data found! Please provide a dataset!")
  } else {
    if("data.frame" %in% class(data)){
      load_data <- data
    } else if("character" %in% class(data)){
      if(file.exists(data)){
        load_data <- read.csv(data)
      } else {
        stop("Invalid path provided! No such file exists!")
      }
    } else {
      stop("Invalid arguement provided to 'data'! Please provide a valid arguement!")
    }
    
    m_num_check <- TRUE
    if(identical("M1", model_num)){
      mtrx <- "Plasma/Serum/Blood"
      d_route <- "Extravascular"
    } else if(identical("M2", model_num)){
      mtrx <- "Plasma/Serum/Blood"
      d_route <- "Bolus IV"
    } else if(identical("M3", model_num)){
      mtrx <- "Plasma/Serum/Blood"
      d_route <- "Constant Infusion"
    } else if(identical("M4", model_num)){
      mtrx <- "Urine"
      d_route <- "Constant Infusion"
    } else if(identical("M5", model_num)){
      mtrx <- "Urine"
      d_route <- "Constant Infusion"
    } else if(identical("M6", model_num)){
      mtrx <- "Urine"
      d_route <- "Constant Infusion"
    } else {
      m_num_check <- FALSE
      stop("Invalid Model Number! Please enter a valid model number!")
    }
    
    if(is.null(sid)){         ## 'SID' is the default name for subject identification number 
      sid <- "SID"
    }
    if(is.null(time)){        ## 'TIME' is the default name for time 
      time <- "TIME"
    }
    if(is.null(result)){      ## 'RESULT' is the default name for the concentration/result
     result <- "RESULT"
    }
    default <- FALSE
    if(is.null(matrix)){      ## 'MATRIX' is the default name for the matrix
      default <- TRUE
      matrix <- "MATRIX"
    }
      
    if(!is.null(sid) && !is.null(time) && !is.null(result)){
      tmp_cols <- colnames(load_data)
      sid_detect <- FALSE
      time_detect <- FALSE
      result_detect <- FALSE
      dose_detect <- FALSE
      r_u_detect <- FALSE
      d_u_detect <- FALSE
      
      for(i in 1:length(tmp_cols)){
        if(identical(sid, tmp_cols[i])){
          colnames(load_data)[i] <- "SID"
          sid_detect <- TRUE
        } else if(identical(time, tmp_cols[i])){
          colnames(load_data)[i] <- "TIME"
          time_detect <- TRUE
        } else if(identical(result, tmp_cols[i])){
          colnames(load_data)[i] <- "RESULT"
          result_detect <- TRUE
        }
        if(!(is.null(dose))){
          if(identical(dose, tmp_cols[i])){
            colnames(load_data)[i] <- "DOSE"
            dose_detect <- TRUE
          }
        }
        if(!(is.null(result_units))){
          if(identical(result_units, tmp_cols[i])){
            colnames(load_data)[i] <- "RESULT_U"
            r_u_detect <- TRUE
          }
        }
        if(!(is.null(dose_units))){
          if(identical(dose_units, tmp_cols[i])){
            colnames(load_data)[i] <- "DOSE_U"
            d_u_detect <- TRUE
          }
        }
      }
      if(!isTRUE(sid_detect)){
        stop("Column name for sid not found! Please provide a valid sid column name!")
      }
      if(!isTRUE(time_detect)){
        stop("Column name for time not found! Please provide a valid time column name!")
      }
      if(!isTRUE(result_detect)){
        stop("Column name for result not found! Please provide a valid result column name!")
      }
      if(!(is.null(dose))){
        if(!isTRUE(dose_detect)){
          stop("Column name for dose not found! Please provide a valid dose column name!")
        }
      }
      if(!(is.null(result_units))){
        if(!isTRUE(r_u_detect)){
          stop("Column name for result units not found! Please provide a valid result units column name!")
        }
      }
      if(!(is.null(dose_units))){
        if(!isTRUE(d_u_detect)){
          stop("Column name for dose units not found! Please provide a valid dose units column name!")
        }
      }
      
      if(!is.null(matrix)){
        if(matrix %in% names(load_data)){
          matrix_tmp <- strsplit(mtrx, "/")[[1]]
          load_temp <- load_data[load_data[matrix] == matrix_tmp,]
          if(nrow(load_temp) > 0){
            load_data <- load_temp
          } else {
            stop("Matrix corresponding to the model number is not found in the column provided")
          } 
        } else {
          if(!isTRUE(default)) {
            stop("Column name for matrix not found! Please provide a valid matrix column name!")
          }
        }
      }
      
      if(isTRUE(sid_detect) && isTRUE(time_detect) && isTRUE(result_detect)){
        load_data$TIME <- as.double(as.character(load_data$TIME))
        check_result <- as.double(as.character(load_data$RESULT))
        if(is.na(unique(check_result)[1]) && length(unique(check_result)) == 1){
          #print("entered the units case")
          load_data$RESULT_U <- NA
          n_row <- 1
          r_unit <- NULL
          load_data$RESULT <- as.character(load_data$RESULT)
          load_data$RESULT <- sapply(load_data$RESULT, function(x){ 
            for(i in nchar(x):1){
              if(!(is.na(as.numeric(substr(x,i,i))))){
                if(identical(substr(x,i+1,i+1), " ")){
                  r_unit <<- substr(x,i+2,nchar(x))
                  load_data$RESULT_U[n_row] <<- r_unit
                  n_row <<- n_row + 1
                  return(as.numeric(substr(x,1,i)))
                } else {
                  r_unit <<- substr(x,i+1,nchar(x))
                  load_data$RESULT_U[n_row] <<- r_unit
                  n_row <<- n_row + 1
                  return(as.numeric(substr(x,1,i)))  
                }
              } else if(i == 1){
                r_unit <<- NA
                load_data$RESULT_U[n_row] <<- r_unit
                n_row <<- n_row + 1 
                return(NA)
              }
            } 
          })
          #if(!is.null(r_unit)){
          #  if(is.null(result_units)){
          #    load_data$RESULT_U <- r_unit    
          #  }
          #}
        } else {
          load_data$RESULT <- check_result
        }
        if((!(is.null(dose))) || ("DOSE" %in% tmp_cols)){
          check_dose <- as.double(as.character(load_data$DOSE))
          if(is.na(unique(check_dose)[1]) && length(unique(check_dose)) == 1){
            #print("entered the units case")
            load_data$DOSE_U <- NA
            n_row2 <- 1
            d_unit <- NULL
            load_data$DOSE <- as.character(load_data$DOSE)
            load_data$DOSE <- sapply(load_data$DOSE, function(x){ 
              for(i in nchar(x):1){
                if(!(is.na(as.numeric(substr(x,i,i))))){
                  if(identical(substr(x,i+1,i+1), " ")){
                    d_unit <<- substr(x,i+2,nchar(x))
                    load_data$DOSE_U[n_row2] <<- d_unit
                    n_row2 <<- n_row2 + 1
                    return(as.numeric(substr(x,1,i)))
                  } else {
                    d_unit <<- substr(x,i+1,nchar(x))
                    load_data$DOSE_U[n_row2] <<- d_unit
                    n_row2 <<- n_row2 + 1
                    return(as.numeric(substr(x,1,i)))  
                  }
                } else if(i == 1){
                  r_unit <<- NA
                  load_data$DOSE_U[n_row2] <<- d_unit
                  n_row2 <<- n_row2 + 1 
                  return(NA)
                }
              } 
            })
            #if(!is.null(d_unit)){
            #  if(is.null(dose_units)){
            #    load_data$DOSE_U <- d_unit  
            #  }
            #}
          } else {
            load_data$DOSE <- check_dose
          }
        }
        if(omit){
          load_data$RESULT[is.na(load_data$RESULT)] <- 0
          load_data$TIME[is.na(load_data$TIME)] <- 0
          if("DOSE" %in% names(load_data)){
            load_data$DOSE[is.na(load_data$DOSE)] <- 0 
          }
        }
        
        if(m_num_check){
          model_spec <- list(model_num = model_num, matrix = mtrx, dose_route = d_route)
          #print(paste0("Matrix: ", mtrx))
          #print(paste0("Dosing route: ", d_route))
          NCA_object <- structure(list(data = load_data, model = model_spec), class = "NCA")
          return(NCA_object)
        }
      }
    }
  }
}