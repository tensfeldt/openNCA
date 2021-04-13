### tc2008_M4SD
### openNCA computation engine Model M3 Single Dose Demonstation Example  

### Setup -------------------------------------------------------------------
library(readr)
library(tidyverse)
library(openNCA)

setwd("./demo")

### Prepare Data for test execution
rm(list=ls())

params <- c()
params["testcase.id"] = "tc2008"
params["tcpath"] = "./testcases/DEMO1008_M4SS"
params["INPUT1"] = "DEMO1008-DT.csv"
params["map"] = "DEMO1008-MCT.csv"
params["flags"] = "DEMO1008-KEL.csv"

### Read Input Concentration data  
conc <- read.csv(file=file.path(params["tcpath"], params["INPUT1"]), header=TRUE, sep=",", as.is=TRUE)

### Read mct  
mct <- read_csv(file.path(params["tcpath"], params["map"]))
mct <- as.data.frame(mct)

### Read flags  
flags <- read_csv(file.path(params["tcpath"], params["flags"]))
flags <- as.data.frame(flags)

### Execute openNCA::run_computation
if(exists("results_list")) { rm(results_list) }
results_list <- run_computation(data=conc, map=mct, flag=flags, parameterset="PARAMETERLIST")
w <- warnings()
print(w)
r <- results_list$data_out
e <- results_list$est_data
f <- results_list$flag_data

### Remove columns that are completely empty  
any_na <- function(x) any(!is.na(x))
r <- r %>% select_if(any_na) %>% head()

### Display Parameters
r %>% head(.)

### Display Estimated Concentration Dataset
e %>% head(.)

### Display Flags Dataset  
f %>% head(.)

### Create Test Results Output Files
if(exists("results_list")) {
  write.csv(r, file=paste(params["testcase.id"], "_", mct$MODEL, mct$DOSINGTYPE, "_", mct$TIME, "_", mct$AUCMETHOD, "_", params["commit"], "_parameters.csv", sep=""), row.names=FALSE)
  write.csv(e, file=paste(params["testcase.id"], "_", mct$MODEL, mct$DOSINGTYPE, "_", mct$TIME, "_", mct$AUCMETHOD, "_", params["commit"], "_estconc.csv", sep=""), row.names=FALSE)
  write.csv(f, file=paste(params["testcase.id"], "_", mct$MODEL, mct$DOSINGTYPE, "_", mct$TIME, "_", mct$AUCMETHOD, "_", params["commit"], "_flags.csv", sep=""), row.names=FALSE)
}
