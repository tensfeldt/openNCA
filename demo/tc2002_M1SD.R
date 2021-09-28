# tc2002_M1SD
### openNCA computation engine Model M1 Single Dose Demonstation Example  

# Setup -------------------------------------------------------------------
library(readr)
library(tidyverse)
library(openNCA)

# Prepare Data for test execution
rm(list=ls())

params <- c()
params["testcase.id"] = "tc2002"
params["tcpath"] = "./testcases/DEMO1001_M1SD"
params["INPUT1"] = "DEMO1001-DT.csv"
params["map"] = "DEMO1001-MCT.csv"
params["flags"] = "DEMO1001-KEL.csv"

### Read Input Concentration data  
conc <- read.csv(file=file.path(params["tcpath"], params["INPUT1"]), header=TRUE, sep=",", as.is=TRUE)

### Read mct  
mct <- read_csv(file.path(params["tcpath"], params["map"]))
mct <- as.data.frame(mct)

### Read flags  
flags <- read_csv(file.path(params["tcpath"], params["flags"]))
flags <- as.data.frame(flags)

### Merge Concentration and Flags Datasets
conc <- conc %>% left_join(flags, by="PKDATAROWID")

### Remove records with missing concentrations (PKCNCN) or times (PKATPD)
conc <- conc %>% filter(!is.na(mct$CONC) & !is.na(mct$TIME)) 

### Compute Parameters for a single selected profile
sdeid <- conc %>% select(SDEID) %>% slice(1)

d <- conc %>% filter(SDEID %in% sdeid) %>% select(PKDATAROWID, SDEID, SUBJID, mct$DOSE, mct$DOSEU, mct$NOMTIME, mct$ACTTIME, mct$ACTTIMEU, mct$CONC, mct$CONCU, FLGEXKEL)

### Compute AUCLAST
openNCA::auc_last(conc=d$PKCNCN, time=d$PKATPD)

### Compute CMAX
openNCA::cmax(conc=d$PKCNCN, time=d$PKATPD)

### Compute TMAX
openNCA::tmax(conc=d$PKCNCN, time=d$PKATPD)

### Compute THALF
(kelresults <- openNCA::kel(conc=d$PKCNCN[d$FLGEXKEL==0], time=d$PKATPD[d$FLGEXKEL==0]))

### Compute AUCINFP
(aucinfp <- openNCA::auc_inf_p(conc=d$PKCNCN, time=d$PKATPD, kelflag=d$FLGEXKEL))

### Compute CLFP (note not automatically scaled to outputunit)
openNCA::clfp(aucinfp=aucinfp, dose=unique(d$DOSE))

### Compute VZFP (note not automatically scaled to outputunit)
openNCA::vzfp(kel=kelresults["KEL"], aucinfp=aucinfp, dose=unique(d$DOSE))

### Compute all pk parameter results
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
r
