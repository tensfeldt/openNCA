# Update analysis info to MCT files for regression reference test cases
library(tidyverse)
library(gdata)
library(ggplot2)
library(readr)
library(jsonlite)

# Standard MCT file
f <- 'C:/Users/tensfeldt/Documents/eNCA & EP2/~eNCA/eNCA Computation Engine/EQuIP/MCT_Defintions.xlsx'
std_mct <- read.xls(xls=f, sheet=1)
std_mct$PARAMETER <- casefold(std_mct$PARAMETER,upper=TRUE)
head(std_mct)

# TC031-4 - tc031_M2_SD_IV_CTerminal_AN2_MCT_original.csv
# 
fp   <- 'C:/Users/tensfeldt/Documents/eNCA & EP2/~eNCA/eNCA Computation Engine/EQuIP/testcases/tc031-M2SD'
setwd(fp)
mctf <- 'tc031_M2_SD_IV_CTerminal_AN2_MCT_original.csv'
mctf <- file.path(fp, mctf)
mct  <- read.csv(file=mctf, header=TRUE, as.is=TRUE)
head(mct)

# Define missing entries from the TC031 mct
(x <- setdiff(std_mct$PARAMETER, names(mct)))

# Create missing entries dataframe
missing.entries <- std_mct[match(x, std_mct$PARAMETER),c("PARAMETER", "VALUE")]

# Add into TC031 mct
for(i in 1:nrow(missing.entries)) {
  cat("i: ", i, " Parameter name = ", missing.entries$PARAMETER[i], " Parameter value = ", missing.entries$VALUE[i], "\n")
  mct$var <- as.character(missing.entries$VALUE[i])
  names(mct)[match("var", names(mct))] <- as.character(missing.entries$PARAMETER[i])
}

# remove DOSE, DOSEU to DOSEi and DOSEiU
mct <- mct[,-match("DOSE", names(mct))]
mct <- mct[,-match("DOSEU", names(mct))]

# remove TAU, TAUU to TAUi and TAUiU
mct <- mct[,-match("TAU", names(mct))]
mct <- mct[,-match("TAUU", names(mct))]

# Rename Partial Area MCT entries
#mct$AUCNPAIR <- 4
#names(mct)[match("AUC.I.T1", names(mct))] <- "AUC.1.T1" 
#names(mct)[match("AUC.I.T2", names(mct))] <- "AUC.1.T2" 
#names(mct)[match("AUC.I+1.T1", names(mct))] <- "AUC.2.T1" 
#names(mct)[match("AUC.I+1.T2", names(mct))] <- "AUC.2.T2" 
# Remove
mct <- mct[,-match("AUC.I.T1", names(mct))]
mct <- mct[,-match("AUC.I.T2", names(mct))]
mct <- mct[,-match("AUC.I+1.T1", names(mct))]
mct <- mct[,-match("AUC.I+1.T2", names(mct))]

# Remove AUC.AUCNPAIR.T1 and AUC.AUCNPAIR.T2 entries
mct <- mct[,-match("AUC.AUCNPAIR.T1", names(mct))]
mct <- mct[,-match("AUC.AUCNPAIR.T2", names(mct))]

# RETURNCOLS
mct$RETURNCOLS <- "UDSDEID,SDEID,STUDY,SITEID,SUBJID,RAND,TREATXT,TRTCD,PKCOLL,PKBDFLD,PKTERM,PERIODU,PERIOD,VISITU,VISIT,PHASE,PCMETHOD,HT,WT,AGEDERU,AGEDER,WTUNI,WTRAW,HTUNI,HTRAW,RACEOTH,RACES,SEX,RACIALD,ETHNIC,DOSEi,DOSEiU,DRGDATE,DOSETIM,ACTTRT,ACTTRTC,ACTTRTS,TREATSEQ,FLGEMESIS,FU,UDFNAME1,UDFVALUE1,UDFUNIT1,UDFNAMEn,UDFVALUEn,UDFUNITn,DATASTATUS,PKPCOM,PKSIGFIG/SIGFIG,PKDECPL/DECPL,DATABLINDSTATUS,PCLLOQ,PCSTRESU,PCNAM,ROUTE"

# Parameter List for M2SD
mct$PARAMETERLIST <- as.character(mct$PARAMETERLIST)
mct$PARAMETERLIST <- "AUCALL,AUCDN,AUCINFO,AUCINFOC,AUCINFODN,AUCINFP,AUCINFPC,AUCINFPDN,AUCLASTi,AUCLASTiDN,AUCLASTCi,AUCT,AUCT1_T2,AUCXBpctO,AUCXBpctP,AUCXPCTO,AUCXPCTP,AUMCINFO,AUMCINFP,AUMCLAST,AUMCXPTO,AUMCXPTP,C0,CLASTi,CLP,CLOW,CLPW,CLR,CMAXi,CMAXCi,CMAXiDN,DOSEC,DIi,DOSEi,F,FREL,FRELLAST,KEL,KELNOPT,KELRSQ,KELRSQA,KELTMHI,KELTMLO,LASTTIME,MRAUCINF,MRAUCLAST,MRCMAXi,MRTLAST,MRTIVIFOi,MRTIVIFPi,TAU,TAUi,THALF,THALFF,TLAST,TMAXi,TOLD,V0,VSSPi,VSSO,VZO,VZOW,VZP,VZPW"

# Parameter Display List for M2SD
mct$PARAMETERDISPLAYLIST <- as.character(mct$PARAMETERDISPLAYLIST)
mct$PARAMETERDISPLAYLIST <- "AUCINFP,AUCLASTi,AUCLASTiDN,AUCLASTCi,AUCTAUi,AUCXPCTP,AURCINFP,AURCLAST,AURCXPCTP,CMAXiDN,KEL,KELRSQ,MAXRATEi,THALF,THALFF,TLAST,TMAXi,TMAXRATEi"

# Reorder names of MCT
#names(mct) <- names(mct)[order(std_mct$PARAMETER)]

# Remove DP. and SF. entries that don't match PARAMETERLIST
y <- grep("SF.|DP.", names(mct), ignore.case=TRUE, perl=TRUE)
y <- names(mct)[y]
y <- gsub("SF.|DP.", "", y)
rmlist <- c()
for(i in y) {
  k <- grep(i, mct$PARAMETERLIST, ignore.case=TRUE, perl=TRUE)
  if(length(k)==0) { rmlist <- c(rmlist, i) }
}
rmlist <- rmlist[!duplicated(rmlist)]
rmlist <- rmlist[-match("DEFAULT", rmlist)]
n <- names(mct)
rmnames <- c()
for(i in rmlist) {
  g <- grep(i, n, ignore.case=TRUE, perl=TRUE)
  print(g)
  if(length(g)>0) {
    print(names(mct)[g])
    rmnames <- c(rmnames, match(names(mct)[g], names(mct)))
  }
}
mct <- mct[,-rmnames]

setdiff(std_mct$PARAMETER, names(mct))
setdiff(names(mct), std_mct$PARAMETER)

# Create new TC031 MCT file
new_mctf <- file.path(fp, 'tc031_M2_SD_IV_CTerminal_AN2_MCT.csv')
write.csv(mct, file=new_mctf, row.names = FALSE, na="")
jsonf <- file.path(fp, 'tc031_M2_SD_IV_CTerminal_AN2_mct.json')
write_json(mct, path=jsonf)

# Plot data
getwd()
fp   <- 'C:/Users/tensfeldt/Documents/eNCA & EP2/~eNCA/eNCA Computation Engine/EQuIP/testcases/tc031'
fc    <- 'tc031_M2_SD_IV_CTerminal_AN2_INPUT.csv'
fc <- file.path(fp, fc)
d <- read.csv(file=fc, as.is=TRUE)
d$PKCNCN <- as.numeric(d$PKCNCN)
unique(d$SUBJID)
d[d$SUBJID=="10051004", c("PERIOD", "VISIT", "PKPTMS", "PKCNC")]
p <- ggplot(data=d, aes(x=PKPTMS, y=PKCNCN, group=SUBJID)) +  geom_point(shape=1) + geom_line() 
p <- p + facet_grid(. ~ SUBJID)
p <- p + xlim(0,60)
print(p)

# -----------------------------------------------------------------
# TC031 - tc031_M2_SD_IV_CTerminal_MCT_original.csv
fp   <- 'C:/Users/tensfeldt/Documents/eNCA & EP2/~eNCA/eNCA Computation Engine/EQuIP/testcases/tc031-M2SD'
mctf <- 'tc031_M2_SD_IV_CTerminal_MCT_original.csv'
mctf <- file.path(fp, mctf)
mct  <- read.csv(file=mctf, header=TRUE, as.is=TRUE)
head(mct)

# Define missing entries from the TC031 mct
(x <- setdiff(std_mct$PARAMETER, names(mct)))

# Create missing entries dataframe
missing.entries <- std_mct[match(x, std_mct$PARAMETER),c("PARAMETER", "VALUE")]

# Add into TC031 mct
for(i in 1:nrow(missing.entries)) {
  cat("i: ", i, " Parameter name = ", missing.entries$PARAMETER[i], " Parameter value = ", missing.entries$VALUE[i], "\n")
  mct$var <- as.character(missing.entries$VALUE[i])
  names(mct)[match("var", names(mct))] <- as.character(missing.entries$PARAMETER[i])
}

# remove DOSE, DOSEU to DOSEi and DOSEiU
mct <- mct[,-match("DOSE", names(mct))]
mct <- mct[,-match("DOSEU", names(mct))]

# remove TAU, TAUU to TAUi and TAUiU
mct <- mct[,-match("TAU", names(mct))]
mct <- mct[,-match("TAUU", names(mct))]

# Rename Partial Area MCT entries
#mct$AUCNPAIR <- 4
#names(mct)[match("AUC.I.T1", names(mct))] <- "AUC.1.T1" 
#names(mct)[match("AUC.I.T2", names(mct))] <- "AUC.1.T2" 
#names(mct)[match("AUC.I+1.T1", names(mct))] <- "AUC.2.T1" 
#names(mct)[match("AUC.I+1.T2", names(mct))] <- "AUC.2.T2" 
# Remove
mct <- mct[,-match("AUC.I.T1", names(mct))]
mct <- mct[,-match("AUC.I.T2", names(mct))]
mct <- mct[,-match("AUC.I+1.T1", names(mct))]
mct <- mct[,-match("AUC.I+1.T2", names(mct))]

# Remove AUC.AUCNPAIR.T1 and AUC.AUCNPAIR.T2 entries
mct <- mct[,-match("AUC.AUCNPAIR.T1", names(mct))]
mct <- mct[,-match("AUC.AUCNPAIR.T2", names(mct))]

# RETURNCOLS
mct$RETURNCOLS <- "UDSDEID,SDEID,STUDY,SITEID,SUBJID,RAND,TREATXT,TRTCD,PKCOLL,PKBDFLD,PKTERM,PERIODU,PERIOD,VISITU,VISIT,PHASE,PCMETHOD,HT,WT,AGEDERU,AGEDER,WTUNI,WTRAW,HTUNI,HTRAW,RACEOTH,RACES,SEX,RACIALD,ETHNIC,DOSEi,DOSEiU,DRGDATE,DOSETIM,ACTTRT,ACTTRTC,ACTTRTS,TREATSEQ,FLGEMESIS,FU,UDFNAME1,UDFVALUE1,UDFUNIT1,UDFNAMEn,UDFVALUEn,UDFUNITn,DATASTATUS,PKPCOM,PKSIGFIG/SIGFIG,PKDECPL/DECPL,DATABLINDSTATUS,PCLLOQ,PCSTRESU,PCNAM,ROUTE"

# Parameter List for M2SD
mct$PARAMETERLIST <- as.character(mct$PARAMETERLIST)
mct$PARAMETERLIST <- "AUCALL,AUCDN,AUCINFO,AUCINFOC,AUCINFODN,AUCINFP,AUCINFPC,AUCINFPDN,AUCLASTi,AUCLASTiDN,AUCLASTCi,AUCT,AUCT1_T2,AUCXBpctO,AUCXBpctP,AUCXPCTO,AUCXPCTP,AUMCINFO,AUMCINFP,AUMCLAST,AUMCXPTO,AUMCXPTP,C0,CLASTi,CLP,CLOW,CLPW,CLR,CMAXi,CMAXCi,CMAXiDN,DOSEC,DIi,DOSEi,F,FREL,FRELLAST,KEL,KELNOPT,KELRSQ,KELRSQA,KELTMHI,KELTMLO,LASTTIME,MRAUCINF,MRAUCLAST,MRCMAXi,MRTLAST,MRTIVIFOi,MRTIVIFPi,TAU,TAUi,THALF,THALFF,TLAST,TMAXi,TOLD,V0,VSSPi,VSSO,VZO,VZOW,VZP,VZPW"

# Parameter Display List for M2SD
mct$PARAMETERDISPLAYLIST <- as.character(mct$PARAMETERDISPLAYLIST)
mct$PARAMETERDISPLAYLIST <- "AUCINFP,AUCLASTi,AUCLASTiDN,AUCLASTCi,AUCTAUi,AUCXPCTP,AURCINFP,AURCLAST,AURCXPCTP,CMAXiDN,KEL,KELRSQ,MAXRATEi,THALF,THALFF,TLAST,TMAXi,TMAXRATEi"

# Reorder names of MCT
#names(mct) <- names(mct)[order(std_mct$PARAMETER)]

# Remove DP. and SF. entries that don't match PARAMETERLIST
y <- grep("SF.|DP.", names(mct), ignore.case=TRUE, perl=TRUE)
y <- names(mct)[y]
y <- gsub("SF.|DP.", "", y)
rmlist <- c()
for(i in y) {
  k <- grep(i, mct$PARAMETERLIST, ignore.case=TRUE, perl=TRUE)
  if(length(k)==0) { rmlist <- c(rmlist, i) }
}
rmlist <- rmlist[!duplicated(rmlist)]
rmlist <- rmlist[-match("DEFAULT", rmlist)]
n <- names(mct)
rmnames <- c()
for(i in rmlist) {
  g <- grep(i, n, ignore.case=TRUE, perl=TRUE)
  print(g)
  if(length(g)>0) {
    print(names(mct)[g])
    rmnames <- c(rmnames, match(names(mct)[g], names(mct)))
  }
}
mct <- mct[,-rmnames]

setdiff(std_mct$PARAMETER, names(mct))
setdiff(names(mct), std_mct$PARAMETER)

# Create new TC031 MCT file
new_mctf <- file.path(fp, 'tc031_M2_SD_IV_CTerminal_MCT.csv')
write.csv(mct, file=new_mctf, row.names = FALSE, na="")
jsonf <- file.path(fp, 'tc031_M2_SD_IV_CTerminal_mct.json')
write_json(mct, path=jsonf)

# -----------------------------------------------------------------
# TC031 - tc031_M2_SD_IV_NTerminal_AN2_MCT_original.csv
fp   <- 'C:/Users/tensfeldt/Documents/eNCA & EP2/~eNCA/eNCA Computation Engine/EQuIP/testcases/tc031-M2SD'
mctf <- 'tc031_M2_SD_IV_NTerminal_AN2_MCT_original.csv'
mctf <- file.path(fp, mctf)
mct  <- read.csv(file=mctf, header=TRUE, as.is=TRUE)
head(mct)

# Define missing entries from the TC031 mct
(x <- setdiff(std_mct$PARAMETER, names(mct)))

# Create missing entries dataframe
missing.entries <- std_mct[match(x, std_mct$PARAMETER),c("PARAMETER", "VALUE")]

# Add into TC031 mct
for(i in 1:nrow(missing.entries)) {
  cat("i: ", i, " Parameter name = ", missing.entries$PARAMETER[i], " Parameter value = ", missing.entries$VALUE[i], "\n")
  mct$var <- as.character(missing.entries$VALUE[i])
  names(mct)[match("var", names(mct))] <- as.character(missing.entries$PARAMETER[i])
}

# remove DOSE, DOSEU to DOSEi and DOSEiU
mct <- mct[,-match("DOSE", names(mct))]
mct <- mct[,-match("DOSEU", names(mct))]

# remove TAU, TAUU to TAUi and TAUiU
mct <- mct[,-match("TAU", names(mct))]
mct <- mct[,-match("TAUU", names(mct))]

# Rename Partial Area MCT entries
#mct$AUCNPAIR <- 4
#names(mct)[match("AUC.I.T1", names(mct))] <- "AUC.1.T1" 
#names(mct)[match("AUC.I.T2", names(mct))] <- "AUC.1.T2" 
#names(mct)[match("AUC.I+1.T1", names(mct))] <- "AUC.2.T1" 
#names(mct)[match("AUC.I+1.T2", names(mct))] <- "AUC.2.T2" 
mct[,names(mct)[grep("AUC.[0-9]", names(mct))]]

# Remove AUC.AUCNPAIR.T1 and AUC.AUCNPAIR.T2 entries
mct <- mct[,-match("AUC.AUCNPAIR.T1", names(mct))]
mct <- mct[,-match("AUC.AUCNPAIR.T2", names(mct))]

# RETURNCOLS
mct$RETURNCOLS <- "UDSDEID,SDEID,STUDY,SITEID,SUBJID,RAND,TREATXT,TRTCD,PKCOLL,PKBDFLD,PKTERM,PERIODU,PERIOD,VISITU,VISIT,PHASE,PCMETHOD,HT,WT,AGEDERU,AGEDER,WTUNI,WTRAW,HTUNI,HTRAW,RACEOTH,RACES,SEX,RACIALD,ETHNIC,DOSEi,DOSEiU,DRGDATE,DOSETIM,ACTTRT,ACTTRTC,ACTTRTS,TREATSEQ,FLGEMESIS,FU,UDFNAME1,UDFVALUE1,UDFUNIT1,UDFNAMEn,UDFVALUEn,UDFUNITn,DATASTATUS,PKPCOM,PKSIGFIG/SIGFIG,PKDECPL/DECPL,DATABLINDSTATUS,PCLLOQ,PCSTRESU,PCNAM,ROUTE"

# Parameter List for M2SD
mct$PARAMETERLIST <- as.character(mct$PARAMETERLIST)
mct$PARAMETERLIST <- "AUCALL,AUCDN,AUCINFO,AUCINFOC,AUCINFODN,AUCINFP,AUCINFPC,AUCINFPDN,AUCLASTi,AUCLASTiDN,AUCLASTCi,AUCT,AUCT1_T2,AUCXBpctO,AUCXBpctP,AUCXPCTO,AUCXPCTP,AUMCINFO,AUMCINFP,AUMCLAST,AUMCXPTO,AUMCXPTP,C0,CLASTi,CLP,CLOW,CLPW,CLR,CMAXi,CMAXCi,CMAXiDN,DOSEC,DIi,DOSEi,F,FREL,FRELLAST,KEL,KELNOPT,KELRSQ,KELRSQA,KELTMHI,KELTMLO,LASTTIME,MRAUCINF,MRAUCLAST,MRCMAXi,MRTLAST,MRTIVIFOi,MRTIVIFPi,TAU,TAUi,THALF,THALFF,TLAST,TMAXi,TOLD,V0,VSSPi,VSSO,VZO,VZOW,VZP,VZPW"

# Parameter Display List for M2SD
mct$PARAMETERDISPLAYLIST <- as.character(mct$PARAMETERDISPLAYLIST)
mct$PARAMETERDISPLAYLIST <- "AUCINFP,AUCLASTi,AUCLASTiDN,AUCLASTCi,AUCTAUi,AUCXPCTP,AURCINFP,AURCLAST,AURCXPCTP,CMAXiDN,KEL,KELRSQ,MAXRATEi,THALF,THALFF,TLAST,TMAXi,TMAXRATEi"

# Reorder names of MCT
#names(mct) <- names(mct)[order(std_mct$PARAMETER)]

# Remove DP. and SF. entries that don't match PARAMETERLIST
y <- grep("SF.|DP.", names(mct), ignore.case=TRUE, perl=TRUE)
y <- names(mct)[y]
y <- gsub("SF.|DP.", "", y)
rmlist <- c()
for(i in y) {
  k <- grep(i, mct$PARAMETERLIST, ignore.case=TRUE, perl=TRUE)
  if(length(k)==0) { rmlist <- c(rmlist, i) }
}
rmlist <- rmlist[!duplicated(rmlist)]
rmlist <- rmlist[-match("DEFAULT", rmlist)]
n <- names(mct)
rmnames <- c()
for(i in rmlist) {
  g <- grep(i, n, ignore.case=TRUE, perl=TRUE)
  print(g)
  if(length(g)>0) {
    print(names(mct)[g])
    rmnames <- c(rmnames, match(names(mct)[g], names(mct)))
  }
}
mct <- mct[,-rmnames]

setdiff(std_mct$PARAMETER, names(mct))
setdiff(names(mct), std_mct$PARAMETER)

# Create new TC031 MCT file
new_mctf <- file.path(fp, 'tc031_M2_SD_IV_NTerminal_AN2_MCT.csv')
write.csv(mct, file=new_mctf, row.names = FALSE, na="")
jsonf <- file.path(fp, 'tc031_M2_SD_IV_NTerminal_AN2_mct.json')
write_json(mct, path=jsonf)

# -----------------------------------------------------------------
# TC031 - tc031_M2_SD_IV_NTerminal_MCT_original.csv
fp   <- 'C:/Users/tensfeldt/Documents/eNCA & EP2/~eNCA/eNCA Computation Engine/EQuIP/testcases/tc031-M2SD'
mctf <- 'tc031_M2_SD_IV_NTerminal_MCT_original.csv'
mctf <- file.path(fp, mctf)
mct  <- read.csv(file=mctf, header=TRUE, as.is=TRUE)
head(mct)

# Define missing entries from the TC031 mct
(x <- setdiff(std_mct$PARAMETER, names(mct)))

# Create missing entries dataframe
missing.entries <- std_mct[match(x, std_mct$PARAMETER),c("PARAMETER", "VALUE")]

# Add into TC031 mct
for(i in 1:nrow(missing.entries)) {
  cat("i: ", i, " Parameter name = ", missing.entries$PARAMETER[i], " Parameter value = ", missing.entries$VALUE[i], "\n")
  mct$var <- as.character(missing.entries$VALUE[i])
  names(mct)[match("var", names(mct))] <- as.character(missing.entries$PARAMETER[i])
}

# remove DOSE, DOSEU to DOSEi and DOSEiU
mct <- mct[,-match("DOSE", names(mct))]
mct <- mct[,-match("DOSEU", names(mct))]

# remove TAU, TAUU to TAUi and TAUiU
mct <- mct[,-match("TAU", names(mct))]
mct <- mct[,-match("TAUU", names(mct))]

# Rename Partial Area MCT entries
#mct$AUCNPAIR <- 4
#names(mct)[match("AUC.I.T1", names(mct))] <- "AUC.1.T1" 
#names(mct)[match("AUC.I.T2", names(mct))] <- "AUC.1.T2" 
#names(mct)[match("AUC.I+1.T1", names(mct))] <- "AUC.2.T1" 
#names(mct)[match("AUC.I+1.T2", names(mct))] <- "AUC.2.T2" 
mct[,names(mct)[grep("AUC.[0-9]", names(mct))]]
mct[,names(mct)[grep("NPAIR", names(mct))]]

# Remove AUC.AUCNPAIR.T1 and AUC.AUCNPAIR.T2 entries
mct <- mct[,-match("AUC.AUCNPAIR.T1", names(mct))]
mct <- mct[,-match("AUC.AUCNPAIR.T2", names(mct))]

# RETURNCOLS
mct$RETURNCOLS <- "UDSDEID,SDEID,STUDY,SITEID,SUBJID,RAND,TREATXT,TRTCD,PKCOLL,PKBDFLD,PKTERM,PERIODU,PERIOD,VISITU,VISIT,PHASE,PCMETHOD,HT,WT,AGEDERU,AGEDER,WTUNI,WTRAW,HTUNI,HTRAW,RACEOTH,RACES,SEX,RACIALD,ETHNIC,DOSEi,DOSEiU,DRGDATE,DOSETIM,ACTTRT,ACTTRTC,ACTTRTS,TREATSEQ,FLGEMESIS,FU,UDFNAME1,UDFVALUE1,UDFUNIT1,UDFNAMEn,UDFVALUEn,UDFUNITn,DATASTATUS,PKPCOM,PKSIGFIG/SIGFIG,PKDECPL/DECPL,DATABLINDSTATUS,PCLLOQ,PCSTRESU,PCNAM,ROUTE"

# Parameter List for M2SD
mct$PARAMETERLIST <- as.character(mct$PARAMETERLIST)
mct$PARAMETERLIST <- "AUCALL,AUCDN,AUCINFO,AUCINFOC,AUCINFODN,AUCINFP,AUCINFPC,AUCINFPDN,AUCLASTi,AUCLASTiDN,AUCLASTCi,AUCT,AUCT1_T2,AUCXBpctO,AUCXBpctP,AUCXPCTO,AUCXPCTP,AUMCINFO,AUMCINFP,AUMCLAST,AUMCXPTO,AUMCXPTP,C0,CLASTi,CLP,CLOW,CLPW,CLR,CMAXi,CMAXCi,CMAXiDN,DOSEC,DIi,DOSEi,F,FREL,FRELLAST,KEL,KELNOPT,KELRSQ,KELRSQA,KELTMHI,KELTMLO,LASTTIME,MRAUCINF,MRAUCLAST,MRCMAXi,MRTLAST,MRTIVIFOi,MRTIVIFPi,TAU,TAUi,THALF,THALFF,TLAST,TMAXi,TOLD,V0,VSSPi,VSSO,VZO,VZOW,VZP,VZPW"

# Parameter Display List for M2SD
mct$PARAMETERDISPLAYLIST <- as.character(mct$PARAMETERDISPLAYLIST)
mct$PARAMETERDISPLAYLIST <- "AUCINFP,AUCLASTi,AUCLASTiDN,AUCLASTCi,AUCTAUi,AUCXPCTP,AURCINFP,AURCLAST,AURCXPCTP,CMAXiDN,KEL,KELRSQ,MAXRATEi,THALF,THALFF,TLAST,TMAXi,TMAXRATEi"

# Reorder names of MCT
#names(mct) <- names(mct)[order(std_mct$PARAMETER)]

# Remove DP. and SF. entries that don't match PARAMETERLIST
y <- grep("SF.|DP.", names(mct), ignore.case=TRUE, perl=TRUE)
y <- names(mct)[y]
y <- gsub("SF.|DP.", "", y)
rmlist <- c()
for(i in y) {
  k <- grep(i, mct$PARAMETERLIST, ignore.case=TRUE, perl=TRUE)
  if(length(k)==0) { rmlist <- c(rmlist, i) }
}
rmlist <- rmlist[!duplicated(rmlist)]
rmlist <- rmlist[-match("DEFAULT", rmlist)]
n <- names(mct)
rmnames <- c()
for(i in rmlist) {
  g <- grep(i, n, ignore.case=TRUE, perl=TRUE)
  print(g)
  if(length(g)>0) {
    print(names(mct)[g])
    rmnames <- c(rmnames, match(names(mct)[g], names(mct)))
  }
}
mct <- mct[,-rmnames]

setdiff(std_mct$PARAMETER, names(mct))
setdiff(names(mct), std_mct$PARAMETER)

# Create new TC031 MCT file
new_mctf <- file.path(fp, 'tc031_M2_SD_IV_NTerminal_MCT.csv')
write.csv(mct, file=new_mctf, row.names = FALSE, na="")
jsonf <- file.path(fp, 'tc031_M2_SD_IV_NTerminal_mct.json')
write_json(mct, path=jsonf)

# -----------------------------------------------------------------
# TC031 - tc031_M3_SD_IV_CTerminal_MCT_original.csv
fp   <- 'C:/Users/tensfeldt/Documents/eNCA & EP2/~eNCA/eNCA Computation Engine/EQuIP/testcases/tc031-M2SD'
mctf <- 'tc031_M3_SD_IV_CTerminal_MCT_original.csv'
mctf <- file.path(fp, mctf)
mct  <- read.csv(file=mctf, header=TRUE, as.is=TRUE)
head(mct)

# Define missing entries from the TC031 mct
(x <- setdiff(std_mct$PARAMETER, names(mct)))

# Create missing entries dataframe
missing.entries <- std_mct[match(x, std_mct$PARAMETER),c("PARAMETER", "VALUE")]

# Add into TC031 mct
for(i in 1:nrow(missing.entries)) {
  cat("i: ", i, " Parameter name = ", missing.entries$PARAMETER[i], " Parameter value = ", missing.entries$VALUE[i], "\n")
  mct$var <- as.character(missing.entries$VALUE[i])
  names(mct)[match("var", names(mct))] <- as.character(missing.entries$PARAMETER[i])
}

# remove DOSE, DOSEU to DOSEi and DOSEiU
mct <- mct[,-match("DOSE", names(mct))]
mct <- mct[,-match("DOSEU", names(mct))]

# remove TAU, TAUU to TAUi and TAUiU
mct <- mct[,-match("TAU", names(mct))]
mct <- mct[,-match("TAUU", names(mct))]

# Rename Partial Area MCT entries
#mct$AUCNPAIR <- 4
#names(mct)[match("AUC.I.T1", names(mct))] <- "AUC.1.T1" 
#names(mct)[match("AUC.I.T2", names(mct))] <- "AUC.1.T2" 
#names(mct)[match("AUC.I+1.T1", names(mct))] <- "AUC.2.T1" 
#names(mct)[match("AUC.I+1.T2", names(mct))] <- "AUC.2.T2" 
mct[,names(mct)[grep("AUC.[0-9]", names(mct))]]
mct[,names(mct)[grep("NPAIR", names(mct))]]

# Remove AUC.AUCNPAIR.T1 and AUC.AUCNPAIR.T2 entries
mct <- mct[,-match("AUC.AUCNPAIR.T1", names(mct))]
mct <- mct[,-match("AUC.AUCNPAIR.T2", names(mct))]

# RETURNCOLS
mct$RETURNCOLS <- "UDSDEID,SDEID,STUDY,SITEID,SUBJID,RAND,TREATXT,TRTCD,PKCOLL,PKBDFLD,PKTERM,PERIODU,PERIOD,VISITU,VISIT,PHASE,PCMETHOD,HT,WT,AGEDERU,AGEDER,WTUNI,WTRAW,HTUNI,HTRAW,RACEOTH,RACES,SEX,RACIALD,ETHNIC,DOSEi,DOSEiU,DRGDATE,DOSETIM,ACTTRT,ACTTRTC,ACTTRTS,TREATSEQ,FLGEMESIS,FU,UDFNAME1,UDFVALUE1,UDFUNIT1,UDFNAMEn,UDFVALUEn,UDFUNITn,DATASTATUS,PKPCOM,PKSIGFIG/SIGFIG,PKDECPL/DECPL,DATABLINDSTATUS,PCLLOQ,PCSTRESU,PCNAM,ROUTE"

# Parameter List for M3SD
mct$PARAMETERLIST <- as.character(mct$PARAMETERLIST)
mct$PARAMETERLIST <- "AUCALL,AUCDN,AUCINFO,AUCINFOC,AUCINFODN,AUCINFP,AUCINFPC,AUCINFPDN,AUCLASTi,AUCLASTiDN,AUCLASTCi,AUCT,AUCT1_T2,AUCXPCTO,AUCXPCTP,AUMCINFO,AUMCINFP,AUMCLAST,AUMCXPTO,AUMCXPTP,CLASTi,CLP,CLOW,CLPW,CLR,CMAXi,CMAXCi,CMAXiDN,DOSEC,DIi,DOF,DOSEi,F,FREL,FRELLAST,KEL,KELNOPT,KELRSQ,KELRSQA,KELTMHI,KELTMLO,LASTTIME,MRAUCINF,MRAUCLAST,MRCMAXi,MRTLAST,MRTIVIFOi,MRTIVIFPi,TAU,TAUi,THALF,THALFF,TLAST,TMAXi,TOLD,VSSPi,VSSO,VZO,VZOW,VZP,VZPW"

# Parameter Display List for M3SD
mct$PARAMETERDISPLAYLIST <- as.character(mct$PARAMETERDISPLAYLIST)
mct$PARAMETERDISPLAYLIST <- "AUCINFP,AUCLASTi,AUCLASTiDN,AUCLASTCi,AUCTAUi,AUCXPCTP,AURCINFP,AURCLAST,AURCXPCTP,CMAXiDN,KEL,KELRSQ,MAXRATEi,THALF,THALFF,TLAST,TMAXi,TMAXRATEi"

# Reorder names of MCT
#names(mct) <- names(mct)[order(std_mct$PARAMETER)]

# Remove DP. and SF. entries that don't match PARAMETERLIST
y <- grep("SF.|DP.", names(mct), ignore.case=TRUE, perl=TRUE)
y <- names(mct)[y]
y <- gsub("SF.|DP.", "", y)
rmlist <- c()
for(i in y) {
  k <- grep(i, mct$PARAMETERLIST, ignore.case=TRUE, perl=TRUE)
  if(length(k)==0) { rmlist <- c(rmlist, i) }
}
rmlist <- rmlist[!duplicated(rmlist)]
rmlist <- rmlist[-match("DEFAULT", rmlist)]
n <- names(mct)
rmnames <- c()
for(i in rmlist) {
  g <- grep(i, n, ignore.case=TRUE, perl=TRUE)
  print(g)
  if(length(g)>0) {
    print(names(mct)[g])
    rmnames <- c(rmnames, match(names(mct)[g], names(mct)))
  }
}
mct <- mct[,-rmnames]

setdiff(std_mct$PARAMETER, names(mct))
setdiff(names(mct), std_mct$PARAMETER)

mct[,names(mct)[grep("DOSE", names(mct))]]
mct[,names(mct)[grep("TAU", names(mct))]]

# Create new TC031 MCT file
new_mctf <- file.path(fp, 'tc031_M3_SD_IV_CTerminal_MCT.csv')
write.csv(mct, file=new_mctf, row.names = FALSE, na="")
jsonf <- file.path(fp, 'tc031_M3_SD_IV_CTerminal_mct.json')
write_json(mct, path=jsonf)

# -----------------------------------------------------------------
# TC031 - tc031_M3_SD_IV_NTerminal_AN3_MCT_original.csv
fp   <- 'C:/Users/tensfeldt/Documents/eNCA & EP2/~eNCA/eNCA Computation Engine/EQuIP/testcases/tc031-M2SD'
mctf <- 'tc031_M3_SD_IV_NTerminal_AN3_MCT_original.csv'
mctf <- file.path(fp, mctf)
mct  <- read.csv(file=mctf, header=TRUE, as.is=TRUE)
head(mct)

# Define missing entries from the TC031 mct
(x <- setdiff(std_mct$PARAMETER, names(mct)))

# Create missing entries dataframe
missing.entries <- std_mct[match(x, std_mct$PARAMETER),c("PARAMETER", "VALUE")]

# Add into TC031 mct
for(i in 1:nrow(missing.entries)) {
  cat("i: ", i, " Parameter name = ", missing.entries$PARAMETER[i], " Parameter value = ", missing.entries$VALUE[i], "\n")
  mct$var <- as.character(missing.entries$VALUE[i])
  names(mct)[match("var", names(mct))] <- as.character(missing.entries$PARAMETER[i])
}

# remove DOSE, DOSEU to DOSEi and DOSEiU
mct <- mct[,-match("DOSE", names(mct))]
mct <- mct[,-match("DOSEU", names(mct))]

# remove TAU, TAUU to TAUi and TAUiU
mct <- mct[,-match("TAU", names(mct))]
mct <- mct[,-match("TAUU", names(mct))]

mct[,names(mct)[grep("DOSE", names(mct))]]
mct[,names(mct)[grep("TAU", names(mct))]]

# Rename Partial Area MCT entries
#mct$AUCNPAIR <- 4
#names(mct)[match("AUC.I.T1", names(mct))] <- "AUC.1.T1" 
#names(mct)[match("AUC.I.T2", names(mct))] <- "AUC.1.T2" 
#names(mct)[match("AUC.I+1.T1", names(mct))] <- "AUC.2.T1" 
#names(mct)[match("AUC.I+1.T2", names(mct))] <- "AUC.2.T2" 
mct[,names(mct)[grep("AUC.[0-9]", names(mct))]]
mct[,names(mct)[grep("NPAIR", names(mct))]]

# Remove AUC.AUCNPAIR.T1 and AUC.AUCNPAIR.T2 entries
mct <- mct[,-match("AUC.AUCNPAIR.T1", names(mct))]
mct <- mct[,-match("AUC.AUCNPAIR.T2", names(mct))]

# RETURNCOLS
mct$RETURNCOLS <- "UDSDEID,SDEID,STUDY,SITEID,SUBJID,RAND,TREATXT,TRTCD,PKCOLL,PKBDFLD,PKTERM,PERIODU,PERIOD,VISITU,VISIT,PHASE,PCMETHOD,HT,WT,AGEDERU,AGEDER,WTUNI,WTRAW,HTUNI,HTRAW,RACEOTH,RACES,SEX,RACIALD,ETHNIC,DOSEi,DOSEiU,DRGDATE,DOSETIM,ACTTRT,ACTTRTC,ACTTRTS,TREATSEQ,FLGEMESIS,FU,UDFNAME1,UDFVALUE1,UDFUNIT1,UDFNAMEn,UDFVALUEn,UDFUNITn,DATASTATUS,PKPCOM,PKSIGFIG/SIGFIG,PKDECPL/DECPL,DATABLINDSTATUS,PCLLOQ,PCSTRESU,PCNAM,ROUTE"

# Parameter List for M3SD
mct$PARAMETERLIST <- as.character(mct$PARAMETERLIST)
mct$PARAMETERLIST <- "AUCALL,AUCDN,AUCINFO,AUCINFOC,AUCINFODN,AUCINFP,AUCINFPC,AUCINFPDN,AUCLASTi,AUCLASTiDN,AUCLASTCi,AUCT,AUCT1_T2,AUCXPCTO,AUCXPCTP,AUMCINFO,AUMCINFP,AUMCLAST,AUMCXPTO,AUMCXPTP,CLASTi,CLP,CLOW,CLPW,CLR,CMAXi,CMAXCi,CMAXiDN,DOSEC,DIi,DOF,DOSEi,F,FREL,FRELLAST,KEL,KELNOPT,KELRSQ,KELRSQA,KELTMHI,KELTMLO,LASTTIME,MRAUCINF,MRAUCLAST,MRCMAXi,MRTLAST,MRTIVIFOi,MRTIVIFPi,TAU,TAUi,THALF,THALFF,TLAST,TMAXi,TOLD,VSSPi,VSSO,VZO,VZOW,VZP,VZPW"

# Parameter Display List for M3SD
mct$PARAMETERDISPLAYLIST <- as.character(mct$PARAMETERDISPLAYLIST)
mct$PARAMETERDISPLAYLIST <- "AUCINFP,AUCLASTi,AUCLASTiDN,AUCLASTCi,AUCTAUi,AUCXPCTP,AURCINFP,AURCLAST,AURCXPCTP,CMAXiDN,KEL,KELRSQ,MAXRATEi,THALF,THALFF,TLAST,TMAXi,TMAXRATEi"

# Reorder names of MCT
#names(mct) <- names(mct)[order(std_mct$PARAMETER)]

# Remove DP. and SF. entries that don't match PARAMETERLIST
y <- grep("SF.|DP.", names(mct), ignore.case=TRUE, perl=TRUE)
y <- names(mct)[y]
y <- gsub("SF.|DP.", "", y)
rmlist <- c()
for(i in y) {
  k <- grep(i, mct$PARAMETERLIST, ignore.case=TRUE, perl=TRUE)
  if(length(k)==0) { rmlist <- c(rmlist, i) }
}
rmlist <- rmlist[!duplicated(rmlist)]
rmlist <- rmlist[-match("DEFAULT", rmlist)]
n <- names(mct)
rmnames <- c()
for(i in rmlist) {
  g <- grep(i, n, ignore.case=TRUE, perl=TRUE)
  print(g)
  if(length(g)>0) {
    print(names(mct)[g])
    rmnames <- c(rmnames, match(names(mct)[g], names(mct)))
  }
}
mct <- mct[,-rmnames]

setdiff(std_mct$PARAMETER, names(mct))
setdiff(names(mct), std_mct$PARAMETER)

# Create new TC031 MCT file
new_mctf <- file.path(fp, 'tc031_M3_SD_IV_NTerminal_AN3_MCT.csv')
write.csv(mct, file=new_mctf, row.names = FALSE, na="")
# Create JSON equivalent
jsonf <- file.path(fp, 'tc031_M3_SD_IV_NTerminal_AN3_mct.json')
write_json(mct, path=jsonf)


# -----------------------------------------------------------------
# Update the DOSE variable in the input datasets from DOSE to DOSEi
fp <- 'C:/Users/tensfeldt/Documents/eNCA & EP2/~eNCA/eNCA Computation Engine/EQuIP/testcases/tc031-M2SD'
f  <- list.files(pattern="INPUT_original.csv")
for(i in f) {
  d  <- read.csv(file=i, header=TRUE, as.is=TRUE)
  names(d)[match("DOSE", names(d))] <- "DOSEi"
  ni <- gsub("INPUT_original", "INPUT", i, ignore.case=TRUE, perl=TRUE)
  write.csv(d, file=ni, row.names = FALSE)
}

# -----------------------------------------------------------------
# TC017 (M4SD) - regression - original: B1261002_parameter_data.in.csv
#                      updated:  tc017_mct.csv
# 
#fp   <- 'C:/Users/tensfeldt/Box Sync/EQuIP Computation Engine/testcases/Regression Tests from UAT-Mar2016 Identified/tc017'
fp   <- 'C:/Users/tensfeldt/Documents/eNCA & EP2/~eNCA/eNCA Computation Engine/EQuIP/testcases/regression/tst/UAT-Mar2016-EQuIP/reference/analysis/M4SD/B1261002/ppCalc/tc017'
setwd(fp)
list.files()
mctf <- 'B1261002_parameter_data.in.csv'
mctf <- file.path(fp, mctf)
mct  <- read.csv(file=mctf, header=TRUE, as.is=TRUE)
head(mct)

# Define missing entries from the TC017 mct
(x <- setdiff(std_mct$PARAMETER, names(mct)))

# Create missing entries dataframe
missing.entries <- std_mct[match(x, std_mct$PARAMETER),c("PARAMETER", "VALUE")]

# Add into TC017 mct
for(i in 1:nrow(missing.entries)) {
  cat("i: ", i, " Parameter name = ", missing.entries$PARAMETER[i], " Parameter value = ", missing.entries$VALUE[i], "\n")
  mct$var <- as.character(missing.entries$VALUE[i])
  names(mct)[match("var", names(mct))] <- as.character(missing.entries$PARAMETER[i])
}

# Update FLGACCEPTKELCRIT to include AUCXPCT<=0.2 (not necessary since incorporated into std_mct template now)
#mct$FLGACCEPTKELCRIT <- paste(mct$FLGACCEPTKELCRIT, "AUCXPCT<=0.2", sep=", ")

# Update NORMBS
mct$NORMBS <- "WT"

# remove NDOSEI
mct[,names(mct)[grep("NDOSE", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove NDOSEI
mct <- mct[,-match("NDOSEI", names(mct))]

# remove DOSE, DOSEU to DOSEi and DOSEiU
mct[,names(mct)[grep("DOSE", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove DOSE, DOSEU
mct <- mct[,-match("DOSE", names(mct))]
mct <- mct[,-match("DOSEU", names(mct))]
mct <- mct[,-match("DOSEI", names(mct))]
mct <- mct[,-match("DOSEIU", names(mct))]
# Update DOSE1, DOSE1U
mct$DOSE1 <- "DOSE"
mct$DOSE1U <- "DOSEUNI"

# remove TAU, TAUU, TAUi and TAUiU - SD (Single Dose)
mct[,names(mct)[grep("TAU", names(mct), ignore.case = TRUE, perl=TRUE)]]
# remove TAU, TAUU
y <- names(mct)[grep("^TAU", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]
# Remove DP|SF.*TAUI
y <- names(mct)[grep("^(DP|SF)[.]{1}(AE|AUC)TAU", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]

# remove TOLD
mct[,names(mct)[grep("^TOLD", names(mct), ignore.case = TRUE, perl=TRUE)]]
y <- names(mct)[grep("^TOLD", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]

# Rename Partial Area MCT entries
mct[,names(mct)[grep("AUCNPAIR|AUC[//.]", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove AUC.*.T* pairs
y <- names(mct)[grep("^AUC[.]{1}.+?[.]{1}T[0-9]+?", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]

# Rename MIDPOINT* and RATE* MCT entries
mct[,names(mct)[grep("MIDPOINT|RATEA|RATEN", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove AUC.*.T* pairs
y <- names(mct)[grep("MIDPOINT|RATEA|RATEN", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]

# RETURNCOLS
mct$RETURNCOLS <- "UDSDEID;SDEID;STUDY;SITEID;SUBJID;RAND;TREATXT;TRTCD;PKCOLL;PKBDFLD;PKTERM;PERIODU;PERIOD;VISITU;VISIT;PHASE;PCMETHOD;COLLDATE;HT;WT;AGEDERU;AGEDER;WTUNI;WTRAW;HTUNI;HTRAW;RACEOTH;RACES;SEX;RACIALD;ETHNIC;DOSEP;DOSPND;DOSEPUNI;DOSPTM;DOSE1;DOSE1U;ACTTRT;ACTTRTC;ACTTRTS;TREATSEQ;FLGEMESIS;FU;UDFNAME1;UDFVALUE1;UDFUNIT1;UDFNAMEn;UDFVALUEn;UDFUNITn;DATASTATUS;PKPCOM;PKSIGFIG;PKDECPL;DATABLINDSTATUS;PCLLOQ;PCSTRESU;PCNAM;ROUTE"

# Parameter List for M4SD - Note that these can be expressed in regular expression format 
mct$PARAMETERLIST <- as.character(mct$PARAMETERLIST)
mct$PARAMETERLIST <- "AE;AEPCT;AET;AETAUi;AETAUpti;AETPCT;AT;AET;AUCDN;AURCALL;AURCINFO;AURCINFP;AURCLAST;AURCT1_T2;AURCXPCTO;AURCXPCTP;CLR;DOSEC;DIi;DOSEi;FA;KEL;KELNOPT;KELRSQ;KELRSQA;KELTMHI;KELTMLO;MAXRATEi;MIDPTLASTi;RATELASTi;TAU;TAUi;THALF;THALFF;TLAG;TMAXRATEi;TOLD;VOLSUM;AE[//.]*?;AMT[//.]*?"

# Parameter Display List for M4SD - Note that these can be expressed in regular expression format 
mct$PARAMETERDISPLAYLIST <- as.character(mct$PARAMETERDISPLAYLIST)
mct$PARAMETERDISPLAYLIST <- "AUCINFP;AUCLASTi;AUCLASTiDN;AUCLASTCi;AUCTAUi;AUCXPCTP;AURCINFP;AURCLAST;AURCXPCTP;CMAXiDN;KEL;KELRSQ;KELNOPT;MAXRATEi;THALF;THALFF;TLAST;TMAXi;TMAXRATEi"

# Reorder names of MCT
#names(mct) <- names(mct)[order(std_mct$PARAMETER)]

# Remove DP. and SF. entries that don't match PARAMETERLIST
y <- grep("SF.|DP.", names(mct), ignore.case=TRUE, perl=TRUE)
y <- names(mct)[y]
y <- gsub("SF.|DP.", "", y)
rmlist <- c()
for(i in y) {
  k <- grep(i, mct$PARAMETERLIST, ignore.case=TRUE, perl=TRUE)
  if(length(k)==0) { rmlist <- c(rmlist, i) }
}
rmlist <- rmlist[!duplicated(rmlist)]
rmlist <- rmlist[-match("DEFAULT", rmlist)]
n <- names(mct)
rmnames <- c()
for(i in rmlist) {
  g <- grep(i, n, ignore.case=TRUE, perl=TRUE)
  print(g)
  if(length(g)>0) {
    print(names(mct)[g])
    rmnames <- c(rmnames, match(names(mct)[g], names(mct)))
  }
}
mct <- mct[,-rmnames]

setdiff(std_mct$PARAMETER, names(mct))
setdiff(names(mct), std_mct$PARAMETER)

# Create new TC017 MCT file
new_mctf <- file.path(fp, 'tc017_mct.csv')
write.csv(mct, file=new_mctf, row.names = FALSE, na="")
# Create JSON equivalent
jsonf <- file.path(fp, 'tc017_mct.json')
write_json(mct, path=jsonf)

# -----------------------------------------------------------------
# TC018 (M4SS) - regression - original: B1731007_parameter_data.in.csv
#                      updated:  tc018_mct.csv
# 
fp   <- 'C:/Users/tensfeldt/Documents/eNCA & EP2/~eNCA/eNCA Computation Engine/EQuIP/testcases/regression/tst/UAT-Mar2016-EQuIP/reference/analysis/M4SS/B1731007/ppCalc/tc018'
setwd(fp)
list.files()
mctf <- 'B1731007_parameter_data.in.csv'
mctf <- file.path(fp, mctf)
mct  <- read.csv(file=mctf, header=TRUE, as.is=TRUE)
head(mct)

concf <- "B1731007.in.csv"
concf <- file.path(fp, concf)
conc  <- read.csv(file=concf, header=TRUE, as.is=TRUE)
head(conc,2)

# Define missing entries from the TC018 mct
(x <- setdiff(std_mct$PARAMETER, names(mct)))

# Create missing entries dataframe
missing.entries <- std_mct[match(x, std_mct$PARAMETER),c("PARAMETER", "VALUE")]

# Add into TC018 mct
for(i in 1:nrow(missing.entries)) {
  cat("i: ", i, " Parameter name = ", missing.entries$PARAMETER[i], " Parameter value = ", missing.entries$VALUE[i], "\n")
  mct$var <- as.character(missing.entries$VALUE[i])
  names(mct)[match("var", names(mct))] <- as.character(missing.entries$PARAMETER[i])
}

# Update FLGACCEPTKELCRIT to include AUCXPCT<=0.2 (not necessary since incorporated into std_mct template now)
#mct$FLGACCEPTKELCRIT <- paste(mct$FLGACCEPTKELCRIT, "AUCXPCT<=0.2", sep=", ")

# Update NORMBS
mct$NORMBS <- "WT"

# remove NDOSEI
mct[,names(mct)[grep("NDOSE", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove NDOSEI
mct <- mct[,-match("NDOSEI", names(mct))]

# remove DOSE, DOSEU to DOSEi and DOSEiU
mct[,names(mct)[grep("DOSE", names(mct), ignore.case = TRUE, perl=TRUE)]]
# remove DOSEi and DOSEiU
mct <- mct[,-match("DOSEI", names(mct))]
mct <- mct[,-match("DOSEIU", names(mct))]
# rename DOSE, DOSEU to DOSE1, DOSE1U
names(mct)[match("DOSE", names(mct))] <- "DOSE1"
names(mct)[match("DOSEU", names(mct))] <- "DOSE1U"
# Set DOSE1 to "DOSE"
mct$DOSE1 <- "DOSE"
# Set DOSE1U to "DOSEUNI"
mct$DOSE1U <- "DOSEUNI"

# remove TAU, TAUU to TAUi and TAUiU
mct[,names(mct)[grep("TAU", names(mct), ignore.case = TRUE, perl=TRUE)]]
# remove TAUI, TAUIU
mct <- mct[,-match("TAUI", names(mct))]
mct <- mct[,-match("TAUIU", names(mct))]
# Rename TAU and TAUU to TAU1 and TAU1U
names(mct)[match("TAU", names(mct))] <- "TAU1"
names(mct)[match("TAUU", names(mct))] <- "TAU1U"

# TOLD
mct[,names(mct)[grep("TOLD", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Rename TOLD and TOLDU to TOLD1 and TOLD1U
names(mct)[match("TOLD", names(mct))] <- "TOLD1"
names(mct)[match("TOLDU", names(mct))] <- "TOLD1U"
# Remove TOLDI, TOLDIU
mct <- mct[,-match("TOLDI", names(mct))]
mct <- mct[,-match("TOLDIU", names(mct))]

# Rename Partial Area MCT entries
mct[,names(mct)[grep("AUCNPAIR|AUC[//.]", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove AUC.*.T* pairs
y <- names(mct)[grep("^AUC[.]{1}.+?[.]{1}T[0-9]+?", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]

# Rename MIDPOINT* and RATE* MCT entries
mct[,names(mct)[grep("MIDPOINT|RATEA|RATEN", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove AUC.*.T* pairs
y <- names(mct)[grep("MIDPOINT|RATEA|RATEN", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]

# RETURNCOLS
mct$RETURNCOLS <- "UDSDEID;SDEID;STUDY;SITEID;SUBJID;RAND;TREATXT;TRTCD;PKCOLL;PKBDFLD;PKTERM;PERIODU;PERIOD;VISITU;VISIT;PHASE;PCMETHOD;HT;WT;AGEDERU;AGEDER;WTUNI;WTRAW;HTUNI;HTRAW;RACEOTH;RACES;SEX;RACIALD;ETHNIC;DOSEP;DOSPND;DOSEPUNI;DOSPTM;DOSE1;DOSE1U;ACTTRT;ACTTRTC;ACTTRTS;TREATSEQ;FLGEMESIS;FU;UDFNAME1;UDFVALUE1;UDFUNIT1;UDFNAMEn;UDFVALUEn;UDFUNITn;DATASTATUS;PKPCOM;PKSIGFIG;PKDECPL;DATABLINDSTATUS;PCLLOQ;PCSTRESU;PCNAM,ROUTE;TAU1;TAU1U;TOLD1;TOLD1U"

# Parameter List for M4SS - Note that these can be expressed in regular expression format 
mct$PARAMETERLIST <- as.character(mct$PARAMETERLIST)
mct$PARAMETERLIST <- "AE;AEPCT;AET;AETAUi;AETAUpti;AETPCT;AT;AET;AUCDN;AURCALL;AURCINFO;AURCINFP;AURCLAST;AURCT1_T2;AURCXPCTO;AURCXPCTP;CLRTAUi;DOSEC;DIi;DOSEi;FA;KEL;KELNOPT;KELRSQ;KELRSQA;KELTMHI;KELTMLO;MAXRATEi;MIDPTLASTi;RATELASTi;TAU;TAUi;THALF;THALFF;TLAG;TMAXRATEi;TOLD;VOLSUM;AE[//.]*?;AMT[//.]*?"

# Parameter Display List for M4SS - Note that these can be expressed in regular expression format 
mct$PARAMETERDISPLAYLIST <- as.character(mct$PARAMETERDISPLAYLIST)
mct$PARAMETERDISPLAYLIST <- "AUCINFP;AUCLASTi;AUCLASTiDN;AUCLASTCi;AUCTAUi;AUCXPCTP;AURCINFP;AURCLAST;AURCXPCTP;CMAXiDN;KEL;KELRSQ;KELNOPT;MAXRATEi;THALF;THALFF;TLAST;TMAXi;TMAXRATEi"

# Remove DP. and SF. entries that don't match PARAMETERLIST
y <- grep("SF|DP[.]", names(mct), ignore.case=TRUE, perl=TRUE)
y <- names(mct)[y]
y <- gsub("SF.|DP.", "", y)
rmlist <- c()
for(i in y) {
  k <- grep(i, mct$PARAMETERLIST, ignore.case=TRUE, perl=TRUE)
  if(length(k)==0) { rmlist <- c(rmlist, i) }
}
rmlist <- rmlist[!duplicated(rmlist)]
rmlist <- rmlist[-match("DEFAULT", rmlist)]
n <- names(mct)
rmnames <- c()
for(i in rmlist) {
  g <- grep(i, n, ignore.case=TRUE, perl=TRUE)
  print(g)
  if(length(g)>0) {
    print(names(mct)[g])
    rmnames <- c(rmnames, match(names(mct)[g], names(mct)))
  }
}
mct <- mct[,-rmnames]

setdiff(std_mct$PARAMETER, names(mct))
setdiff(names(mct), std_mct$PARAMETER)

# Create new TC018 MCT file
new_mctf <- file.path(fp, 'tc018_mct.csv')
write.csv(mct, file=new_mctf, row.names = FALSE, na="")
# Create JSON equivalent
jsonf <- file.path(fp, 'tc018_mct.json')
write_json(mct, path=jsonf)

# -----------------------------------------------------------------
# TC015 (M3SD) - regression - original: B2901001_parameter_data.in.csv
#                      updated:  tc015_mct.csv
# 
fp   <- 'C:/Users/tensfeldt/Documents/eNCA & EP2/~eNCA/eNCA Computation Engine/EQuIP/testcases/regression/tst/UAT-Mar2016-EQuIP/reference/analysis/M3SD/B2901001/ppCalc/tc015'
setwd(fp)
list.files()
mctf <- 'B2901001_parameter_data.in.csv'
mctf <- file.path(fp, mctf)
mct  <- read.csv(file=mctf, header=TRUE, as.is=TRUE)
head(mct)

concf <- "B2901001.in.csv"
concf <- file.path(fp, concf)
conc  <- read.csv(file=concf, header=TRUE, as.is=TRUE)
head(conc,2)
table(conc$DOSE)
table(conc$TREATXT)
table(conc$VISIT)
table(conc$PERIOD)
p <- ggplot(data=conc, aes(x=PKPTMS, y=PKCNCN, group=SUBJID)) + geom_line() + geom_point() + facet_grid(.~TREATXT)
(p)

# Define missing entries from the TC015 mct
(x <- setdiff(std_mct$PARAMETER, names(mct)))

# Create missing entries dataframe
missing.entries <- std_mct[match(x, std_mct$PARAMETER),c("PARAMETER", "VALUE")]

# Add into TC015 mct
for(i in 1:nrow(missing.entries)) {
  cat("i: ", i, " Parameter name = ", missing.entries$PARAMETER[i], " Parameter value = ", missing.entries$VALUE[i], "\n")
  mct$var <- as.character(missing.entries$VALUE[i])
  names(mct)[match("var", names(mct))] <- as.character(missing.entries$PARAMETER[i])
}

# Update FLGACCEPTKELCRIT to include AUCXPCT<=0.2 (not necessary since incorporated into std_mct template now)
#mct$FLGACCEPTKELCRIT <- paste(mct$FLGACCEPTKELCRIT, "AUCXPCT<=0.2", sep=", ")

# Update NORMBS
mct$NORMBS <- "WT"

# remove NDOSEI
mct[,names(mct)[grep("NDOSE", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove NDOSEI
mct <- mct[,-match("NDOSEI", names(mct))]

# remove DOSE, DOSEU, DOSEi and DOSEiU
mct[,names(mct)[grep("DOSE", names(mct), ignore.case = TRUE, perl=TRUE)]]
mct <- mct[,-match("DOSEI", names(mct))]
mct <- mct[,-match("DOSEIU", names(mct))]
# Update DOSE, DOSEU - note synonyms for DOSE1, DOSE1U
mct$DOSE <- "DOSE"
mct$DOSEU <- "DOSEUNI"

# DOF, DOFU -> DOF1, DOF1U
mct[,names(mct)[grep("DOF", names(mct), ignore.case = TRUE, perl=TRUE)]]
# rename DOF to DOF1
names(mct)[match("DOFI", names(mct))] <- "DOF1"
# Add DOF1U
mct$DOF1U <- "DOFU"

# remove TAU, TAUU, TAUi and TAUiU - SD (Single Dose)
mct[,names(mct)[grep("TAU", names(mct), ignore.case = TRUE, perl=TRUE)]]
# remove TAU, TAUU
y <- names(mct)[grep("^TAU", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]
# Remove DP|SF.*TAUI
y <- names(mct)[grep("^(DP|SF)[.]{1}(AE|AUC)TAU", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]

# remove TOLD, TOLDU, TOLDi and TOLDiU - SD (Single Dose)
mct[,names(mct)[grep("TOLD", names(mct), ignore.case = TRUE, perl=TRUE)]]
# remove TOLD, TOLDU
y <- names(mct)[grep("^TOLD", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]

# Rename Partial Area MCT entries
mct[,names(mct)[grep("AUCNPAIR|AUC[//.]", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove AUC.*.T* pairs
y <- names(mct)[grep("^AUC[.]{1}.+?[.]{1}T[0-9]+?", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]

# Rename MIDPOINT* and RATE* MCT entries
mct[,names(mct)[grep("MIDPOINT|RATEN|RATEA", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove AUC.*.T* pairs
y <- names(mct)[grep("MIDPOINT|RATEN|RATEA", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]

# RETURNCOLS
mct$RETURNCOLS <- "UDSDEID;SDEID;STUDY;SITEID;SUBJID;RAND;TREATXT;TRTCD;PKCOLL;PKBDFLD;PKTERM;PERIODU;PERIOD;VISITU;VISIT;PHASE;PCMETHOD;HT;WT;AGEDERU;AGEDER;WTUNI;WTRAW;HTUNI;HTRAW;RACEOTH;RACES;SEX;RACIALD;ETHNIC;DOSEP;DOSPND;DOSEPUNI;DOSPTM;DOSE;DOSEU;DOF1;DOF1U;ACTTRT;ACTTRTC;ACTTRTS;TREATSEQ;FLGEMESIS;FU;UDFNAME1;UDFVALUE1;UDFUNIT1;UDFNAMEn;UDFVALUEn;UDFUNITn;DATASTATUS;PKPCOM;PKSIGFIG;PKDECPL;DATABLINDSTATUS;PCLLOQ;PCSTRESU;PCNAM;ROUTE"

# Parameter List for M3SD - Note that these can be expressed in regular expression format 
mct$PARAMETERLIST <- as.character(mct$PARAMETERLIST)
mct$PARAMETERLIST <- "AUCALL;AUCDN;AUCINFO;AUCINFOC;AUCINFODN;AUCINFP;AUCINFPC;AUCINFPDN;AUCLASTi;AUCLASTiDN;AUCLASTCi;AUCT;AUCT1_T2;AUCXPCTO;AUCXPCTP;AUMCINFO;AUMCINFP;AUMCLAST;AUMCXPTO;AUMCXPTP;CLASTi;CLP;CLOW;CLPW;CLR;CMAXi;CMAXCi;CMAXiDN;DOSEC;DIi;F;FREL;FRELLAST;KEL;KELNOPT;KELRSQ;KELRSQA;KELTMHI;KELTMLO;LASTTIME;MRAUCINF;MRAUCLAST;MRCMAXi;MRTLAST;MRTIVIFOi;MRTIVIFPi;TAU;TAUi;THALF;THALFF;TLAST;TMAXi;TOLD;VSSPi;VSSO;VZO;VZOW;VZP;VZPW"

# Parameter Display List for M3SD - Note that these can be expressed in regular expression format 
mct$PARAMETERDISPLAYLIST <- as.character(mct$PARAMETERDISPLAYLIST)
mct$PARAMETERDISPLAYLIST <- "AUCINFP;AUCLASTi;AUCLASTiDN;AUCLASTCi;AUCTAUi;AUCXPCTP;AURCINFP;AURCLAST;AURCXPCTP;CMAXiDN;KEL;KELRSQ;KELNOPT;MAXRATEi;THALF;THALFF;TLAST;TMAXi;TMAXRATEi"

# Reorder names of MCT
#names(mct) <- names(mct)[order(std_mct$PARAMETER)]

# Remove DP. and SF. entries that don't match PARAMETERLIST
y <- grep("SF.|DP.", names(mct), ignore.case=TRUE, perl=TRUE)
y <- names(mct)[y]
y <- gsub("SF.|DP.", "", y)
rmlist <- c()
for(i in y) {
  k <- grep(i, mct$PARAMETERLIST, ignore.case=TRUE, perl=TRUE)
  if(length(k)==0) { rmlist <- c(rmlist, i) }
}
rmlist <- rmlist[!duplicated(rmlist)]
rmlist <- rmlist[-match("DEFAULT", rmlist)]
n <- names(mct)
rmnames <- c()
for(i in rmlist) {
  g <- grep(i, n, ignore.case=TRUE, perl=TRUE)
  print(g)
  if(length(g)>0) {
    print(names(mct)[g])
    rmnames <- c(rmnames, match(names(mct)[g], names(mct)))
  }
}
mct <- mct[,-rmnames]

setdiff(std_mct$PARAMETER, names(mct))
setdiff(names(mct), std_mct$PARAMETER)

# Create new TC015 MCT file
new_mctf <- file.path(fp, 'tc015_mct.csv')
write.csv(mct, file=new_mctf, row.names = FALSE, na="")
# Create JSON equivalent
jsonf <- file.path(fp, 'tc015_mct.json')
write_json(mct, path=jsonf)

# -----------------------------------------------------------------
# TC016 (M3SS) - regression - original: B2901002_parameter_data.in.csv
#                      updated:  tc016_mct.csv
# 
fp   <- 'C:/Users/tensfeldt/Documents/eNCA & EP2/~eNCA/eNCA Computation Engine/EQuIP/testcases/regression/tst/UAT-Mar2016-EQuIP/reference/analysis/M3SS/B2901002/ppCalc/tc016'
setwd(fp)
list.files()
mctf <- 'B2901002_parameter_data.in.csv'
mctf <- file.path(fp, mctf)
mct  <- read.csv(file=mctf, header=TRUE, as.is=TRUE)
head(mct)

# Define missing entries from the TC016 mct
(x <- setdiff(std_mct$PARAMETER, names(mct)))

# Create missing entries dataframe
missing.entries <- std_mct[match(x, std_mct$PARAMETER),c("PARAMETER", "VALUE")]

# Add into TC016 mct
for(i in 1:nrow(missing.entries)) {
  cat("i: ", i, " Parameter name = ", missing.entries$PARAMETER[i], " Parameter value = ", missing.entries$VALUE[i], "\n")
  mct$var <- as.character(missing.entries$VALUE[i])
  names(mct)[match("var", names(mct))] <- as.character(missing.entries$PARAMETER[i])
}

# Update FLGACCEPTKELCRIT to include AUCXPCT<=0.2 (not necessary since incorporated into std_mct template now)
#mct$FLGACCEPTKELCRIT <- paste(mct$FLGACCEPTKELCRIT, "AUCXPCT<=0.2", sep=", ")

# Update NORMBS
mct$NORMBS <- "WT"

# remove NDOSEI
mct[,names(mct)[grep("NDOSE", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove NDOSEI
mct <- mct[,-match("NDOSEI", names(mct))]

# remove DOSE, DOSEU to DOSEi and DOSEiU
mct[,names(mct)[grep("DOSE", names(mct), ignore.case = TRUE, perl=TRUE)]]
# rename DOSE, DOSEU to DOSE1, DOSE1U
names(mct)[match("DOSE", names(mct))] <- "DOSE1"
names(mct)[match("DOSEU", names(mct))] <- "DOSE1U"
# Remove DOSEI, DOSEIU
mct <- mct[,-match("DOSEI", names(mct))]
mct <- mct[,-match("DOSEIU", names(mct))]
# set values of DOSE1, DOSE1U
mct$DOSE1 <- "DOSE"
mct$DOSE1U <- "DOSEUNI"

# DOF, DOFU -> DOF1, DOF1U
mct[,names(mct)[grep("DOF", names(mct), ignore.case = TRUE, perl=TRUE)]]
# rename DOF to DOF1
names(mct)[match("DOFI", names(mct))] <- "DOF1"
# Add DOF1U
mct$DOF1U <- "DOFU"

# remove TAU, TAUU to TAUi and TAUiU
mct[,names(mct)[grep("TAU", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove TAUI, TAUIU
mct <- mct[,-match("TAUI", names(mct))]
mct <- mct[,-match("TAUIU", names(mct))]
# Rename TAU and TAUU to TAU1 and TAU1U
names(mct)[match("TAU", names(mct))] <- "TAU1"
names(mct)[match("TAUU", names(mct))] <- "TAU1U"

# TOLD
mct[,names(mct)[grep("TOLD", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Rename TOLD and TOLDU to TOLD1 and TOLD1U
names(mct)[match("TOLD", names(mct))] <- "TOLD1"
names(mct)[match("TOLDU", names(mct))] <- "TOLD1U"
# Remove TOLDI, TOLDIU
mct <- mct[,-match("TOLDI", names(mct))]
mct <- mct[,-match("TOLDIU", names(mct))]

# Rename Partial Area MCT entries
mct[,names(mct)[grep("AUCNPAIR|AUC[//.]", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove AUC.*.T* pairs
y <- names(mct)[grep("^AUC[.]{1}.+?[.]{1}T[0-9]+?", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]

# Rename MIDPOINT* and RATE* MCT entries
mct[,names(mct)[grep("MIDPOINT|RATEN|RATEA", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove AUC.*.T* pairs
y <- names(mct)[grep("MIDPOINT|RATEN|RATEA", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]

# RETURNCOLS
mct$RETURNCOLS <- "UDSDEID;SDEID;STUDY;SITEID;SUBJID;RAND;TREATXT;TRTCD;PKCOLL;PKBDFLD;PKTERM;PERIODU;PERIOD;VISITU;VISIT;PHASE;PCMETHOD;HT;WT;AGEDERU;AGEDER;WTUNI;WTRAW;HTUNI;HTRAW;RACEOTH;RACES;SEX;RACIALD;ETHNIC;DOSEP;DOSPND;DOSEPUNI;DOSPTM;DOSE1;DOSE1U;DOF1;DOF1U;ACTTRT;ACTTRTC;ACTTRTS;TREATSEQ;FLGEMESIS;FU;UDFNAME1;UDFVALUE1;UDFUNIT1;UDFNAMEn;UDFVALUEn;UDFUNITn;DATASTATUS;PKPCOM;PKSIGFIG;PKDECPL;DATABLINDSTATUS;PCLLOQ;PCSTRESU;PCNAM;ROUTE;TAU1;TAU1U;TOLD1;TOLD1U"

# Parameter List for M3SS - Note that these can be expressed in regular expression format 
mct$PARAMETERLIST <- as.character(mct$PARAMETERLIST)
mct$PARAMETERLIST <- "AUCALL;AUCDN;AUCINFO;AUCINFOC;AUCINFODN;AUCINFP;AUCINFPC;AUCINFPDN;AUCLASTi;AUCLASTiDN;AUCLASTCi;AUCT;AUCT1_T2;AUCTAUi;AUCTAUiDN;AUCXPCTO;AUCXPCTP;AUMCTAUi;CAVi;CLASTi;CLRTAUi;CLTAUi;CLTAUWi;CMAXi;CMAXCi;CMAXiDN;CMINi;DOSEC;CTROUGHi;CTROUGHENDi;DIi;FREL;FRELLAST;FTAUi;KEL;KELNOPT;KELRSQ;KELRSQA;KELTMHI;KELTMLO;LASTTIME;MRAUCINF;MRAUCLAST;MRAUCTAUi;MRCMAXi;MRTIVIFOi;MRTIVIFPi;PTFi;PTRi;PTROUGHRi;PTROUGHREND;TAU;TAUi;THALF;THALFF;TLAST;TMAXi;TMINi;TOLD;VSSPi;VSSO;VSSOW;VSSPWi;VZO;VZP;VZTAUi"

# Parameter Display List for M3SS - Note that these can be expressed in regular expression format 
mct$PARAMETERDISPLAYLIST <- as.character(mct$PARAMETERDISPLAYLIST)
mct$PARAMETERDISPLAYLIST <- "AUCINFP;AUCLASTi;AUCLASTiDN;AUCLASTCi;AUCTAUi;AUCXPCTP;AURCINFP;AURCLAST;AURCXPCTP;CMAXiDN;KEL;KELRSQ;KELNOPT;MAXRATEi;THALF;THALFF;TLAST;TMAXi;TMAXRATEi"

# Reorder names of MCT
#names(mct) <- names(mct)[order(std_mct$PARAMETER)]

# Remove DP. and SF. entries that don't match PARAMETERLIST
y <- grep("SF.|DP.", names(mct), ignore.case=TRUE, perl=TRUE)
y <- names(mct)[y]
y <- gsub("SF.|DP.", "", y)
rmlist <- c()
for(i in y) {
  k <- grep(i, mct$PARAMETERLIST, ignore.case=TRUE, perl=TRUE)
  if(length(k)==0) { rmlist <- c(rmlist, i) }
}
rmlist <- rmlist[!duplicated(rmlist)]
rmlist <- rmlist[-match("DEFAULT", rmlist)]
n <- names(mct)
rmnames <- c()
for(i in rmlist) {
  g <- grep(i, n, ignore.case=TRUE, perl=TRUE)
  print(g)
  if(length(g)>0) {
    print(names(mct)[g])
    rmnames <- c(rmnames, match(names(mct)[g], names(mct)))
  }
}
mct <- mct[,-rmnames]

setdiff(std_mct$PARAMETER, names(mct))
setdiff(names(mct), std_mct$PARAMETER)

# Create new TC016 MCT file
new_mctf <- file.path(fp, 'tc016_mct.csv')
write.csv(mct, file=new_mctf, row.names = FALSE, na="")
# Create JSON equivalent
jsonf <- file.path(fp, 'tc016_mct.json')
write_json(mct, path=jsonf)

# -----------------------------------------------------------------
# TC010 - (M1SD) regression - original: B1261002_parameter_data.in.csv
#                      updated:  tc010_mct.csv
# 
fp   <- 'C:/Users/tensfeldt/Documents/eNCA & EP2/~eNCA/eNCA Computation Engine/EQuIP/testcases/regression/tst/UAT-Mar2016-EQuIP/reference/analysis/M1SD/B1261002/ppCalc/tc010'
setwd(fp)
list.files()
mctf <- 'B1261002_parameter_data.in.csv'
mctf <- file.path(fp, mctf)
mct  <- read.csv(file=mctf, header=TRUE, as.is=TRUE)
head(mct)

# Define missing entries from the TC010 mct
(x <- setdiff(std_mct$PARAMETER, names(mct)))

# Create missing entries dataframe
missing.entries <- std_mct[match(x, std_mct$PARAMETER),c("PARAMETER", "VALUE")]

# Add into TC010 mct
for(i in 1:nrow(missing.entries)) {
  cat("i: ", i, " Parameter name = ", missing.entries$PARAMETER[i], " Parameter value = ", missing.entries$VALUE[i], "\n")
  mct$var <- as.character(missing.entries$VALUE[i])
  names(mct)[match("var", names(mct))] <- as.character(missing.entries$PARAMETER[i])
}

# Update FLGACCEPTKELCRIT to include AUCXPCT<=0.2 (not necessary since incorporated into std_mct template now)
#mct$FLGACCEPTKELCRIT <- paste(mct$FLGACCEPTKELCRIT, "AUCXPCT<=0.2", sep=", ")

# Update NORMBS
mct$NORMBS <- "WT"

# remove NDOSEI
mct[,names(mct)[grep("NDOSE", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove NDOSEI
mct <- mct[,-match("NDOSEI", names(mct))]

# remove DOSE, DOSEU to DOSEi and DOSEiU
mct[,names(mct)[grep("DOSE", names(mct), ignore.case = TRUE, perl=TRUE)]]
# remove DOSEI, DOSEIU
mct <- mct[,-match("DOSEI", names(mct))]
mct <- mct[,-match("DOSEIU", names(mct))]
# rename DOSE, DOSEU to DOSE1, DOSE1U
#names(mct)[match("DOSE",names(mct))] <- "DOSE1"
#names(mct)[match("DOSEU",names(mct))] <- "DOSE1U"
# Set DOSE1, DOSE1U - note DOSE, DOSEU are synonyms
mct$DOSE <- "DOSE"
mct$DOSEU <- "DOSEUNI"

# remove TAU, TAUU, TAUi and TAUiU - SD (Single Dose)
mct[,names(mct)[grep("TAU", names(mct), ignore.case = TRUE, perl=TRUE)]]
# remove TAU, TAUU
y <- names(mct)[grep("^TAU", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]
# Remove DP|SF.*TAUI
y <- names(mct)[grep("^(DP|SF)[.]{1}(AE|AUC)TAU", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]

# remove TOLD, TOLDU, TOLDI, TOLDIU - SD (Single Dose)
mct[,names(mct)[grep("TOLD", names(mct), ignore.case = TRUE, perl=TRUE)]]
# remove TOLD, TOLDU, TOLDI,TOLDIU
y <- names(mct)[grep("^TOLD", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]

# Rename Partial Area MCT entries
mct[,names(mct)[grep("AUCNPAIR|AUC[//.]", names(mct), ignore.case = TRUE, perl=TRUE)]]
# reset AUCNPAIR
mct$AUCNPAIR <- 3
# rename AUC.I.*
names(mct)[match("AUC.I.T1", names(mct))] <- "AUC.1.T1"
names(mct)[match("AUC.I.T2", names(mct))] <- "AUC.1.T2"
names(mct)[match("AUC.I+1.T1", names(mct))] <- "AUC.2.T1"
names(mct)[match("AUC.I+1.T2", names(mct))] <- "AUC.2.T2"
names(mct)[match("AUC.AUCNPAIR.T1", names(mct))] <- "AUC.3.T1"
names(mct)[match("AUC.AUCNPAIR.T2", names(mct))] <- "AUC.3.T2"

# Rename MIDPOINT* and RATE* MCT entries
mct[,names(mct)[grep("MIDPOINT|RATEA|RATEN", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove AUC.*.T* pairs
y <- names(mct)[grep("MIDPOINT|RATEA|RATEN", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]

# RETURNCOLS
mct$RETURNCOLS <- "UDSDEID;SDEID;STUDY;SITEID;SUBJID;RAND;TREATXT;TRTCD;PKCOLL;PKBDFLD;PKTERM;PERIODU;PERIOD;VISITU;VISIT;PHASE;PCMETHOD;HT;WT;AGEDERU;AGEDER;WTUNI;WTRAW;HTUNI;HTRAW;RACEOTH;RACES;SEX;RACIALD;ETHNIC;DOSEP;DOSPND;DOSEPUNI;DOSPTM;DOSE;DOSEU;ACTTRT;ACTTRTC;ACTTRTS;TREATSEQ;FLGEMESIS;FU;UDFNAME1;UDFVALUE1;UDFUNIT1;UDFNAMEn;UDFVALUEn;UDFUNITn;DATASTATUS;PKPCOM;PKSIGFIG;PKDECPL;DATABLINDSTATUS;PCLLOQ;PCSTRESU;PCNAM;ROUTE"

# Parameter List for M1SD - Note that these can be expressed in regular expression format 
mct$PARAMETERLIST <- as.character(mct$PARAMETERLIST)
mct$PARAMETERLIST <- "AUCALL;AUCDN;AUCINFO;AUCINFOC;AUCINFODN;AUCINFP;AUCINFPC;AUCINFPDN;AUCLASTi;AUCLASTiDN;AUCLASTCi;AUCT;AUCT1_T2;AUCXPCTO;AUCXPCTP;AUMCINFO;AUMCINFP;AUMCLAST;AUMCXPTO;AUMCXPTP;CLASTi;CLFO;CLFOW;CLFP;CLFPW;CLO;CLR;CMAXi;CMAXCi;CMAXiDN;CMINi;DOSEC;DIi;DOSEi;F;FREL;FRELLAST;KEL;KELNOPT;KELRSQ;KELRSQA;KELTMHI;KELTMLO;LASTTIME;MRAUCINF;MRAUCLAST;MRCMAXi;MRTLAST;MRTEVIFOi;MRTEVIFPi;TAU;TAUi;THALF;THALFF;TLAG;TLAST;TMAXi;TMINi;TOLD;VZFO;VZFOW;VZFP;VZFPW"

# Parameter Display List for M1SD - Note that these can be expressed in regular expression format 
mct$PARAMETERDISPLAYLIST <- as.character(mct$PARAMETERDISPLAYLIST)
mct$PARAMETERDISPLAYLIST <- "STUDY;SITEID;SUBJID;RAND;TREATXT;TRTCD;PKCOLL;PKBDFLD;PKTERM;PERIODU;PERIOD;VISITU;VISIT;PHASE;PCMETHOD;AUCINFP;AUCLASTi;AUCLASTiDN;AUCLASTCi;AUCXPCTP;CMAX1DN;KEL;KELRSQ;KELNOPT;THALF;THALFF;TLAST;TMAX1"

# Reorder names of MCT
#names(mct) <- names(mct)[order(std_mct$PARAMETER)]

# Remove DP. and SF. entries that don't match PARAMETERLIST
y <- grep("SF.|DP.", names(mct), ignore.case=TRUE, perl=TRUE)
y <- names(mct)[y]
y <- gsub("SF.|DP.", "", y)
rmlist <- c()
for(i in y) {
  k <- grep(i, mct$PARAMETERLIST, ignore.case=TRUE, perl=TRUE)
  if(length(k)==0) { rmlist <- c(rmlist, i) }
}
rmlist <- rmlist[!duplicated(rmlist)]
rmlist <- rmlist[-match("DEFAULT", rmlist)]
n <- names(mct)
rmnames <- c()
for(i in rmlist) {
  g <- grep(i, n, ignore.case=TRUE, perl=TRUE)
  print(g)
  if(length(g)>0) {
    print(names(mct)[g])
    rmnames <- c(rmnames, match(names(mct)[g], names(mct)))
  }
}
mct <- mct[,-rmnames]

setdiff(std_mct$PARAMETER, names(mct))
setdiff(names(mct), std_mct$PARAMETER)

# Create new TC010 MCT file
new_mctf <- file.path(fp, 'tc010_mct.csv')
write.csv(mct, file=new_mctf, row.names = FALSE, na="")
# Create JSON equivalent
jsonf <- file.path(fp, 'tc010_mct.json')
write_json(mct, path=jsonf)

# -----------------------------------------------------------------
# TC011 - (M1SS/mTau) regression - original: A0081216_parameter_data.in.csv
#                      updated:  tc011_mct.csv
# 
fp   <- 'C:/Users/tensfeldt/Documents/eNCA & EP2/~eNCA/eNCA Computation Engine/EQuIP/testcases/regression/tst/UAT-Mar2016-EQuIP/reference/analysis/M1SS/mTau/A0081216/ppCalc/tc011'
setwd(fp)
list.files()
mctf <- 'A0081216_parameter_data.in.csv'
mctf <- file.path(fp, mctf)
mct  <- read.csv(file=mctf, header=TRUE, as.is=TRUE)
head(mct)

concf <- "A0081216.in.original.csv"
concf <- file.path(fp, concf)
conc  <- read.csv(file=concf, header=TRUE, as.is=TRUE)
head(conc,2)
table(conc$DOSE)
table(conc$TAU)
table(conc$TOLD)
table(conc$TREATXT)
table(conc$VISIT)
table(conc$PERIOD)
p <- ggplot(data=conc, aes(x=PKPTMS, y=PKCNCN, group=SUBJID)) + geom_line() + geom_point() + facet_grid(.~TREATXT)
(p)

# ----
# Update the Concentration Dataset for TAU1, TAU1U, TAU2, TAU2U, TOLD1, TOLD1U, TOLD2, TOLD2U
head(conc)
mct[,names(mct)[grep("TAU", names(mct), ignore.case = TRUE, perl=TRUE)]]
mct[,names(mct)[grep("TOLD", names(mct), ignore.case = TRUE, perl=TRUE)]]
names(conc)[grep("TAU", names(conc), ignore.case = TRUE, perl=TRUE)]
names(conc)[grep("TOLD", names(conc), ignore.case = TRUE, perl=TRUE)]

# TAU
names(conc)[grep("TAU", names(conc), ignore.case = TRUE, perl=TRUE)]
names(conc)[grep("PKPTMU", names(conc), ignore.case = TRUE, perl=TRUE)]
summary(conc$TAU)

# TOLD
names(conc)[grep("TOLD", names(conc), ignore.case = TRUE, perl=TRUE)]
# rename TOLD to TOLD1
names(conc)[match("TOLD", names(conc))] <- "TOLD1"
# First dose at 0 hours
conc$TOLD1 <- 0 
# Second dose at 12 hours
conc$TOLD2 <- 12

write.csv(conc, file="A0081216.in.csv")

# ----

# Define missing entries from the TC011 mct
(x <- setdiff(std_mct$PARAMETER, names(mct)))

# Create missing entries dataframe
missing.entries <- std_mct[match(x, std_mct$PARAMETER),c("PARAMETER", "VALUE")]

# Add into TC011 mct
for(i in 1:nrow(missing.entries)) {
  cat("i: ", i, " Parameter name = ", missing.entries$PARAMETER[i], " Parameter value = ", missing.entries$VALUE[i], "\n")
  mct$var <- as.character(missing.entries$VALUE[i])
  names(mct)[match("var", names(mct))] <- as.character(missing.entries$PARAMETER[i])
}

# Update FLGACCEPTKELCRIT to include AUCXPCT<=0.2 (not necessary since incorporated into std_mct template now)
#mct$FLGACCEPTKELCRIT <- paste(mct$FLGACCEPTKELCRIT, "AUCXPCT<=0.2", sep=", ")

# Update NORMBS
mct$NORMBS <- "WT"

# remove NDOSEI
mct[,names(mct)[grep("NDOSE", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove NDOSEI
mct <- mct[,-match("NDOSEI", names(mct))]

# remove DOSE, DOSEU to DOSEi and DOSEiU
mct[,names(mct)[grep("DOSE", names(mct), ignore.case = TRUE, perl=TRUE)]]
# remove DOSEI, DOSEIU
mct <- mct[,-match("DOSEI", names(mct))]
mct <- mct[,-match("DOSEIU", names(mct))]
# rename DOSE, DOSEU to DOSE1, DOSE1U
names(mct)[match("DOSE",names(mct))] <- "DOSE1"
names(mct)[match("DOSEU",names(mct))] <- "DOSE1U"
# Set DOSE1, DOSE1U
mct$DOSE1 <- "DOSE"
mct$DOSE1U <- "DOSEUNI"
# Note this second dose simply points to the first dose since the amounts happen to be the same here.
#  (have to check if DOSE for this protocol was "Total Daily Dose" and needs to be split however)
#  confirmed with CSR contribution that the treatments for A0081216 was
#    2x330 mg CR Tablets once daily (this data is not in this dataset)
#    1x300 mg IR Capsule twice daily (this represents the data in this cohort of data for this MCT example)
mct$DOSE2 <- "DOSE"
mct$DOSE2U <- "DOSEUNI"

# TAU, TAUU
mct[,names(mct)[grep("TAU", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove TAUI, TAUIU
mct <- mct[,-match("TAUI", names(mct))]
mct <- mct[,-match("TAUIU", names(mct))]
# Rename TAU, TAUU to TAU1, TAU1U
names(mct)[match("TAU", names(mct))] <- "TAU1"
names(mct)[match("TAUU", names(mct))] <- "TAU1U"
# Add TAU2, TAU2U - not that TAU2 can point to the same data fields as TAU1 since they are both 12 hours, i.e. equal dosing intervals.
mct$TAU2 <- "TAU"
mct$TAU2U <- "PKPTMU"

# TOLD, TOLDU
mct[,names(mct)[grep("TOLD", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Rename TOLD, TOLDU to TOLD1, TOLD1U
names(mct)[match("TOLD", names(mct))] <- "TOLD1"
names(mct)[match("TOLDU", names(mct))] <- "TOLD1U"
# Remove TOLDI, TOLDIU
mct <- mct[,-match("TOLDI", names(mct))]
mct <- mct[,-match("TOLDIU", names(mct))]
# Update TOLD1, TOLD1U
mct$TOLD1  <- "TOLD1"
mct$TOLD1U <- "PKPTMU"
# Add TOLD2, TOLD2U
mct$TOLD2  <- "TOLD2"
mct$TOLD2U <- "PKPTMU"

# Rename Partial Area MCT entries
mct[,names(mct)[grep("AUCNPAIR|AUC[//.]", names(mct), ignore.case = TRUE, perl=TRUE)]]
# reset AUCNPAIR
mct$AUCNPAIR <- 3
# rename AUC.I.*
names(mct)[match("AUC.I.T1", names(mct))] <- "AUC.1.T1"
names(mct)[match("AUC.I.T2", names(mct))] <- "AUC.1.T2"
names(mct)[match("AUC.I+1.T1", names(mct))] <- "AUC.2.T1"
names(mct)[match("AUC.I+1.T2", names(mct))] <- "AUC.2.T2"
names(mct)[match("AUC.AUCNPAIR.T1", names(mct))] <- "AUC.3.T1"
names(mct)[match("AUC.AUCNPAIR.T2", names(mct))] <- "AUC.3.T2"
# Adjust Partial Area specifications
mct$AUC.1.T1 <- 0
mct$AUC.1.T2 <- 12
mct$AUC.2.T1 <- 12
mct$AUC.2.T2 <- 24
mct$AUC.3.T1 <- 0
mct$AUC.3.T2 <- 48

# Rename MIDPOINT* and RATE* MCT entries
mct[,names(mct)[grep("MIDPOINT|RATEA|RATEN", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove AUC.*.T* pairs
y <- names(mct)[grep("MIDPOINT|RATEA|RATEN", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]

# RETURNCOLS
mct$RETURNCOLS <- "UDSDEID;SDEID;STUDY;SITEID;SUBJID;RAND;TREATXT;TRTCD;PKCOLL;PKBDFLD;PKTERM;PERIODU;PERIOD;VISITU;VISIT;PHASE;PCMETHOD;HT;WT;AGEDERU;AGEDER;WTUNI;WTRAW;HTUNI;HTRAW;RACEOTH;RACES;SEX;RACIALD;ETHNIC;DOSEP;DOSPND;DOSEPUNI;DOSPTM;DOSE1;DOSE1U;DOSE2;DOSE2U;ACTTRT;ACTTRTC;ACTTRTS;TREATSEQ;FLGEMESIS;UDFNAME1;UDFVALUE1;UDFUNIT1;UDFNAMEn;UDFVALUEn;UDFUNITn;DATASTATUS;PKPCOM;PKSIGFIG;PKDECPL;DATABLINDSTATUS;PCLLOQ;PCSTRESU;PCNAM;ROUTE;TAU;TAU1U;TOLD1;TOLD1U"

# Parameter List for M1SS - Note that these can be expressed in regular expression format 
mct$PARAMETERLIST <- as.character(mct$PARAMETERLIST)
mct$PARAMETERLIST <- "AUCALL;AUCDN;AUCINFO;AUCINFOC;AUCINFODN;AUCINFP;AUCINFPC;AUCINFPDN;AUCLASTi;AUCLASTiDN;AUCLASTCi;AUCT;AUCT1_T2;AUCTAUi;AUCTAUiDN;AUCXPCTO;AUCXPCTP;AUMCTAUi;CAVi;CLASTi;CLFO;CLFTAUi;CLFTAUWi;CLO;CLRTAUi;CMAXi;CMAXCi;CMAXiDN;CMINi;DOSEC;CTROUGHi;CTROUGHENDi;DIi;DOSEi;FREL;FRELLAST;FTAUi;KEL;KELNOPT;KELRSQ;KELRSQA;KELTMHI;KELTMLO;LASTTIME;MRAUCINF;MRAUCLAST;MRAUCTAUi;MRCMAXi;MRTEVIFOi;MRTEVIFPi;PTFi;PTRi;PTROUGHRi;PTROUGHREND;TAU;TAUi;THALF;THALFF;TLAG;TLAST;TMAXi;TMINi;TOLD;VZFP;VZFTAUi;VZFTAUWi"

# Parameter Display List for M1SS - Note that these can be expressed in regular expression format 
mct$PARAMETERDISPLAYLIST <- as.character(mct$PARAMETERDISPLAYLIST)
mct$PARAMETERDISPLAYLIST <- "AUCINFP;AUCLASTi;AUCLASTiDN;AUCLASTCi;AUCTAUi;AUCXPCTP;CMAX1DN;KEL;KELRSQ;KELNOPT;THALF;THALFF;TLAST;TMAX1"

# Reorder names of MCT
#names(mct) <- names(mct)[order(std_mct$PARAMETER)]

# Remove DP. and SF. entries that don't match PARAMETERLIST
y <- grep("SF.|DP.", names(mct), ignore.case=TRUE, perl=TRUE)
y <- names(mct)[y]
y <- gsub("SF.|DP.", "", y)
rmlist <- c()
for(i in y) {
  k <- grep(i, mct$PARAMETERLIST, ignore.case=TRUE, perl=TRUE)
  if(length(k)==0) { rmlist <- c(rmlist, i) }
}
rmlist <- rmlist[!duplicated(rmlist)]
rmlist <- rmlist[-match("DEFAULT", rmlist)]
n <- names(mct)
rmnames <- c()
for(i in rmlist) {
  g <- grep(i, n, ignore.case=TRUE, perl=TRUE)
  print(g)
  if(length(g)>0) {
    print(names(mct)[g])
    rmnames <- c(rmnames, match(names(mct)[g], names(mct)))
  }
}
mct <- mct[,-rmnames]

setdiff(std_mct$PARAMETER, names(mct))
setdiff(names(mct), std_mct$PARAMETER)

# Create new TC011 MCT file
new_mctf <- file.path(fp, 'tc011_mct.csv')
write.csv(mct, file=new_mctf, row.names = FALSE, na="")
# Create JSON equivalent
jsonf <- file.path(fp, 'tc011_mct.json')
write_json(mct, path=jsonf)

# -----------------------------------------------------------------
# TC012 - (M1SS/sTau) regression - original: A0081216_parameter_data.in.csv
#                      updated:  tc012_mct.csv
# 
fp   <- 'C:/Users/tensfeldt/Documents/eNCA & EP2/~eNCA/eNCA Computation Engine/EQuIP/testcases/regression/tst/UAT-Mar2016-EQuIP/reference/analysis/M1SS/sTau/A0081216/ppCalc/tc012'
setwd(fp)
list.files()
mctf <- 'A0081216_parameter_data.in.csv'
mctf <- file.path(fp, mctf)
mct  <- read.csv(file=mctf, header=TRUE, as.is=TRUE)
head(mct)

concf <- "A0081216.in.original.csv"
concf <- file.path(fp, concf)
conc  <- read.csv(file=concf, header=TRUE, as.is=TRUE)
head(conc,2)
table(conc$DOSE)
table(conc$TAU)
table(conc$TOLD)
table(conc$TREATXT)
table(conc$TREATXT, conc$TAU)
table(conc$TREATXT, conc$TOLD)
table(conc$VISIT)
table(conc$PERIOD)
p <- ggplot(data=conc, aes(x=PKPTMS, y=PKCNCN, group=SUBJID)) + geom_line() + geom_point() + facet_grid(.~TREATXT)
(p)

# ----

mct[,names(mct)[grep("(DOSE|TAU|TOLD)[0-9]+?[U]*?$", names(mct), ignore.case = TRUE, perl=TRUE)]]

# Update the Concentration Dataset for TAU1, TAU1U, TAU2, TAU2U, TOLD1, TOLD1U, TOLD2, TOLD2U
names(conc)[grep("TAU", names(conc), ignore.case = TRUE, perl=TRUE)]
names(conc)[grep("TOLD", names(conc), ignore.case = TRUE, perl=TRUE)]

# Set Treatment Group Indices
# k = "2X330 mg CR Fed QD"
k <- conc$TREATXT=="2X330 mg CR Fed QD"
# j = "1X300 mg IR Fasted q12h"
j <- conc$TREATXT=="1X300 mg IR Fasted q12h"

#
head(conc[,names(conc)[grep("(TREATXT)|(DOSE|TAU|TOLD)[0-9]+?[U]*?$", names(conc), ignore.case = TRUE, perl=TRUE)]][k,])
head(conc[,names(conc)[grep("(TREATXT)|(DOSE|TAU|TOLD)[0-9]+?[U]*?$", names(conc), ignore.case = TRUE, perl=TRUE)]][j,])

# Dose
names(conc)[grep("DOSE", names(conc), ignore.case = TRUE, perl=TRUE)]
# Rename DOSE to DOSE1
names(conc)[match("DOSE", names(conc))] <- "DOSE1"
# Add DOSE2
conc$DOSE2 <- rep(NA, nrow(conc))
# Update DOSE1,DOSE2 by TREATXT
conc$DOSE1 <- ifelse(k, 660, conc$DOSE1)
conc$DOSE1 <- ifelse(j, 300, conc$DOSE1)
conc$DOSE2 <- ifelse(k, NA, conc$DOSE2)
conc$DOSE2 <- ifelse(j, 300, conc$DOSE2)

# TAU
names(conc)[grep("TAU", names(conc), ignore.case = TRUE, perl=TRUE)]
names(conc)[grep("PKPTMU", names(conc), ignore.case = TRUE, perl=TRUE)]
summary(conc$TAU)
# Rename TAU to TAU1
names(conc)[match("TAU", names(conc))] <- "TAU1"
# Add TAU2
conc$TAU2 <- rep(NA, nrow(conc))
# Update TAU1,TAU2 by TREATXT
conc$TAU1 <- ifelse(k, 24, conc$TAU1)
conc$TAU1 <- ifelse(j, 12, conc$TAU1)
conc$TAU2 <- ifelse(k, NA, conc$TAU2)
conc$TAU2 <- ifelse(j, 12, conc$TAU2)

# TOLD
names(conc)[grep("TOLD", names(conc), ignore.case = TRUE, perl=TRUE)]
# rename TOLD to TOLD1
names(conc)[match("TOLD", names(conc))] <- "TOLD1"
# Add TOLD2
conc$TOLD2 <- rep(NA,nrow(conc))

# Update TOLD1,TOLD2 by TREATXT
conc$TOLD1 <- ifelse(k, 0, conc$TOLD1)
conc$TOLD1 <- ifelse(j, 0, conc$TOLD1)
conc$TOLD2 <- ifelse(k, NA, conc$TOLD2)
conc$TOLD2 <- ifelse(j, 12, conc$TOLD2)

table(conc$TREATXT, conc$DOSE1)
table(conc$TREATXT, conc$TAU1)
table(conc$TREATXT, conc$TOLD1)

table(conc$TREATXT, conc$DOSE2)
table(conc$TREATXT, conc$TAU2)
table(conc$TREATXT, conc$TOLD2)

write.csv(conc, file="A0081216.in.csv")
# ----

# Define missing entries from the TC012 mct
(x <- setdiff(std_mct$PARAMETER, names(mct)))

# Create missing entries dataframe
missing.entries <- std_mct[match(x, std_mct$PARAMETER),c("PARAMETER", "VALUE")]

# Add into TC012 mct
for(i in 1:nrow(missing.entries)) {
  cat("i: ", i, " Parameter name = ", missing.entries$PARAMETER[i], " Parameter value = ", missing.entries$VALUE[i], "\n")
  mct$var <- as.character(missing.entries$VALUE[i])
  names(mct)[match("var", names(mct))] <- as.character(missing.entries$PARAMETER[i])
}

# Update FLGACCEPTKELCRIT to include AUCXPCT<=0.2 (not necessary since incorporated into std_mct template now)
#mct$FLGACCEPTKELCRIT <- paste(mct$FLGACCEPTKELCRIT, "AUCXPCT<=0.2", sep=", ")

# Update NORMBS
mct$NORMBS <- "WT"

# remove NDOSEI
mct[,names(mct)[grep("NDOSE", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove NDOSEI
mct <- mct[,-match("NDOSEI", names(mct))]

# remove DOSE, DOSEU to DOSEi and DOSEiU
mct[,names(mct)[grep("DOSE", names(mct), ignore.case = TRUE, perl=TRUE)]]
# remove DOSEI, DOSEIU
mct <- mct[,-match("DOSEI", names(mct))]
mct <- mct[,-match("DOSEIU", names(mct))]
# rename DOSE, DOSEU to DOSE1, DOSE1U
names(mct)[match("DOSE", names(mct))] <- "DOSE1"
names(mct)[match("DOSEU", names(mct))] <- "DOSE1U"
# Set DOSE1
mct$DOSE1 <- "DOSE1"
mct$DOSE1U <- "DOSE1U"
# Set DOSE2
mct$DOSE2 <- "DOSE2"
mct$DOSE2U <- "DOSE2U"

# remove TAU, TAUU to TAUi and TAUiU
mct[,names(mct)[grep("TAU", names(mct), ignore.case = TRUE, perl=TRUE)]]
# remove TAUI and TAUIU
mct <- mct[,-match("TAUI", names(mct))]
mct <- mct[,-match("TAUIU", names(mct))]
# Rename TAU, TAUU to TAU1, TAU1U
names(mct)[match("TAU", names(mct))] <- "TAU1"
names(mct)[match("TAUU", names(mct))] <- "TAU1U"
# Update TAU1, TAU1U
mct$TAU1 <- "TAU1"
# Add TAU2, TAU2U
mct$TAU2 <- "TAU2"
mct$TAU2U <- "PKPTMU"

# TOLD, TOLDU
mct[,names(mct)[grep("TOLD", names(mct), ignore.case = TRUE, perl=TRUE)]]
# remove TOLDI and TOLDIU
mct <- mct[,-match("TOLDI", names(mct))]
mct <- mct[,-match("TOLDIU", names(mct))]
# Rename TOLD, TOLDU to TOLD1, TOLD1U
names(mct)[match("TOLD", names(mct))] <- "TOLD1"
names(mct)[match("TOLDU", names(mct))] <- "TOLD1U"
# Update TOLD1, TOLD1U
mct$TOLD1 <- "TOLD1"
mct$TOLD1U <- "TOLD1U"
# Add TOLD2, TOLD2U
mct$TOLD2 <- "TOLD2"
mct$TOLD2U <- "TOLD2U"

# Rename Partial Area MCT entries
mct[,names(mct)[grep("AUCNPAIR|AUC[//.]", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove AUC.*.T* pairs
y <- names(mct)[grep("^AUC[.]{1}.+?[.]{1}T[0-9]+?", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]

# Rename MIDPOINT* and RATE* MCT entries
mct[,names(mct)[grep("MIDPOINT|RATEN|RATEA", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove AUC.*.T* pairs
y <- names(mct)[grep("MIDPOINT|RATEN|RATEA", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]

# RETURNCOLS
mct$RETURNCOLS <- "UDSDEID;SDEID;STUDY;SITEID;SUBJID;RAND;TREATXT;TRTCD;PKCOLL;PKBDFLD;PKTERM;PERIODU;PERIOD;VISITU;VISIT;PHASE;PCMETHOD;HT;WT;AGEDERU;AGEDER;WTUNI;WTRAW;HTUNI;HTRAW;RACEOTH;RACES;SEX;RACIALD;ETHNIC;DOSEP;DOSPND;DOSEPUNI;DOSPTM;DOSE1;DOSE1U;DOSE2;DOSE2U;ACTTRT;ACTTRTC;ACTTRTS;TREATSEQ;FLGEMESIS;FU;UDFNAME1;UDFVALUE1;UDFUNIT1;UDFNAMEn;UDFVALUEn;UDFUNITn;DATASTATUS;PKPCOM;PKSIGFIG;PKDECPL;DATABLINDSTATUS;PCLLOQ;PCSTRESU;PCNAM;ROUTE;TAU1;TAU1U;TOLD1;TOLD1U;TAU2;TAU2U;TOLD2;TOLD2U"

# Parameter List for M1SS - Note that these can be expressed in regular expression format 
mct$PARAMETERLIST <- as.character(mct$PARAMETERLIST)
mct$PARAMETERLIST <- "AUCALL;AUCDN;AUCINFO;AUCINFOC;AUCINFODN;AUCINFP;AUCINFPC;AUCINFPDN;AUCLASTi;AUCLASTiDN;AUCLASTCi;AUCT;AUCT1_T2;AUCTAUi;AUCTAUiDN;AUCXPCTO;AUCXPCTP;AUMCTAUi;CAVi;CLASTi;CLFO;CLFTAUi;CLFTAUWi;CLO;CLRTAUi;CMAXi;CMAXCi;CMAXiDN;CMINi;DOSEC;CTROUGHi;CTROUGHENDi;DIi;DOSEi;FREL;FRELLAST;FTAUi;KEL;KELNOPT;KELRSQ;KELRSQA;KELTMHI;KELTMLO;LASTTIME;MRAUCINF;MRAUCLAST;MRAUCTAUi;MRCMAXi;MRTEVIFOi;MRTEVIFPi;PTFi;PTRi;PTROUGHRi;PTROUGHREND;TAU;TAUi;THALF;THALFF;TLAG;TLAST;TMAXi;TMINi;TOLD;VZFP;VZFTAUi;VZFTAUWi"

# Parameter Display List for M1SS - Note that these can be expressed in regular expression format 
mct$PARAMETERDISPLAYLIST <- as.character(mct$PARAMETERDISPLAYLIST)
mct$PARAMETERDISPLAYLIST <- "AUCINFP;AUCLASTi;AUCLASTiDN;AUCLASTCi;AUCTAUi;AUCXPCTP;CMAX1DN;KEL;KELRSQ;KELNOPT;THALF;THALFF;TLAST;TMAX1"

# Reorder names of MCT
#names(mct) <- names(mct)[order(std_mct$PARAMETER)]

# Remove DP. and SF. entries that don't match PARAMETERLIST
y <- grep("SF.|DP.", names(mct), ignore.case=TRUE, perl=TRUE)
y <- names(mct)[y]
y <- gsub("SF.|DP.", "", y)
rmlist <- c()
for(i in y) {
  k <- grep(i, mct$PARAMETERLIST, ignore.case=TRUE, perl=TRUE)
  if(length(k)==0) { rmlist <- c(rmlist, i) }
}
rmlist <- rmlist[!duplicated(rmlist)]
rmlist <- rmlist[-match("DEFAULT", rmlist)]
n <- names(mct)
rmnames <- c()
for(i in rmlist) {
  g <- grep(i, n, ignore.case=TRUE, perl=TRUE)
  print(g)
  if(length(g)>0) {
    print(names(mct)[g])
    rmnames <- c(rmnames, match(names(mct)[g], names(mct)))
  }
}
mct <- mct[,-rmnames]

setdiff(std_mct$PARAMETER, names(mct))
setdiff(names(mct), std_mct$PARAMETER)

# Create new TC012 MCT file
new_mctf <- file.path(fp, 'tc012_mct.csv')
write.csv(mct, file=new_mctf, row.names = FALSE, na="")
# Create JSON equivalent
jsonf <- file.path(fp, 'tc012_mct.json')
write_json(mct, path=jsonf)

# -----------------------------------------------------------------
# TC013 - (M2SD) regression - original: B2901001_parameter_data.in.csv
#                      updated:  tc013_mct.csv
# 
fp   <- 'C:/Users/tensfeldt/Documents/eNCA & EP2/~eNCA/eNCA Computation Engine/EQuIP/testcases/regression/tst/UAT-Mar2016-EQuIP/reference/analysis/M2SD/B2901001/ppCalc/tc013'
setwd(fp)
list.files()
mctf <- 'B2901001_parameter_data.in.csv'
mctf <- file.path(fp, mctf)
mct  <- read.csv(file=mctf, header=TRUE, as.is=TRUE)
head(mct)

# Define missing entries from the TC013 mct
(x <- setdiff(std_mct$PARAMETER, names(mct)))

# Create missing entries dataframe
missing.entries <- std_mct[match(x, std_mct$PARAMETER),c("PARAMETER", "VALUE")]

# Add into TC013 mct
for(i in 1:nrow(missing.entries)) {
  cat("i: ", i, " Parameter name = ", missing.entries$PARAMETER[i], " Parameter value = ", missing.entries$VALUE[i], "\n")
  mct$var <- as.character(missing.entries$VALUE[i])
  names(mct)[match("var", names(mct))] <- as.character(missing.entries$PARAMETER[i])
}

# Update FLGACCEPTKELCRIT to include AUCXPCT<=0.2 (not necessary since incorporated into std_mct template now)
#mct$FLGACCEPTKELCRIT <- paste(mct$FLGACCEPTKELCRIT, "AUCXPCT<=0.2", sep=", ")

# Update NORMBS
mct$NORMBS <- "WT"

# remove NDOSEI
mct[,names(mct)[grep("NDOSE", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove NDOSEI
mct <- mct[,-match("NDOSEI", names(mct))]

# remove DOSE, DOSEU to DOSEi and DOSEiU
mct[,names(mct)[grep("DOSE", names(mct), ignore.case = TRUE, perl=TRUE)]]
mct <- mct[,-match("DOSE", names(mct))]
mct <- mct[,-match("DOSEU", names(mct))]
# rename DOSEI and DOSEIU to DOSE1 and DOSE1U
names(mct)[match("DOSEI", names(mct))] <- "DOSE1"
names(mct)[match("DOSEIU", names(mct))] <- "DOSE1U"
# Set DOSE1 to DOSE
mct$DOSE1 <- "DOSE"
mct$DOSE1U <- "DOSEUNI"

# remove TAU, TAUU, TAUi and TAUiU - SD (Single Dose)
mct[,names(mct)[grep("TAU", names(mct), ignore.case = TRUE, perl=TRUE)]]
# remove TAU, TAUU
y <- names(mct)[grep("^TAU", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]
# Remove DP|SF.*TAUI
y <- names(mct)[grep("^(DP|SF)[.]{1}(AE|AUC)TAU", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]

# TOLD
mct[,names(mct)[grep("TOLD", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove TOLD, TOLDU
mct <- mct[,-match("TOLD", names(mct))]
mct <- mct[,-match("TOLDU", names(mct))]
mct <- mct[,-match("TOLDI", names(mct))]
mct <- mct[,-match("TOLDIU", names(mct))]

# Rename Partial Area MCT entries
mct[,names(mct)[grep("AUCNPAIR|AUC[//.]", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove AUC.*.T* pairs
y <- names(mct)[grep("^AUC[.]{1}.+?[.]{1}T[0-9]+?", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]

# Rename MIDPOINT* and RATE* MCT entries
mct[,names(mct)[grep("MIDPOINT|RATEA|RATEN", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove AUC.*.T* pairs
y <- names(mct)[grep("MIDPOINT|RATEA|RATEN", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]

# RETURNCOLS
mct$RETURNCOLS <- "UDSDEID;SDEID;STUDY;SITEID;SUBJID;RAND;TREATXT;TRTCD;PKCOLL;PKBDFLD;PKTERM;PERIODU;PERIOD;VISITU;VISIT;PHASE;PCMETHOD;COLLDATE;HT;WT;AGEDERU;AGEDER;WTUNI;WTRAW;HTUNI;HTRAW;RACEOTH;RACES;SEX;RACIALD;ETHNIC;DOSEP;DOSPND;DOSEPUNI;DOSPTM;DOSE1;DOSE1U;ACTTRT;ACTTRTC;ACTTRTS;TREATSEQ;FLGEMESIS;FU;UDFNAME1;UDFVALUE1;UDFUNIT1;UDFNAMEn;UDFVALUEn;UDFUNITn;DATASTATUS;PKPCOM;PKSIGFIG;PKDECPL;DATABLINDSTATUS;PCLLOQ;PCSTRESU;PCNAM;ROUTE"

# Parameter List for M2SD - Note that these can be expressed in regular expression format 
mct$PARAMETERLIST <- as.character(mct$PARAMETERLIST)
mct$PARAMETERLIST <- "AUCALL;AUCDN;AUCINFO;AUCINFOC;AUCINFODN;AUCINFP;AUCINFPC;AUCINFPDN;AUCLASTi;AUCLASTiDN;AUCLASTCi;AUCT;AUCT1_T2;AUCXBpctO;AUCXBpctP;AUCXPCTO;AUCXPCTP;AUMCINFO;AUMCINFP;AUMCLAST;AUMCXPTO;AUMCXPTP;C0;CLASTi;CLP;CLOW;CLPW;CLR;CMAXi;CMAXCi;CMAXiDN;DOSEC;DIi;DOSEi;F;FREL;FRELLAST;KEL;KELNOPT;KELRSQ;KELRSQA;KELTMHI;KELTMLO;LASTTIME;MRAUCINF;MRAUCLAST;MRCMAXi;MRTLAST;MRTIVIFOi;MRTIVIFPi;TAU;TAUi;THALF;THALFF;TLAST;TMAXi;TOLD;V0;VSSPi;VSSO;VZO;VZOW;VZP;VZPW"

# Parameter Display List for M2SD - Note that these can be expressed in regular expression format 
mct$PARAMETERDISPLAYLIST <- as.character(mct$PARAMETERDISPLAYLIST)
mct$PARAMETERDISPLAYLIST <- "AUCINFP;AUCLASTi;AUCLASTiDN;AUCLASTCi;AUCXPCTP;CMAXiDN;KEL;KELRSQ;KELNOPT;MAXRATEi;THALF;THALFF;TLAST;TMAXi"

# Reorder names of MCT
#names(mct) <- names(mct)[order(std_mct$PARAMETER)]

# Remove DP. and SF. entries that don't match PARAMETERLIST
y <- grep("SF.|DP.", names(mct), ignore.case=TRUE, perl=TRUE)
y <- names(mct)[y]
y <- gsub("SF.|DP.", "", y)
rmlist <- c()
for(i in y) {
  k <- grep(i, mct$PARAMETERLIST, ignore.case=TRUE, perl=TRUE)
  if(length(k)==0) { rmlist <- c(rmlist, i) }
}
rmlist <- rmlist[!duplicated(rmlist)]
rmlist <- rmlist[-match("DEFAULT", rmlist)]
n <- names(mct)
rmnames <- c()
for(i in rmlist) {
  g <- grep(i, n, ignore.case=TRUE, perl=TRUE)
  print(g)
  if(length(g)>0) {
    print(names(mct)[g])
    rmnames <- c(rmnames, match(names(mct)[g], names(mct)))
  }
}
mct <- mct[,-rmnames]

setdiff(std_mct$PARAMETER, names(mct))
setdiff(names(mct), std_mct$PARAMETER)

# Create new TC013 MCT file
new_mctf <- file.path(fp, 'tc013_mct.csv')
write.csv(mct, file=new_mctf, row.names = FALSE, na="")
# Create JSON equivalent
jsonf <- file.path(fp, 'tc013_mct.json')
write_json(mct, path=jsonf)

# -----------------------------------------------------------------
# TC014 - (M2SS) regression - original: A4021010_parameter_data.in.csv
#                      updated:  tc014_mct.csv
# 
fp   <- 'C:/Users/tensfeldt/Documents/eNCA & EP2/~eNCA/eNCA Computation Engine/EQuIP/testcases/regression/tst/UAT-Mar2016-EQuIP/reference/analysis/M2SS/A4021010/ppCalc/tc014'
setwd(fp)
list.files()
mctf <- 'A4021010_parameter_data.in.csv'
mctf <- file.path(fp, mctf)
mct  <- read.csv(file=mctf, header=TRUE, as.is=TRUE)
head(mct)

# Define missing entries from the TC014 mct
(x <- setdiff(std_mct$PARAMETER, names(mct)))

# Create missing entries dataframe
missing.entries <- std_mct[match(x, std_mct$PARAMETER),c("PARAMETER", "VALUE")]

# Add into TC014 mct
for(i in 1:nrow(missing.entries)) {
  cat("i: ", i, " Parameter name = ", missing.entries$PARAMETER[i], " Parameter value = ", missing.entries$VALUE[i], "\n")
  mct$var <- as.character(missing.entries$VALUE[i])
  names(mct)[match("var", names(mct))] <- as.character(missing.entries$PARAMETER[i])
}

# Update FLGACCEPTKELCRIT to include AUCXPCT<=0.2 (not necessary since incorporated into std_mct template now)
#mct$FLGACCEPTKELCRIT <- paste(mct$FLGACCEPTKELCRIT, "AUCXPCT<=0.2", sep=", ")

# Update NORMBS
mct$NORMBS <- "WT"

# remove NDOSEI
mct[,names(mct)[grep("NDOSE", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove NDOSEI
mct <- mct[,-match("NDOSEI", names(mct))]

# remove DOSE, DOSEU to DOSEi and DOSEiU
mct[,names(mct)[grep("DOSE", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove DOSEI and DOSEIU
mct <- mct[,-match("DOSEI", names(mct))]
mct <- mct[,-match("DOSEIU", names(mct))]
# Rename DOSE and DOSEU to DOSEI and DOSEIU
names(mct)[match("DOSE",names(mct))] <- "DOSE1"
names(mct)[match("DOSEU",names(mct))] <- "DOSE1U"

# remove TAU, TAUU, TAUi and TAUiU
mct[,names(mct)[grep("TAU", names(mct), ignore.case = TRUE, perl=TRUE)]]
# remove TAUI and TAUIU
mct <- mct[,-match("TAUI", names(mct))]
mct <- mct[,-match("TAUIU", names(mct))]
# Rename TAU and TAUU to TAU1 and TAU1U
names(mct)[match("TAU",names(mct))] <- "TAU1"
names(mct)[match("TAUU",names(mct))] <- "TAU1U"

# TOLD
mct[,names(mct)[grep("TOLD", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Rename TOLD and TOLDU to TOLDI and TOLDIU
names(mct)[match("TOLD",names(mct))] <- "TOLD1"
names(mct)[match("TOLDU",names(mct))] <- "TOLD1U"
# remove TOLDI and TOLDIU
mct <- mct[,-match("TOLDI", names(mct))]
mct <- mct[,-match("TOLDIU", names(mct))]

# Rename Partial Area MCT entries
mct[,names(mct)[grep("AUCNPAIR|AUC[//.]", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove AUC.*.T* pairs
y <- names(mct)[grep("^AUC[.]{1}.+?[.]{1}T[0-9]+?", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]

# Rename MIDPOINT* and RATE* MCT entries
mct[,names(mct)[grep("MIDPOINT|RATEA|RATEN", names(mct), ignore.case = TRUE, perl=TRUE)]]
# Remove AUC.*.T* pairs
y <- names(mct)[grep("MIDPOINT|RATEA|RATEN", names(mct), ignore.case = TRUE, perl=TRUE)]
mct <- mct[,-match(y, names(mct))]

# RETURNCOLS
mct$RETURNCOLS <- "UDSDEID;SDEID;STUDY;SITEID;SUBJID;RAND;TREATXT;TRTCD;PKCOLL;PKBDFLD;PKTERM;PERIODU;PERIOD;VISITU;VISIT;PHASE;PCMETHOD;COLLDATE;HT;WT;AGEDERU;AGEDER;WTUNI;WTRAW;HTUNI;HTRAW;RACEOTH;RACES;SEX;RACIALD;ETHNIC;DOSEP;DOSPND;DOSEPUNI;DOSPTM;DOSE1;DOSE1U;ACTTRT;ACTTRTC;ACTTRTS;TREATSEQ;FLGEMESIS;FU;UDFNAME1;UDFVALUE1;UDFUNIT1;UDFNAMEn;UDFVALUEn;UDFUNITn;DATASTATUS;PKPCOM;PKSIGFIG;PKDECPL;DATABLINDSTATUS;PCLLOQ;PCSTRESU;PCNAM;ROUTE;TAU1;TAU1U;TOLD1;TOLD1U"

# Parameter List for M2SS - Note that these can be expressed in regular expression format 
mct$PARAMETERLIST <- as.character(mct$PARAMETERLIST)
mct$PARAMETERLIST <- "AUCALL;AUCDN;AUCINFO;AUCINFOC;AUCINFODN;AUCINFP;AUCINFPC;AUCINFPDN;AUCLASTi;AUCLASTiDN;AUCLASTCi;AUCT;AUCT1_T2;AUCTAUi;AUCTAUiDN;AUCXBpctO;AUCXBpctP;AUCXPCTO;AUCXPCTP;AUMCTAUi;C0;CAVi;CLASTi;CLRTAUi;CLTAUi;CLTAUWi;CMAXi;CMAXCi;CMAXiDN;CMINi;DOSEC;CTROUGHi;CTROUGHENDi;DIi;DOSEi;FREL;FRELLAST;FTAUi;KEL;KELNOPT;KELRSQ;KELRSQA;KELTMHI;KELTMLO;LASTTIME;MRAUCINF;MRAUCLAST;MRAUCTAUi;MRCMAXi;MRTIVIFOi;MRTIVIFPi;PTFi;PTRi;PTROUGHRi;PTROUGHREND;TAU;TAUi;THALF;THALFF;TLAST;TMAXi;TMINi;TOLD;V0;VSSPi;VSSO;VSSOW;VSSPWi;VZO;VZP;VZTAUi"

# Parameter Display List for M2SS - Note that these can be expressed in regular expression format 
mct$PARAMETERDISPLAYLIST <- as.character(mct$PARAMETERDISPLAYLIST)
mct$PARAMETERDISPLAYLIST <- "AUCINFP;AUCLASTi;AUCLASTiDN;AUCLASTCi;AUCTAU1;AUCXPCTP;CMAXiDN;KEL;KELRSQ;KELNOPT;THALF;THALFF;TLAST;TMAXi"

# Reorder names of MCT
#names(mct) <- names(mct)[order(std_mct$PARAMETER)]

# Remove DP. and SF. entries that don't match PARAMETERLIST
y <- grep("SF.|DP.", names(mct), ignore.case=TRUE, perl=TRUE)
y <- names(mct)[y]
y <- gsub("SF.|DP.", "", y)
rmlist <- c()
for(i in y) {
  k <- grep(i, mct$PARAMETERLIST, ignore.case=TRUE, perl=TRUE)
  if(length(k)==0) { rmlist <- c(rmlist, i) }
}
rmlist <- rmlist[!duplicated(rmlist)]
rmlist <- rmlist[-match("DEFAULT", rmlist)]
n <- names(mct)
rmnames <- c()
for(i in rmlist) {
  g <- grep(i, n, ignore.case=TRUE, perl=TRUE)
  print(g)
  if(length(g)>0) {
    print(names(mct)[g])
    rmnames <- c(rmnames, match(names(mct)[g], names(mct)))
  }
}
mct <- mct[,-rmnames]

setdiff(std_mct$PARAMETER, names(mct))
setdiff(names(mct), std_mct$PARAMETER)

# Create new TC014 MCT file
new_mctf <- file.path(fp, 'tc014_mct.csv')
write.csv(mct, file=new_mctf, row.names = FALSE, na="")
# Create JSON equivalent
jsonf <- file.path(fp, 'tc014_mct.json')
write_json(mct, path=jsonf)
