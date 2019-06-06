# -----------------------------------------------------------------------------
# DT-wrapper.R
# 2019-06-06/TGT/ Wrapper script that users can utilize to start generating a new ad hoc Data Transformation Script
# -----------------------------------------------------------------------------

# Library Packages
library(readr)
library(dplyr)
library(reshape2)
library(jsonlite)

# -----------------------------------------------------------------------------
# Process Command Line Arguments to load the params named arguments list/vector
### print params
sink("./OUTPUT/argumentprocessing.txt")

verbose = FALSE
args = commandArgs(trailingOnly=TRUE)
params = c()

for (i in 1:length(args)) {
  if (grep("=", args[i]) > 0) {
    matches <- gregexpr("((?:[^=\"]|\"[^\"]*\")+)", args[i])
    parts <- unlist(regmatches(args[i], matches))
    params[parts[1]] <- parts[2]
  }
}
sink()

### Verbose status printing control
if(!is.na(params["VERBOSE"])) { verbose <- as.logical(params["VERBOSE"]) }

### print params
if(verbose) { cat('input arguments: \n'); print(as.list(params)) }

# -----------------------------------------------------------------------------
# name of INPUT1 file
path.in <- "./"
f <- params["INPUT1"]
dirn.out <- "OUTPUT"
path.out <- paste0(params[dirn.out], "/")

# -----------------------------------------------------------------------------
# read contents of INPUT1 file into the tibble "d"
d <- read_csv(file=f, col_names=TRUE)

# -----------------------------------------------------------------------------
# <<Users should initiate their code development below this point>>
# -----------------------------------------------------------------------------

### Remove Profiles with only 1 record
x <- table(d$SDEID, exclude=c())
x <- x[x<2]
k <- is.element(d$SDEID, names(x))
d <- d[k,]

# -----------------------------------------------------------------------------
# Output file
outputFilename <- paste(path.out, "OUTPUT", i, sep="")
write.csv(d, file=outputFilename, row.names = FALSE, na="")
