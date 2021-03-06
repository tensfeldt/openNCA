% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auc_XbpctP.R
\name{auc_XbpctP}
\alias{auc_XbpctP}
\title{Percentage of AUCINFP obtained by back extrapolation from the first non-zero concentration time point}
\usage{
auc_XbpctP(conc = NULL, time = NULL, method = 2, kelflag = NULL,
  aucflag = NULL)
}
\arguments{
\item{conc}{The concentration data (given in a vector form)}

\item{time}{The time data (given in a vector form)}

\item{method}{The method that will be used to calculate AUC (use either 1, 2, 3, or 4)\cr
\enumerate{
\item Linear-Log Trapazoidal Rule (default)
\item Linear Trapazoidal Rule
\item Log Trapazoidal Rule
\item Linear Up - Log DownTrapazoidal Rule
}
Note: check 'Methods' section below for more details \cr}
}
\description{
Percentage of AUCINFP obtained by back extrapolation from the first non-zero concentration time point.\cr
}
\details{
\strong{Equation}
\enumerate{
 \tabular{rl}{
  \tab \figure{auc_XpctP1.png} \cr
 }
}
}
\section{Additional Details}{

}

\section{Note}{

\strong{auc_inf_p}: Refer to \code{\link{auc_inf_p}} for more details \cr
}

\section{Returns}{

\strong{Value} \cr
\itemize{
 \item AUC_PER: percentage of area under the curve
}
}

\examples{
##########
## Data ##
#################################
##  SID  ##  TIME  ##   CONC   ##
#################################
##   30  ##    0   ##   2.89   ##
##   30  ##    1   ##   2.49   ##
##   30  ##    2   ##   2.47   ##
##   30  ##    3   ##   2.38   ##
##   30  ##    4   ##   2.32   ##
##   30  ##    5   ##   2.28   ##
#################################

#data <- data.frame(
#    SID = ...,
#    TIME = ...,
#    RESULT = ...
#)
#Same data as above, just represented as a dataframe

#auc_XpctP()
#Error in auc_all: 'conc' and 'time' vectors are NULL

conc_vector <- data$CONC
time_vector <- data$TIME

auc_XpctP(conc = conc_vector, time = time_vector)
#81.59327

############
## Data 2 ##
#################################
##  SID  ##  TIME  ##   CONC   ##
#################################
##   31  ##    0   ##      0   ##
##   31  ##    1   ##      0   ##
##   31  ##    2   ##      0   ##
#################################

#data2 <- data.frame(
#    SID = ...,
#    TIME = ...,
#    RESULT = ...
#)
#Same data as above, just represented as a dataframe

conc_vector <- data2$CONC
time_vector <- data2$TIME

auc_XpctP(conc = conc_vector, time = time_vector)
#0

}
\author{
\itemize{
 \item \strong{Thomas Tensfeldt, Pfizer}
 \item website: \url{www.pfizer.com}
 \item email: \url{thomas.g.tensfeldt@pfizer.com}
}
}
