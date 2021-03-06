% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auc_log.R
\name{auc_log}
\alias{auc_log}
\title{Log Trapezoidal Area Under the Concentration-Time Curve (AUC) Calculation Method}
\usage{
auc_log(conc = NULL, time = NULL, exflag = NULL)
}
\arguments{
\item{conc}{The concentration data (given in a vector form)}

\item{time}{The time data (given in a vector form)}

\item{exflag}{The exclude flag data (given in a numeric vector)}
}
\description{
The logarithmic method is used for the entire profile. If Ci or Ci+1 is 0 then the
linear trapezoidal rule is used
}
