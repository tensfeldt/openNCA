% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_computation.R
\name{run_computation}
\alias{run_computation}
\title{Run Computation}
\usage{
run_computation(data = NULL, map = NULL, flag = NULL,
  parameterset = NULL)
}
\arguments{
\item{data}{The dataframe that contains the raw data}

\item{map}{The dataframe that contains the map data}

\item{flag}{The dataframe that contains the flag data}

\item{parameterset}{set to either "PARAMETERLIST" or "PARAMETERDISPLAYLIST"}
}
\description{
This function will compute all the relevant parameters for based specified fields provided via map argument.\cr
}
\details{
Please refer to \code{\link{mct_template}} for more details on what the 'map' argument dataframe should contain.
}
\section{Returns}{

\strong{List} \cr
\itemize{
 \item data_out: Calculated default/specified M1SS Parameters
 \item flag_data: Flag Data used during calculation of parameters
 \item est_data: Calculated Estimated Parameters
}
}

\examples{

}
\author{
\itemize{
 \item \strong{Thomas Tensfeldt & Rudraya Technical Team}
 \item website: \url{www.pfizer.com}
 \item email: \url{Thomas.G.Tensfeldt@pfizer.com}
}
}
