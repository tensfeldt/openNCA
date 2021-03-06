% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mrt_last.R
\name{mrt_last}
\alias{mrt_last}
\title{Mean residence time from the time of dosing to the last measurable concentration.}
\usage{
mrt_last(conc = NULL, time = NULL, method = 1, model = "M1",
  aucflag = NULL, dof = NULL, auclast = NULL, aumclast = NULL)
}
\arguments{
\item{method}{The method that will be used to calculate AUC (use either 1, 2, 3, or 4)\cr

\enumerate{
\item Linear-Log Trapazoidal Rule (default)
\item Linear Trapazoidal Rule
\item Log Trapazoidal Rule
\item Linear Up - Log DownTrapazoidal Rule
}
Note: check 'Methods' section below for more details \cr}

\item{data}{The model data that was generated by}

\item{sid}{The subject ID (either can be the id number or 'all' to get kel for all the subjects)}
}
\description{
Mean residence time from the time of dosing to the last measurable concentration.
}
\details{
\strong{Single Dose Equation:}
\strong{For Model M1}
\enumerate{
 \tabular{rl}{
  \tab \figure{aumc_XpctP1.png} \cr
 }
}
\strong{Steady-State Equation:}
\strong{For Model M1}
\enumerate{
 \tabular{rl}{
  \tab \figure{aumc_XpctP1.png} \cr
 }
}
\eqn{AUMClast = Aea under the first moment curve from zero time until the last measurable concentration} \cr
\eqn{AUClast = Area under the concentration versus time curve from zero time until the time (Tlast) of the last measurable concentration (CLASTi} \cr
\eqn{DOF = duration of infusion, used for constant infusion models} \cr
}
\section{Additional Details}{

}

\section{Returns}{

\strong{Dataset} \cr
\itemize{
 \item SID: subject identification number
 \item AUMC_PER: percentage of area under the curve
 \item METHOD: method used to calculate AUC
}
}

\examples{

}
\author{
\itemize{
 \item \strong{Rudraya Technical Team}
 \item website: \url{www.rudraya.com}
 \item email: \url{support@rudraya.com}
}
}
