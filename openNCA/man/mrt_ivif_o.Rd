% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mrt_ivif_o.R
\name{mrt_ivif_o}
\alias{mrt_ivif_o}
\title{The mean residence time (MRT) extrapolated to infinity for a substance administered by extravascular
dosing, calculated using the observed value of the last non-zero concentration.}
\usage{
mrt_ivif_o(conc = NULL, time = NULL, method = 1, model = "M2",
  parameter = "SD", kelflag = NULL, aucflag = NULL, tau = NULL,
  dof = NULL, spanratio = NULL, kel = NULL, orig_conc = NULL,
  orig_time = NULL)
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

\item{model}{This is the model type}

\item{parameter}{This is either single dose (SD) or steady state (SS)}

\item{kelflag}{The kel exclude flag data (given in a numeric vector)}

\item{aucflag}{The auc exclude flag data (given in a numeric vector)}

\item{tau}{The time duration of dosing interval (numeric value)}
}
\description{
Note that the parameter naming convention for Release 2 of the Computaton Engine Specifications
referred to MRTEVIFO(i) as MRTOi for extravascular administration. This has been changed to MRTEVIFO(i)
with Release 3 to distinguish between intra- and extravascular administration.\cr
}
\details{
\strong{Single Dose Equation:}
\strong{For Model M2}
\enumerate{
 \tabular{rl}{
  \tab \figure{mrt_ivifo_1.png} \cr
 }
}
\strong{For Model M3}
\enumerate{
 \tabular{rl}{
  \tab \figure{mrt_ivifo_2.png} \cr
 }
}
\strong{Steady-State Equation:}
\strong{For Model M2}
\enumerate{
 \tabular{rl}{
  \tab \figure{mrt_ivifo_3.png} \cr
 }
}
\strong{For Model M3}
\enumerate{
 \tabular{rl}{
  \tab \figure{mrt_ivifo_4.png} \cr
 }
}
\eqn{AUMCINFO = Area under the first moment curve from zero time to infinity (Observed)} \cr
\eqn{AUCINFO = Area under the concentration versus time curve from zero time to infinity (Observed)} \cr
\eqn{AUMCTAUi = Area under the first moment curve from zero time until the end of the ith dosing interval} \cr
\eqn{AUCTAUi = Area under the concentration versus time curve from zero time until the end of the ith dosing interval} \cr
\eqn{TAUi = ith Dosing interval i refers to the value of NDOSEI} \cr
}
\section{Returns}{

\strong{Dataset} \cr
\itemize{
 \item MRTIVIFO: the mean residence time (MRT) extrapolated to infinity
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
