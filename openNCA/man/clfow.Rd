% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clfow.R
\name{clfow}
\alias{clfow}
\title{The total body clearance}
\usage{
clfow(clfo = NULL, normbs = NULL)
}
\arguments{
\item{clfo}{The AUCINFO data (numeric value)}

\item{normbs}{The dose data (numeric value)}
}
\description{
The total body clearance for extravascular administration divided by the fraction of dose absorbed,
calculated using the observed value of the last non-zero concentration, divided by the weight. \cr
}
\details{
\strong{Model M1 (SD) Derived}
\enumerate{
 \tabular{rl}{
  \tab \figure{clfow.png} \cr
 }
}
\eqn{CLFO = Apparent clearance (observed) of drug for extravascular routes of administration} \cr
\eqn{NORMBS = BW = Body weight} \cr
}
\section{Returns}{

\strong{Value} \cr
\itemize{
 \item CLFOW: The total body clearance
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
