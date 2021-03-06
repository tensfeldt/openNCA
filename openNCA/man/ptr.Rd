% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ptr.R
\name{ptr}
\alias{ptr}
\title{Peak/Trough (CMAXi to CMINi) Ratio over the dosing interval i}
\usage{
ptr(cmax = NULL, cmin = NULL)
}
\arguments{
\item{cmax}{The maxium observed concentration data (given in a vector form)}

\item{cmin}{The minimum observed concentration data (given in a vector form)}
}
\description{
Peak/Trough (CMAXi to CMINi) Ratio over the dosing interval i
}
\details{
Steady-State Equation
\enumerate{
 \tabular{rl}{
  \tab \figure{ptr.png} \cr
 }
} 
\eqn{CMAXi = Maximum observed concentration} \cr   
\eqn{CMINi = Minimum observed concentration} \cr
}
\section{Returns}{

\strong{Value} \cr 
\itemize{
 \item PTR: peak/trough ratio
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
