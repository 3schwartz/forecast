% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/for_VAR_Par.R
\name{for_VAR_Par}
\alias{for_VAR_Par}
\title{for_VAR_Par}
\usage{
for_VAR_Par(d, returnsVAR = Period, wL = windowLength)
}
\arguments{
\item{d}{iterator}

\item{returnsVAR}{xts of returns}

\item{wL}{window length}
}
\value{
Return positin of ne period look ahead
}
\description{
Backtest VAR. Function designed to be used for parallel
computing
}
