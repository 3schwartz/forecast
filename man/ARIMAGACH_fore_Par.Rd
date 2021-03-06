% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ARIMAGACH_fore_Par.R
\name{ARIMAGACH_fore_Par}
\alias{ARIMAGACH_fore_Par}
\title{ARIMAGACH_fore_Par}
\usage{
ARIMAGACH_fore_Par(Period = Period, foreLength = foreLength,
  windowLength = windowLength, ARIMAfit = TRUE, outDir = "./output/",
  version = "v1")
}
\arguments{
\item{Period}{xts object of log returns (one-dim)}

\item{foreLength}{length of forecast}

\item{windowLength}{length of window used for fitting}

\item{ARIMAfit}{If true, then an ARIMA will be fit for each iterations. Else a ARMA(2,1) is used}

\item{outDir}{Output dir}

\item{version}{Version  of dataset (if run multipe times and want to save each data)}
}
\value{
Write to files position each day and its cumulative return
}
\description{
Forecast one-day ahead forecast using ARIMA-GARCH.
}
