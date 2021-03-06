% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculations.R
\name{calc_dens}
\alias{calc_dens}
\title{Calculate 1D density of age data}
\usage{
calc_dens(dat, bw = 30, type = "kde", age_range = c(0, 4560))
}
\arguments{
\item{dat}{data.frame containing at least ages and percentage of discordancy}

\item{bw}{Bandwidth}

\item{type}{Type to calculate 'kde': proper KDE; 'pdd': detrital zircon PDD}

\item{age_range}{Range over which to calculate density}
}
\value{
Density
}
\description{
Calculate the 1d density of U-Pb age data using KDE or PDD.
}
