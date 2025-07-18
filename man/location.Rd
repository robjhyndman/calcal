% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aa_locale.R
\name{location}
\alias{location}
\title{Locations}
\usage{
location(
  latitude = numeric(),
  longitude = numeric(),
  elevation = numeric(),
  zone = numeric()
)
}
\arguments{
\item{latitude}{A numeric vector of latitudes}

\item{longitude}{A numeric vector of longitudes}

\item{elevation}{A numeric vector of elevations above sea level (in metres)}

\item{zone}{A numeric vector of time zones (in hours, relative to UTC)}
}
\value{
A location vector object
}
\description{
Create a location object. These are used for calculating the timing of
astronomical events such as sunrise and sunset.
}
\examples{
melbourne <- location(-37.8136, 144.9631, 31, 10)
sunrise("2025-01-01", melbourne)
}
