% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buoy.R
\name{dod.buoy}
\alias{dod.buoy}
\title{Download buoy data}
\usage{
dod.buoy(program, ID = NULL, destdir = ".", debug = 0)
}
\arguments{
\item{program}{argument specifying the desired oceanographic
program to download buoy data from. Options include \code{MEDS}, etc.}

\item{ID}{For \code{MEDS} this argument is either a location name
or ID number.}

\item{destdir}{character value indicating the directory
in which to store downloaded files.}

\item{debug}{integer value indicating level of debugging.
If this is less than 1, no debugging is done. Otherwise,
some functions will print debugging information.}
}
\value{
If \code{index} is TRUE, and \code{program} is \code{"BBMP"} or \code{"BATS"},
return a data frame.  Otherwise, return the name of the downloaded file.
}
\description{
This function downloads buoy data from various programs
including...
}
