#' Download topographic data
#'
#' This function downloads topographic data using
#' [oce::download.topo].
#'
#' @param west,east numeric values for the limits
#' of the data-selection box, in degrees. These
#' are converted to the -180 to 180 degree notation,
#' if needed. Then, west is rounded down to the nearest
#' 1/100th degree, and east is rounded up to the the
#' nearest 1/100th degree. The results of these
#' operations are used in constructing the query for
#' the NOAA data server.
#'
#' @param south,north latitude limits, treated in
#' a way that corresponds to the longitude limits.
#'
#' @param ... additional arguments passed to
#' [oce::download.topo].
#'
#' @importFrom oce download.topo
#' @importFrom oce read.topo
#'
#' @return An oce topo object.
#'
#' @export

dod.topo <- function(west=-66, east=-60, south=43, north=47, ...) {
  topoFile <- oce::download.topo(west=west, east=east, north=north, south=south,...)
  topo <- read.topo(topoFile)
  return(topo)
}

