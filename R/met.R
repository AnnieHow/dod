#' Download meteorological data
#'
#' This function downloads topographic data using
#' [oce::download.met].
#'
#' @param
#'
#' @param
#'
#' @param ... additional arguments passed to
#' [oce::download.met].
#'
#' @importFrom oce download.met
#'
#'
#' @return
#'
#' @export


dod.met <- function(...) {
  #topoFile <- oce::download.met(...)
  #topo <- read.topo(topoFile)
  return(met)
}
