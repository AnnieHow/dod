#' Download meteorological data
#'
#' This function downloads topographic data using
#' [oce::download.met].
#'
#' @param ... additional arguments passed to
#' [oce::download.met].
#'
#' @importFrom oce download.met
#'
#'
#' @return fix me
#'
#' @export

dod.met <- function(...) {
  oce::download.met(...)
}
