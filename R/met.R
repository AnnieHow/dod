#' Download meteorological data
#'
#' Get meteorological data from an Environment Canada website, using
#' [oce::download.met()].
#'
#' @param ... arguments passed to [oce::download.met()].
#'
#' @importFrom oce download.met
#'
#' @return [dod.met] returns a character value holding the full pathname
#' of the downloaded file.
#'
#' @export

dod.met <- function(...)
{
    oce::download.met(...)
}
