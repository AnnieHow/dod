#' Download meteorological data
#'
#' This function downloads topographic data using
#' [oce::download.met()].
#'
#' @param ... arguments passed to
#' [oce::download.met()].
#'
#' @importFrom oce download.met
#'
#' @return String indicating the full pathname to the downloaded file.
#'
#' @export

dod.met <- function(...)
{
    oce::download.met(...)
}
