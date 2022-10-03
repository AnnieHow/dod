#' Download topographic data
#'
#' This function downloads topographic data using
#' [oce::download.topo()].
#'
#' @param ... arguments passed to
#' [oce::download.topo()].
#'
#' @importFrom oce download.topo
#'
#' @return String indicating the full pathname to the downloaded file.
#'
#' @export

dod.topo <- function(...)
{
  oce::download.topo(...)
}

