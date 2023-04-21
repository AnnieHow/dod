#' Download coastline data
#'
#' This function downloads topographic data using
#' [oce::download.coastline()].
#'
#' @param ... arguments passed to
#' [oce::download.coastline()].
#'
#' @importFrom oce download.coastline
#'
#' @return String indicating the full pathname to the downloaded file.
#'
#' @export

dod.coastline <- function(...)
{
    oce::download.coastline(...)
}
