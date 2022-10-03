#' Download Advanced Microwave Scanning Radiometer data
#'
#' This function downloads topographic data using
#' [oce::download.amsr()].
#'
#' @param ... arguments passed to
#' [oce::download.amsr()].
#'
#' @importFrom oce download.amsr
#'
#' @return String indicating the full pathname to the downloaded file.
#'
#' @export

dod.amsr <- function(...)
{
    oce::download.amsr(...)
}
