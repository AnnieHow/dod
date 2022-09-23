#' General function for downloading data
#'
#' bfbwyfbfbwhfyeerwhbdbgdfgf
#'
#' @param type a character value indicating the type of data eg. `"ctd"`
#'
#' @author Annie Howard, Jaimie Harbin
#'
#' @export
download <- function(type, ...)
{
	message(type)
	if (type == "topo") {
		oce::download.topo(...)
	} else if (type == "met") {
		oce::download.met(...)
	} else {
		stop("dog")
	}
}


