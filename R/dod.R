#' General function for downloading data
#'
#' For example if `type` is `"topo"`, [oce::download.topo()] is called.
#'
#' @param type a character value indicating the type of data eg. `"topo"`
#'
#' @author Annie Howard, Jaimie Harbin
#'
#' @export
dod <- function(type, ...)
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


