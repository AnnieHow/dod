#' Debug dod package functions
#'
#' This function is used throughout the `dod` package to catch
#' bugs and provide assistance to users.
#'
#' @param debug integer value indicating level of debugging.
#' If this is less than 1, no debugging is done. Otherwise,
#' some functions will print debugging information.
#'
#' @param ... FILL IN not sure if this pulling from another
#' package or what.
#'
#' @export
dodDebug <- function(debug=0, ...)
{
    if (debug)
        cat(...)
}
