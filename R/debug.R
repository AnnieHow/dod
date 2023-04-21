#' Debug dod package functions
#'
#' This function is used throughout the `dod` package to provide
#' information on how processing is being done.
#'
#' @param ... content passed to [cat()].
#'
#' @param debug integer valaue indicating level of debugging. If
#' this exceeds zero, then `...` is passed to [cat()], for printing.
#'
#' @export
dodDebug <- function(debug=0, ...)
{
    if (debug > 0)
        cat(...)
}
