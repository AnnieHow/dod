#' Download a file with error checking
#'
#' `dod.download()` is mainly intended for use by other functions
#' in the `dod` package.
#'
#' @param url character value giving the web address of a file to be
#' downloaded. This has different values for different file types.
#'
#' @param file character value giving the name to be used for
#' the downloaded file.
#'
#' @param destdir character value indicating the directory
#' in which to store the downloaded file.
#'
#' @param debug integer giving the level of debugging. If
#' this exceeds zero, then some information is printed about
#' the processing.
#'
#' @param silent logical value passed to [download.file()], which
#' does the downloading.  The default, TRUE, indicates not to show
#' a progress indicator.
#'
#' @return `dod.download` returns a character value holding the full
#' name of the file, including the path to `destdir`.
#'
#' @importFrom utils download.file
#'
#' @export
dod.download <- function(url=NULL, file=NULL, destdir=".", silent=TRUE, debug=0)
{
    if (is.null(url))
        stop("url must not be NULL")
    if (!dir.exists(destdir))
        stop("destdir \"", destdir, "\" does not exist; please create it first.")
    filepath <- file.path(destdir, file)
    #browser()
    owarn <- options("warn")$warn
    options(warn=-1)
    dodDebug(debug,"About to retrieve\n    \"", url, "\"\nand save it locally as\n    \"", filepath, "\"\n", sep="")
    t <- try(download.file(url=url, destfile=filepath, quiet=silent), silent=silent)
    options(warn=owarn)
    if (inherits(t, "try-error"))
        stop("Unable to download \"", url, "\" to \"", filepath, "\"")
    filepath
}
