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
#' @template destdirTemplate
#'
#' @template ageTemplate
#'
#' @param silent logical value passed to [download.file()], which
#' does the downloading.  The default, TRUE, indicates not to show
#' a progress indicator.
#'
#' @template debugTemplate
#'
#' @return `dod.download` returns a character value holding the full
#' name of the file, including the path to `destdir`.
#'
#' @importFrom utils download.file
#'
#' @export
dod.download <- function(url=NULL, file=NULL, destdir=".", age=0, silent=TRUE, debug=0)
{
    if (is.null(url))
        stop("url must not be NULL")
    if (!dir.exists(destdir))
        stop("destdir \"", destdir, "\" does not exist; please create it first.")
    dodDebug(debug, "    dod.download(",
        "url=\"", url, "\", ",
        "file=\"", file, "\", ",
        "destdir=\"", destdir, "\", ",
        "age=", age, ", silent=", silent, ", debug=", debug, ")\n    {\n", sep="")
    filepath <- file.path(destdir, file)
    if (file.exists(filepath)) {
        mtime <- file.info(filepath)$mtime
        fileAge <- (as.numeric(Sys.time()) - as.numeric(mtime))/86400
        fileAgeMsg <- if (fileAge < 1)
            sprintf("%.2f hours", fileAge*24)
        else
            sprintf("%.2f days", fileAge)
        if (fileAge < age) {
            dodDebug(debug,"        existing file is ", fileAgeMsg,
                " so it will not be downloaded\n    } # dod.download()\n", sep="")
            return(filepath)
        } else {
            dodDebug(debug, "      existing file age is ", fileAgeMsg, " old, so must download a newer version\n", sep="")
        }
    } else {
        dodDebug(debug, "      new file, so must download\n")
    }
    owarn <- options("warn")$warn
    options(warn=-1)
    t <- try(download.file(url=url, destfile=filepath, quiet=silent), silent=silent)
    options(warn=owarn)
    if (inherits(t, "try-error"))
        stop("An error occured when trying to download \"", url, "\"")
    dodDebug(debug, "    } # dod.download()\n")
    filepath
}
