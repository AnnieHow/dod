#' Download with diagnostics
#'
#' @param url hello
#'
#' @param file hello
#'
#' @param destdir hello
#'
#'
#' @export
dod.download <- function(url, file, destdir)
{
    message(file)
    fullfile <- paste0(destdir, "/", file)
    message(fullfile)
    t <- try(download.file(url, fullfile), silent=TRUE);
    if (inherits(t,"try-error")) {
        stop("fix me later")
    }
    file
}
