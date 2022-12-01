#' Download CTD data from the Bermuda Atlantic Time Series (BATS)
#'
#' This function downloads CTD data from BATS.
#'
#' @param ID a character value consisting of 8 digits where
#' the first two are an ocean basin initial (at, pa or in), the
#' next four are the year of interest and the last two are the
#' month of interest.
#'
#' @param file character value giving the name to be used for
#' the downloaded file.
#'
#' @template destdirTemplate
#'
#' @template debugTemplate
#'
#' @importFrom utils read.csv
#' @importFrom oce read.odf
#'
#' @return Return a data frame.
#'
#' @export

dod.ctd.gtspp <- function(ID=NULL, file=NULL, destdir=".", debug=0)
{
    server <- "https://www.ncei.noaa.gov/data/oceans/gtspp/bestcopy/meds_ascii/"
        if (is.null(ID)) {
            stop("must give an ID")
        }
        server <- paste0(server)
        if (is.null(file)) {
            file <- paste0(ID,".gz")
        } else {
            file=paste0(file, ".gz")
        }
        dodDebug(debug, oce::vectorShow(server))
        dodDebug(debug, oce::vectorShow(destdir))
        return(dod.download(url=server, file=file, destdir=destdir, silent=TRUE,debug=debug))

}



