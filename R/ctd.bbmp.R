#' Download CTD data from the Bedford Basin Mooring Project (BBMP)
#'
#' This function downloads CTD data from BBMP.
#'
#' @param year a character value specifying the year of interest.
#'
#' @param index a boolean value indicating whether the index
#' should be downloaded.
#'
#' @param ID a character value specifying the file of interest
#' that is copied from the index.
#'
#' @param file character value giving the name to be used for
#' the downloaded file. This does include the extension.
#'
#' @template destdirTemplate
#'
#' @template debugTemplate
#'
#' @importFrom utils read.csv
#' @importFrom oce read.odf
#'
#' @return If `index` is TRUE, return a data frame. Otherwise,
#' return the name of the downloaded file.
#'
#' @examples
#'\dontrun{
#' # Download the first file of year 2022.
#' library(dod)
#' library(oce)
#' destdir <- "~/data/ctd"
#' i <- dod.ctd("BBMP", year=2022, index=TRUE, file="index.txt")
#' f <- dod.ctd("BBMP", year=2022, ID=i$file[1], destdir=destdir, file="bbmp.txt")
#' ctd <- read.ctd(f)
#' plot(ctd)
#'}
#'
#' @export

dod.ctd.bbmp <- function(year, ID=NULL, index=FALSE, file=NULL, destdir=".", debug=0)
{
    server <- "ftp://ftp.dfo-mpo.gc.ca/BIOWebMaster/BBMP/ODF"
    if (missing(year))
        stop("must give 'year'")
    server <- paste0(server, "/", year)
    dodDebug(debug, oce::vectorShow(server))
    if (index) {
        if (is.null(file)) {
            file <- paste0(year, "667ODFSUMMARY.tsv")
        } else {
            file=file
        }
        dodDebug(debug, oce::vectorShow(file))
        url <- paste0(server, "/",  paste0(year, "667ODFSUMMARY.tsv"))
        dodDebug(debug, oce::vectorShow(url))
        dod.download(url, file, destdir)
        dodDebug(debug, oce::vectorShow(file))
        url <- paste0(server, "/", file)
        file <- paste0(destdir,"/",file)
        return(read.csv(file, header=FALSE, skip=3, col.names=c("file", "time")))
    } else {
        if (is.null(ID)) {
            stop("Must provide an ID from the index")
        }
        if (is.null(file)) {
            file <- paste0(ID, ".txt", sep="")
        } else {
            file=file
        }
        url <- paste0(server, "/", ID)
        dodDebug(debug, oce::vectorShow(url))
        dodDebug(debug, oce::vectorShow(ID))
        return(dod.download(url=url, file=file, destdir=destdir, silent=TRUE,debug=debug))
    }
}
