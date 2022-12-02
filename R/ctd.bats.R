#' Download CTD data from the Bermuda Atlantic Time Series (BATS)
#'
#' This function downloads CTD data from BATS.
#'
#' @param year a character value specifying the year of interest.
#'
#' @param index a boolean value indicating whether the index
#' should be downloaded.
#'
#' @param ID a character value specifying the file of interest.
#'
#' @param file character value giving the name to be used for
#' the downloaded file.
#'
#' @param read a boolean indicated weather or not the downloaded
#' file should be downloaded
#'
#' @template destdirTemplate
#'
#' @template debugTemplate
#'
#' @importFrom utils read.csv
#' @importFrom oce read.odf
#'
#' @return If `read` is FALSE, a downloaded file is returned. If `read`
#' is TRUE a data frame is returned.
#'
#' @examples
#' \dontrun{
#' library(dod)
#' test <- dod.ctd.bats(index=FALSE, ID=10001, read=TRUE)
#' head(test)
#' }
#' @export

dod.ctd.bats <- function(year, ID=NULL, index=FALSE, file=NULL, destdir=".", debug=0, read=FALSE)
{
    server <- "http://batsftp.bios.edu/BATS/ctd/ASCII/"
    if (is.null(ID)) {
        stop("Must provide an ID number greater than 10000")
    } else if (ID < 10000) {
        stop("Must provide an ID number greater than 10000")
    }
    if (index) {
        if (is.null(file)) {
            file <- paste0("b",ID, "_info.txt")
        } else {
            file=paste0(file, ".txt")
        }
        url <- paste0(server, "b",ID, "_info.txt")
        dodDebug(debug, "The url is equal to ", url, "\n")
        #browser()
        f <- dod.download(url, destdir=destdir, debug=debug, file=file, silent=TRUE)
        if (read) {
            namesInfo <- c("ID", "dateDeployed","dateRecovered","decimalDateDeployed","decimalDateRecovered",
                "decimalDayDeployed", "timeDeployed", "timeRecovered", "latitudeDeployed", "latitudeRecovered",
                "longitudeDeployed", "longitudeRecovered")
            t <- read.csv(f, sep="\t", header=FALSE, col.names= namesInfo)
            return(t)
        }
    } else {
        if (is.null(file)) {
            file <- paste0("b",ID, "_ctd.txt")
        } else {
            file=paste0(file, ".txt")
        }
        url <- paste0(server, "b",ID, "_ctd.txt")
        dodDebug(debug, oce::vectorShow(url))
        f <- dod.download(url=url, destdir=destdir, debug=debug, file=file)
        dodDebug(debug, oce::vectorShow(f))
        if (read) {
            names <- c("ID", "date","latitude", "longitude", "pressure","depth","temperature","conductivity", "salinity", "oxygen", "beamAttenuationCoefficient",
                "fluorescence", "PAR")
            f <- read.csv(f, sep="\t", header=FALSE, col.names= names)
            return(f)
        }
    }
}
