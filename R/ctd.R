#' Download CTD data
#'
#' This function downloads CTD data from various programs.
#'
#' |       **Project**       | **Program** | **Index** |   **ID**   |
#' |                        :---- |       :---- |     :---- |      :---- |
#' |Bedford Basin Mooring Project |      `BBMP` |       Yes | From index |
#' |Bermuda Atlantic Time Series  |      `BATS` |       Yes |  Cruise ID |
#'
#' @param program a character value specifying the oceanographic
#' program from which the data derive (see \sQuote{Details}).
#'
#' @param year a character value specifying the year of interest.
#'
#' @param index a boolean value indicating whether the index
#' should be downloaded.
#'
#' @param ID a character value specifying the file of interest
#' (see \sQuote{Details}).
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
#' @return If `index` is TRUE, and `program` is `"BBMP"` or `"BATS"`,
#' return a data frame.  Otherwise, return the name of the downloaded file.
#'
#' @examples
#'\dontrun{
#' # Download the first file of year 2022.
#' destdir <- "~/data/ctd"
#' i <- dod.ctd("BBMP", year=2022, index=TRUE, file="index.txt")
#' f <- dod.ctd("BBMP", year=2022, ID=i$file[1], destdir=destdir, file="bbmp.txt")
#' library(oce)
#' ctd <- read.ctd(f)
#' plot(ctd)
#'}
#'
#' @export

dod.ctd <- function(program, year, ID=NULL, index=FALSE, file=NULL, destdir=".", debug=0)
{
    if (program == "?") {
        stop("Must provide a program argument, possibilities include: BBMP, BATS")
    }
    if (!is.logical(index)) {
        stop("'index' must be a logical value")
    }
    if (is.null(file)) {
        stop("Must provide file argument with an extension")
    }
    if (program == "BBMP") {
        server <- "ftp://ftp.dfo-mpo.gc.ca/BIOWebMaster/BBMP/ODF"
        if (missing(year))
            stop("must give 'year'")
        server <- paste0(server, "/", year)
        dodDebug(debug, oce::vectorShow(server))
        if (index) {
            file <- paste0(year, "667ODFSUMMARY.tsv")
            dodDebug(debug, oce::vectorShow(file))
            url <- paste0(server, "/", file)
            dodDebug(debug, oce::vectorShow(url))
            dod.download(url, file, destdir)
            dodDebug(debug, oce::vectorShow(file))
            url <- paste0(server, "/", file)
            message(file)
            file <- paste0(destdir,"/",file)
            message(file)
            return(read.csv(file, header=FALSE, skip=3, col.names=c("file", "time")))
        } else {
            if (is.null(ID)) {
                stop("Must provide an ID from the index")
            }
            url <- paste0(server, "/", ID)
            dodDebug(debug, oce::vectorShow(url))
            dodDebug(debug, oce::vectorShow(ID))
            return(dod.download(url=url, file=file, destdir=destdir, silent=TRUE,debug=debug))
        }
    }
    if (program == "BATS") {
        dodDebug(debug, "The program is equal to ", program, "\n")
        server <- "http://batsftp.bios.edu/BATS/ctd/ASCII/"
        if (is.null(ID)) {
            stop("Must provide an ID number greater than 10000")
        } else if (ID < 10000) {
            stop("Must provide an ID number greater than 10000")
        }
        if (index) {
            url <- paste0(server, "b",ID, "_info.txt")
            dodDebug(debug, "The url is equal to ", url, "\n")
            #browser()
            f <- dod.download(url, destdir=destdir, debug=debug, file=file, silent=TRUE)
            namesInfo <- c("ID", "dateDeployed","dateRecovered","decimalDateDeployed","decimalDateRecovered",
                "decimalDayDeployed", "timeDeployed", "timeRecovered", "latitudeDeployed", "latitudeRecovered",
                "longitudeDeployed", "longitudeRecovered")
            t <- read.csv(f, sep="\t", header=FALSE, col.names= namesInfo)
            return(t)
        } else {
            url <- paste0(server, "b",ID, "_ctd.txt")
            dodDebug(debug, oce::vectorShow(url))
            f <- dod.download(url, ID, destdir=destdir, debug=debug, file=file)
            dodDebug(debug, oce::vectorShow(f))
            names <- c("ID", "date","latitude", "longitude", "pressure","depth","temperature","conductivity", "salinity", "oxygen", "beamAttenuationCoefficient",
                "fluorescence", "PAR")
            t <- read.csv(f, sep="\t", header=FALSE, col.names= names)
            return(t)
        }
    }
}
