#' Download ctd data
#'
#' This function downloads ctd data from various programs.
#'
#' |       **Program Name**       | **Program** | **Index** |   **ID**   |
#' |                        :---- |       :---- |     :---- |      :---- |
#' |Bedford Basin Mooring Project |      `BBMP` |       Yes | From index |
#' |Bermuda Atlantic Time Series  |      `BATS` |       Yes |  Cruise ID |
#'
#' @param program a character value specifying the oceanographic
#' program from which the data derive see \sQuote{Details}.
#'
#' @param year a character value specifying the year of interest.
#'
#' @param index a boolean value indicating whether the index
#' should be downloaded.
#'
#' @param ID a character value specifying the file of interest,
#' see \sQuote{Details}.
#'
#' @param destdir a character value indicating the directory
#' in which to store downloaded files.
#'
#' @param debug an integer value indicating level of debugging.
#' If this is less than 1, no debugging is done. Otherwise,
#' the function will print debugging information.
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
#' i <- dod.ctd("BBMP", year=2022, index=TRUE)
#' f <- dod.ctd("BBMP", year=2022, ID=i$file[1], destdir=destdir)
#' library(oce)
#' ctd <- read.ctd(f)
#' plot(ctd)
#'}
#'
#' @export

dod.ctd <- function(program, year, ID=NULL, index=FALSE, destdir=".", debug=0)
{
    if (program == "?") {
        stop("Must provide a program argument, possibilities include: BBMP, BATS")
    }
    if (!is.logical(index)) {
        stop("'index' must be a logical value")
    }
    if (program == "BBMP") {
        server <- "ftp://ftp.dfo-mpo.gc.ca/BIOWebMaster/BBMP/ODF"
        if (missing(year))
            stop("must give 'year'")
        server <- paste0(server, "/", year)

        if (debug)
            cat(oce::vectorShow(server))
        if (index) {
            file <- paste0(year, "667ODFSUMMARY.tsv")
            if (debug)
                cat(oce::vectorShow(file))
            url <- paste0(server, "/", file)
            if (debug)
                cat(oce::vectorShow(url))
            dod.download(url, file, destdir)
            if (debug)
                cat(oce::vectorShow(file))
            url <- paste0(server, "/", file)
            return(read.csv(file, header=FALSE, skip=3, col.names=c("file", "time")))
        } else {
            url <- paste0(server, "/", ID)
            if (debug) {
                cat(oce::vectorShow(url))
            }
            if (debug) {
                cat(oce::vectorShow(ID))
            }
            return(dod.download(url, ID, destdir=destdir, debug=debug))
            #t <- read.odf(ID)
            #return(t)
            #return(ID)
        }
    }
    if (program == "BATS") {
        if (debug) {
            message("The program is equal to ", program)
        }
        server <- "http://batsftp.bios.edu/BATS/ctd/ASCII/"
        if (index) {
            if (is.null(ID) | ID < 10000)
                stop("Must provide an ID number greater than 10000")

            url <- paste0(server, "b",ID, "_info.txt")
            if (debug) {
                message("The url is equal to ",url)
            }
            f <- dod.download(url, ID, destdir=destdir, debug=debug)
            namesInfo <- c("ID", "dateDeployed","dateRecovered","decimalDateDeployed","decimalDateRecovered",
                "decimalDayDeployed", "timeDeployed", "timeRecovered", "latitudeDeployed", "latitudeRecovered",
                "longitudeDeployed", "longitudeRecovered")
            t <- read.csv(ID, sep="\t", header=FALSE, col.names= namesInfo)
            return(t)
        }
        else {
            if (debug) {
                message("The ID type is ",ID)
            }

            url <- paste0(server, "b",ID, "_ctd.txt")

            if (debug) {
                cat(oce::vectorShow(url))
            }

            f <- dod.download(url, ID, destdir=destdir, debug=debug)

            if (debug) {
                cat(oce::vectorShow(f))
            }

            names <- c("ID", "date","latitude", "longitude", "pressure","depth","temperature","conductivity", "salinity", "oxygen", "beamAttenuationCoefficient",
                "fluorescence", "PAR")
            #t <- read.csv(ID, sep="\t", header=FALSE, col.names= names)

            #return(t)
            return(ID)
        }


    }
}
