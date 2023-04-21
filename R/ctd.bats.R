#' Download CTD data from the Bermuda Atlantic Time Series program
#'
#' [dod.ctd.bats()] downloads CTD data from Bermuda Atlantic Time Series
#' (BATS) server, at <http://batsftp.bios.edu/BATS/ctd/ASCII/>.  Note that this
#' server does not provide an index, so users will need to visit the website
#' to determine IDs of interest, unless they already know them. Another problem
#' is that the data are in a nonstandard format that the `oce` package cannot
#' read, so a user may wish to write code to reframe the data as shown
#' in the \sQuote{Examples} section.
#'
#' @param ID an integer specifying the ID of the file of interest.
#' This gets expanded to e.g. URL/b(ID)_ctd.txt or URL/b(ID)_ctd_QC.txt
#' if `info` is FALSE, or to URL/b(ID)_info.txt otherwise.
#'
#' @param info a logical value.  If `info` is FALSE, which is the default, then
#' [dod.ctd.bats()] downloads the actual CTD data.  Altenatively, it downloads
#' a file holding information about sampling location, etc.
#'
#' @param file character value giving the name to be used for the downloaded
#' file.  If `file` is NULL, which is the default, then the filename on the
#' server is used.
#'
#' @template destdirTemplate
#'
#' @template ageTemplate
#'
#' @template debugTemplate
#'
#' @importFrom utils read.csv
#'
#' @return [dod.ctd.bats()] returns the full pathname of the
#' downloaded file.
#'
#' @examples
#'\dontrun{
#' library(dod)
#' # Read info file
#' infoFile <- dod.ctd.bats(ID=10001, info=TRUE)
#' info.names <- c("ID",
#'     "dateDeployed","dateRecovered","decimalDateDeployed","decimalDateRecovered",
#'     "decimalDayDeployed", "timeDeployed", "timeRecovered", "latitudeDeployed",
#'     "latitudeRecovered", "longitudeDeployed", "longitudeRecovered")
#' info <- read.delim(infoFile, sep="\t", header=FALSE, col.names=info.names)
#'
#' # Read data file
#' dataFile <- dod.ctd.bats(ID=10001)
#' dataNames <- c("ID", "date","latitude", "longitude",
#'     "pressure","depth","temperature","conductivity", "salinity", "oxygen",
#'     "beamAttenuationCoefficient", "fluorescence", "PAR")
#' data <- read.delim(dataFile, sep="\t", header=FALSE, col.names=dataNames)
#'
#' # Use oce to construct a basic CTD object (using just some of the data),
#' # and then to summarize and plot it.  Note that longitude in these files
#' # is in degrees west, so they must be negated for use in oce.
#' library(oce)
#' time <- as.POSIXct(paste(info$dateDeployed, info$timeDeployed),format="%Y%m%d %H%M")
#' ctd <- as.ctd(salinity=data$salinity, temperature=data$temperature,
#'     pressure=data$pressure, latitude=data$latitude[1],
#'     longitude=-data$longitude[1], time=time)
#' # Add some extra things that are in at least some file. Units can also be added,
#' # if known.
#' for (item in c("oxygen", "beamAttenuationCoefficient", "fluorescence", "PAR")) {
#'     ctd <- oceSetData(ctd, item, data[[item]])
#' }
#' summary(ctd)
#' plot(ctd)
#'}
#'
#' @family functions that download CTD data
#'
#' @export
dod.ctd.bats <- function(ID, info=FALSE, file=NULL, destdir=".", age=0, debug=0)
{
    server <- "http://batsftp.bios.edu/BATS/ctd/ASCII/"
    if (missing(ID))
        stop("ID must be supplied")
    owarn <- options("warn")$warn
    options(warn=-1)
    ID <- as.numeric(ID)
    options(warn=owarn)
    if (is.na(ID))
        stop("ID must be a numeric value, convertible to an integer")
    ID <- as.integer(ID)
    if (ID < 10000)
        stop("ID must exceed 10000, but it is ", ID)
    if (info) {
        if (is.null(file))
            file <- paste0("b", ID, "_info.txt")
        url <- paste0(server, "b", ID, "_info.txt")
        filename <- dod.download(url, destdir=destdir, file=file, age=age, debug=debug-1)
        return(filename)
    } else {
        if (is.null(file))
            file <- paste0("b",ID, "_ctd.txt")
        url <- paste0(server, "b",ID, "_ctd.txt")
        # Try e.g. b50056_ctd.txt and if that fails, try b50056_ctd_QC.txt
        filename <- try(dod.download(url=url, destdir=destdir, file=file, age=age, debug=debug-1),
            silent=TRUE)
        if (inherits(filename, "try-error")) {
            url2 <- gsub(".txt", "_QC.txt", url)
            filename <- try(dod.download(url=url2, destdir=destdir, file=file, age=age, debug=debug-1),
                silent=TRUE)
            if (inherits(filename, "try-error"))
                stop("Unable to download \"", url, "\" or \"", url2, "\" to \"", filename, "\"")
        }
        return(filename)
        #if (read) {
        #    names <- c("ID", "date","latitude", "longitude", "pressure","depth","temperature","conductivity", "salinity", "oxygen", "beamAttenuationCoefficient",
        #        "fluorescence", "PAR")
        #    return(read.csv(filename, sep="\t", header=FALSE, col.names= names))
        #}
    }
}
