#' Download CTD data from the Bedford Basin Mooring Project
#'
#' [dod.ctd.bbmp] can retrieve both index files and data files
#' from the Bedford Basin Mooring Project (BBMP). Since the naming
#' convention of the data files may alter from year to year, it is
#' important to start by downloading an index first, to ascertain
#' the name of a file of interest; see \sQuote{Examples}.
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
#' the downloaded file.
#'
#' @template destdirTemplate
#'
#' @template debugTemplate
#'
#' @importFrom utils read.csv
## @importFrom oce read.oce
#'
#' @return [dod.ctd.bbmp] returns a character value naming the file that
#' was retrieved. This may either be an index file or a data file;
#' see \sQuote{Examples} for a typical workflow in which an index is
#' retrieved and then used as a guide to the name of a data file of
#' interest.
#'
#' @examples
#'\dontrun{
#' # Download the first file of year 2022.
#' library(dod)
#' # Download the index
#' indexFile <- dod.ctd("BBMP", year=2022, index=TRUE, file="index_bbmp_2022")
#' # Read the index (altering this call if the file format changes)
#' index <- read.csv(indexFile, header=FALSE, col.names=c("file","time"), skip=3)
#' # Download the first file in the index
#' ctdFile <- dod.ctd("BBMP", year=2022, ID=index$file[1], file="bbmp_2022_001")
#' # Use oce to read, summarize and plot the data.
#' library(oce)
#' ctd <- read.ctd(ctdFile)
#' summary(ctd)
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
            file=paste0(file, ".tsv")
        }
        dodDebug(debug, oce::vectorShow(file))
        url <- paste0(server, "/",  paste0(year, "667ODFSUMMARY.tsv"))
        dodDebug(debug, oce::vectorShow(url))
        dod.download(url, file, destdir)
        dodDebug(debug, oce::vectorShow(file))
        url <- paste0(server, "/", file)
        file <- paste0(destdir,"/",file)
        return(file)
        #if (read) {
        #return(read.csv(file, header=FALSE, skip=3, col.names=c("file", "time")))
        #}
    } else {
        if (is.null(ID))
            stop("Must provide an ID from the index")
        if (is.null(file))
            if (grepl("ODF", ID == TRUE)) {
                file <- gsub("\\.ODF", "",ID)
            } else {
                file <- ID
            }
        url <- paste0(server, "/", ID)
        dodDebug(debug, oce::vectorShow(url))
        dodDebug(debug, oce::vectorShow(ID))
        file <- dod.download(url=url, file=ifelse(grepl("ODF", file) == FALSE, paste0(file, ".ODF"), file), destdir=destdir, silent=TRUE,debug=debug)
        return(file)
        #if (read == TRUE) {
        #    t <- oce::read.oce(file)
        #    return(t)
        #}
    }
}
