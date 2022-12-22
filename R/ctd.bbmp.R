#' Download CTD data from the Bedford Basin Mooring Project
#'
#' [dod.ctd.bbmp] can retrieve both index files and data files
#' from the Bedford Basin Mooring Project (BBMP). Since the naming
#' convention of the data files may alter from year to year, it is
#' important to start by downloading an index first, to ascertain
#' the name of a file of interest; see \sQuote{Examples}.
#'
#' @param year a numeric or character value specifying the year of interest.
#' If this is not provided, it defaults to the current year.
#'
#' @param index a boolean value. If this is TRUE, then [dod.ctd.bbmp]
#' downloads an index file that provides the names of files containing CTD data
#' along with the sampling dates.  The value of `ID` is ignored in this case.
#' By contrast, if `index` is FALSE, then [dod.ctd.bbmp] downloads
#' the CTD file with the specified `ID`.  See \sQuote{Examples}.
#'
#' @param ID a character value specifying the file of interest
#' that is copied from the index.
#'
#' @param file character value giving the name to be used for
#' the downloaded file.  If this is NULL (which is the default) then
#' the filename is as on the remote data server.
#'
#' @template destdirTemplate
#'
#' @template debugTemplate
#'
#' @importFrom utils read.csv
## @importFrom oce read.oce
#'
#' @return [dod.ctd.bbmp] returns a character value naming the file that
#' was retrieved. This may be either an index file or a data file; see
#' the \sQuote{Examples} section for an example of a typical workflow.
#'
#' @examples
#'\dontrun{
#' # Download and study this year's first BBMP CTD file
#' library(dod)
#' # Download and read the index
#' indexFile <- dod.ctd("BBMP", index=TRUE)
#' index <- read.csv(indexFile, header=FALSE, col.names=c("file","time"), skip=3)
#' # Download the first file in the index
#' ctdFile <- dod.ctd("BBMP", ID=index$file[1])
#' # Use oce to read, summarize and plot the data.
#' library(oce)
#' ctd <- read.oce(ctdFile)
#' summary(ctd)
#' plot(ctd)
#'}
#'
#' @family functions that download CTD data
#'
#' @export
dod.ctd.bbmp <- function(year, ID=NULL, index=FALSE, file=NULL, destdir=".", debug=0)
{
    server <- "ftp://ftp.dfo-mpo.gc.ca/BIOWebMaster/BBMP/ODF"
    if (missing(year))
        year <- format(Sys.Date(),"%Y")
    dodDebug(debug, oce::vectorShow(year))
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
