#' Download meteorological timeseries data
#'
#' Get meteorological data from an Environment Canada website, using
#' [oce::download.met()].
#'
#' @param ... arguments passed to [oce::download.met()].
#'
#' @importFrom oce download.met
#'
#' @return [dod.met] returns a character value holding the full pathname
#' of the downloaded file.
#'
#' @export
dod.met <- function(...)
{
    oce::download.met(...)
}

#' Download sounding data
#'
#' Download an atmospheric sounding file from the University of Wyoming
#' Department of Atmospheric science website at
#' <https://weather.uwyo.edu/upperair/sounding.html>.
#'
#' @param station character value indicating the station identifier.  The
#' default is for a station near Halifax, Nova Scotia.
#'
#' @param year integer or character value indicating the year.  If this is not
#' supplied, the present year is used.
#'
#' @param month integer or character value indicating the month. If this is not
#' supplied, the present month is used.
#'
#' @param day integer or character value indicating the day If this is not
#' supplied, the present day is used.
#'
#' @param region character value indicating the region. For example, stations in north
#' America seem to be associated with region `"naconf"` (which is the default).
#'
#' @template destdirTemplate
#'
#' @template ageTemplate
#'
#' @template debugTemplate
#'
#' @return The local name of the downloaded file.
#'
#' @examples
#' # Download
#' tempdir <- tempfile()
#' dir.create(tempdir)
#' station <- "73110"
#' year <- "2023"
#' month <- "01"
#' day <- "08"
#' file <- dod.met.sounding(station, year=year, month=month, day=day, destdir=tempdir)
#' # Read data, extracting the table crudely.
#' lines <- readLines(file)
#' start <- grep("<PRE>", lines)[1]
#' end <- grep("</PRE>", lines)[1]
#' table <- lines[seq(start+5, end-1)]
#' col.names <- strsplit(gsub("^ [ ]*", "", lines[start+2]), "[ ]+")[[1]]
#' # Must read in fixed-width format because missing data are blanked out
#' data <- read.fwf(file=textConnection(table),
#'    widths=rep(7, 11), col.names=col.names)
#' # Plot mixing ratio variation with height
#' plot(data$MIXR, data$HGHT, type="l", cex=0.5, pch=20, col=4,
#'     xlab="Mixing Ratio", ylab="Height [m]")
#' unlink(tempdir, recursive=TRUE)
#' @export
dod.met.sounding <- function(station="73110", year, month, day, region="naconf", destdir=".", age=0, debug=0)
{
    # https://weather.uwyo.edu/upperair/sounding.html
    # url <- "https://weather.uwyo.edu/cgi-bin/sounding?region=naconf&TYPE=TEXT%3ALIST&YEAR=2023&MONTH=01&FROM=0812&TO=0812&STNM=73110"
    ymd <- strsplit(format(Sys.Date()), "-")[[1]]
    if (missing(year)) year <- ymd[1]
    if (missing(month)) month <- ymd[2]
    if (missing(day)) day <- ymd[3]
    from <- paste0(day, "12")
    to <- from
    base <- "https://weather.uwyo.edu/cgi-bin/sounding"
    url <- sprintf("%s?region=%s&TYPE=TEXT%%3ALIST&YEAR=%s&MONTH=%s&FROM=%s&TO=%s&STNM=%s",
        base, region, year, month, from, to, station)
    dodDebug(debug, "url=\"", url, "\"\n", sep="")
    file <- paste0("/sounding", "_", station, "_", year, "_", month, ".dat")
    dodDebug(debug, "file=\"", destdir, "/", file , "\"\n", sep="")
    dod.download(url, destdir=destdir, file=file, age=age, debug=debug-1)
}
