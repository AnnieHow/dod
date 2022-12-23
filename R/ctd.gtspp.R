#' Download CTD data from the GTSPP server
#'
#' This function downloads CTD data from the GRSPP server at
#' <https://www.ncei.noaa.gov/data/oceans/gtspp/bestcopy/>. This website is
#' somewhat challenging to use; see \sQuote{Details}.
#'
#' The `.zip` files provided on the NCEI server are not readible on a macOS
#' machine, which might also indicate problems on other machines.  Therefore,
#' inventory files are read in `.txt` format, which increases download and read
#' time by over an order of magnitude.
#'
#' @param basin character value indicating the ocean basin in which the data
#' were acquired.  This must be one of `"at"`, `"pa"` or `"in"`, for the
#' Atlantic, Pacific and Indian basins, respectively.
#'
#' @param year integer value giving the year in which the data were acquired.
#'
#' @param month integer value giving the month in which the data were acquired.
#' If in character form, a two-digit value is required, with the first digit
#' being `"0"` for early months of the year.
#'
#' @param file character value giving the name to be used for the downloaded
#' file.
#'
#' @param index a boolean value indicating whether the index should be
#' downloaded. This is FALSE by default.
#'
#' @param nc the name of a netcdf file to download, likely from the index.
#'
#' @template destdirTemplate
#'
#' @template debugTemplate
#'
#' @return [dod.ctd.gtspp()] returns the local name of the downloaded file.
#'
#' @family functions that download CTD data
#'
#' @examplesIf interactive()
#' library(dod)
#' # Get an index file for Atlantic Ocean observations in January 2022.
#' # EG https://www.ncei.noaa.gov/data/oceans/gtspp/bestcopy/inventory/at198501_gtsppinv.zip
#' indexFile <- dod.ctd.gtspp(basin="at", year=2022, month=1, index=TRUE)
#' col.names <- strsplit(gsub("#", "", readLines(indexFile, 1L)), ",")[[1]]
#' index <- read.csv(indexFile, skip=1L, col.names=col.names)
#' # Plot sampling locations near Halifax, Nova Scotia
#' library(oce)
#' library(ocedata)
#' data(coastlineWorldFine, package="ocedata")
#' hlon <- -63.5728
#' hlat <- 44.6476
#' plot(coastlineWorldFine, clongitude=hlon, clatitude=hlat, span=2000)
#' points(index$longitude, index$latitude, col=4)
#' # Focus on the point nearest Halifax
#' # https://www.ncei.noaa.gov/data/oceans/gtspp/bestcopy/atlantic/2022/01/gtspp_47477452_te_111.nc
#' dist <- geodDist(index$longitude, index$latitude, hlon, hlat)
#' focus <- index[which.min(dist), ]
#' dataFile <- dod.ctd.gtspp("at", 2022, 1, nc=gsub(".*/","",focus$data_URL))
#' # Read and plot the data file
#' library(ncdf4)
#' library(oce)
#' nc <- nc_open(dataFile)
#' S <- ncvar_get(nc, "salinity")
#' T <- ncvar_get(nc, "temperature")
#' z <- ncvar_get(nc, "z")
#' lon <- ncvar_get(nc, "longitude")
#' lat <- ncvar_get(nc, "latitude")
#' p <- swPressure(z, lat)
#' ctd <- as.ctd(S, T, p, longitude=lon, latitude=lat)
#' summary(ctd)
#' plot(ctd)
#'
#' @export
dod.ctd.gtspp <- function(basin, year, month, file=NULL, destdir=".", debug=0, index=FALSE, nc=NULL)
{
    read <- FALSE
    if (missing(basin))
        stop("must provide basin")
    basinOrig <- basin
    basin <- match.arg(basin, c("atlantic", "pacific", "indian"))
    dodDebug(debug, "basin = \"", basin, "\" (converted from \"", basinOrig, "\")\n", sep="")
    basinID <- substr(basin, 1, 2)
    dodDebug(debug, "basinID = \"", basinID, "\"\n", sep="")
    if (missing(year))
        stop("must provide year")
    year <- as.integer(year)
    dodDebug(debug, "year =", year, "\n")
    if (missing(month))
        stop("must provide month")
    month <- as.integer(month)
    dodDebug(debug, "month =", month, "\n")
    if (!is.logical(index))
        stop("index must be a logical value")
    server <- "https://www.ncei.noaa.gov/data/oceans/gtspp/bestcopy"
    dodDebug(debug, "server = \"", server, "\"\n", sep="")
    dodDebug(debug, "destdir = \"", destdir, "\"\n", sep="")
    ID <- sprintf("%2s%4d%02d", basinID, year, month)
    dodDebug(debug, "ID = \"", ID, "\"\n", sep="")
    if (index) {                       # get an index file
        # E.G. https://www.ncei.noaa.gov/data/oceans/gtspp/bestcopy/inventory/at198501_gtsppinv.zip
        dodDebug(debug, "about to try downloading an index file\n")
        rfile <- paste0(ID, "_gtsppinv.txt") # .zip files are unreadible
        server <- paste0(server, "/inventory/", rfile)
        dodDebug(debug, "server = \"", server, "\"\n", sep="")
        if (!(is.null(nc)))
            message("nc argument being ignored. Only need to specify when index=FALSE")
        if (is.null(file))
            file <- rfile
        file <- paste0(destdir, "/", file)
        dodDebug(debug, "file = \"", file, "\"\n", sep="")
        return(dod.download(url=server, file=file, destdir=destdir, silent=TRUE, debug=debug))
    } else {                           # get a data file
        # E.G. https://www.ncei.noaa.gov/data/oceans/gtspp/bestcopy/atlantic/2022/11/gtspp_48577347_ba_111.nc
        dodDebug(debug, "about to try downloading a data file\n")
        dodDebug(debug, "nc = \"", nc, "\"\n", sep="")
        server <- sprintf("%s/%s/%04d/%02d", server, basin, year, month)
        dodDebug(debug, "server = \"", server, "\"\n", sep="")
        server <- paste0(server, "/", nc)
        file <- if (is.null(file)) nc else paste0(file, ".nc")
        return(dod.download(url=server, file=file, destdir=destdir, silent=TRUE, debug=debug))
    }
}

