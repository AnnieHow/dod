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
#' @param index a boolean value indicating whether the index
#' should be downloaded.
#'
#' @param read a boolean indicated weather or not the downloaded
#' file should be downloaded
#'
#' @param nc the name of a netcdf file to download, likely from
#' the index
#'
#' @template destdirTemplate
#'
#' @template debugTemplate
#'
#' @importFrom utils read.csv
#' @importFrom ncdf4 nc_open
#'
#' @return 1) If `index` is TRUE and `read` is FALSE, then a gz file is returned.
#' 2) If both `index` and `read` are TRUE, then a data frame with a list of
#' NetCDF files names is returned. 3) If both `index` and
#' `read` are FALSE, then a netcdf file of the specified index is returned.
#' 4) Finally, if `index` is FALSE and `read` is TRUE, the contents of the data file are
#' returned as an oce `ctd` object.
#'
#' @family functions that download CTD data
#'
#' @export
dod.ctd.gtspp <- function(ID=NULL, file=NULL, destdir=".", debug=0, index=TRUE, read=FALSE, nc=NULL)
{
    server <- "https://www.ncei.noaa.gov/data/oceans/gtspp/bestcopy/"
    if (is.null(ID))
        stop("must give an ID. See help pages for the correct format")
    if (grepl("at", ID)) {
        dodDebug(debug, "Ocean has been identified as atlantic\n")
        server <- paste0(server, "atlantic")
    } else if (grepl("pa", ID)) {
        dodDebug(debug, "Ocean has been identified as pacific\n")
        server <- paste0(server, "pacific")
    } else if (grepl("in", ID)) {
        dodDebug(debug, "Ocean has been identified as indian\n")
        server <- paste0(server, "indian")
    }
    year <- substr(ID, start = 3, stop = 6)
    dodDebug(debug, oce::vectorShow(year))
    month <- substr(ID, start = 7, stop = 8)
    dodDebug(debug, oce::vectorShow(month))
    server <- paste0(server, "/", year, "/", month)
    dodDebug(debug, oce::vectorShow(server))
    dodDebug(debug, oce::vectorShow(destdir))
    if (index) {
        if (!(is.null(nc)))
            message("nc argument being ignored. Only need to specify when index=FALSE")
        file <- if (is.null(file)) paste0(ID,".gz") else paste0(file, ".gz")
        d <- dod.download(url=server, file=file, destdir=destdir, silent=TRUE,debug=debug)
        if (read) {
            lines <- readLines(d)
            lines <- lines[-(1:11)]
            lines <- gsub(".*href=\"","",lines)
            lines <- gsub("\".*","",lines)
            df <- data.frame(lines)
            names(df) <- "index"
            return(df)
        }
    } else {
        dodDebug(debug, oce::vectorShow(nc))
        server <- paste0(server,"/", nc)
        file  <- if (is.null(file)) paste0(nc) else paste0(file, ".nc")
        d <- dod.download(url=server, file=file, destdir=destdir, silent=TRUE,debug=debug)
        if (read) {
            dodDebug(debug, oce::vectorShow(file))
            netcdf <- ncdf4::nc_open(file)
            return(netcdf)
        }
    }
}

