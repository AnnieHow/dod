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
#' @importFrom oce read.odf
#' @importFrom ncdf4 nc_open
#'
#' @return 1) If `index` = TRUE and `read`=FALSE a gz file is returned.
#' 2) If `index`=TRUE and `read` = TRUE, a data frame with a list of
#' NetCDF files names is returned. 3) If `index`=FALSE and
#' `read` = FALSE, a netcdf file of the specified index is returned.
#' 4) If `index` = FALSE and `read` = TRUE, a read netcdf file is
#' turned.
#'
#' @family functions that download CTD data
#'
#' @export
dod.ctd.gtspp <- function(ID=NULL, file=NULL, destdir=".", debug=0, index=TRUE, read=FALSE, nc=NULL)
{
    server <- "https://www.ncei.noaa.gov/data/oceans/gtspp/bestcopy/"
        if (is.null(ID)) {
            stop("must give an ID. See help pages for the correct format")
        }
        server <- paste0(server)
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
            if (!(is.null(nc))) {
                message("nc argument being ignored. Only need to specify when index=FALSE")
            }
        if (is.null(file)) {
            file <- paste0(ID,".gz")
        } else {
            file=paste0(file, ".gz")
        }
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
            if (is.null(file)) {
                file <- paste0(nc)
            } else {
                file=paste0(file, ".nc")
            }
            d <- dod.download(url=server, file=file, destdir=destdir, silent=TRUE,debug=debug)
            if (read) {
                dodDebug(debug, oce::vectorShow(file))
                netcdf <- ncdf4::nc_open(file)
                return(netcdf)
            }


        }

}



