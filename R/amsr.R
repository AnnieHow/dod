#' Download Advanced Microwave Scanning Radiometer data
#'
#' This function downloads AMSR data.
#'
#' @param year integer giving year (defaults to value 3 days in past).
#'
#' @param month integer giving month (defaults to value 3 days in past).
#'
#' @param day integer giving day (defaults to value 3 days in past).
#'
#' @param destdir character giving destination directory (defaults to `"."`, the present
#' directory).
#'
#' @param age integer giving the number of days old a local file has to be, for it to
#' be regarded as in need of replacement.
#'
#' @param server character value indicating where to get the data.  This may be `"new"` (the default)
#' for a scheme that worked in year 2023, `"old"` for a scheme that worked a few years 
#'
#' @param debug integer giving debugging level (defaults to 0, to make the function work
#' without much output).
#'
#' @return character value indicating the full pathname to the downloaded file.
#'
#' @export

dod.amsr <- function(year, month, day, destdir=".", age=14, server="new", debug=0)
{
    # If year, month, day not given, default to 3 days ago.
    recent <- as.POSIXlt(Sys.Date() - 3)
    dodDebug(debug, "recent=", format(recent), "\n")
    if (missing(year))
        year <- 1900 + recent$year
    if (missing(month))
        month <- 1 + recent$mon
    if (missing(day))
        day <- recent$mday
    year <- as.integer(year)
    month <- as.integer(month)
    day <- as.integer(day)
    dodDebug(debug, sprintf("year=%d, month=%d, day=%d\n", year, month, day))
    # Special cases for 'new' and 'old'; otherwise, will
    # use as-is.
    serverOrig <- server
    if (identical(server, "new")) {
        url <- sprintf("https://data.remss.com/amsr2/ocean/L3/v08.2/daily/%d/RSS_AMSR2_ocean_L3_daily_%d-%02d-%02d_v08.2.nc", year, year, month, day)
        file <- gsub(".*/", "", url)
        #dodDebug(debug, "will try url \"", url, "\"\n", sep="")
        #dodDebug(debug, "will try file \"", file, "\"\n", sep="")
        #message("https://data.remss.com/amsr2/ocean/L3/v08.2/daily/2023/RSS_AMSR2_ocean_L3_daily_2023-07-23_v08.2.nc")
        #message(url)
        filename <- dod.download(url, destdir=destdir, file=file, age=age, debug=debug-1)
        return(filename)
    } else if (identical(server, "old")) {
        stop("FIX FOR OLD")
        server <- "http://data.remss.com/amsr2/bmaps_v08"
    }
}
