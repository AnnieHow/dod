# vim:textwidth=200:expandtab:shiftwidth=4:softtabstop=4

#' Download Advanced Microwave Scanning Radiometer data
#'
#' This function downloads AMSR data.
#'
#' This works by constructing URLs based on the arguments provided.  The author
#' is unaware of any documentation that specifies the directory structure on
#' the server, and so the construction is based on examining the server
#' with a web browser.  Obviously, this is a fragile approach that will
#' lead to failed downloads if the remote directory structure changes.  Indeed,
#' [dod.amsr()] was completely rewritten in July 2023, because a previous version
#' was seen to fail on that date.  Users are asked to report any failed downloads
#' they encounter.  Careful inspection of the documentation for `year`, `month`
#' and `day` is, of course, the first step in debugging errors.
#'
#' @param year,month,day integer values indicating the desired observation time.
#' Set `year` to NULL (the default) to default to the most recent data; otherwise
#' specify all three of these values if `type` is `"3day"`, `"daily"` or `"weekly"`,
#' or just the first two of them if `type` is `"monthly"`.  If these
#' things are provided, then they just match exactly the values in the sought-after
#' file on the remote server.  If `year` is NULL, then [dod.amsr()] constructs
#' a URL that ought to be the most recent available file: 3 days prior
#' to the present date for `type` of `"3day"` or `"daily"`, the Saturday
#' two weeks prior to the present date for `type` of `"weekly"`, or
#' two months in the past, for `type of `"monthly"`.
#'
#' @param destdir character giving destination directory (defaults to `"."`, the present
#' directory).  The directory must exist.  (The author uses `"~/data/amsr"`.)
#'
#' @param age integer giving the number of days old a local file has to be, for it to
#' be regarded as in need of replacement. This defaults to a year.
#'
#' @param server character value indicating the base server location. The
#' default value ought to be used unless the data provider changes their
#' web scheme ... but in that case, it is hoped that users will contact
#' the developers so that the package can be updated.
#'
#' @param type character value indicating where to get the data.  This may be
#' `"3day"` (the default), for a composite covering 3 days of observation, which
#' removes most viewing-path and cloud blanks, `"daily"` for a daily reading,
#' `"weekly"` for a composite covering a week, or `"monthly"` for a composite
#' covering a month.  In the `"daily"` case, the data arrays are 3D, with the
#' third dimension representing ascending and descending traces, but in all the
#' other cases, the arrays are 2D.
#'
#' @param debug integer giving debugging level (defaults to 0, to make the function work
#' without much output).
#'
#' @return character value indicating the full pathname to the downloaded file.
#'
#' @section Historical note:
#' In July 2023, the author noticed major changes in the organization of
#' the remove server, and also of the data files provided. This required
#' major changes to [dod.amsr()], especially in regard to the `server`
#' argument and the newly-added `type` argument. Users are asked to be
#' on the lookout for problems.  Also note that [oce::read.amsr()] will
#' need adjustment to read the new file format.
#'
#' @export
#'
#' @author Dan Kelley
dod.amsr <- function(year=NULL, month, day, destdir=".", age=365,
    server="https://data.remss.com/amsr2/ocean/L3/v08.2",
    type="3day", debug=0)
{
    dodDebug(debug, "dod.amsr(type=\"", type, "\", ...) {\n", sep="")
    if (!type %in% c("3day", "daily", "weekly", "monthly"))
        stop("type='", type, "' not permitted; try '3day', 'daily', 'weekly' or 'monthly'")
    # If year, month, day not given, default to 3 days ago.
    today <- as.POSIXlt(Sys.Date())
    usingDefaultTime <- is.null(year)
    if (usingDefaultTime) {
        dodDebug(debug, "year is NULL, so a default time will be used\n")
    } else {
        if (missing(month))
            stop("month must be provided, if year is provided")
        if (type %in% c("3day", "daily") && missing(day))
            stop("day must be provided for type of '3day' or 'daily'")
        # convert to integers (needed for formatting URLs, below)
        year <- as.integer(year)
        month <- as.integer(month)
        day <- as.integer(day)
    }
    if (type %in% c("3day", "daily")) {
        # https://data.remss.com/amsr2/ocean/L3/v08.2/3day/2023/RSS_AMSR2_ocean_L3_3day_2023-07-24_v08.2.nc
        # ^                                           ^    ^                       ^    ^    ^  ^
        # server                                      type year                  type year month day
        if (usingDefaultTime) {
            focus <- as.POSIXlt(Sys.Date() - 3L)
            year <- 1900L + focus$year
            month <- 1L + focus$mon
            day <- focus$mday
            dodDebug(debug, "defaulting to year=", year, ", month=", month, " and day=", day, "\n", sep="")
        } else {
            dodDebug(debug, "user-supplied year=", year, ", month=", month, " and day=", day, "\n", sep="")
        }
        url <- sprintf("%s/%s/%d/RSS_AMSR2_ocean_L3_%s_%04d-%02d-%02d_v08.2.nc",
            server, type, year, type, year, month, day)
    } else if (identical(type, "weekly")) {
        if (usingDefaultTime) {
            # use the Saturday previous to the most recent Saturday
            today <- Sys.Date()
            dayName <- weekdays(today)
            offset <- switch(dayName,
                "Saturday"=0, "Sunday"=1, "Monday"=2, "Tuesday"=3, "Wednesday"=4, "Thursday"=5, "Friday"=6)
            ymd <- format(today - offset - 7L)
            dodDebug(debug, "defaulting to ymd=\"", ymd, "\"\n")
        } else {
            ymd <- sprintf("%4d-%02d-%02d", year, month, day)
            dodDebug(debug, "user-provided ymd=\"", ymd, "\"\n")
        }
        # https://data.remss.com/amsr2/ocean/L3/v08.2/weekly/RSS_AMSR2_ocean_L3_weekly_2023-07-15_v08.2.nc
        # ^                                           ^                            ^    ^
        # server                                      type                       type   ymd
        url <- sprintf("%s/%s/RSS_AMSR2_ocean_L3_%s_%s_v08.2.nc",
            server, type, type, ymd)
    } else if (identical(type, "monthly")) {
        # https://data.remss.com/amsr2/ocean/L3/v08.2/monthly/RSS_AMSR2_ocean_L3_monthly_2023-05_v08.2.nc
        # ^                                           ^                            ^    ^    ^
        # server                                      type                       type year month
        # use the month previous to the previous month
        if (usingDefaultTime) {
            year <- 1900L + today$year
            month <- 1L + today$mon
            if (month == 2L) {
                year <- year - 1L
                month <- 12
            } else {
                month <- month - 2L
            }
            dodDebug(debug, "defaulting to year=", year, ", month=", month, "\n", sep="")
        } else {
            dodDebug(debug, "user-supplied year=", year, ", month=", month, "\n", sep="")
        }
        url <- sprintf("%s/%s/RSS_AMSR2_ocean_L3_%s_%04d-%02d_v08.2.nc",
            server, type, type, year, month)
    } else {
        # check again (but should not be able to get here)
        stop("type='", type, "' not permitted; try '3day', 'daily', 'weekly' or 'monthly'")
    }
    file <- gsub(".*/", "", url)
    dodDebug(debug, "url=\"", url, "\"\n", sep="")
    dodDebug(debug, "file=\"", file, "\"\n", sep="")
    rval <- dod.download(url, destdir=destdir, file=file, age=age, debug=debug-1, silent=debug==0)
    dodDebug(debug, "} # dod.amsr\n", sep="")
    rval
}
