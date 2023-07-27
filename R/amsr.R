# vim:textwidth=200:expandtab:shiftwidth=4:softtabstop=4

#' Download Advanced Microwave Scanning Radiometer data
#'
#' This function downloads AMSR data.
#'
#' @param year,month,day integer values indicating the desired observation time.
#' All three are required for `server` values of `"3day"` and `"daily"`,
#' but day is not required (and are ignored) for the other two choices of `server`.
#' If these values are not provided, the focus is 3 days prior to the present day.
#'
#' @param destdir character giving destination directory (defaults to `"."`, the present
#' directory).
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
dod.amsr <- function(year, month, day, destdir=".", age=365,
    server="https://data.remss.com/amsr2/ocean/L3/v08.2",
    type="3day", debug=0)
{
    dodDebug(debug, "dod.amsr(type=\"", type, "\", ...) {\n", sep="")
    if (!type %in% c("3day", "daily", "weekly", "monthly"))
        stop("type value \"", type, "\" is not permitted.  Please see documentation")
    # If year, month, day not given, default to 3 days ago.
    recent <- as.POSIXlt(Sys.Date() - 3)
    if (missing(year))
        year <- 1900 + recent$year
    if (missing(month))
        month <- 1 + recent$mon
    if (missing(day))
        day <- recent$mday
    year <- as.integer(year)
    month <- as.integer(month)
    day <- as.integer(day)
    dodDebug(debug, sprintf("year=%d, month=%d, day=%d, type=%s\n", year, month, day, type))
    if (type %in% c("3day", "daily")) {
        # https://data.remss.com/amsr2/ocean/L3/v08.2/3day/2023/RSS_AMSR2_ocean_L3_3day_2023-07-24_v08.2.nc
        # ^                                           ^    ^                       ^    ^    ^  ^
        # server                                      type year                  type year month day
        url <- sprintf("%s/%s/%d/RSS_AMSR2_ocean_L3_%s_%04d-%02d-%02d_v08.2.nc",
            server, type, year, type, year, month, day)
    } else if (identical(type, "weekly")) {
        stop("FIXME weekly")
    } else if (identical(type, "monthley")) {
        stop("FIXME monthly")
    } else {
        stop("unrecognized '", server, "' value; see documentation.")
    }
    file <- gsub(".*/", "", url)
    dodDebug(debug, "url=\"", url, "\"\n", sep="")
    dodDebug(debug, "file=\"", file, "\"\n", sep="")
    rval <- dod.download(url, destdir=destdir, file=file, age=age, debug=debug-1, silent=debug==0)
    dodDebug(debug, "} # dod.amsr\n", sep="")
    rval
}
