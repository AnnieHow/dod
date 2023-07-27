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
#' was seen to fail on that date.  Users are asked to report any failed downloades
#' they encounter.
#'
#' @param year,month,day integer values indicating the desired observation time.
#' All three are required for `server` values of `"3day"` and `"daily"`,
#' but day is not required (and is ignored) for the other two choices of `server`.
#' If these values are not provided, reasonable defaults are chosen.  For
#' the `"3day"` and `"daily"` cases, this default is 3 days prior to the present date.
#' For the `"weekly"` case, it is two Saturdays previous to the present date.  And
#' for the `"monthly"` case, it is two months previous to today's month (e.g. it
#' is 5, if today is in July).  Users who choose to specify values for `year`,
#' `month`, and `day` for either `weekly` or `monthly` data are advised to
#' examine the server with a web browser to discover the year-month-day values
#' of the available files.
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
dod.amsr <- function(year, month, day, destdir=".", age=365,
    server="https://data.remss.com/amsr2/ocean/L3/v08.2",
    type="3day", debug=0)
{
    dodDebug(debug, "dod.amsr(type=\"", type, "\", ...) {\n", sep="")
    if (!type %in% c("3day", "daily", "weekly", "monthly"))
        stop("type='", type, "' not permitted; try '3day', 'daily', 'weekly' or 'monthly'")
    # If year, month, day not given, default to 3 days ago.
    today <- Sys.Date()
    recent <- as.POSIXlt(today - 3)
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
        dayName <- weekdays(today)
        offset <- switch(dayName,
            "Saturday"=0, "Sunday"=1, "Monday"=2, "Tuesday"=3, "Wednesday"=4, "Thursday"=5, "Friday"=6)
        #print(today)
        #print(today-offset)
        #print(today-offset-7)
        ymd <- format(today - offset - 7)
        # https://data.remss.com/amsr2/ocean/L3/v08.2/weekly/RSS_AMSR2_ocean_L3_weekly_2023-07-15_v08.2.nc
        # ^                                           ^                            ^    ^
        # server                                      type                       type   ymd
        url <- sprintf("%s/%s/RSS_AMSR2_ocean_L3_%s_%s_v08.2.nc",
            server, type, type, ymd)
    } else if (identical(type, "monthly")) {
        # https://data.remss.com/amsr2/ocean/L3/v08.2/monthly/RSS_AMSR2_ocean_L3_monthly_2023-05_v08.2.nc
        # ^                                           ^                            ^    ^    ^
        # server                                      type                       type year month
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
