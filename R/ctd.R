#' Download CTD data
#'
#' This function downloads CTD data from various programs.
#'
#' |                    **Project**                   | **Program** |**Function Called**|
#' |                                            :---- |       :---- |              :----|
#' |Bedford Basin Mooring Project                     |      `BBMP` |   [dod.ctd.bbmp()]|
#' |Bermuda Atlantic Time Series                      |      `BATS` |   [dod.ctd.bats()]|
#' |Global Temperature and Salinity Profile Programme |     `GTSPP` |  [dod.ctd.gtspp()]|
#'
#' @param program character value naming the program (one of listthem).
#'
#' @param ... extra arguments passed to [dod.ctd.bats()], [dod.ctd.bbmp()] or
#' [dod.ctd.gtspp()].
#'
#' @export
dod.ctd <- function(program=NULL, ...)
{
    allowed <- c("BATS", "BBMP", "GTSPP")
    if (is.null(program))
        stop("must give 'program'; try \"",
             paste(allowed, collapse='" "'), "\"")
    if (program == "BATS")
        dod.ctd.bats(...)
    else if (program == "BBMP")
        dod.ctd.bbmp(...)
    else if (program == "GTSPP")
        dod.ctd.gtspp(...)
    else
        stop("'program' must be one of: \"",
             paste(allowed, collapse='" "'), "\"")
}
