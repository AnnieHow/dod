#' Download CTD data
#'
#' This function downloads CTD data from various programs.
#'
#' |                    **Project**                   | **Program** |
#' |                                            :---- |       :---- |
#' |Bedford Basin Mooring Project                     |      `BBMP` |
#' |Bermuda Atlantic Time Series                      |      `BATS` |
#' |Global Temperature and Salinity Profile Programme |     `GTSPP` |
#'
#' @param program character value naming the program (one of listthem).
#'
#' @param ... extra arguments passed to [dod.ctd.bats()] or one of the
#' other functions.
#'
#' If `program` is `"BATS"`, call [dod.ctd.bats()].
#' If `program` is `"BBMP"`, call [dod.ctd.bbmp()].
#' If `program` is `"GTSPP"`, call [dod.ctd.gtspp()].
#'
#' @export

dod.ctd <- function(program=NULL, ...)
{
    if (is.null(program))
        stop("must give 'program'; try \"BATS\", \"BBMP\",\"GTSPP\", ...")
    if (program == "BATS")
        dod.ctd.bats(...)
    else if (program == "BBMP")
        dod.ctd.bbmp(...)
    else if (program == "GTSPP")
        dod.ctd.gtspp(...)
}
