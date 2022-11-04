#' Download CTD data
#'
#' This function downloads CTD data from various programs.
#'
#' |                    **Project**                  | **Program** | **Index** |                     **ID**                    |
#' |                                           :---- |       :---- |     :---- |                                         :---- |
#' |Bedford Basin Mooring Project                    |      `BBMP` |       Yes |                                    From index |
#' |Bermuda Atlantic Time Series                     |      `BATS` |       Yes |                                      Cruise ID|
#' |Global Temperature and Salinity Profile Programme|     `GTSPP` |         No|Ocean Basin Initial _ _ Year _ _ _ _ Month _ _ |
#'
#' @param program character value naming the program (one of listthem).
#'
#' @param ... extra arguments passed to [dod.ctd.bats()] or one of the
#' other functions.
#'
#' If `program` is `"BATS"`, call [dod.ctd.bats()].
#' If `program` is `"BBMP"`, call [dod.ctd.bbmp()].
#'
#' @export

dod.ctd <- function(program=NULL, ...)
{
    if (is.null(program))
        stop("must give 'program'; try \"BATS\", \"BBMP\", ...")
    if (program == "BATS")
        dod.ctd.bats(...)
    else if (program == "BBMP")
        dod.ctd.bbmp(...)
}
