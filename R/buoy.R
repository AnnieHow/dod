#' Download buoy data
#'
#' This function downloads buoy data from various programs
#' including...
#'
#' @param program argument specifying the desired oceanographic
#' program to download buoy data from. Options include `MEDS`, etc.
#'
#' @param year argument specifying the year of interest.
#'
#' @param index a boolean value indicating whether the index
#' should be downloaded.
#'
#' @param ID for `MEDS`...
#'
#' @param destdir character value indicating the directory
#' in which to store downloaded files.
#'
#' @param debug integer value indicating level of debugging.
#' If this is less than 1, no debugging is done. Otherwise,
#' some functions will print debugging information.
#'
#' @importFrom utils download.file
#' @importFrom utils unzip
#'
#' @return If `index` is TRUE, and `program` is `"BBMP"` or `"BATS"`,
#' return a data frame.  Otherwise, return the name of the downloaded file.
#'
#' @export

#https://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/wave/waveshare/csvData/c44258_csv.zip
#https://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/wave/waveshare/csvData/c44139_csv.zip

dod.buoy <- function(program, year, ID=NULL, index=FALSE, destdir=".", debug=0)
{
    if (program == "?") {
        stop("Must provide a program argument, possibilities include: MEDS")
    }
    if (program == "MEDS") {
    server <- "https://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/wave/waveshare/csvData"
    url <- paste0(server, "/c", ID, "_csv.zip")
    zipfile <- paste0("c", ID, "_csv.zip")
    download.file(url, zipfile)
    unzip(zipfile)
    # NOTE: we should delete the zipfile too; see ?unlink
    return(paste0("c", ID, ".csv"))
}

    }
