#' Download buoy data
#'
#' This function downloads buoy data from various programs
#' including...
#'
#' @param program argument specifying the desired oceanographic
#' program to download buoy data from. Options include `MEDS`, etc.
#'
#' @param location
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

dod.buoy <- function(program, ID=NULL, destdir=".", debug=0)
{
    if (program == "?")
        stop("Must provide a program argument, possibilities include: MEDS")
    if (is.null(ID))
        stop("Must provide an ID argument")
    if (program == "MEDS") {
        loc <- list("East Scotian Slope"="44137","Banquereau Bank"="44139")
        dodDebug(debug, "Initial ID=", ID,"\n")
        if (ID %in% names(loc))
            ID <- loc[[ID]]
        dodDebug(debug, "Final ID=", ID,"\n")
        server <- "https://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/wave/waveshare/csvData"
        url <- paste0(server, "/c", ID, "_csv.zip")
        zipfile <- paste0("c", ID, "_csv.zip")
        message(url)
        download.file(url, zipfile)
        unzip(zipfile)
        unlink(zipfile)
        # NOTE: we should delete the zipfile too; see ?unlink
        return(paste0("C", ID, ".CSV"))
    }

}
