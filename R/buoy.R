#' Download buoy data
#'
#' This function downloads buoy data from various programs
#' including...
#'
#' @param program argument specifying the desired oceanographic
#' program to download buoy data from. Options include `MEDS`, etc.
#'
#' @param ID For `MEDS` this argument is either a location name
#' or ID number.
#'
#' @param destdir character value indicating the directory
#' in which to store downloaded files.
#'
#' @param debug integer value indicating level of debugging.
#' If this is less than 1, no debugging is done. Otherwise,
#' some functions will print debugging information.
#'
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
        loc <- list("East Scotian Slope"="44137","Banquereau Bank"="44139", "Halifax Harbour"="44258","Halifax"="44172","Halifax DISCUS TriAx"= "44299","Tail of the Bank"= "44140","Laurentian Fan"="44141","Port Hope"="45135", "Prince Edward Point"= "45135","Minas Basin"= "MEDS027")
        dodDebug(debug, "Initial ID=", ID,"\n")
        if (ID %in% names(loc))
            ID <- loc[[ID]]
        dodDebug(debug, "Final ID=", ID,"\n")
        server <- "https://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/wave/waveshare/csvData"
        url <- paste0(server, "/c", ID, "_csv.zip")
        zipfile <- paste0("c", ID, "_csv.zip")
        dodDebug(debug, url)
        dod.download(url, zipfile)
        unzip(zipfile)
        unlink(zipfile)
        # NOTE: we should delete the zipfile too; see ?unlink
        return(paste0("C", ID, ".CSV"))
    }

}
