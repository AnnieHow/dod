#' Download ctd data
#'
#' This function downloads ctd data from various programs
#' including the Bedford Basin Mooring Program (BBMP), etc.
#'
#' @param program argument specifying the desired oceanographic
#' program to download ctd data from. Options include BBMP, etc.
#'
#' @param year argument specifying the year of interest.
#'
#' @param item the type of data the user wishes to download.
#' For example, `index` or `data`.
#'
#'  @param debug integer value indicating level of debugging.
#'  If this is less than 1, no debugging is done. Otherwise,
#'  some functions will print debugging information.

dod.ctd <- function(program, year, item, debug=0)
{
  if (program == "?") {
    stop("Must provide a program argument, possibilities include: BBMP")
  }
  if (program == "BBMP") {
    server <- "ftp://ftp.dfo-mpo.gc.ca/BIOWebMaster/BBMP/ODF"
    if (missing(year))
      stop("must give 'year'")
    server <- paste0(server, "/", year)
    if (item == "index") {
      file <- paste0(year, "667ODFSUMMARY.tsv")
      if (debug)
        cat(oce::vectorShow(file))
      url <- paste0(server, "/", file)
      if (debug)
        cat(oce::vectorShow(url))
      download.file(url, file)
      if (debug)
        cat(oce::vectorShow(file))
      url <- paste0(server, "/", file)
      return(read.csv(file, header=FALSE, skip=3, col.names=c("file", "time")))
    } else {
      url <- paste0(server, "/", item)
      download.file(url, item)
      return(item)
    }
  }
}
