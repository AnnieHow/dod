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
#' @param index a boolean value indicating whether the index
#' should be downloaded.
#'
#' @param ID for BBMP, this is the desired file from the index.
#' For BATS, is the 5 digit cruise ID.
#'
#' @param debug integer value indicating level of debugging.
#' If this is less than 1, no debugging is done. Otherwise,
#' some functions will print debugging information.
#'
#' @importFrom utils download.file
#' @importFrom utils read.csv
#' @importFrom oce read.odf
#'
#' @return If program = "BBMP", an `oce` object is returned.
#' If program = "BATS", a data frame is returned.
#'
#' @export

dod.ctd <- function(program, year, ID=NULL, index= FALSE, debug=0)
{
  if (program == "?") {
    stop("Must provide a program argument, possibilities include: BBMP, BATS")
  }
  if (program == "BBMP") {
    server <- "ftp://ftp.dfo-mpo.gc.ca/BIOWebMaster/BBMP/ODF"
    if (missing(year))
      stop("must give 'year'")
    server <- paste0(server, "/", year)

    if (debug)
      cat(oce::vectorShow(server))
    if (index == TRUE) {
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
      url <- paste0(server, "/", ID)
      if (debug) {
        cat(oce::vectorShow(url))
      }
      if (debug) {
        cat(oce::vectorShow(ID))
      }
      f <- download.file(url, ID)

      if (debug) {
        cat(oce::vectorShow(f))
      }
      t <- read.odf(ID)
      return(t)
    }
  }
if (program == "BATS") {
  if (debug) {
    message("The program is equal to ", program)
  }
  server <- "http://batsftp.bios.edu/BATS/ctd/ASCII/"
  if (index==TRUE) {
    if (is.null(ID) | ID < 10000)
    stop("Must provide an ID number greater than 10000")

    url <- paste0(server, "b",ID, "_info.txt")
    if (debug) {
      message("The url is equal to ",url)
    }
    f <- download.file(url, ID)
    namesInfo <- c("ID", "dateDeployed","dateRecovered","decimalDateDeployed","decimalDateRecovered",
                   "decimalDayDeployed", "timeDeployed", "timeRecovered", "latitudeDeployed", "latitudeRecovered",
                   "longitudeDeployed", "longitudeRecovered")
    t <- read.csv(ID, sep="\t", header=FALSE, col.names= namesInfo)
    return(t)
  }
  else {
    if (debug) {
    message("The ID type is ",ID)
    }

  url <- paste0(server, "b",ID, "_ctd.txt")

  if (debug) {
    cat(oce::vectorShow(url))
  }
  f <- download.file(url, ID)

  if (debug) {
    cat(oce::vectorShow(f))
  }

  names <- c("ID", "date","latitude", "longitude", "pressure","depth","temperature","conductivity", "salinity", "oxygen", "beamAttenuationCoefficient",
             "fluorescence", "PAR")
  t <- read.csv(ID, sep="\t", header=FALSE, col.names= names)

  return(t)
  }


}
}
