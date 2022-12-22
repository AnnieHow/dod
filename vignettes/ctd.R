## ---- echo = FALSE------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ---- eval=FALSE--------------------------------------------------------------
#  library(dod)
#  library(oce)
#  index <- dod.ctd("BBMP", 2022, index=TRUE, file="bbmp.txt", read=TRUE)
#  item <- index[1,"file"]
#  file <- dod.ctd(program="BBMP", year=2022, ID=item, file="bbmp.txt", read=TRUE)
#  plot(file)

