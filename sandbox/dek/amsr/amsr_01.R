library(dod)
system("rm -rf RSS*.nc")
# Next will download 4 files
dod.amsr() # defaults to 3day
dod.amsr(type="daily")
dod.amsr(type="weekly")
dod.amsr(type="monthly")
# Next won't download anything (cached)
dod.amsr() # defaults to 3day
dod.amsr(type="daily")
dod.amsr(type="weekly")
dod.amsr(type="monthly")

