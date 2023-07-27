library(dod)
system("rm -rf RSS*.nc")
dod.amsr() # defaults to 3day
dod.amsr(type="daily")
dod.amsr(type="weekly")
dod.amsr(type="monthly")

