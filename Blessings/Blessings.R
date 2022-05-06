library(haven)
library(labelled)

wm <- read_sav("wm.sav")
#View(wm)
write_dta(wm,"wm.csv")

bh <- read_sav("bh.sav")
write_dta(bh, "bh.dta")
#View(bh)


