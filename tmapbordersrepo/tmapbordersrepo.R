remotes::install_github("afrimapr/afrilearndata")

library(afrilearndata)
library(tidyverse)
library(tmap)

tm_shape(africountries) +
  tm_borders(col = "grey", lwd = .5) +
  tm_text("name_fr", auto.placement=FALSE, remove.overlap=FALSE, just='centre', col='red4', size=0.7 )


#How can I get the borders to be coloured differently based on the income group
tm_shape(africountries) +
  tm_borders(col = "income_grp", lwd = .5) +
  tm_text("name_fr", auto.placement=FALSE, remove.overlap=FALSE, just='centre', col='red4', size=0.7 )

