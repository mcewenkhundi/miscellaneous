devtools::install_github("petermacp/BlantyreTBEpi")
library(BlantyreTBEpi)

data("dat", package="BlantyreTBEpi")

library(sf)
st_write(BlantyreTBEpi::hsas,"hsas.shp")
hsas_new <- BlantyreTBEpi::hsas

#https://community.rstudio.com/t/issues-with-writing-a-dataframe-to-a-shapefile/42449/7
hsas_new <-st_zm(hsas_new, drop=T, what='ZM')
st_write(hsas_new,"hsas.shp",delete_layer = TRUE)

hsas_denoms <- BlantyreTBEpi::dat
write.csv(hsas_denoms, file = "hsas_denoms.csv")
