library(sf)
library(dplyr)
library(reprex)

nc = st_read(system.file("shape/nc.shp", package="sf"))

#creating an example camp boundary, since I do not have access to your camp
#boundary. This could be replaced by your actual camp boundar
camp_boundary_polygon <- nc %>%
  filter(NAME == "Ashe") %>%
  st_geometry()

#Object class should be sf polygon
class(camp_boundary_polygon)

#Now,taking a random sample of points from within the boundaries of a polygon boundary,
#I have taken 10, for illustration, but you are looking for 348 points so you could change the size to 348

random_spatial_rnd_pts <-  st_sample(camp_boundary_polygon, size = 50, type ="random")


plot(camp_boundary_polygon)
plot(random_spatial_rnd_pts, add = TRUE, col = "red")

st_write(random_spatial_rnd_pts, dsn = "random_spatial_rnd_pts.shp")

#You could then take the sampled spatial points and load them into a GPS device,
#the next step would be to trace the physical location of the points using the GPS device, the houses that
#will be selected for participation in the survey will be the one's closest to each of the
#spatial point.

