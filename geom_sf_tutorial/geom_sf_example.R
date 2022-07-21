#Using geom_sf
#https://ggplot2.tidyverse.org/reference/ggsf.html#:~:text=geom_sf()%20is%20an%20unusual,geom_sf_text()%20and%20geom_sf_label()%20.

library(tidyverse)
library(sf)
library(ggspatial)

nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

  ggplot(nc) +
    geom_sf(aes(fill = AREA))
  
  # If not supplied, coord_sf() will take the CRS from the first layer
  # and automatically transform all other layers to use that CRS. This
  # ensures that all data will correctly line up
  nc_3857 <- sf::st_transform(nc, 3857)
  ggplot() +
    geom_sf(data = nc) +
    geom_sf(data = nc_3857, colour = "red", fill = NA)
  
  # Unfortunately if you plot other types of feature you'll need to use
  # show.legend to tell ggplot2 what type of legend to use
  nc_3857$mid <- sf::st_centroid(nc_3857$geometry)
  ggplot(nc_3857) +
    geom_sf(colour = "white") +
    geom_sf(aes(geometry = mid, size = AREA), show.legend = "point")
  
  # You can also use layers with x and y aesthetics. To have these interpreted
  # as longitude/latitude you need to set the default CRS in coord_sf()
  ggplot(nc_3857) +
    geom_sf() +
    annotate("point", x = -80, y = 35, colour = "red", size = 4) +
    coord_sf(default_crs = sf::st_crs(4326))
  
  # Thanks to the power of sf, a geom_sf nicely handles varying projections
  # setting the aspect ratio correctly.
  library(maps)
  world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
  ggplot() + geom_sf(data = world1)
  
  world2 <- sf::st_transform(
    world1,
    "+proj=laea +y_0=0 +lon_0=155 +lat_0=-90 +ellps=WGS84 +no_defs"
  )
  ggplot() + geom_sf(data = world2)
  
  # To add labels, use geom_sf_label().
  ggplot(nc_3857[1:3, ]) +
    annotation_map_tile(type = "osm") +
    geom_sf(aes(fill = AREA)) +
    geom_sf_label(aes(label = NAME)) + 
     annotation_scale() +
    annotation_north_arrow(location = "br", which_north = "true") +
    labs(title = "New plot title", x = NULL, y = NULL) +
    theme_bw()


