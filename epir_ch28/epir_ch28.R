# Author: McEwen Khundi
# Task: Rclub book review
# Book: Epir chapter 28, GIS basics

pacman::p_load(
  rio, # to import data
  here, # to locate files
  tidyverse, # to clean, handle, and plot the data (includes ggplot2 package)
  sf, # to manage spatial data using a Simple Feature format
  tmap, # to produce simple maps, works for both interactive and static maps
  janitor, # to clean column names
  OpenStreetMap, # to add OSM basemap in ggplot map
  spdep # spatial statistics
)

# import clean case linelist
linelist <- import("linelist_cleaned.rds")

set.seed(40)
# generate 1000 random row numbers, from the number of rows in linelist
sample_rows <- sample(nrow(linelist), 1000)

# subset linelist to keep only the sample rows, and all columns
linelist <- linelist[sample_rows, ]

# Create sf object
linelist_sf <- linelist %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

linelist_sf

# Admin boundary for Sierra Leone.

# ADM3 level clean
sle_adm3 <- st_read("sle_adm3.shp")

sle_adm3 <- sle_adm3 %>%
  clean_names() %>% # standardize column names
  filter(admin2name %in% c("Western Area Urban", "Western Area Rural")) # filter to keep certain areas

# Check the graph
plot(st_geometry(sle_adm3))

# Population denominators of Sierra Leone by ADM3
# Population by ADM3
sle_adm3_pop <- import("sle_admpop_adm3_2020.csv") %>%
  clean_names()

# Health Facilities
# Sierra Leone: Health facility data from OpenStreetMap
# OSM health facility shapefile
sle_hf <- sf::read_sf("sle_hf.shp") %>%
  clean_names() %>%
  filter(amenity %in% c("hospital", "clinic", "doctors"))

# Check the graph
plot(st_geometry(sle_hf))

# Plotting using the tmap package
tmap_mode("plot") # choose either "view" or "plot"
# Just the cases (points)
tm_shape(linelist_sf) +
  tm_dots(size = 0.08, col = "blue")

# Alone, the points do not tell us much. So we should also map the administrative boundaries:

# Just the administrative boundaries (polygons)
tm_shape(sle_adm3) + # admin boundaries shapefile
  tm_polygons(col = "#F7F7F7") + # show polygons in light grey
  tm_borders(
    col = "#000000", # show borders with color and line weight
    lwd = 2
  ) +
  tm_text("admin3name") # column text to display for each polygon


# Same as above, but with zoom from bounding box
tm_shape(sle_adm3,
  bbox = c(
    -13.3, 8.43, # corner
    -13.2, 8.5
  )
) + # corner
  tm_polygons(col = "#F7F7F7") +
  tm_borders(col = "#000000", lwd = 2) +
  tm_text("admin3name")

# And now both points and polygons together:
# All together
tm_shape(sle_adm3, bbox = c(-13.3, 8.43, -13.2, 8.5)) + #
  tm_polygons(col = "#F7F7F7") +
  tm_borders(col = "#000000", lwd = 2) +
  tm_text("admin3name") +
  tm_shape(linelist_sf) +
  tm_dots(size = 0.08, col = "blue", alpha = 0.5) +
  tm_layout(title = "Distribution of Ebola cases") # give title to map

# Spatial joins

# 1) assign administrative units to cases
linelist_adm <- linelist_sf %>%
  # join the administrative boundary file to the linelist, based on spatial intersection
  sf::st_join(sle_adm3, join = st_intersects)

# All the columns from sle_adms have been added to the linelist! Each case now
# has columns detailing the administrative levels that it falls within. In this
# example, we only want to keep two of the new columns (admin level 3), so we
# select() the old column names and just the two additional of interest:

linelist_adm <- linelist_sf %>%
  # join the administrative boundary file to the linelist, based on spatial intersection
  sf::st_join(sle_adm3, join = st_intersects) %>%
  # Keep the old column names and two new admin ones of interest
  select(names(linelist_sf), admin3name, admin3pcod)

# Now you will see the ADM3 names attached to each case
linelist_adm %>% select(case_id, admin3name, admin3pcod)

# Now we can describe our cases by administrative unit -
# something we were not able to do before the spatial join!

# Make new dataframe containing counts of cases by administrative unit
case_adm3 <- linelist_adm %>% # begin with linelist with new admin cols
  as_tibble() %>% # convert to tibble for better display
  group_by(admin3pcod, admin3name) %>% # group by admin unit, both by name and pcode
  summarise(cases = n()) %>% # summarize and count rows
  arrange(desc(cases)) # arrange in descending order

#case_adm3
#We can also create a bar plot of case counts by administrative unit.
#In this example, we begin the ggplot() with the linelist_adm, so that we can 
#apply factor functions like fct_infreq() which orders the bars by frequency
#(see page on Factors for tips).
ggplot(
  data = linelist_adm,                       # begin with linelist containing admin unit info
  mapping = aes(
    x = fct_rev(fct_infreq(admin3name))))+ # x-axis is admin units, ordered by frequency (reversed)
  geom_bar()+                                # create bars, height is number of rows
  coord_flip()+                              # flip X and Y axes for easier reading of adm units
  theme_classic()+                           # simplify background
  labs(                                      # titles and labels
    x = "Admin level 3",
    y = "Number of cases",
    title = "Number of cases, by adminstative unit",
    caption = "As determined by a spatial join, from 1000 randomly sampled cases from linelist"
  )

#Nearest neighbor
#Finding the nearest health facility / catchment area
#It might be useful to know where the health facilities are located in relation to the disease hot spots.
#We can use the st_nearest_feature join method from the st_join() 
#function (sf package) to visualize the closest health facility to individual cases.

# Closest health facility to each case
linelist_sf_hf <- linelist_sf %>%                  # begin with linelist shapefile  
  st_join(sle_hf, join = st_nearest_feature) %>%   # data from nearest clinic joined to case data 
  select(case_id, osm_id, name, amenity) %>%       # keep columns of interest, including id, name, type, and geometry of healthcare facility
  rename("nearest_clinic" = "name")                # re-name for clarity


#We can see that “Den Clinic” is the closest health facility for about ~30% of the cases.
#Count cases by health facility
hf_catchment <- linelist_sf_hf %>%   # begin with linelist including nearest clinic data
  as.data.frame() %>%                # convert from shapefile to dataframe
  count(nearest_clinic,              # count rows by "name" (of clinic)
        name = "case_n") %>%         # assign new counts column as "case_n"
  arrange(desc(case_n))              # arrange in descending order

hf_catchment                         # print to console

#To visualize the results, we can use tmap - this time interactive mode for easier viewing

tmap_mode("view")   # set tmap mode to interactive  

# plot the cases and clinic points 
tm_shape(linelist_sf_hf) +            # plot cases
  tm_dots(size=0.08,                  # cases colored by nearest clinic
          col='nearest_clinic') +    
  tm_shape(sle_hf) +                    # plot clinic facilities in large black dots
  tm_dots(size=0.3, col='black', alpha = 0.4) +      
  tm_text("name") +                   # overlay with name of facility
  tm_view(set.view = c(-13.2284, 8.4699, 13), # adjust zoom (center coords, zoom)
          set.zoom.limits = c(13,14))+
  tm_layout(title = "Cases, colored by nearest clinic")


#Buffers
# 1 decimal degree = ~111km.
# ~2.5km around each health facility

sle_hf_2k <- sle_hf %>%
  st_buffer(dist=0.02)       # decimal degrees translating to approximately 2.5km 

tmap_mode("view")
# Create circular buffers
tm_shape(sle_hf_2k) +
  tm_borders(col = "black", lwd = 2) +
  tm_shape(sle_hf) +                    # plot clinic facilities in large red dots
  tm_dots(size=0.3, col='black')          


#Find cases that are within the buffers
# Intersect the cases with the buffers

#Good to point out that none of the buffers intersected
linelist_sf_hf_2k <- linelist_sf_hf %>%
  st_join(sle_hf_2k, join = st_intersects, left = TRUE) %>%
  filter(osm_id.x==osm_id.y | is.na(osm_id.y)) %>%
  select(case_id, osm_id.x, nearest_clinic, amenity.x, osm_id.y)


# Cases which did not get intersected with any of the health facility buffers
linelist_sf_hf_2k %>% 
  filter(is.na(osm_id.y)) %>%
  nrow()

tmap_mode("view")

# First display the cases in points
tm_shape(linelist_sf_hf) +
  tm_dots(size=0.08, col='nearest_clinic') +
  
  # plot clinic facilities in large black dots
  tm_shape(sle_hf) +                    
  tm_dots(size=0.3, col='black')+   
  
  # Then overlay the health facility buffers in polylines
  tm_shape(sle_hf_2k) +
  tm_borders(col = "black", lwd = 2) +
  
  # Highlight cases that are not part of any health facility buffers
  # in red dots  
  tm_shape(linelist_sf_hf_2k %>%  filter(is.na(osm_id.y))) +
  tm_dots(size=0.1, col='red') +
  tm_view(set.view = c(-13.2284,8.4699, 13), set.zoom.limits = c(13,14))+
  
  # add title  
  tm_layout(title = "Cases by clinic catchment area")

#Other spatial joins
# Alternative values for argument join include (from the documentation)
# 
# st_contains_properly
# st_contains
# st_covered_by
# st_covers
# st_crosses
# st_disjoint
# st_equals_exact
# st_equals
# st_is_within_distance
# st_nearest_feature
# st_overlaps
# st_touches
# st_within

#Choropleth maps

# Add population data and calculate cases per 10K population
case_adm3 <- case_adm3 %>% 
  left_join(sle_adm3_pop,                             # add columns from pop dataset
            by = c("admin3pcod" = "adm3_pcode")) %>%  # join based on common values across these two columns
  select(names(case_adm3), total) %>%                 # keep only important columns, including total population
  mutate(case_10kpop = round(cases/total * 10000, 3)) # make new column with case rate per 10000, rounded to 3 decimals

case_adm3                                                # print to console for viewing


#Join this table with the ADM3 polygons shapefile for mapping

case_adm3_sf <- case_adm3 %>%                 # begin with cases & rate by admin unit
  left_join(sle_adm3, by="admin3pcod") %>%    # join to shapefile data by common column
  select(objectid, admin3pcod,                # keep only certain columns of interest
         admin3name = admin3name.x,           # clean name of one column
         admin2name, admin1name,
         cases, total, case_10kpop,
         geometry) %>%                        # keep geometry so polygons can be plotted
  drop_na(objectid) %>%                       # drop any empty rows
  st_as_sf()                                  # convert to shapefile

#Mapping the results

# tmap mode
tmap_mode("plot")               # view static map

# plot polygons
tm_shape(case_adm3_sf) + 
  tm_polygons("cases") +  # color by number of cases column
  tm_text("admin3name")   # name display

#We can also map the incidence rates

# Cases per 10K population
tmap_mode("plot")             # static viewing mode

# plot
tm_shape(case_adm3_sf) +                # plot polygons
  tm_polygons("case_10kpop",            # color by column containing case rate
              breaks=c(0, 10, 50, 100), # define break points for colors
              palette = "Purples"       # use a purple color palette
  ) +
  tm_text("admin3name")                 # display text

#Mapping with ggplot2
sle_adm3_dat <- sle_adm3 %>% 
  inner_join(case_adm3, by = "admin3pcod") # inner join = retain only if in both data objects

select(sle_adm3_dat, admin3name.x, cases) # print selected variables to console

ggplot(data=sle_adm3_dat) +
  geom_col(aes(x=fct_reorder(admin3name.x, cases, .desc=T),   # reorder x axis by descending 'cases'
               y=cases)) +                                  # y axis is number of cases by region
  theme_bw() +
  labs(                                                     # set figure text
    title="Number of cases, by administrative unit",
    x="Admin level 3",
    y="Number of cases"
  ) + 
  guides(x=guide_axis(angle=45))                            # angle x-axis labels 45 degrees to fit better

ggplot(data=sle_adm3_dat) + 
  geom_sf(aes(fill=cases))    # set fill to vary by case count variable


#We can then customize the appearance of our map using grammar that is consistent across ggplot2, for example:
  
ggplot(data=sle_adm3_dat) +                           
  geom_sf(aes(fill=cases)) +                        
  scale_fill_continuous(high="#54278f", low="#f2f0f7") +    # change color gradient
  theme_bw() +
  labs(title = "Number of cases, by administrative unit",   # set figure text
       subtitle = "Admin level 3"
  )

####
