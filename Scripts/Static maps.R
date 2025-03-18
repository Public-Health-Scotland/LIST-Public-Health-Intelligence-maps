# 1. Housekeeping and set up -------------------------------------------------

# Mapping Packages
# For installation instructions & setting up environment
# see the following link:
# https://public-health-scotland.github.io/knowledge-base/docs/Posit%20Infrastructure?doc=How%20to%20Install%20and%20Use%20Geospatial%20R%20Packages.md

library(tidyverse)
library(leaflet) # Creating map
library(sf) # Reading shape files
library(RColorBrewer) # colour palette
 
# 2 Importing shapefile data ----------------------------------------------

# greenbelt shapefiles
greenbelt <- read_sf(paste0("Greenbelt data/", "Green_Belt_-_Scotland/pub_grnblt.shp")) %>% 
  filter(str_detect(local_auth, "Lanarkshire")) %>%  # Filtering for necessary data only
  st_transform(4326) %>% 
  mutate(colour = "green") # Adding colour variable for mapping

# Scottish Vacant and Derelict Land 
vdl <- read_sf(paste0("Greenbelt data/", "Vacant_and_Derelict_Land_-_Scotland/pub_vdlPolygon.shp")) %>% 
  filter(str_detect(local_auth, "Lanarkshire")) %>%  # Filtering for necessary data only
  st_transform(4326) %>% 
  mutate(colour = case_when(site_type == "Derelict" ~ "brown",
                            site_type == "Vacant Land" ~ "grey"),
    site_name = str_to_sentence(site_name))

# Greenfiled/Brownfield status - housing & land supply
housing_land <- read_sf("/conf/LIST_analytics/West Hub/Geospatial Cross Team/Projects/Housing Land Supply/HLAA2019_GCV.shp") %>%
  janitor::clean_names() %>% 
  filter(authority %in% c("NL", "SL")) %>%  # Filtering for necessary data only
  st_transform(4326)

# Population density & urbanity data
# read in datazone shapefiles
population_density <- read_sf("/conf/linkage/output/lookups/Unicode/Geography/Shapefiles/Data Zones 2011/SG_DataZone_Bdry_2011.shp") %>% 
  janitor::clean_names() %>% 
  dplyr::select(data_zone, std_area_km2, hscp2019) %>%
  filter(hscp2019 %in% c("S37000028", "S37000035")) %>%
  st_transform(4326) %>% 
  
# add most recent pop estimates
  left_join(
    phsopendata::get_resource("c505f490-c201-44bd-abd1-1bd7a64285ee") %>% 
      janitor::clean_names() %>% 
      filter(year == max(year), sex == "All") %>%
      dplyr::select(data_zone, pop_year = year, all_ages),
    
    by = "data_zone") %>%  # end of left_join() call

# add urban-rural classification
  left_join(read_rds("/conf/linkage/output/lookups/Unicode/Geography/Urban Rural Classification/DataZone2011_urban_rural_2020v2.rds") %>% 
              janitor::clean_names() %>% 
              dplyr::select(data_zone = datazone2011, ur8_2020_name),
            
            by = "data_zone") %>%
  
# calculate population density
  mutate(pop_density = all_ages/std_area_km2)

# rural dataset with combined polygons for each classification 
rurality <- population_density %>%
  group_by(ur8_2020_name) %>%
  summarize(geometry = st_union(geometry))

### 3 Build maps ----

# blues <- RColorBrewer::brewer.pal("Blues")
# darker_blues <- RColorBrewer::brewer.pal(name = "Blues", n = 9)[4:8]

#bin_colours <- brewer.pal(n = 9, name = "BuPu")

# Define colour palettes
#pal_1 <- colorBin(bin_colours, population_density$pop_density, bins = 7, pretty = T)
pal_1 <- colorNumeric("magma", population_density$pop_density)
pal_2 <- colorFactor("magma", population_density$ur8_2020_name)

# Greenbelt map
leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = greenbelt,
              fillColor = "green",
              weight = 1,
              color = "black") %>%
  addLegend(position = "bottomleft", 
            title = "Green Belt",
            colors = c("green"), 
            labels = c("")) 

# Vacant & Derelict Land map

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = vdl,
              fillColor = ~colour,
              weight = 1,
              color = "black") %>%
  addLegend(position = "bottomleft", 
            title = "Vacant and Derelict Land",
            colors = c("grey", "brown"), 
            labels = c("Vacant", "Derelict")) 

# Greenfield/Brownfield map
leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = housing_land,
              fillColor = ~devtype2,
              weight = 1,
              color = "black") %>%
  addLegend(position = "bottomleft", 
            title = "Greenfield/Brownfield status",
            colors = c("green", "brown"), 
            labels = c("Greenfield", "Brownfield")) 


# Population density
leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = population_density,
              fillColor = ~pal_1(pop_density),
              fillOpacity = 0.6,
              smoothFactor = 0.1, # better accuracy to avoid "gaps" between polygons when zoomed out
              stroke = F, # this removes borderlines around polygons
              weight = 1
              ) %>%
  addLegend(data = population_density,
            position = "bottomleft", 
            title = "Population density (pop/km2)",
            pal = pal_1,
            values = ~pop_density) 

  
# Rurality
leaflet() %>% 
  addTiles() %>%   
  addPolygons(data = rurality,
              fillColor = ~pal_2(ur8_2020_name),
              fillOpacity = 0.5,
              weight = 1,
              color = ~pal_2(ur8_2020_name)) %>%
  addLegend(data = population_density,
            position = "bottomleft", 
            title = "Urban/Rural Classification",
            pal = pal_2, 
            values = ~ur8_2020_name) 

