
library(tidyverse)
library(leaflet)
library(sf) # Reading shape files



# Read in public transport network shapefiles --------------------------------------

# Cycling data
cycling_network <- read_sf("Cycling_Network_-_Scotland/pub_cycnt.shp") %>% 
  st_transform(4326) # transform polygons to long/lat system

rail_network <- read_sf("UK_Railways/Railway.shp") %>% 
  filter(FILE_NAME == "gb_north") %>% 
  st_set_crs(27700) %>% # Assign coordinate ref. system to data
  st_transform(4326) # Convert to long/lat system



pal <- colorFactor(
  palette = c('red', 'blue', 'green', 'purple', 'orange', "grey", "black"),
  domain = All_Services_Locations$service_type
)

All_Services_Locations %>% 
  leaflet()  %>% 
  addProviderTiles(providers$Stadia.AlidadeSmooth) %>% 
  # Add train network
  # Add train network
  addPolylines(data = rail_network,
               weight = 2,
               opacity = 0.25,
               color = "red") %>% 
  # Add cycling network
  addPolylines(data = cycling_network,
              weight = 2,
              opacity = 0.25,
              color = "purple") %>% 
  # Markers for sites
  addCircleMarkers(
    lng = All_Services_Locations$longitude, lat = All_Services_Locations$latitude, group = "Sites",
    radius = 3.5, weight = 1, opacity = 1, fillOpacity = 1, fillColor = ~pal(service_type), color = ~pal(service_type)) %>% 
  addLegend(data = All_Services_Locations,
            position = "bottomleft", 
            title = "Site Type",
            pal = pal, 
            values = ~service_type) 




