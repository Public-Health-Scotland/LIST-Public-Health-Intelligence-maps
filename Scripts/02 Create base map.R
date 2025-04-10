
library(tidyverse)
library(leaflet)
library(sf) # Reading shape files



# Read in cycling network shapefiles --------------------------------------


cycling_network <- read_sf("Cycling_Network_-_Scotland/pub_cycnt.shp") %>% 
  # transform the polygons
  st_transform(4326)



pal <- colorFactor(
  palette = c('red', 'blue', 'green', 'purple', 'orange', "grey", "black"),
  domain = All_Services_Locations$service_type
)

All_Services_Locations %>% 
  leaflet()  %>% 
  addProviderTiles(providers$Stadia.AlidadeSmooth) %>% 
  # Add train network
  addProviderTiles(providers$OpenRailwayMap) %>% 
  # Add cycling network
  addPolylines(data = cycling_network,
              weight = 1,
              opacity = 0.75,
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




