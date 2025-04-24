
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
  palette = c('beige', 'pink', 'blue', 'darkred', 'green', "red", "darkgreen"),
  domain = All_Services_Locations$service_type
)




logos <- awesomeIconList(
  "Care Home" = makeAwesomeIcon(
    icon = "glyphicon-bed",
    markerColor = "beige"),
  "Community hospital"= makeAwesomeIcon(
    icon = "glyphicon-heart",
    markerColor = "pink"),  
  "Dentist"= makeAwesomeIcon(
    icon = "glyphicon-apple",
    markerColor = "blue"),
  "Emergency Department"= makeAwesomeIcon(
    icon = "glyphicon-header",
    markerColor = "darkred"),
  "GP Practice"= makeAwesomeIcon(
    icon = "glyphicon-user",
    markerColor = "green"),
  "Minor Injuries Unit"= makeAwesomeIcon(
    icon = "glyphicon-alert",
    markerColor = "red"),
  "Pharmacy"= makeAwesomeIcon(
    icon = "glyphicon-plus-sign",
    markerColor = "darkgreen")
)
  

base_map <- All_Services_Locations %>% 
  # Load map
  leaflet()  %>%
  addProviderTiles(providers$Stadia.AlidadeSmooth) %>% 
  # Set default area & zoom
  setView(lng = -3.99, lat = 55.74, zoom = 9) %>% 
  # Add train network
  addPolylines(data = rail_network,
               weight = 2,
               opacity = 0.5,
               color = "darkgreen") %>% 
  # Add cycling network
  addPolylines(data = cycling_network,
              weight = 2,
              opacity = 0.25,
              color = "purple") %>% 
  # Markers for sites
  #addCircleMarkers(
  #  lng = All_Services_Locations$longitude, lat = All_Services_Locations$latitude, group = "Sites",
  #  radius = 3.5, weight = 1, opacity = 1, fillOpacity = 1, fillColor = ~pal(service_type), color = ~pal(service_type)) %>% 
  addAwesomeMarkers(icon = ~logos[service_type], # lookup from list based on ticker
                    label = ~service_type) %>% 
  addLegend(data = All_Services_Locations,
            position = "bottomleft", 
            title = "Site Type",
            pal = pal, 
            values = ~service_type) 

