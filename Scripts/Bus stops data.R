
bus_url <- "https://naptan.api.dft.gov.uk/v1/access-nodes?dataFormat=csv"

locality_filter <- c("Airdrie",
                     "Coatbridge",
                     "North Lanarkshire North",
                     "Bellshill",
                     "Motherwell",
                     "Wishaw",
                     "Cambuslang", # Need to separate these two out
                     "Rutherglen", # Need to separate these two out 
                     "East Kilbride",
                     "Clydesdale",
                     "Hamilton")

bus_routes_data <- read_csv(bus_url) %>% 
  janitor::clean_names() 

bus_routes_data <- bus_routes_data %>% 
  filter(parent_locality_name %in% locality_filter)



# Testing completion ------------------------------------------------------

bus_routes_data %>% 
  filter(is.na(longitude) | is.na(latitude)) %>% 
  nrow

bus_routes_data %>% 
  filter(is.na(easting) | is.na(northing)) %>% 
  nrow


bus_routes_data <- cbind(bus_routes_data,
                         bus_routes_data %>%
                           st_as_sf(coords = c("easting", "northing"), crs = 27700) %>% st_transform(4326) %>%
                           st_coordinates() %>%
                           as_tibble()
                         )#  end of cbind() call

# Okay, we'll have to convert N/E-ing data to long-lat



leaflet()  |> 
  addProviderTiles(providers$OpenStreetMap) |> 
  addCircleMarkers(
    lng = bus_routes_data$X, lat = bus_routes_data$Y, group = "Bus routes",
    color = "black", radius = 1.5, weight = 1, opacity = 1, fillOpacity = 1, fillColor = "blue"
  ) 

# Incomplete. Seems OpenStreepMap base layer has bus stop icons we could work with already.
# Maybe better to look into routes data a-la trains and bikes done already.

