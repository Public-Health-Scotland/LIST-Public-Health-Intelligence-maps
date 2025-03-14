#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Lanarkshire Mapping req.

# James Kilgour, james.kilgour2@phs.scot

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# 1. Housekeeping and set up -------------------------------------------------

# 1.1 Setting up environment variables for maaping ------------------------

# Amend 'LD_LIBRARY_PATH' ----
# Get the existing value of 'LD_LIBRARY_PATH'
old_ld_path <- Sys.getenv("LD_LIBRARY_PATH") 

# Append paths to GDAL and PROJ to 'LD_LIBRARY_PATH'
Sys.setenv(LD_LIBRARY_PATH = paste(old_ld_path,
                                   "/usr/gdal34/lib",
                                   "/usr/proj81/lib",
                                   sep = ":"))
rm(old_ld_path)

## Specify additional proj path in which pkg-config should look for .pc files ----

Sys.setenv("PKG_CONFIG_PATH" = "/usr/proj81/lib/pkgconfig")

## Specify the path to GDAL data ----
Sys.setenv("GDAL_DATA" = "/usr/gdal34/share/gdal")

# Load geospatial libraries
dyn.load("/usr/gdal34/lib/libgdal.so")
dyn.load("/usr/geos310/lib64/libgeos_c.so", local = FALSE)



# Now we can load our packages:
library(tidyverse)
library(sf)
library(leaflet)
library(sp)
library(raster)
library(terra)


# 2 Importing shapefile data ----------------------------------------------

path <- "/conf/LIST_analytics/Lanarkshire/Documentation/James/Mapping for Ruth/mapping-for-ruth/Greenbelt data/"

greenbelt <- read_sf(paste0(path, "Green_Belt_-_Scotland/pub_grnblt.shp")) %>% 
  filter(str_detect(local_auth, "Lanarkshire")) %>%  # Filtering for necessary data only
  st_transform(4326) %>% 
  dplyr::select(local_auth) %>% # Selecting key variables only
  mutate(colour = "green") # Adding colour variable for mapping

brownfield <- read_sf(paste0(path, "Vacant_and_Derelict_Land_-_Scotland/pub_vdlPolygon.shp")) %>% 
  filter(str_detect(local_auth, "Lanarkshire")) %>%  # Filtering for necessary data only
  st_transform(4326) %>% 
  dplyr::select(local_auth)%>% 
  mutate(colour = "brown")

rm(path)



population_density <- read_sf("/conf/linkage/output//lookups/Unicode/Geography/Shapefiles/SG_DataZoneCent_2011/SG_DataZone_Cent_2011.shp") %>% 
  janitor::clean_names() %>% 
  dplyr::select(data_zone, tot_pop2011) %>% 
  st_transform(4326) %>% 
  
  left_join(
  phsopendata::get_resource("d6e500c4-c1f2-4507-979a-e18855efd7a4") %>% 
    janitor::clean_names() %>% 
    filter(hscp %in% c("S37000021", "S37000028", "S37000035")) %>% 
    mutate(hscp = if_else(hscp == "S37000028", "South Lanarkshire", "North Lanarkshire")) %>% 
    dplyr::select(data_zone, locality = sub_hscp_name, hscp),
  
  by = "data_zone") %>%  # end of left_join() call
  
  na.omit() %>% 
  
  left_join(read_rds("/conf/linkage/output//lookups/Unicode/Geography/Urban Rural Classification/DataZone2011_urban_rural_2020v2.rds") %>% 
              janitor::clean_names() %>% 
              dplyr::select(data_zone = datazone2011, ur8_2020_name),
            
            by = "data_zone") 

pal_1 <- colorNumeric("magma", population_density$tot_pop2011)
pal_2 <- colorFactor("magma", population_density$ur8_2020_name)


### 3 Section Heading ----

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = greenbelt,
              fillColor = "green",
              weight = 1,
              color = "green",
              group = "Land use") %>% 
  addPolygons(data = brownfield,
              fillColor = "brown",
              weight = 1,
              color = "brown",
              group = "Land use") %>% 
  addPolygons(data = population_density,
              fillColor = ~pal_1(tot_pop2011),
              weight = 1,
              color = "black",
              group = "Population estimates") %>%
  addPolygons(data = population_density,
              fillColor = ~pal_2(ur8_2020_name),
              weight = 1,
              color = "black",
              group = "Urbanity") %>%
  addLegend(position = "bottomleft", 
            title = "Site type",
            colors = c("green", "brown"), 
            labels = c("Greenfield", "Brownfield"),
            group = "Land use") %>%
  addLegend(pal = pal,
            position = "bottomleft", 
            title = "Urbanity",
            labels = population_density$ur8_2020_name,
            group = "Urbanity") %>%
  addLayersControl(
    overlayGroups = c("Land use", "Population estiamtes", "Urbanity"),
    options = layersControlOptions(collapsed = FALSE)
  )


### END OF SCRIPT ###


