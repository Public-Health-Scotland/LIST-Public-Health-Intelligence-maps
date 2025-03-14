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
  st_transform(4326)

greenbelt <- greenbelt[2] %>% # Selecting key variables only
  mutate(colour = "green") # Adding colour variable for mapping

brownfield <- read_sf(paste0(path, "Vacant_and_Derelict_Land_-_Scotland/pub_vdlPolygon.shp")) %>% 
  filter(str_detect(local_auth, "Lanarkshire")) %>%  # Filtering for necessary data only
  st_transform(4326)

brownfield <- brownfield[1] %>% 
  mutate(colour = "brown")

rm(path)

### 3 Section Heading ----

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = greenbelt,
              fillColor = "green",
              weight = 1,
              color = "green") %>% 
  addPolygons(data = brownfield,
              fillColor = "brown",
              weight = 1,
              color = "brown") %>% 
  addLegend(position = "bottomleft", 
            title = "Site type",
            colors = c("green", "brown"), 
            labels = c("Greenfield", "Brownfield"))


### END OF SCRIPT ###


