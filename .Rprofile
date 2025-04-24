# Set environment variables to point to installations of geospatial libraries ----
# Append paths to GDAL and PROJ to 'LD_LIBRARY_PATH'
Sys.setenv(LD_LIBRARY_PATH = paste(Sys.getenv("LD_LIBRARY_PATH"), "/usr/gdal34/lib", "/usr/proj81/lib", sep = ':'))

# Specify additional proj path in which pkg-config should look for .pc files ----
Sys.setenv("PKG_CONFIG_PATH" = "/usr/proj81/lib/pkgconfig")

# Specify the path to GDAL data ----
Sys.setenv("GDAL_DATA" = "/usr/gdal34/share/gdal")

# Load geospatial libraries
dyn.load("/usr/gdal34/lib/libgdal.so")
dyn.load("/usr/geos310/lib64/libgeos_c.so", local = FALSE)

