# Map 
library(sf)
library(tmap)
library(readxl)

# read excel with coords
coords <- read_excel("data/coords.xlsx", col_names = T)

# transform to sf object
coords_sf <- st_as_sf(coords, coords = c("Long", "Lat"), crs = 4326)

# plot 
tmap_mode("view")

tm_shape(coords_sf) +
  tm_dots()
