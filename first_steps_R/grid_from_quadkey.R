
library(stars)
library(ggplot2)
library(dplyr)
library(arrow)
library(slippymath)
# quadkey_to_tile = function(qk){
# 
#   if(nchar(qk)==0){
#     return(list(x=0,y=0,zoom=0))
#   }
# 
#   digits = rev(strsplit(qk,"")[[1]])
# 
#   i = length(digits)
# 
#   masks = 2**(0:(i-1))
#   xt = sum(masks[digits=="1" | digits=="3"])
#   yt = sum(masks[digits=="2" | digits=="3"])
# 
#   return(list(x=xt, y=yt, zoom=i))
# }

# Function to calculate quadkey for a given x, y, and zoom level
generate_quadkey <- function(x, y, z) {
  quadkey <- ""
  for (i in seq(z, 1, -1)) {
    digit <- 0
    mask <- bitwShiftL(1, i - 1)
    if (bitwAnd(x, mask) != 0) digit <- digit + 1
    if (bitwAnd(y, mask) != 0) digit <- digit + 2
    quadkey <- paste0(quadkey, digit)
  }
  return(quadkey)
}

# Define the zoom level
z <- 18

# Define the bounding box for Greater London (approximately)
#change this to smaller region if needed
lon_min <- -0.5103
lon_max <- 0.3340
lat_min <- 51.2868
lat_max <- 51.6919

# Calculate the tile numbers for the bounding box at zoom level z
tile_min <- slippymath::lonlat_to_tilenum(lon_min, lat_min, z)
tile_max <- slippymath::lonlat_to_tilenum(lon_max, lat_max, z)

# Create a grid of tile x and y numbers that cover the Greater London bounding box
tile_x_range <- tile_min$x:tile_max$x
tile_y_range <- tile_min$y:tile_max$y


# Convert tile numbers to longitudes and latitudes
lonlat_data <- expand.grid(x = tile_x_range, y = tile_y_range) |>
  mutate(lon_min = slippymath::tilenum_to_lonlat(x, y + 1, z)$lon,
         lon_max = slippymath::tilenum_to_lonlat(x + 1, y, z)$lon,
         lat_min = slippymath::tilenum_to_lonlat(x, y + 1, z)$lat,
         lat_max = slippymath::tilenum_to_lonlat(x + 1, y, z)$lat)

# Create a grid of polygons
grid_polygons <- lonlat_data |>
  rowwise() |>
  mutate(geometry = list(st_polygon(list(matrix(c(lon_min, lat_min,
                                                  lon_max, lat_min,
                                                  lon_max, lat_max,
                                                  lon_min, lat_max,
                                                  lon_min, lat_min), 
                                                ncol=2, byrow=TRUE))))) |>
  ungroup()

# Convert to an sf object with explicit geometry
grid_polygons <- st_sf(grid_polygons, crs = 4326)

grid_polygons |> st_write("grids_18.shp")


##reading the existing parquet file
##optional
df_lonlat <- read_parquet("Desktop/Study_project/distinct_LONLAT.parquet")
df_lonlat |> st_as_sf(coords = c("XLON", "XLAT")) -> df_lonlat.sf
df_lonlat.sf |> st_write("distinct_lonlat.shp")

