setwd("D:/Dokumente/Studium/9 FS/Study Project/Data")
library(osmdata)
library(sf)
library(tmap)
library(arrow)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(shiny)


london<- read_sf('msoa2021/merged_districts.shp')
london <- st_transform(london, crs = 4326)
movement <- read_parquet("pre_processed_movement.parquet")
tiles <- read_parquet("distinct_LONLAT.parquet")
#pois <- read.csv("D:/Dokumente/Studium/9 FS/Study Project/Mobile-ity-Scope/first_steps_python/attractionsGreaterLondon.csv")
shops <- read_sf('D:/Dokumente/Studium/9 FS/Study Project/Mobile-ity-Scope/POIs/shops.shp')
museums <- read_sf('D:/Dokumente/Studium/9 FS/Study Project/Mobile-ity-Scope/POIs/museums.shp')
shops$type <- "shop"
museums$type <- "museum"
pois <- rbind(shops, museums)

poi_name <- "MCC Museum and Tour"
poi_filter <- pois %>%
  filter(name == poi_name)

#Buffer around POI
poi_buffer <- st_buffer(poi_filter, 100)

tiles_sf <- st_as_sf(tiles, coords = c("XLON", "XLAT"), crs = 4326)

tiles_indices <- st_within(tiles_sf, poi_buffer, sparse = FALSE)
tiles_within <- tiles_sf[apply(tiles_indices, 1, any), ]

#plot poi_filter as point and buffer on leaflet
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(data = poi_filter, radius = 5, color = "red") %>%
  addCircleMarkers(data = tiles_within, radius = 5, color = "green") %>%
  addPolygons(data = poi_buffer, color = "blue", fillOpacity = 0.2)


movement_merge <- merge(movement, tiles_within, by.x = "LONLAT_ID", by.y = "LONLAT_ID")
movement_mean <- movement_merge %>%
  group_by(AGG_DAY_PERIOD) %>%
  summarise(mean_value = mean(mean_column, na.rm = TRUE))
movement_mean$AGG_DAY_PERIOD <- as.Date(movement_mean$AGG_DAY_PERIOD)

ggplot(data = movement_mean, aes(x = AGG_DAY_PERIOD, y = mean_value)) +
  geom_line(color = "blue") +
  labs(
    title = poi_name,
    x = "Date",
    y = "Acitivity"
  ) +
  theme_minimal()    
