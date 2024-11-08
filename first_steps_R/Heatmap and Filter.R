setwd("D:/Dokumente/Studium/9 FS/Study Project/Data")
library(osmdata)
library(sf)
library(tmap)
library(arrow)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(dplyr)

# Ort festlegen
place_name_GL <- "London"

# OSM-Daten abrufen
GL <- opq(place_name_GL) %>% 
  add_osm_feature(key = "boundary", value = "administrative") %>%
  osmdata_sf()

# Visualisierung mit tmap
tm_shape(GL$osm_multipolygons) + tm_polygons()

# Alternativ: Mit plot
plot(GL$osm_multipolygons["geometry"])

########################################################

london_city<- read_sf('msoa2021/City of London.shp')
movement <- read_parquet("pre_processed_movement.parquet")
tiles <- read_parquet("distinct_LONLAT.parquet")
movement <- merge(movement, tiles, by.x = "LONLAT_ID", by.y = "LONLAT_ID")
movement_sf <- st_as_sf(movement, coords = c("XLON", "XLAT"), crs = st_crs(london_city))
movement_filter <- movement_sf[movement_sf$AGG_DAY_PERIOD == "2020-04-06",]
movement_filter_join <- st_within(movement_filter, london_city, sparse = FALSE)

#movement_filter <- st_join(movement_filter, london_city, join = st_within)
#movement_filter_join <- movement_filter[!is.na(movement_filter$lad22nm), ]
movement_filter_join <- movement_filter_join[order(movement_filter_join$mean_column),]
movement_filter_coords <- as.data.frame(movement_filter_join)
movement_filter_coords <- cbind(movement_filter_coords, st_coordinates(movement_filter_join))

ggplot(movement_filter_coords, aes(x = X, y = Y, color = mean_column)) +
  geom_point() +
  theme_minimal() +
  scale_color_gradient(low = "blue", high = "red", limits = c(0, 3)) +
  labs(title = "Mean activity index in tiles on 2020-04-06")




london_city<- read_sf('msoa2021/City of London.shp')
london_city <- st_transform(london_city, crs = 4326)
movement <- read_parquet("pre_processed_movement.parquet")
tiles <- read_parquet("distinct_LONLAT.parquet")
movement_filter <- movement[movement$AGG_DAY_PERIOD == "2020-01-13",]
movement_merge <- merge(movement_filter, tiles, by.x = "LONLAT_ID", by.y = "LONLAT_ID")
movement_sf <- st_as_sf(movement_merge, coords = c("XLON", "XLAT"), crs = st_crs(london_city))
within_indices <- st_within(movement_sf, london_city, sparse = FALSE)
movement_join_within <- movement_sf[apply(within_indices, 1, any), ]
movement_join_within <- movement_join_within[order(movement_join_within$mean_column),]

ggplot(movement_join_within) +
  geom_sf(aes(color = mean_column)) +
  theme_minimal() +
  scale_color_gradient(low = "blue", high = "red", limits = c(0, 3)) +
  labs(title = "Mean activity index in tiles on 2020-04-06")

# Extrahiere die Koordinaten für movement_join_within
movement_join_within_coords <- movement_join_within %>%
  mutate(X = st_coordinates(.)[,1], Y = st_coordinates(.)[,2])

# Leaflet-Karte erstellen
leaflet() %>%
  # Grundkarte hinzufügen
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Heatmap hinzufügen
  addHeatmap(
    data = movement_join_within_coords,
    lng = ~X, lat = ~Y,
    intensity = ~mean_column,       # Nutze `mean_column` für Intensität
    blur = 20, max = max(movement_join_within_coords$mean_column, na.rm = TRUE), radius = 15, # Passe Parameter für die Darstellung an
    gradient = c("blue", "green", "yellow", "red")
  ) %>%
  
  # London City Polygon hinzufügen
  addPolygons(data = london_city,
              color = "black",
              weight = 2,
              fillOpacity = 0.1,
              group = "London City") %>%
  
  # Gefilterte Punkte hinzufügen, Farbskala für mean_column
  addCircleMarkers(data = movement_join_within_coords,
                   ~X, ~Y,
                   color = ~colorNumeric(palette = c("blue", "red"), domain = c(0, 3))(mean_column),
                   fillOpacity = 0.7,
                   radius = 4,
                   popup = ~paste("Mean Column:", mean_column),
                   group = "Movement Points") %>%
  
  # Layers control hinzufügen
  addLayersControl(
    overlayGroups = c("Heatmap", "London City", "Movement Points"),
    options = layersControlOptions(collapsed = FALSE)
  )

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addHeatmap(
    data = movement_join_within_coords,
    lng = ~X, lat = ~Y,
    intensity = ~mean_column,
    radius = 30,     # Startgröße des Radius
    blur = 1,       # Startwert für die Unschärfe 
    max = 1,
    gradient = c("blue", "green", "yellow", "red")
  )
