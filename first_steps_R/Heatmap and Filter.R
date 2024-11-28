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


################################################################################

london_city<- read_sf('msoa2021/Westminster.shp')
london_city <- st_transform(london_city, crs = 4326)
movement <- read_parquet("pre_processed_movement.parquet")
tiles <- read_parquet("distinct_LONLAT.parquet")
movement_filter <- movement[movement$AGG_DAY_PERIOD == "2020-04-06",]
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

#Pois
# CSV-Datei laden
# (Ersetze 'deine_datei.csv' durch den Pfad zur Datei)
pois <- read.csv("D:/Dokumente/Studium/9 FS/Study Project/Mobile-ity-Scope/first_steps_python/attractionsGreaterLondon.csv")

# GeoJSON-Geometrie in sf-Objekte konvertieren
pois_sf <- pois %>%
  mutate(geometry = st_as_sfc(geometry)) %>%
  st_as_sf()
st_crs(pois_sf) <- 4326

# Berechne den Schwerpunkt für Polygone, Punkte bleiben unverändert
pois_sf <- pois_sf %>%
  mutate(geometry = st_centroid(geometry))

# Extrahiere Koordinaten aus dem sf-Objekt
coords <- st_coordinates(pois_sf)

# Füge die Koordinaten dem sf-Objekt als zusätzliche Spalten hinzu
pois_sf <- pois_sf %>%
  mutate(lon = coords[, 1], lat = coords[, 2])

pois_indices <- st_within(pois_sf, london_city, sparse = FALSE)
pois_within <- pois_sf[apply(pois_indices, 1, any), ]

# Leaflet-Karte erstellen
leaflet() %>%
  # Grundkarte hinzufügen
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Heatmap hinzufügen
  addHeatmap(
    data = movement_join_within_coords,
    lng = ~X, lat = ~Y,
    intensity = ~mean_column,
    radius = 100,     # Startgröße des Radius
    blur = 0,       # Startwert für die Unschärfe 
    max = 3,
    gradient = c("blue", "green", "yellow", "red")) %>%
  
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
  
  # POIs
  addMarkers(data=pois_within,lng = ~lon, lat = ~lat,
                   popup = ~paste("Name:", name),
                   group = "POIs") %>%
  
  # Layers control hinzufügen
  addLayersControl(
    overlayGroups = c("Heatmap", "London City", "Movement Points","POIs"),
    options = layersControlOptions(collapsed = FALSE)
  )

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addHeatmap(
    data = movement_join_within_coords,
    lng = ~X, lat = ~Y,
    intensity = ~mean_column,
    radius = 100,     # Startgröße des Radius
    blur = 0,       # Startwert für die Unschärfe 
    max = 3,
    gradient = c("blue", "green", "yellow", "red")
  )


st_write(movement_join_within, "Shape_Auswahl/2020_01_13.shp", driver = "ESRI Shapefile")
st_write(movement_join_within, "Shape_Auswahl/2020_04_06.shp", driver = "ESRI Shapefile")

################################################################################
# Mehrere Districte

# Shiny-App
# ui <- fluidPage(
#   leafletOutput("map")
# )
# 
# server <- function(input, output, session) {
#   output$map <- renderLeaflet({
#     filter_data(movement, tiles, london, "2020-04-06", c("City of London", "Westminster"))  # Verwende die Funktion hier
#   })
# }
# 
# shinyApp(ui, server)

filter_data <- function(movement_data, tiles, london, pois, hexagons, date, districts) {
  print(districts)
  print(date)
  # Filter districts
  london_filtered <- london[london$district_n %in% districts, ]
  # Filter movement data with date
  movement_filter <- movement_data[movement_data$AGG_DAY_PERIOD == date,]
  # Merge movement data with tiles and convert to sf object
  movement_merge <- merge(movement_filter, tiles, by.x = "LONLAT_ID", by.y = "LONLAT_ID")
  movement_sf <- st_as_sf(movement_merge, coords = c("XLON", "XLAT"), crs = st_crs(london))
  # Get movement data which is within the given districts
  within_indices <- st_within(movement_sf, london_filtered, sparse = FALSE)
  movement_join_within <- movement_sf[apply(within_indices, 1, any), ]
  movement_join_within <- movement_join_within[order(movement_join_within$mean_column),]
  # Filter hexagons
  intersects_indices <- st_intersects(hexagons, london_filtered, sparse = FALSE)
  hexagons_intersects <- hexagons[apply(intersects_indices, 1, any), ]
  
  # Hexagons
  movement_join_within_hex <- st_join(movement_join_within, hexagons_intersects, join = st_within)
  hexagon_means <- movement_join_within_hex %>%
    group_by(grid_id) %>%
    summarise(mean_value = mean(mean_column, na.rm = TRUE))
  hexagon_means <- hexagon_means %>% st_drop_geometry()
  hexagons_with_means <- hexagons_intersects %>%
    left_join(hexagon_means, by = c("grid_id" = "grid_id"))
  
  # Add coords to the table
  movement_join_within_coords <- movement_join_within %>%
    mutate(X = st_coordinates(.)[,1], Y = st_coordinates(.)[,2])
  
  # GeoJSON-Geometrie in sf-Objekte konvertieren
  pois_sf <- pois %>%
    mutate(geometry = st_as_sfc(geometry)) %>%
    st_as_sf()
  st_crs(pois_sf) <- 4326
  
  # Berechne den Schwerpunkt für Polygone, Punkte bleiben unverändert
  pois_sf <- pois_sf %>%
    mutate(geometry = st_centroid(geometry))
  
  # Extrahiere Koordinaten aus dem sf-Objekt
  coords <- st_coordinates(pois_sf)
  
  # Füge die Koordinaten dem sf-Objekt als zusätzliche Spalten hinzu
  pois_sf <- pois_sf %>%
    mutate(lon = coords[, 1], lat = coords[, 2])
  
  pois_indices <- st_within(pois_sf, london_filtered, sparse = FALSE)
  pois_within <- pois_sf[apply(pois_indices, 1, any), ]
  
  return (list(
    london_filtered = london_filtered, 
    movement_join_within_coords = movement_join_within_coords,
    pois_within = pois_within,
    hexagons_with_means = hexagons_with_means))
}

london<- read_sf('msoa2021/merged_districts.shp')
london <- st_transform(london, crs = 4326)
movement <- read_parquet("pre_processed_movement.parquet")
tiles <- read_parquet("distinct_LONLAT.parquet")
pois <- read.csv("D:/Dokumente/Studium/9 FS/Study Project/Mobile-ity-Scope/first_steps_python/attractionsGreaterLondon.csv")
hexagons <- read_sf('hexgr.shp')
#hexagons <- st_transform(hexagons, st_crs(london))

london_filtered <- london[london$district_n %in% c("City of London", "Westminster"), ]

result <- filter_data(movement, tiles, london, pois, hexagons, "2020-01-30", c("City of London", "Westminster"))
movement_join_within_coords <- result$movement_join_within_coords
london_filtered <- result$london_filtered
pois_within <- result$pois_within
hexagons_with_means <- result$hexagons_with_means

# Leaflet-Karte erstellen
leaflet() %>%
  # Grundkarte hinzufügen
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Heatmap hinzufügen
  addHeatmap(
    data = movement_join_within_coords,
    lng = ~X, lat = ~Y,
    intensity = ~mean_column,
    radius = 100,     # Startgröße des Radius
    blur = 0,       # Startwert für die Unschärfe 
    max = 3,
    gradient = c("blue", "green", "yellow", "red"),
    group = "Heatmap") %>%
  
  # London City Polygon hinzufügen
  addPolygons(data = london_filtered,
              color = "black",
              weight = 2,
              fillOpacity = 0.1,
              group = "London") %>%
  
  # Hexagons
  addPolygons(data = hexagons_with_means,
              color = "black",
              weight = 1,
              fillColor = ~colorNumeric(palette = c("blue", "red"), domain = c(0, 3))(mean_value),
              fillOpacity = 0.7,
              popup = ~paste("Mean Column:", mean_value),
              group = "Hexagons") %>%
  
  # Gefilterte Punkte hinzufügen, Farbskala für mean_column
  addCircleMarkers(data = movement_join_within_coords,
                   ~X, ~Y,
                   color = ~colorNumeric(palette = c("blue", "red"), domain = c(0, 3))(mean_column),
                   fillOpacity = 0.7,
                   radius = 4,
                   popup = ~paste("Mean Column:", mean_column),
                   group = "Movement Points") %>%
  
  # POIs
  addMarkers(data=pois_within,lng = ~lon, lat = ~lat,
             popup = ~paste("Name:", name),
             group = "POIs") %>%
  
  # Layers control hinzufügen
  addLayersControl(
    overlayGroups = c("Heatmap", "London", "Movement Points","POIs","Hexagons"),
    options = layersControlOptions(collapsed = FALSE)
  )


################################################################################

library(MASS)
# Benutzerdefinierte Funktion für gewichtete Kernel-Dichte
kde2d.weighted <- function(x, y, w, n = 100, lims = c(range(x), range(y))) {
  library(MASS)
  nx <- length(x)
  kde <- kde2d(x, y, n = n, lims = lims)
  kde$z <- matrix(0, nrow = n, ncol = n)
  for (i in 1:nx) {
    kde$z <- kde$z + w[i] * dnorm(kde$x - x[i], sd = bw.nrd(x)) %o% dnorm(kde$y - y[i], sd = bw.nrd(y))
  }
  kde$z <- kde$z / sum(w)
  return(kde)
}

kde <- kde2d.weighted(movement_join_within_coords$X, movement_join_within_coords$Y, movement_join_within_coords$mean_column)
# In ein Datenframe für ggplot2 umwandeln
kde_df <- data.frame(
  lon = rep(kde$x, each = length(kde$y)),
  lat = rep(kde$y, length(kde$x)),
  density = as.vector(kde$z)
)

# Heatmap plotten
ggplot(kde_df, aes(x = lon, y = lat, fill = density)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(title = "Weighted Kernel Density Heatmap", x = "Längengrad", y = "Breitengrad") +
  theme_minimal()

##################################################################################
# Umsetzung ChatGPT Code zu DBSCAN

library(dbscan)
library(sf)

# Funktion zur Berechnung räumlich-zeitlicher Distanzen
spatio_temporal_dist <- function(data, eps1, eps2, delta_time, activity_col) {
  # Aktivitätsspalte validieren
  if (!activity_col %in% colnames(data)) {
    stop("Die angegebene Aktivitätsspalte existiert nicht im Datensatz.")
  }
  
  # Räumliche Distanz berechnen
  dist_matrix <- as.matrix(dist(cbind(data$x, data$y))) # Koordinaten
  time_diff <- outer(data$time, data$time, FUN = function(t1, t2) abs(t1 - t2)) # Zeitliche Differenzen
  activity_diff <- as.matrix(dist(data[[activity_col]])) # Aktivitätsdifferenzen
  
  # Kombination der Bedingungen
  valid_dist <- (dist_matrix <= eps1) &
    (time_diff <= delta_time) &
    (activity_diff <= eps2)
  return(valid_dist)
}

st_dbscan <- function(data, eps1, eps2, delta_time, min_pts) {
  n <- nrow(data)
  visited <- rep(FALSE, n)
  clusters <- rep(NA, n)
  cluster_id <- 0
  
  dist_matrix <- spatio_temporal_dist(data, eps1, eps2, delta_time, "mean_column")
  
  for (i in 1:n) {
    if (!visited[i]) {
      visited[i] <- TRUE
      neighbors <- which(dist_matrix[i, ])
      
      if (length(neighbors) >= min_pts) {
        cluster_id <- cluster_id + 1
        clusters <- expand_cluster(data, i, neighbors, clusters, cluster_id, visited, dist_matrix, min_pts)
      }
    }
  }
  return(clusters)
}

expand_cluster <- function(data, point_id, neighbors, clusters, cluster_id, visited, dist_matrix, min_pts) {
  clusters[point_id] <- cluster_id
  stack <- neighbors
  
  while (length(stack) > 0) {
    current_point <- stack[1]
    stack <- stack[-1]
    
    if (!visited[current_point]) {
      visited[current_point] <- TRUE
      new_neighbors <- which(dist_matrix[current_point, ])
      
      if (length(new_neighbors) >= min_pts) {
        stack <- union(stack, new_neighbors)
      }
    }
    
    if (is.na(clusters[current_point])) {
      clusters[current_point] <- cluster_id
    }
  }
  return(clusters)
}

clusters <- st_dbscan(data, eps1 = 20, eps2 = 5, delta_time = 2, min_pts = 2)
# Ergebnisse dem Datensatz hinzufügen
data$cluster <- clusters

ggplot(data, aes(x = x, y = y, color = as.factor(cluster))) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(color = "Cluster")

################################################################################
# Morans I
# https://stats.oarc.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/

library(ape)

movement_join_within_coords.dists <- as.matrix(dist(cbind(movement_join_within_coords$X, movement_join_within_coords$Y)))

movement_join_within_coords.dists.inv <- 1/movement_join_within_coords.dists
diag(movement_join_within_coords.dists.inv) <- 0

movement_join_within_coords.dists.inv[1:5, 1:5]

Moran.I(movement_join_within_coords$mean_column, movement_join_within_coords.dists.inv)

################################################################################
# Shiny App

library(shiny)

# define the user interface
ui <- fluidPage(
  titlePanel("Mobility Scope"),
  sidebarLayout(
    sidebarPanel(
      dateInput("selected_date", "Choose Date:", value = "2020-01-01"),
      checkboxGroupInput("districts", "Choose districts:", 
                         choices = unique(london$district_n),
                         selected = c("City of London", "Westminster")),
      actionButton("select_all", "Select All"),
      actionButton("submit", "Update Map")
    ),
    mainPanel(
      leafletOutput("map", height = 600)
    )
  )
)

#movement_data, tiles, london, pois, date, districts
# define the server logic
server <- function(input, output, session) {
  # "Select All"-Button Logik
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "districts", 
                             selected = unique(london$district_n))  # Alle Districte auswählen
  })
  
  # Daten filtern nur bei Knopfdruck
  filtered_data <- eventReactive(input$submit, {
    req(input$districts, input$selected_date)
    filter_data(
      movement_data = movement,
      tiles = tiles,
      london = london,
      pois = pois,
      hexagons = hexagons,
      date = as.character(input$selected_date),
      districts = input$districts
    )
  })
  
  # Initiale Karte rendern
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -0.1, lat = 51.5, zoom = 12)
  })
  
  # Karte aktualisieren, wenn sich die gefilterten Daten ändern
  observe({
    print(paste("Chosen Date:", input$selected_date))
    print(paste("Chosen Districts:", paste(input$districts, collapse = ", ")))
    data <- filtered_data()
    
    fixed_bin_scale <- colorBin(
      palette = c("blue", "red"),  # Farbpalette von Blau bis Rot
      domain = c(0, 3),  # Feste Skala von 0 bis 3
      bins = 5  # Anzahl der Farben (Bins)
    )
    
    # Karte aktualisieren
    leafletProxy("map") %>%
      clearGroup(c("Heatmap", "London", "Movement Points", "POIs", "Hexagons")) %>%
      clearControls() %>%
      # Heatmap
      addHeatmap(
        data = data$movement_join_within_coords,
        lng = ~X, lat = ~Y,
        intensity = ~mean_column,
        radius = 100,
        blur = 0,
        max = 3,
        gradient = c("blue", "green", "yellow", "red"),
        group = "Heatmap"
      ) %>%
      
      # London-Polygon
      addPolygons(
        data = data$london_filtered,
        color = "black",
        weight = 2,
        fillOpacity = 0.1,
        group = "London"
      ) %>%
      
      # Hexagons
      addPolygons(data = data$hexagons_with_means,
                  color = "black",
                  weight = 1,
                  fillColor = ~colorNumeric(palette = c("blue", "red"), domain = c(0, 3))(mean_value),
                  fillOpacity = 0.7,
                  popup = ~paste("Mean Column:", mean_value),
                  group = "Hexagons") %>%
      
      # Punkte
      addCircleMarkers(
        data = data$movement_join_within_coords,
        lng = ~X, lat = ~Y,
        color = ~fixed_bin_scale(mean_column),
        fillOpacity = 0.7,
        radius = 4,
        popup = ~paste("Mean Column:", mean_column),
        group = "Movement Points"
      ) %>%
      
      # POIs
      addMarkers(
        data = data$pois_within, lng = ~lon, lat = ~lat,
        popup = ~paste("Name:", name),
        group = "POIs"
      ) %>%
      
      addLegend(
        position = "bottomright",
        pal = fixed_bin_scale,  # Farbpalette für die binäre Skala
        values = c(0, 3),  # Wertebereich
        title = "Mean Column",
        opacity = 1
      ) %>%
      
      # Layers control
      addLayersControl(
        overlayGroups = c("Heatmap", "London", "Hexagons", "Movement Points", "POIs"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
}

# run the application
shinyApp(ui = ui, server = server)

