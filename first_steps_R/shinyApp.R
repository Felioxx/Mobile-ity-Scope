setwd("C:\\Users\\Anne\\Documents\\Studium\\9.Semester\\StudyProject\\Data")
library(osmdata)
library(sf)
library(tmap)
library(arrow)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(shiny)


london<- read_sf('merged_districts.shp')
london <- st_transform(london, crs = 4326)
movement <- read_parquet("pre_processed_movement.parquet")
tiles <- read_parquet("distinct_LONLAT.parquet")
#pois <- read.csv("D:/Dokumente/Studium/9 FS/Study Project/Mobile-ity-Scope/first_steps_python/attractionsGreaterLondon.csv")

shops <- read_sf('shops.shp')
museums <- read_sf('museums.shp')
shops$type <- "shop"
museums$type <- "museum"
pois <- rbind(shops, museums)

#hexagons <- read_sf('hexgr_2.shp')

grid_18 <- read_sf('grids_18.shp')
grid_17 <- read_sf('grids_17.shp')
grid_16 <- read_sf('grids_16.shp')
grid_15 <- read_sf('grids_15.shp')
grid_14 <- read_sf('grids_14.shp')

grid_18$grid_id <- seq(1, nrow(grid_18))
grid_17$grid_id <- seq(1, nrow(grid_17))
grid_16$grid_id <- seq(1, nrow(grid_16))
grid_15$grid_id <- seq(1, nrow(grid_15))
grid_14$grid_id <- seq(1, nrow(grid_14))
#grid <- st_transform(grid, crs = 4326)
#hexagons <- grid

filter_grid_pois <- function(london, districts, pois, hexagons) {
  
  # Filter districts
  london_filtered <- london[london$district_n %in% districts, ]
  
  # GeoJSON-Geometrie in sf-Objekte konvertieren
  # pois_sf <- pois %>%
  #   mutate(geometry = st_as_sfc(geometry)) %>%
  #   st_as_sf()
  # st_crs(pois_sf) <- 4326
  
  pois_sf <- pois
  
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
  
  # Filter hexagons
  intersects_indices <- st_intersects(hexagons, london_filtered, sparse = FALSE)
  hexagons_intersects <- hexagons[apply(intersects_indices, 1, any), ]
  
  return (list(
    london_filtered = london_filtered,
    pois_within = pois_within,
    hexagons_intersects = hexagons_intersects))
}

filter_movement_data <- function(movement_data, tiles, london_filtered, hexagons_intersects, date){
  
  # Filter movement data with date
  movement_filter <- movement_data[movement_data$AGG_DAY_PERIOD == date,]
  # Merge movement data with tiles and convert to sf object
  movement_merge <- merge(movement_filter, tiles, by.x = "LONLAT_ID", by.y = "LONLAT_ID")
  movement_sf <- st_as_sf(movement_merge, coords = c("XLON", "XLAT"), crs = st_crs(london))
  # Get movement data which is within the given districts
  within_indices <- st_within(movement_sf, london_filtered, sparse = FALSE)
  movement_join_within <- movement_sf[apply(within_indices, 1, any), ]
  movement_join_within <- movement_join_within[order(movement_join_within$mean_column),]
  
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
  
  return (list(
    movement_join_within_coords = movement_join_within_coords,
    hexagons_with_means = hexagons_with_means))
}

calculate_one_date <- function(movement_data, tiles, london, pois, hexagons, date, districts){
  print("calculate_one_date:")
  print("Chosen date:")
  print(date)
  
  print("Chosen districts:")
  print(districts)
  
  result <- filter_grid_pois(london, districts, pois, hexagons)
  result1 <- filter_movement_data(movement_data, tiles, result$london_filtered, result$hexagons_intersects, date)
  
  return (list(
    london_filtered = result$london_filtered, 
    movement_join_within_coords = result1$movement_join_within_coords,
    pois_within = result$pois_within,
    hexagons_with_means = result1$hexagons_with_means))
}

compare_dates <- function(movement_data, tiles, london, pois, hexagons, date1, date2, districts) {
  
  print("compare_dates:")
  print("Chosen dates:")
  print(date1)
  print(date2)
  
  print("Chosen districts:")
  print(districts)
  
  result <- filter_grid_pois(london, districts, pois, hexagons)
  result1 <- filter_movement_data(movement_data, tiles, result$london_filtered, result$hexagons_intersects, date1)
  result2 <- filter_movement_data(movement_data, tiles, result$london_filtered, result$hexagons_intersects, date2)
  
  hexagons1 <- result1$hexagons_with_means
  hexagons2 <- result2$hexagons_with_means
  movement1 <- result1$movement_join_within_coords
  movement2 <- result2$movement_join_within_coords
  
  # 1. Geometrie temporär entfernen
  hexagons1_geom <- st_geometry(hexagons1)
  hexagons2_geom <- st_geometry(hexagons2)
  
  # 2. Daten ohne Geometrie joinen
  hexagons1 <- hexagons1 %>% st_set_geometry(NULL)
  hexagons2 <- hexagons2 %>% st_set_geometry(NULL)
  movement1 <- movement1 %>% st_set_geometry(NULL)
  movement2 <- movement2 %>% st_set_geometry(NULL)
  
  merged_hexagons <- inner_join(hexagons1, hexagons2, by = "grid_id", suffix = c("_1", "_2"))
  merged_movement <- full_join(movement1, movement2, by = "LONLAT_ID", suffix = c("_1", "_2"))
  
  # 3. Geometrie wieder hinzufügen
  merged_hexagons <- st_as_sf(merged_hexagons, geometry = hexagons1_geom)
  # Determine which coordinates to use
  merged_movement$new_X <- ifelse(is.na(merged_movement$X_1), merged_movement$X_2, merged_movement$X_1)
  merged_movement$new_Y <- ifelse(is.na(merged_movement$Y_1), merged_movement$Y_2, merged_movement$Y_1)
  merged_movement <- st_as_sf(merged_movement, coords = c("new_X", "new_Y"), crs = st_crs(london))
  
  # Differenz der Mittelwerte berechnen
  #merged_hexagons <- merged_hexagons %>%
  #  mutate(mean_value_diff = ifelse(is.na(mean_value_1) | is.na(mean_value_2), NA, mean_value_1 - mean_value_2))
  #merged_movement <- merged_movement %>%
  #  mutate(mean_column_diff = ifelse(is.na(mean_column_1) | is.na(mean_column_2), NA, mean_column_1 - mean_column_2))
  
  # Prozentualen Anteil berechnen
  merged_hexagons <- merged_hexagons %>%
    mutate(mean_value_diff = ifelse(is.na(mean_value_1) | is.na(mean_value_2), NA, round(((mean_value_2/mean_value_1)-1) *100,2)))
  merged_movement <- merged_movement %>%
    mutate(mean_column_diff = ifelse(is.na(mean_column_1) | is.na(mean_column_2), NA, round(((mean_column_2/mean_column_1)-1) *100,2)))
  
  # Ergebnis in die Struktur von hexagons1 übertragen
  result_hexagons <- merged_hexagons %>%
    select(grid_id, everything(), mean_value = mean_value_diff)
  result_movement <- merged_movement %>%
    select(LONLAT_ID, everything(), mean_column = mean_column_diff)
  
  return (list(
    london_filtered = result$london_filtered, 
    movement_join_within_coords = movement1,
    pois_within = result$pois_within,
    hexagons_with_means = result_hexagons))
}

# define the user interface
ui <- fluidPage(
  titlePanel("Mobility Scope"),
  sidebarLayout(
    sidebarPanel(
      dateInput("selected_date", "Choose a Day:", value = "2020-01-01"),
      dateInput("selected_date_2", "Choose a Day you want to compare:", value = "2020-01-01"),
      selectInput(
        inputId = "grid_size",
        label = "Grid Size:",
        choices = c("1x1", "2x2", "4x4", "8x8", "16x16"),
        selected = "1x1"
      ),
      checkboxGroupInput("districts", "Choose districts:", 
                         choices = unique(london$district_n),
                         selected = c("City of London", "Westminster")),
      actionButton("select_all", "Select All"),
      actionButton("submit", "Update Map according first date"),
      actionButton("submit2", "Update Map according both dates")
    ),
    mainPanel(
      leafletOutput("map", height = 600)
    )
  )
)

#movement_data, tiles, london, pois, date, districts
# define the server logic
server <- function(input, output, session) {
  #"Select All"-Button Logik
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "districts",
                             selected = unique(london$district_n))  # Alle Districte auswählen
  })
  
  
  
  # Define filtered data for submit
  filtered_data_single <- eventReactive(input$submit, {
    req(input$districts, input$selected_date)
    if(input$grid_size == "1x1"){
      hexagons <- grid_18
    } else if(input$grid_size == "2x2"){
      hexagons <- grid_17
    } else if(input$grid_size == "4x4"){
      hexagons <- grid_16
    } else if(input$grid_size == "8x8"){
      hexagons <- grid_15
    } else if(input$grid_size == "16x16"){
      hexagons <- grid_14
    }
    calculate_one_date(
      movement_data = movement,
      tiles = tiles,
      london = london,
      pois = pois,
      hexagons = hexagons,
      date = as.character(input$selected_date),
      districts = input$districts
    )
  })
  
  # Define filtered data for submit2
  filtered_data_compare <- eventReactive(input$submit2, {
    req(input$districts, input$selected_date, input$selected_date_2)
    if(input$grid_size == "1x1"){
      hexagons <- grid_18
    } else if(input$grid_size == "2x2"){
      hexagons <- grid_17
    } else if(input$grid_size == "4x4"){
      hexagons <- grid_16
    } else if(input$grid_size == "8x8"){
      hexagons <- grid_15
    } else if(input$grid_size == "16x16"){
      hexagons <- grid_14
    }
    compare_dates(
      movement_data = movement,
      tiles = tiles,
      london = london,
      pois = pois,
      hexagons = hexagons,
      date1 = as.character(input$selected_date),
      date2 = as.character(input$selected_date_2),
      districts = input$districts
    )
  })
  
  # Combine the two filtered data sources into one reactive expression
  filtered_data <- reactive({
    if (input$submit2 > input$submit) {
      filtered_data_compare()
    } else {
      filtered_data_single()
    }
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
    print(paste("Chosen Date2:", input$selected_date_2))
    print(paste("Chosen Districts:", paste(input$districts, collapse = ", ")))
    data <- filtered_data()
    
    min <- min(data$hexagons_with_means$mean_value, na.rm = TRUE)
    max <- max(data$hexagons_with_means$mean_value, na.rm = TRUE)
    
    my_domain <- c(min, max)
    
    if (input$submit2 > input$submit) {
      fixed_bin_scale <- colorBin(palette = c("blue","cyan","white", "yellow","red"), 
                                  domain=c(-500,0-.Machine$double.eps,0,0+.Machine$double.eps,500))
      legendTitle <- "Relative Change in %"
      popuptxt = "Change (%):"
    }
    else {
      fixed_bin_scale <- colorBin(
        palette = c("blue", "red"),  # Farbpalette von Blau bis Rot
        domain = my_domain,
        bins = 7
      )
      legendTitle <- "Mean Activity Value"
      popuptxt = "Mean Activity Value:"
    }
    
    # Karte aktualisieren
    leafletProxy("map") %>%
      clearGroup(c("Heatmap", "London", "Hexagons", "Movement Points", "Shops", "Museums")) %>%
      clearControls() %>%
      # Heatmap
      # addHeatmap(
      #   data = data$movement_join_within_coords,
      #   lng = ~X, lat = ~Y,
      #   intensity = ~mean_column,
      #   radius = 100,
      #   blur = 0,
      #   max = 3,
      #   gradient = c("blue", "green", "yellow", "red"),
      #   group = "Heatmap"
      # ) %>%
      
      # POIs
      addAwesomeMarkers(
        data = data$pois_within[data$pois_within$type == "shop", ],
        lng = ~lon, lat = ~lat,
        popup = ~paste("Name:", name),
        group = "Shops",
        icon=awesomeIcons(
          icon = 'fa-shopping-cart',
          iconColor = 'black',
          library = 'fa',
          markerColor = "green"
        )) %>%
      
      addAwesomeMarkers(
        data = data$pois_within[data$pois_within$type == "museum", ],
        lng = ~lon, lat = ~lat,
        popup = ~paste("Name:", name),
        group = "Museums",
        icon=awesomeIcons(
          icon = 'ion-android-color-palette',
          iconColor = 'black',
          library = 'ion',
          markerColor = "orange"
        )) %>%
      
      # London-Polygon
      addPolygons(
        data = data$london_filtered,
        color = "black",
        weight = 2,
        fillOpacity = 0.1,
        group = "London"
      ) %>%
      
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
      
      # Hexagons
      addPolygons(data = data$hexagons_with_means,
                  color = "black",
                  weight = 1,
                  fillColor = ~fixed_bin_scale(mean_value),
                  fillOpacity = 0.7,
                  popup = ~paste(popuptxt, round(mean_value,2)),
                  group = "Hexagons") %>%
      
      addLegend(
        position = "bottomright",
        pal = fixed_bin_scale,
        values = my_domain,
        title = legendTitle,
        opacity = 1
      ) %>%
      
      # Layers control
      addLayersControl(
        overlayGroups = c("London", "Hexagons", "Movement Points", "Shops", "Museums"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
}

# run the application
shinyApp(ui = ui, server = server)



#Tests
result <- calculate_one_date(movement, tiles, london, pois, grid_15, "2020-03-23", c("City of London", "Westminster"))
result <- compare_dates(movement, tiles, london, pois, hexagons, "2020-03-23", "2020-04-06", c("City of London", "Westminster"))

library(terra)

# Aggregation: 2x2 Zellen zu einer
r_agg <- aggregate(hexagons_with_means, fact=2, fun=mean)  # Hier Mittelwert (mean) als Aggregation

# Ergebnis anzeigen
plot(r, main="Original Raster")
plot(r_agg, main="Aggregiertes Raster (4 Zellen -> 1)")
print(values(r_agg))  # Werte des neuen Rasters
