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
library(plotly)
library(lubridate)

london<- read_sf('msoa2021/merged_districts.shp')
london <- st_transform(london, crs = 4326)
movement <- read_parquet("pre_processed_movement.parquet")
tiles <- read_parquet("distinct_LONLAT.parquet")
tiles_sf <- st_as_sf(tiles, coords = c("XLON", "XLAT"), crs = 4326)
#pois <- read.csv("D:/Dokumente/Studium/9 FS/Study Project/Mobile-ity-Scope/first_steps_python/attractionsGreaterLondon.csv")
shops <- read_sf('D:/Dokumente/Studium/9 FS/Study Project/Mobile-ity-Scope/POIs/shops.shp')
museums <- read_sf('D:/Dokumente/Studium/9 FS/Study Project/Mobile-ity-Scope/POIs/museums.shp')
shops$type <- "shop"
museums$type <- "museum"
pois <- rbind(shops, museums)

poi_name <- "MCC Museum and Tour"
buffer_size <- 100

calculate_movement_mean <- function(poi_name, buffer_size = 100) {
  poi_filter <- pois %>% filter(name == poi_name)
  poi_buffer <- st_buffer(poi_filter, buffer_size)
  
  tiles_indices <- st_within(tiles_sf, poi_buffer, sparse = FALSE)
  tiles_within <- tiles_sf[apply(tiles_indices, 1, any), ]
  
  movement_merge <- merge(
    movement, 
    tiles_within, 
    by.x = "LONLAT_ID", 
    by.y = "LONLAT_ID"
  )
  movement_merge %>%
    group_by(AGG_DAY_PERIOD, POI = poi_name) %>%
    summarise(mean_value = mean(mean_column, na.rm = TRUE)) %>%
    mutate(AGG_DAY_PERIOD = as.Date(AGG_DAY_PERIOD))
}

# Bewegungsdaten für alle POIs vorberechnen
movement_data <- do.call(
  rbind,
  lapply(unique(pois$name), calculate_movement_mean)
)

# Join durchführen
movement_data <- movement_data %>%
  left_join(pois %>% select(name, type), by = c("POI" = "name"))

# Berechnung der Wochenenden
movement_data <- movement_data %>%
  mutate(weekday = weekdays(AGG_DAY_PERIOD))

filter <- movement_data %>%
  filter(POI == "MCC Museum and Tour")

# Big plotly map
p <- ggplot(movement_data, aes(x = AGG_DAY_PERIOD, y = mean_value, color = POI, text = paste("Type:", type, sep=" "))) +
  geom_vline(data = subset(movement_data, weekday == "Sonntag"), 
             aes(xintercept = as.numeric(AGG_DAY_PERIOD)), 
             color = "lightgrey", linetype = "solid", linewidth = 1.2) +
  geom_vline(data = subset(movement_data, weekday == "Samstag"), 
             aes(xintercept = as.numeric(AGG_DAY_PERIOD)), 
             color = "lightgrey", linetype = "solid", linewidth = 1.2) +
  geom_line() +
  labs(
    title = "Activity Trends Across POIs. Sundays are highlighted.",
    x = "Date",
    y = "Activity",
    color = "POI"
  ) +
  theme_minimal()

ggplotly(p) %>%
  layout(legend = list(orientation = "h", x = 0, y = -0.2))

# FIlter only one type
filtered_data <- movement_data %>%
  filter(type == "museum")

# Plot erstellen mit gefilterten Daten
p <- ggplot(filtered_data, aes(x = AGG_DAY_PERIOD, y = mean_value, color = POI, text = paste("Type:", type, sep = " "))) +
  geom_line() +
  labs(
    title = "Activity Trends",
    x = "Date",
    y = "Activity",
    color = "POI"
  ) +
  theme_minimal()

# Interaktive Visualisierung
ggplotly(p) %>%
  layout(legend = list(orientation = "h", x = 0, y = -0.2))


#result <- calculate_graph(poi_name, buffer_size)
#poi_filter <- result$poi_filter
#poi_buffer <- result$poi_buffer

#plot poi_filter as point and buffer on leaflet
# leaflet() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addCircleMarkers(data = poi_filter, radius = 5, color = "red") %>%
#   addCircleMarkers(data = tiles_within, radius = 5, color = "green") %>%
#   addPolygons(data = poi_buffer, color = "blue", fillOpacity = 0.2)
# 
# ggplot(data = movement_mean, aes(x = AGG_DAY_PERIOD, y = mean_value)) +
#   geom_line(color = "blue") +
#   labs(
#     title = poi_name,
#     x = "Date",
#     y = "Acitivity"
#   ) +
#   theme_minimal()  

# Heatmap Plot
# Sortiere die POIs nach ihrem Typ
movement_data$POI <- reorder(movement_data$POI, movement_data$type)

ggplot(movement_data, aes(x = AGG_DAY_PERIOD, y = POI, fill = mean_value)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    title = "Heatmap of POI Activity",
    x = "Date",
    y = "POI",
    fill = "Activity"
  ) +
  theme_minimal()

# Museums and shops aggregated
aggregated_data <- movement_data %>%
  group_by(AGG_DAY_PERIOD, category = type) %>% # Gruppierung nach Typ
  summarise(mean_activity = mean(mean_value, na.rm = TRUE))

ggplot(aggregated_data, aes(x = AGG_DAY_PERIOD, y = mean_activity, color = category)) +
  geom_line() +
  labs(
    title = "Activity Trends by Category",
    x = "Date",
    y = "Average Activity",
    color = "Category"
  ) +
  theme_minimal()


# Shiny App with one POI
ui <- fluidPage(
  titlePanel("POI Analysis in London"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "selected_poi", 
        "Select POI:", 
        choices = unique(pois$name)
      ),
      sliderInput(
        "buffer_size", 
        "Buffer Size (meters):", 
        min = 50, 
        max = 500, 
        value = 100, 
        step = 50
      )
    ),
    mainPanel(
      leafletOutput("map"),
      plotOutput("line_plot")
    )
  )
)

server <- function(input, output, session) {
  # Reactive POI filtering
  filtered_poi <- reactive({
    pois %>% filter(name == input$selected_poi)
  })
  
  # Create buffer based on input
  poi_buffer <- reactive({
    st_buffer(filtered_poi(), input$buffer_size)
  })
  
  # Find tiles within buffer
  tiles_within <- reactive({
    tiles_indices <- st_within(tiles_sf, poi_buffer(), sparse = FALSE)
    tiles_sf[apply(tiles_indices, 1, any), ]
  })
  
  # Merge movement with tiles and calculate mean
  movement_mean <- reactive({
    if (nrow(tiles_within()) > 0) {
      movement_merge <- merge(
        movement, 
        tiles_within(), 
        by.x = "LONLAT_ID", 
        by.y = "LONLAT_ID"
      )
      movement_merge %>%
        group_by(AGG_DAY_PERIOD) %>%
        summarise(mean_value = mean(mean_column, na.rm = TRUE)) %>%
        mutate(AGG_DAY_PERIOD = as.Date(AGG_DAY_PERIOD))
    } else {
      data.frame(AGG_DAY_PERIOD = as.Date(character()), mean_value = numeric())
    }
  })
  
  # Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(data = filtered_poi(), radius = 5, color = "red") %>%
      addCircleMarkers(data = tiles_within(), radius = 5, color = "green") %>%
      addPolygons(data = poi_buffer(), color = "blue", fillOpacity = 0.2)
  })
  
  # Line plot
  output$line_plot <- renderPlot({
    data <- movement_mean()
    ggplot(data, aes(x = AGG_DAY_PERIOD, y = mean_value)) +
      geom_line(color = "blue") +
      labs(
        title = input$selected_poi,
        x = "Date",
        y = "Activity"
      ) +
      theme_minimal()
  })
}

shinyApp(ui, server)



# Shiny App with all POIs
ui <- fluidPage(
  titlePanel("POI Activity Comparison"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "selected_pois", 
        "Select POIs to Display:", 
        choices = unique(movement_data$POI),
        selected = unique(movement_data$POI) # Standard: alle ausgewählt
      )
    ),
    mainPanel(
      plotlyOutput("line_plot")  # Verwende plotly
    )
  )
)

server <- function(input, output, session) {
  # Reaktive Daten basierend auf der Auswahl
  filtered_data <- reactive({
    movement_data %>% filter(POI %in% input$selected_pois)
  })
  
  # Liniendiagramm
  output$line_plot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = AGG_DAY_PERIOD, y = mean_value, color = POI, group = POI)) +
      geom_line() +
      labs(
        title = "Activity Trends Across POIs",
        x = "Date",
        y = "Activity",
        color = "POI"
      ) +
      theme_minimal()
    
    ggplotly(p) %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.2)) # Horizontale Legende
  })
}

shinyApp(ui, server)
