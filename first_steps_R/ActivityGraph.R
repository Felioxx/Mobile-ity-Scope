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
library(tidyr)
library(patchwork)

london<- read_sf('msoa2021/merged_districts.shp')
london <- st_transform(london, crs = 4326)
london <- london %>%
  group_by(district_n) %>%
  summarise(
    geometry = st_union(geometry)
  )
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
#save(movement_data, file = "movement_data.RData")

# Join durchführen
movement_data <- movement_data %>%
  left_join(pois %>% select(name, type), by = c("POI" = "name"))

# Berechnung der Wochenenden
movement_data <- movement_data %>%
  mutate(weekday = weekdays(AGG_DAY_PERIOD))

#filter <- movement_data %>%
#  filter(POI == "MCC Museum and Tour")

events <- data.frame(
  Date = as.Date(c("2020-02-12", "2020-03-17", "2020-03-23", "2020-04-15", "2020-06-01", "2020-09-22", "2020-11-04")),
  Event = c(
    "First Corona Case", 
    "Reduced Service of the London Underground", 
    "Lockdown",
    "Extension of lockdown",
    "Outdoor markets and gatherings of up to six people with two-meter distancing are permitted",
    "Prime Minister Boris Johnson announced new restrictions on everyday life",
    "Partial lockdown"
    )
)

# Big plotly map
p <- ggplot(movement_data, aes(x = AGG_DAY_PERIOD, y = mean_value, color = POI, text = paste("Type:", type, sep=" "))) +
  geom_vline(data = subset(movement_data, weekday == "Sonntag"), 
             aes(xintercept = as.numeric(AGG_DAY_PERIOD)), 
             color = "lightgrey", linetype = "solid", linewidth = 1.2) +
  geom_vline(data = subset(movement_data, weekday == "Samstag"), 
             aes(xintercept = as.numeric(AGG_DAY_PERIOD)), 
             color = "lightgrey", linetype = "solid", linewidth = 1.2) +
  geom_vline(data = events, aes(xintercept = as.numeric(Date)), 
             color = "red", linetype = "solid", linewidth = 0.5) +
  geom_line() +
  labs(
    title = "Activity Trends Across POIs. Weekends are highlighted in grey.",
    x = "Date",
    y = "Activity",
    color = "POI"
  ) +
  theme_minimal()

# Konvertiere zu Plotly und passe die Legenden-Position an
ggplotly(p) %>%
  layout(legend = list(orientation = "h", x = 0, y = -0.2))

# Bewegung Daten nach drei POIs filtern
filtered_movement_data <- movement_data %>%
  filter(POI %in% pois_with_increase_names)

# Gefilterte Daten plotten
p <- ggplot(filtered_movement_data, aes(x = AGG_DAY_PERIOD, y = mean_value, color = POI, text = paste("Type:", type, sep=" "))) +
  geom_vline(data = subset(filtered_movement_data, weekday == "Sonntag"), 
             aes(xintercept = as.numeric(AGG_DAY_PERIOD)), 
             color = "lightgrey", linetype = "solid", linewidth = 1.2) +
  geom_vline(data = subset(filtered_movement_data, weekday == "Samstag"), 
             aes(xintercept = as.numeric(AGG_DAY_PERIOD)), 
             color = "lightgrey", linetype = "solid", linewidth = 1.2) +
  geom_vline(data = events, aes(xintercept = as.numeric(Date)), 
             color = "red", linetype = "solid", linewidth = 0.5) +
  geom_line() +
  ylim(0, 3) +
  labs(
    title = "Activity Trends Across Selected POIs. Weekends are highlighted in grey.",
    x = "Date",
    y = "Activity",
    color = "POI"
  ) +
  theme_minimal()

# Konvertiere zu Plotly und passe die Legenden-Position an
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

p <- ggplot(aggregated_data, aes(x = AGG_DAY_PERIOD, y = mean_activity, color = category)) +
  geom_vline(data = subset(movement_data, weekday == "Sonntag"), 
             aes(xintercept = as.numeric(AGG_DAY_PERIOD)), 
             color = "lightgrey", linetype = "solid", linewidth = 1.2) +
  geom_vline(data = subset(movement_data, weekday == "Samstag"), 
             aes(xintercept = as.numeric(AGG_DAY_PERIOD)), 
             color = "lightgrey", linetype = "solid", linewidth = 1.2) +
  geom_vline(data = events, aes(xintercept = as.numeric(Date)), 
             color = "red", linetype = "solid", linewidth = 0.5) +
  geom_line() +
  labs(
    title = "Activity Trends by Category. Weekends are highlighted in grey.",
    x = "Date",
    y = "Average Activity",
    color = "Category"
  ) +
  theme_minimal()

ggplotly(p) %>%
  layout(legend = list(orientation = "h", x = 0, y = -1), 
         height = 800, 
         annotations = list(
           list(
             x = 0.5,
             y = -0.7,
             text = annotation_text, 
             showarrow = FALSE,
             xref = "paper", 
             yref = "paper",
             font = list(size = 10),  # Schriftgröße
             align = "left"  # Text linksbündig ausrichten
           )
         ))




# Analysis with mean values
# Zeiträume definieren
lockdown_start <- as.Date("2020-03-23")
lockdown_end <- as.Date("2020-06-01")
partial_lockdown_start <- as.Date("2020-11-04")

# Daten filtern und Zeiträume markieren
movement_data <- movement_data %>%
  mutate(
    period = case_when(
      AGG_DAY_PERIOD < lockdown_start ~ "Before_Lockdown",
      AGG_DAY_PERIOD >= lockdown_start & AGG_DAY_PERIOD <= lockdown_end ~ "Lockdown",
      AGG_DAY_PERIOD > lockdown_end & AGG_DAY_PERIOD < partial_lockdown_start ~ "Post_Lockdown",
      AGG_DAY_PERIOD >= partial_lockdown_start ~ "Partial_Lockdown",
      TRUE ~ "Other"
    )
  )

# Durchschnitt für jeden Zeitraum berechnen
mean_values <- movement_data %>%
  group_by(period, POI, type) %>%
  summarise(
    mean_activity = mean(mean_value, na.rm = TRUE),
    .groups = "drop"
  )

# Daten in breites Format bringen
mean_values_wide <- mean_values %>%
  pivot_wider(names_from = period, values_from = mean_activity, 
              names_prefix = "mean_")

# Prozentuale Veränderungen zwischen benachbarten Zeiträumen berechnen
mean_values_wide <- mean_values_wide %>%
  mutate(
    change_Before_to_Lockdown = (mean_Lockdown - mean_Before_Lockdown) / mean_Before_Lockdown * 100,
    change_Lockdown_to_Post = (mean_Post_Lockdown - mean_Lockdown) / mean_Lockdown * 100,
    change_Post_to_Partial = (mean_Partial_Lockdown - mean_Post_Lockdown) / mean_Post_Lockdown * 100
  )

# Veränderungskategorien hinzufügen
mean_values_wide <- mean_values_wide %>%
  mutate(
    category_Before_to_Lockdown = case_when(
      abs(change_Before_to_Lockdown) < 5 ~ "No Significant Change",
      change_Before_to_Lockdown > 50 ~ "High Increase",
      change_Before_to_Lockdown < -50 ~ "High Decrease",
      change_Before_to_Lockdown > 0 ~ "Moderate Increase",
      TRUE ~ "Moderate Decrease"
    ),
    category_Lockdown_to_Post = case_when(
      abs(change_Lockdown_to_Post) < 5 ~ "No Significant Change",
      change_Lockdown_to_Post > 50 ~ "High Increase",
      change_Lockdown_to_Post < -50 ~ "High Decrease",
      change_Lockdown_to_Post > 0 ~ "Moderate Increase",
      TRUE ~ "Moderate Decrease"
    ),
    category_Post_to_Partial = case_when(
      abs(change_Post_to_Partial) < 5 ~ "No Significant Change",
      change_Post_to_Partial > 50 ~ "High Increase",
      change_Post_to_Partial < -50 ~ "High Decrease",
      change_Post_to_Partial > 0 ~ "Moderate Increase",
      TRUE ~ "Moderate Decrease"
    )
  )

# Daten für den Plot vorbereiten
# plot_data <- mean_values_wide %>%
#   select(POI, starts_with("change_")) %>%
#   pivot_longer(
#     cols = starts_with("change_"),
#     names_to = "comparison",
#     values_to = "relative_change"
#   )
# Plot erstellen
# ggplot(plot_data, aes(x = reorder(POI, relative_change), y = relative_change, fill = comparison)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   coord_flip() +
#   labs(
#     title = "Prozentuale Veränderungen der Aktivität nach POI",
#     x = "POI",
#     y = "Relative Veränderung (%)",
#     fill = "Vergleich"
#   ) +
#   theme_minimal()

# Daten von wide nach long transformieren, um Kategorien darzustellen
plot_data_categories <- mean_values_wide %>%
  select(POI, type, category_Before_to_Lockdown, category_Lockdown_to_Post, category_Post_to_Partial) %>%
  pivot_longer(
    cols = starts_with("category_"),
    names_to = "comparison",
    values_to = "category"
  )

plot_data_categories <- plot_data_categories %>%
  mutate(category = factor(category, levels = c(
    "High Increase",
    "Moderate Increase",
    "No Significant Change",
    "Moderate Decrease",
    "High Decrease"
  )))

# Daten für 'shop'
plot_data_shop <- plot_data_categories %>% filter(type == "shop")

# Daten für 'museum' und in zwei Gruppen aufteilen
plot_data_museum <- plot_data_categories %>% filter(type == "museum") %>%
  mutate(group = ntile(row_number(), 2)) # Teilt die Museen in zwei Gruppen

# Plot für Gruppe 1
plot1 <- ggplot(plot_data_shop, aes(x = reorder(POI, category), fill = category)) +
  geom_bar(stat = "count", position = "dodge") +
  facet_wrap(~ comparison) +
  coord_flip() +
  labs(
    title = "Categorized Changes in Activity per POI (Shop)",
    x = NULL,
    y = NULL,
    fill = "Category"
  ) +
  scale_fill_manual(values = c(
    "High Increase" = "darkgreen",
    "Moderate Increase" = "lightgreen",
    "No Significant Change" = "gray",
    "Moderate Decrease" = "orange",
    "High Decrease" = "red"
  )) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

# Plot für Gruppe 2
plot2 <- ggplot(plot_data_museum %>% filter(group == 1), aes(x = reorder(POI, category), fill = category)) +
  geom_bar(stat = "count", position = "dodge") +
  facet_wrap(~ comparison) +
  coord_flip() +
  labs(
    title = "Categorized Changes in Activity per POI (Museum)",
    x = NULL,
    y = NULL,
    fill = "Category"
  ) +
  scale_fill_manual(values = c(
    "High Increase" = "darkgreen",
    "Moderate Increase" = "lightgreen",
    "No Significant Change" = "gray",
    "Moderate Decrease" = "orange",
    "High Decrease" = "red"
  )) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

plot3 <- ggplot(plot_data_museum %>% filter(group == 2), aes(x = reorder(POI, category), fill = category)) +
  geom_bar(stat = "count", position = "dodge") +
  facet_wrap(~ comparison) +
  coord_flip() +
  labs(
    title = "Categorized Changes in Activity per POI (Museum)",
    x = NULL,
    y = NULL,
    fill = "Category"
  ) +
  scale_fill_manual(values = c(
    "High Increase" = "darkgreen",
    "Moderate Increase" = "lightgreen",
    "No Significant Change" = "gray",
    "Moderate Decrease" = "orange",
    "High Decrease" = "red"
  )) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

plot1
plot2
plot3

# Daten filtern und Zeiträume markieren
movement_data_filtered <- movement_data %>%
  filter(POI == "Bank of England") %>%
  mutate(
    period = case_when(
      AGG_DAY_PERIOD < lockdown_start ~ "Before Lockdown",
      AGG_DAY_PERIOD >= lockdown_start & AGG_DAY_PERIOD <= lockdown_end ~ "Lockdown",
      AGG_DAY_PERIOD > lockdown_end & AGG_DAY_PERIOD < partial_lockdown_start ~ "Post Lockdown",
      AGG_DAY_PERIOD >= partial_lockdown_start ~ "Partial Lockdown",
      TRUE ~ "Other"
    ),
    # Konvertiere period zu einem Faktor mit der gewünschten Reihenfolge
    period = factor(period, levels = c("Before Lockdown", "Lockdown", "Post Lockdown", "Partial Lockdown"))
    
  )

# Durchschnitt für jeden Zeitraum berechnen
mean_values_filtered <- movement_data_filtered %>%
  group_by(period) %>%
  summarise(
    mean_activity = mean(mean_value, na.rm = TRUE),
    .groups = "drop"
  )

# Plot erstellen
ggplot(mean_values_filtered, aes(x = period, y = mean_activity, fill = period)) +
  geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
  labs(
    title = "Veränderung der Aktivität für die Bank of England",
    x = "Zeitraum",
    y = "Mittlere Aktivität"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


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
