setwd("D:/Dokumente/Studium/9 FS/Study Project/Mobile-ity-Scope")

# Load required libraries
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
library(shinycssloaders)
library(shinyWidgets)
library(lubridate)
library(tidyr)
library(patchwork)

# define loader
options(page.spinner.type = 1)
options(page.spinner.caption = "Your map is being calculated :)")
options(page.spinner.color = "orange")

# Load districts and union them
london<- read_sf('Districts/merged_districts.shp')
london <- st_transform(london, crs = 4326)
london <- london %>%
  group_by(district_n) %>%
  summarise(
    geometry = st_union(geometry)
  )

# Here the data from Learnweb is loaded. Please use your own local data.
movement <- read_parquet("../data/pre_processed_movement.parquet")
tiles <- read_parquet("../data/distinct_LONLAT.parquet")

# Load POIs
shops <- read_sf('POIs/shops.shp')
museums <- read_sf('POIs/museums.shp')
shops$type <- "shop"
museums$type <- "museum"
pois <- rbind(shops, museums)
coords <- st_coordinates(pois)
pois <- pois %>%
  mutate(lon = coords[, 1], lat = coords[, 2])

# Load grids and assign grid IDs
grid_18 <- read_sf('grids/grids_18.shp')
grid_17 <- read_sf('grids/grids_17.shp')
grid_16 <- read_sf('grids/grids_16.shp')
grid_15 <- read_sf('grids/grids_15.shp')
grid_14 <- read_sf('grids/grids_14.shp')

grid_18$grid_id <- seq(1, nrow(grid_18))
grid_17$grid_id <- seq(1, nrow(grid_17))
grid_16$grid_id <- seq(1, nrow(grid_16))
grid_15$grid_id <- seq(1, nrow(grid_15))
grid_14$grid_id <- seq(1, nrow(grid_14))

# Define the Corona events
events <- data.frame(
  Date = as.Date(c("2020-02-12", "2020-03-17", "2020-03-23", "2020-04-15", "2020-06-01", "2020-09-22", "2020-11-04")),
  Event = c(
    "First Corona Case", 
    "Reduced Service of the London Underground", 
    "Lockdown",
    "Extension of lockdown",
    "Outdoor markets and gatherings of up to six people with two-meter distancing are permitted",
    "New restrictions on everyday life were announced",
    "Partial lockdown"
  )
)
annotation_text <- paste0(
  "Events:<br>",
  paste(paste(events$Date, events$Event, sep = " - "), collapse = "<br>")
)

# Define time periods
lockdown_start <- as.Date("2020-03-23")
lockdown_end <- as.Date("2020-06-01")
partial_lockdown_start <- as.Date("2020-11-04")

# Filter data with the given districts
filter_grid_pois <- function(london, districts, pois, grid) {
  
  london_filtered <- london[london$district_n %in% districts, ]
  
  pois <- pois %>%
    mutate(geometry = st_centroid(geometry))
  coords <- st_coordinates(pois)
  pois <- pois %>%
    mutate(lon = coords[, 1], lat = coords[, 2])
  pois_indices <- st_within(pois, london_filtered, sparse = FALSE)
  pois_within <- pois[apply(pois_indices, 1, any), ]
  
  intersects_indices <- st_intersects(grid, london_filtered, sparse = FALSE)
  grid_intersects <- grid[apply(intersects_indices, 1, any), ]
  
  return (list(
    london_filtered = london_filtered,
    pois_within = pois_within,
    grid_intersects = grid_intersects))
}

# Filter movement dataset with the given date and districts
filter_movement_data <- function(movement_data, tiles, london_filtered, grid_intersects, date){
  
  # Filter movement data with date
  movement_filter <- movement_data[movement_data$AGG_DAY_PERIOD == date,]
  # Merge movement data with tiles and convert to sf object
  movement_merge <- merge(movement_filter, tiles, by.x = "LONLAT_ID", by.y = "LONLAT_ID")
  movement_sf <- st_as_sf(movement_merge, coords = c("XLON", "XLAT"), crs = st_crs(london))
  # Get movement data which is within the given districts
  within_indices <- st_within(movement_sf, london_filtered, sparse = FALSE)
  movement_join_within <- movement_sf[apply(within_indices, 1, any), ]
  movement_join_within <- movement_join_within[order(movement_join_within$mean_column),]
  
  # grid
  movement_join_within_g <- st_join(movement_join_within, grid_intersects, join = st_within)
  grid_means <- movement_join_within_g %>%
    group_by(grid_id) %>%
    summarise(mean_value = mean(mean_column, na.rm = TRUE))
  grid_means <- grid_means %>% st_drop_geometry()
  grid_with_means <- grid_intersects %>%
    left_join(grid_means, by = c("grid_id" = "grid_id"))
  
  # Add coords to the table
  movement_join_within_coords <- movement_join_within %>%
    mutate(X = st_coordinates(.)[,1], Y = st_coordinates(.)[,2])
  
  return (list(
    movement_join_within_coords = movement_join_within_coords,
    grid_with_means = grid_with_means))
}

# Is executed, when the button "Update Map according to first date" is clicked
calculate_one_date <- function(movement_data, tiles, london, pois, grid, date, districts){
  print("calculate_one_date:")
  print("Chosen date:")
  print(date)
  
  print("Chosen districts:")
  print(districts)
  
  result <- filter_grid_pois(london, districts, pois, grid)
  result1 <- filter_movement_data(movement_data, tiles, result$london_filtered, result$grid_intersects, date)
  
  return (list(
    london_filtered = result$london_filtered, 
    movement_join_within_coords = result1$movement_join_within_coords,
    pois_within = result$pois_within,
    grid_with_means = result1$grid_with_means))
}

# Is executed when the button "Update Map according to both dates" is clicked
compare_dates <- function(movement_data, tiles, london, pois, grid, date1, date2, districts) {
  
  print("compare_dates:")
  print("Chosen dates:")
  print(date1)
  print(date2)
  
  print("Chosen districts:")
  print(districts)
  
  result <- filter_grid_pois(london, districts, pois, grid)
  result1 <- filter_movement_data(movement_data, tiles, result$london_filtered, result$grid_intersects, date1)
  result2 <- filter_movement_data(movement_data, tiles, result$london_filtered, result$grid_intersects, date2)
  
  # Combine the two results
  grid1 <- result1$grid_with_means
  grid2 <- result2$grid_with_means
  movement1 <- result1$movement_join_within_coords
  movement2 <- result2$movement_join_within_coords
  
  grid1_geom <- st_geometry(grid1)
  grid2_geom <- st_geometry(grid2)
  
  grid1 <- grid1 %>% st_set_geometry(NULL)
  grid2 <- grid2 %>% st_set_geometry(NULL)
  movement1 <- movement1 %>% st_set_geometry(NULL)
  movement2 <- movement2 %>% st_set_geometry(NULL)
  
  merged_grid <- inner_join(grid1, grid2, by = "grid_id", suffix = c("_1", "_2"))
  merged_movement <- full_join(movement1, movement2, by = "LONLAT_ID", suffix = c("_1", "_2"))
  
  merged_grid <- st_as_sf(merged_grid, geometry = grid1_geom)
  merged_movement$new_X <- ifelse(is.na(merged_movement$X_1), merged_movement$X_2, merged_movement$X_1)
  merged_movement$new_Y <- ifelse(is.na(merged_movement$Y_1), merged_movement$Y_2, merged_movement$Y_1)
  merged_movement <- st_as_sf(merged_movement, coords = c("new_X", "new_Y"), crs = st_crs(london))
  
  # Calculate relative change of the two results
  merged_grid <- merged_grid %>%
    mutate(mean_value_diff = ifelse(is.na(mean_value_1) | is.na(mean_value_2), NA, round(((mean_value_2/mean_value_1)-1) *100,2)))
  merged_movement <- merged_movement %>%
    mutate(mean_column_diff = ifelse(is.na(mean_column_1) | is.na(mean_column_2), NA, round(((mean_column_2/mean_column_1)-1) *100,2)))
  
  result_grid <- merged_grid %>%
    select(grid_id, everything(), mean_value = mean_value_diff)
  result_movement <- merged_movement %>%
    select(LONLAT_ID, everything(), mean_column = mean_column_diff)
  
  return (list(
    london_filtered = result$london_filtered, 
    movement_join_within_coords = movement1,
    pois_within = result$pois_within,
    grid_with_means = result_grid))
}

# Function to calculate the mean movement for a given POI
calculate_movement_mean <- function(poi_name, buffer_size = 100) {
  # Function for line graph
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

# Calculations for line graph. Attention: It will take some time!
# Load this instead:
load("first_steps_R/movement_data.RData")
# movement_data <- do.call(
#   rbind,
#   lapply(unique(pois$name), calculate_movement_mean)
# )
# movement_data <- movement_data %>%
#   left_join(pois %>% select(name, type), by = c("POI" = "name"))
# movement_data <- movement_data %>%
#   mutate(weekday = weekdays(AGG_DAY_PERIOD))
#save(movement_data, file = "first_steps_R/movement_data.RData")

# Create data for the category plot
# Filter data and assign time periods
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

# Calculate the average activity for each period
mean_values <- movement_data %>%
  group_by(period, POI, type) %>%
  summarise(
    mean_activity = mean(mean_value, na.rm = TRUE),
    .groups = "drop"
  )
mean_values_wide <- mean_values %>%
  pivot_wider(names_from = period, values_from = mean_activity, 
              names_prefix = "mean_")
mean_values_wide <- mean_values_wide %>%
  mutate(
    change_Before_to_Lockdown = (mean_Lockdown - mean_Before_Lockdown) / mean_Before_Lockdown * 100,
    change_Lockdown_to_Post = (mean_Post_Lockdown - mean_Lockdown) / mean_Lockdown * 100,
    change_Post_to_Partial = (mean_Partial_Lockdown - mean_Post_Lockdown) / mean_Post_Lockdown * 100
  )
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

# Filter data for 'shop'
plot_data_shop <- plot_data_categories %>% filter(type == "shop")

# Filter data for 'museum' and split it into two groups
plot_data_museum <- plot_data_categories %>% filter(type == "museum")

# define the user interface
ui <- fluidPage(
  titlePanel("Mobility Scope"),
    tabsetPanel(
      tabPanel("Map", 
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
            dropdownButton(
              circle = F,
              label = "Select Districts",
              status = "default", 
              width = 450,
              tags$label("Choose:"),
              fluidRow(
                column(
                  width = 4,
                  checkboxGroupInput(
                    inputId = "districtsA",
                    label = NULL,
                    choices = unique(london$district_n)[1:11],
                    selected = c("City of London")
                  )
                ),
                column(
                  width = 4,
                  checkboxGroupInput(
                    inputId = "districtsB",
                    label = NULL,
                    choices = unique(london$district_n)[12:22],
                    selected = c()
                  )
                ),
                column(
                  width = 4,
                  checkboxGroupInput(
                    inputId = "districtsC",
                    label = NULL,
                    choices = unique(london$district_n)[23:33],
                    selected = c("Westminster")
                  )
                )
              )
            ),
            div(style="margin-bottom:10px"),
            actionButton("select_all", "Select All"),
            div(style="margin-bottom:10px"),
            actionButton("submit", "Update Map according to first date"),
            div(style="margin-bottom:10px"),
            actionButton("submit2", "Update Map according to both dates"),
          ),
          mainPanel(
              leafletOutput("map", height = 600)
            ),
          ),
        ),
      tabPanel("Plot", 
        plotlyOutput("linePlot")
      ),
      tabPanel("Plot Aggregated", 
        plotlyOutput("linePlot_aggregated")
      ),
      tabPanel("Plot Categories", 
                  plotOutput("categories_shops"),
                  plotOutput("categories_museums")
      )
    )
  )


#movement_data, tiles, london, pois, date, districts
# define the server logic
server <- function(input, output, session) {
  #"Select All"-Button Logik
  observeEvent(input$select_all, {
    updateCheckboxGroupInput(session, "districtsA",
                             selected = unique(london$district_n)[1:11])
    updateCheckboxGroupInput(session, "districtsB",
                             selected = unique(london$district_n)[12:22])
    updateCheckboxGroupInput(session, "districtsC",
                             selected = unique(london$district_n)[23:33])
  })
  
  # Define filtered data for submit
  filtered_data_single <- eventReactive(input$submit, {
    showPageSpinner()
    req(c(input$districtsA, input$districtsB, input$districtsC), input$selected_date)
    if(input$grid_size == "1x1"){
      grid <- grid_18
    } else if(input$grid_size == "2x2"){
      grid <- grid_17
    } else if(input$grid_size == "4x4"){
      grid <- grid_16
    } else if(input$grid_size == "8x8"){
      grid <- grid_15
    } else if(input$grid_size == "16x16"){
      grid <- grid_14
    }
    calculate_one_date(
      movement_data = movement,
      tiles = tiles,
      london = london,
      pois = pois,
      grid = grid,
      date = as.character(input$selected_date),
      districts = c(input$districtsA, input$districtsB, input$districtsC)
    )
  })
  
  # Define filtered data for submit2
  filtered_data_compare <- eventReactive(input$submit2, {
    showPageSpinner()
    req(c(input$districtsA, input$districtsB, input$districtsC), input$selected_date, input$selected_date_2)
    if(input$grid_size == "1x1"){
      grid <- grid_18
    } else if(input$grid_size == "2x2"){
      grid <- grid_17
    } else if(input$grid_size == "4x4"){
      grid <- grid_16
    } else if(input$grid_size == "8x8"){
      grid <- grid_15
    } else if(input$grid_size == "16x16"){
      grid <- grid_14
    }
    compare_dates(
      movement_data = movement,
      tiles = tiles,
      london = london,
      pois = pois,
      grid = grid,
      date1 = as.character(input$selected_date),
      date2 = as.character(input$selected_date_2),
      districts = c(input$districtsA, input$districtsB, input$districtsC)
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
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -0.1, lat = 51.5, zoom = 12) %>%
    # POIs
    addAwesomeMarkers(
      data = pois[pois$type == "shop", ],
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
        data = pois[pois$type == "museum", ],
        lng = ~lon, lat = ~lat,
        popup = ~paste("Name:", name),
        group = "Museums",
        icon=awesomeIcons(
          icon = 'ion-android-color-palette',
          iconColor = 'black',
          library = 'ion',
          markerColor = "orange"
        )) %>%
      
      addPolygons(
        data = london,
        color = "black",
        weight = 2,
        fillOpacity = 0.1,
        group = "London",
        label = ~district_n,
        highlight = highlightOptions(
          weight = 5,
          color = "blue",
          bringToFront = TRUE
        )
      ) %>%
      
      # Layers control
      addLayersControl(
        overlayGroups = c("London", "grid", "Movement Points", "Shops", "Museums"),
        options = layersControlOptions(collapsed = FALSE)
      ) 
  })
  
  output$linePlot <- renderPlotly({
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
        title = "Activity Trends Across POIs. Weekends are highlighted.",
        x = "",
        y = "Activity",
        color = "POI"
      ) +
      theme_minimal()
    
    plotly_obj <- ggplotly(p) %>%
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
                  font = list(size = 10),
                  align = "left"
                )
      ))
    
    plotly_obj
  })
  
  output$linePlot_aggregated <- renderPlotly({
    aggregated_data <- movement_data %>%
      group_by(AGG_DAY_PERIOD, category = type) %>%
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
                 font = list(size = 10),
                 align = "left"
               )
             ))
  })
  
  output$categories_shops <- renderPlot({
    # Plot for group 1 (Shops)
    ggplot(plot_data_shop, aes(x = reorder(POI, as.numeric(category)), fill = category)) +
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
  }, height = 600)
  
  output$categories_museums <- renderPlot({
    # Plot for group 2 (Museums - First Half)
    ggplot(plot_data_museum, aes(x = reorder(POI, as.numeric(category)), fill = category)) +
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
  }, height = 1600)

  # Update the map
  observe({
    print(paste("Chosen Date:", input$selected_date))
    print(paste("Chosen Date2:", input$selected_date_2))
    print(paste("Chosen Districts:", paste(c(input$districtsA, input$districtsB, input$districtsC), collapse = ", ")))
    data <- filtered_data()
    
    my_domain <- c(0, 3)
    
    if (input$submit2 > input$submit) {
      fixed_bin_scale <- colorBin(palette = c("blue","cyan","white", "yellow","red"), 
                                  domain=c(-500,0-.Machine$double.eps,0,0+.Machine$double.eps,500))
      legendTitle <- "Relative Change in %"
      popuptxt = "Change (%):"
    }
    else {
      fixed_bin_scale <- colorBin(
        palette = c("blue", "red"),
        domain = my_domain,
        bins = 7
      )
      legendTitle <- "Mean Activity Value"
      popuptxt = "Mean Activity Value:"
    }
    
    # Karte aktualisieren
    leafletProxy("map") %>%
      clearGroup(c("London", "grid", "Movement Points", "Shops", "Museums")) %>%
      clearControls() %>%
      
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
        group = "London",
        label = ~district_n,
        highlight = highlightOptions(
          weight = 5,
          color = "blue",
          bringToFront = TRUE
        )
      ) %>%
      
      # Points
      addCircleMarkers(
        data = data$movement_join_within_coords,
        lng = ~X, lat = ~Y,
        color = ~fixed_bin_scale(mean_column),
        fillOpacity = 0.7,
        radius = 4,
        popup = ~paste("Mean Column:", mean_column),
        group = "Movement Points"
      ) %>%
      
      # grid
      addPolygons(data = data$grid_with_means,
                  color = "black",
                  weight = 1,
                  fillColor = ~fixed_bin_scale(mean_value),
                  fillOpacity = 0.7,
                  popup = ~paste(popuptxt, round(mean_value,2)),
                  group = "grid") %>%
      
      addLegend(
        position = "bottomright",
        pal = fixed_bin_scale,
        values = my_domain,
        title = legendTitle,
        opacity = 1
      ) %>%
      
      # Layers control
      addLayersControl(
        overlayGroups = c("London", "grid", "Movement Points", "Shops", "Museums"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      
      hideGroup("Movement Points")
    
    hidePageSpinner()
  })
}

# run the application
shinyApp(ui = ui, server = server)

#Tests
#result <- calculate_one_date(movement, tiles, london, pois, grid_15, "2020-03-23", c("City of London", "Westminster"))
#result <- compare_dates(movement, tiles, london, pois, grid, "2020-03-23", "2020-04-06", c("City of London", "Westminster"))