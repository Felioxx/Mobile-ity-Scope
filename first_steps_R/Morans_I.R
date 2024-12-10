setwd("D:/Dokumente/Studium/9 FS/Study Project/Data")

install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
library(spdep) |> suppressPackageStartupMessages()
library(parallel)
library(tmap)
library(sf)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(shiny)
library(arrow)

plot(ldeaths)
acf(ldeaths)

movement <- read_parquet("pre_processed_movement.parquet")
tiles <- read_parquet("distinct_LONLAT.parquet")
london<- read_sf('msoa2021/merged_districts.shp')
london <- st_transform(london, crs = 4326)
districts <- c("City of London")
hexagons <- read_sf('hexgr.shp')
london_filtered <- london[london$district_n %in% districts, ]


movement$AGG_DAY_PERIOD <- as.Date(movement$AGG_DAY_PERIOD, format = "%Y-%m-%d")
movement_filter <- filter(movement, AGG_DAY_PERIOD >= as.Date("2020-01-01") & AGG_DAY_PERIOD <= as.Date("2020-01-31"))
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
  group_by(AGG_DAY_PERIOD) %>%
  group_by(grid_id) %>%
  summarise(mean_value = mean(mean_column, na.rm = TRUE))
hexagon_means <- hexagon_means %>% st_drop_geometry()
hexagons_with_means <- hexagons_intersects %>%
  left_join(hexagon_means, by = c("grid_id" = "grid_id"))

# NA Werte löschen
hexagons_with_means %>%
  filter(!is.na(mean_value)) -> hexagons_with_means

glance_htest <- function(ht) c(ht$estimate,
                               "Std deviate" = unname(ht$statistic),
                               "p.value" = unname(ht$p.value))

set.seed(1)
hexagons_with_means |> poly2nb(queen = TRUE) -> mov_nb_q

mov_nb_q |> nb2listw(style = "B", zero.policy = TRUE) -> mov_lw_q_B

hexagons_with_means |> 
  st_geometry() |> 
  st_centroid(of_largest_polygon = TRUE) -> mov_coords 

mov_coords |> dnearneigh(0, 18300) -> mov_nb_d183
mov_nb_d183 |> nbdists(mov_coords) |> lapply(function(x) 1/(x/1000)) -> mov_gwts
mov_nb_d183 |> nb2listw(glist=mov_gwts, style="B") -> mov_lw_d183_idw_B

## random case

str(hexagons_with_means)

str(mov_lw_q_B)

(hexagons_with_means |>
    nrow() |>
    rnorm() -> x) |>
  moran.test(mov_lw_q_B, randomisation = FALSE,
             alternative = "two.sided") |>
  glance_htest()

## trend surface

beta <- 0.0015
mov_coords |> 
  st_coordinates() |> 
  subset(select = 1, drop = TRUE) |> 
  (function(x) x/1000)() -> t
(x + beta * t -> x_t) |> 
  moran.test(mov_lw_q_B, randomisation = FALSE,
             alternative = "two.sided") |> 
  glance_htest()

## linear model

lm(x_t ~ t) |> 
  lm.morantest(mov_lw_q_B, alternative = "two.sided") |> 
  glance_htest()

### Moran's I ####

hexagons_with_means |>
  st_drop_geometry() |>
  subset(select = mean_value, drop = TRUE) -> mean_value

mean_value |> moran.test(listw = mov_lw_q_B, randomisation = FALSE) |>
  glance_htest()

mean_value |> moran.test(listw = mov_lw_d183_idw_B, randomisation = FALSE) |>
  glance_htest()

## with linear model
lm(mean_value ~ 1, hexagons_with_means) |>
  lm.morantest(listw = lw_q_B) |>
  glance_htest()

plot(lm(mean_value ~ 1, hexagons_with_means))


plot(lm(Imean_value ~ mean_value, hexagons_with_means))
lm(Imean_value ~ mean_value, hexagons_with_means) |>
  lm.morantest(listw = lw_q_B) |>
  glance_htest()

plot(hexagons_with_means$Imean_value, hexagons_with_means$mean_value)
plot(lm(Imean_value ~ mean_value, hexagons_with_means))
