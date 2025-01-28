setwd("C:\\Users\\Anne\\Documents\\Studium\\9.Semester\\StudyProject\\Data")

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
london<- read_sf('merged_districts.shp')
london <- st_transform(london, crs = 4326)
districts <- c("City of London", "Westminster")
hexagons <- read_sf('grids_18.shp')
london_filtered <- london[london$district_n %in% districts, ]


#movement$AGG_DAY_PERIOD <- as.Date(movement$AGG_DAY_PERIOD, format = "%Y-%m-%d")
#movement_filter <- filter(movement, AGG_DAY_PERIOD >= as.Date("2020-01-01") & AGG_DAY_PERIOD <= as.Date("2020-01-01"))
movement_filter <- movement[movement$AGG_DAY_PERIOD == "2020-04-06",]
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
  lm.morantest(listw = mov_lw_q_B) |>
  glance_htest()

plot(lm(mean_value ~ 1, hexagons_with_means))

### local Moran's I ####
mov_nb_q |> nb2listw(style = "W", zero.policy = TRUE) -> mov_lw_q_W

mean_value |> 
  localmoran(listw = mov_lw_q_W) -> mov_locm
mov_locm

mean_value |> 
  moran.plot(listw = mov_lw_q_W, labels = hexagons_with_means$grid_id, 
             cex = 1, pch = ".", xlab = "I round turnout", 
             ylab = "lagged turnout") -> mov_infl_W

hexagons_with_means$hat_value <- mov_infl_W$hat
tm_shape(hexagons_with_means) + tm_fill("hat_value")

## global vs local
sum(mov_locm[,1])/Szero(mov_lw_q_W)
unname(moran.test(mean_value, mov_lw_q_W)$estimate[1])

##

hexagons_with_means$locm_pv <- p.adjust(mov_locm[, "Pr(z != E(Ii))"], "fdr")

tm_shape(hexagons_with_means) +
  tm_fill(c("locm_pv"), 
          breaks=c(0, 0.0005, 0.001, 0.005, 0.01, 
                   0.05, 0.1, 0.2, 0.5, 0.75, 1), 
          title = "Pseudo p-values\nLocal Moran's I",
          palette="-YlOrBr", legend.show = F) +
  tm_layout(panel.labels = c("Analytical conditional"))





mov_quadr <- attr(mov_locm, "quadr")$mean
mov_a <- table(addNA(mov_quadr))
mov_locm |> hotspot(Prname="Pr(z != E(Ii))", cutoff = 0.001, 
                droplevels=FALSE) -> hexagons_with_means$hs_an_q
mov_b <- table(addNA(hexagons_with_means$hs_an_q))
t(rbind("Moran plot quadrants" = mov_a, "Analytical cond." = mov_b))

hexagons_with_means$hs_an_q <- droplevels(hexagons_with_means$hs_an_q)

tm_shape(hexagons_with_means) +
  tm_fill(c("hs_an_q"),
          colorNA = "grey95", textNA="Not \"interesting\"",
          title = "Turnout hotspot status \nLocal Moran's I",
          palette = RColorBrewer::brewer.pal(4, "Set3")[-c(2,3)]) +
  tm_layout(panel.labels = c("Analytical conditional"))


## models


lm(I_turnout ~ 1) -> lm_null



lm_null |> localmoran.sad(nb = nb_q, style = "W",
                          alternative = "two.sided") |>
  summary() -> locm_sad_null



lm(I_turnout ~ 1, weights = hexagons_with_means$I_entitled_to_vote) ->
  lm_null_weights
lm_null_weights |>
  localmoran.sad(nb = nb_q, style = "W",
                 alternative = "two.sided") |>
  summary() -> locm_sad_null_weights



lm(I_turnout ~ Types, weights=hexagons_with_means$I_entitled_to_vote) ->
  lm_types
lm_types |> localmoran.sad(nb = nb_q, style = "W",
                           alternative = "two.sided") |>
  summary() -> locm_sad_types



locm_sad_null |> hotspot(Prname="Pr. (Sad)",
                         cutoff=0.005) -> hexagons_with_means$locm_sad0
locm_sad_null_weights |> hotspot(Prname="Pr. (Sad)",
                                 cutoff = 0.005) -> hexagons_with_means$locm_sad1
locm_sad_types |> hotspot(Prname="Pr. (Sad)",
                          cutoff = 0.005) -> hexagons_with_means$locm_sad2

### local Getis Ord
I_turnout |> 
  localG(lw_q_W, return_internals = TRUE)
