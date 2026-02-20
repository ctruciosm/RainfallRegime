###################################################
####                 Figures                   ####
###################################################
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(stringr)
library(lubridate)
library(trend)
library(purrr)
library(sf)
library(geobr)

# Read files
dir.create("./results_figures", showWarnings = FALSE)
files_names <- list.files(path = './yearly_data', pattern = '*.csv', full.names = T)
aux <- readxl::read_xlsx("./daily_data/Celulas-FF.xlsx") |> rename(station = GRADE, lat = LAT, lon = LONG)

data_max <- files_names |>
  set_names(tools::file_path_sans_ext(basename(files_names))) |>
  map(~ read_csv(.x, show_col_types = FALSE) |> select(year, pr_max)) |>
  imap(~ rename(.x, !!.y := pr_max)) |>
  reduce(full_join, by = "year") |> 
  pivot_longer(cols = -year, names_to = "station", values_to = "pr_max") |>
  separate(station, into = c("lat", "lon"), sep = "_") |>
  mutate(lat = as.numeric(sub("lat", "", lat)), lon = as.numeric(sub("lon", "", lon))) |>
  left_join(aux, by = c("lat", "lon"))

data_sum <- files_names |>
  set_names(tools::file_path_sans_ext(basename(files_names))) |>
  map(~ read_csv(.x, show_col_types = FALSE) |> select(year, pr_sum)) |>
  imap(~ rename(.x, !!.y := pr_sum)) |>
  reduce(full_join, by = "year") |> 
  pivot_longer(cols = -year, names_to = "station", values_to = "pr_sum") |>
  separate(station, into = c("lat", "lon"), sep = "_") |>
  mutate(lat = as.numeric(sub("lat", "", lat)), lon = as.numeric(sub("lon", "", lon))) |>
  left_join(aux, by = c("lat", "lon"))


### Sequential plor for all stations

annual_rainfall_plot <- ggplot(data_sum, aes(x = year, y = pr_sum, color = factor(station))) + 
                          geom_line() + theme_bw() + theme(legend.position = "none") +
                          xlab("Year") + ylab("Annual rainfall (mm)") 
ggsave("./results_figures/annual_rainfall.png", plot = annual_rainfall_plot, width = 14, height = 6, dpi = 300)


max_daily_rainfall_plot <- ggplot(data_max, aes(x = year, y = pr_max, color = factor(station))) + 
                              geom_line() + theme_bw() + theme(legend.position = "none") +
                              xlab("Year") + ylab("Maximum daily rainfall (mm)")
ggsave("./results_figures/max_daily_rainfall.png", plot = max_daily_rainfall_plot, width = 14, height = 6, dpi = 300)


### Time Series Plot for which Pettit p-values < 0.1

results_max_year <- read.csv("./results_tables/results_max_year.csv") |> rename(station = Posto)
results_sum_year <- read.csv("./results_tables/results_sum_year.csv") |> rename(station = Posto)

which_obs_max <- results_max_year[which(results_max_year$Pettit < 0.1 & results_max_year$Mann.Kendall < 0.1), ]$station
which_obs_sum <- results_sum_year[which(results_sum_year$Pettit < 0.1 & results_sum_year$Mann.Kendall < 0.1), ]$station


facet_sum_plot <- data_sum |> left_join(results_sum_year[, c("station", "Pettit.Change")], by = "station") |> filter(station %in% which_obs_sum) |> 
  ggplot(aes(x = year, y = pr_sum)) + 
  geom_line(color = "blue") + 
  geom_vline(aes(xintercept = Pettit.Change), linetype = "dashed", color = "red") +
  facet_wrap(~ station) + xlab("Year") + ylab("Annual rainfall (mm)")
ggsave("./results_figures/annual_rainfall_facet.png", plot = facet_sum_plot, width = 14, height = 6, dpi = 300)

facet_max_plot <- data_max |> left_join(results_max_year[, c("station", "Pettit.Change")], by = "station") |> filter(station %in% which_obs_max) |> 
  ggplot(aes(x = year, y = pr_max)) + 
  geom_line(color = "blue") + 
  geom_vline(aes(xintercept = Pettit.Change), linetype = "dashed", color = "red") +
  facet_wrap(~ station) + xlab("Year") + ylab("Maximum daily rainfall (mm)")
ggsave("./results_figures/max_daily_rainfall_facet.png", plot = facet_max_plot, width = 14, height = 6, dpi = 300)



### Maps

basin <- st_read("./shp_files/GEOFT_BHO_PCJ_TDR.shp")

points_sum <- data_sum |> 
  filter(station %in% which_obs_sum) |> 
  distinct(station, .keep_all = TRUE) |> 
  select(station, lat, lon) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(st_crs(basin))
points_max <- data_max |> 
  filter(station %in% which_obs_max) |> 
  distinct(station, .keep_all = TRUE) |> 
  select(station, lat, lon) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(st_crs(basin))


plot_basin_sum <- ggplot(basin) +
  geom_sf(fill = "lightblue", color = "gray70", size = 0.2) +
  labs(title = "Piracicaba River Basin") +
  geom_sf(data = points_sum, color = "red", size = 2) +
  xlab(" ") + ylab(" ")  +
  geom_sf_text(data = points_sum, aes(label = station), color = "black", nudge_y = 0.02, size = 3) +
  theme_minimal() + theme(legend.position = "none")
ggsave("./results_figures/basin_significant_sum_01_pettit_and_mann_kendall.png", plot = plot_basin_sum, width = 14, height = 6, dpi = 300)

plot_basin_max <- ggplot(basin) +
  geom_sf(fill = "lightblue", color = "gray70", size = 0.2) +
  labs(title = "Piracicaba River Basin") +
  geom_sf(data = points_max, color = "red", size = 2) +
  xlab(" ") + ylab(" ")  +
  geom_sf_text(data = points_max, aes(label = station), color = "black", nudge_y = 0.02, size = 3) +
  theme_minimal() + theme(legend.position = "none")
ggsave("./results_figures/basin_significant_max_01_pettit_and_mann_kendall.png", plot = plot_basin_max, width = 14, height = 6, dpi = 300)

### Auxiliary maps

which_obs_max_pettit <- results_max_year[which(results_max_year$Pettit < 0.1), ]$station
which_obs_sum_pettit <- results_sum_year[which(results_sum_year$Pettit < 0.1), ]$station
which_obs_max_mann.kendall <- results_max_year[which(results_max_year$Mann.Kendall < 0.1), ]$station
which_obs_sum_mann.kendall <- results_sum_year[which(results_sum_year$Mann.Kendall < 0.1), ]$station


points_sum_pettit <- data_sum |> 
  filter(station %in% which_obs_sum_pettit) |> 
  distinct(station, .keep_all = TRUE) |> 
  select(station, lat, lon) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(st_crs(basin))
points_max_pettit <- data_max |> 
  filter(station %in% which_obs_max_pettit) |> 
  distinct(station, .keep_all = TRUE) |> 
  select(station, lat, lon) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(st_crs(basin))

points_sum_mann.kendall  <- data_sum |> 
  filter(station %in% which_obs_sum_mann.kendall ) |> 
  distinct(station, .keep_all = TRUE) |> 
  select(station, lat, lon) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(st_crs(basin))
points_max_mann.kendall  <- data_max |> 
  filter(station %in% which_obs_max_mann.kendall) |> 
  distinct(station, .keep_all = TRUE) |> 
  select(station, lat, lon) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
  st_transform(st_crs(basin))



plot_basin_sum_pettit <- ggplot(basin) +
  geom_sf(fill = "lightblue", color = "gray70", size = 0.2) +
  labs(title = "Piracicaba River Basin") +
  geom_sf(data = points_sum_pettit, color = "red", size = 2) +
  xlab(" ") + ylab(" ")  +
  geom_sf_text(data = points_sum_pettit, aes(label = station), color = "black", nudge_y = 0.02, size = 3) +
  theme_minimal() + theme(legend.position = "none")
ggsave("./results_figures/basin_significant_sum_01_pettit.png", plot = plot_basin_sum_pettit, width = 14, height = 6, dpi = 300)

plot_basin_max_pettit <- ggplot(basin) +
  geom_sf(fill = "lightblue", color = "gray70", size = 0.2) +
  labs(title = "Piracicaba River Basin") +
  geom_sf(data = points_max_pettit, color = "red", size = 2) +
  xlab(" ") + ylab(" ")  +
  geom_sf_text(data = points_max_pettit, aes(label = station), color = "black", nudge_y = 0.02, size = 3) +
  theme_minimal() + theme(legend.position = "none")
ggsave("./results_figures/basin_significant_max_01_pettit.png", plot = plot_basin_max_pettit, width = 14, height = 6, dpi = 300)

plot_basin_sum_mann.kendall <- ggplot(basin) +
  geom_sf(fill = "lightblue", color = "gray70", size = 0.2) +
  labs(title = "Piracicaba River Basin") +
  geom_sf(data = points_sum_mann.kendall, color = "red", size = 2) +
  xlab(" ") + ylab(" ")  +
  geom_sf_text(data = points_sum_mann.kendall, aes(label = station), color = "black", nudge_y = 0.02, size = 3) +
  theme_minimal() + theme(legend.position = "none")
ggsave("./results_figures/basin_significant_sum_01_mannkendall.png", plot = plot_basin_sum_mann.kendall, width = 14, height = 6, dpi = 300)

plot_basin_max_mann.kendall <- ggplot(basin) +
  geom_sf(fill = "lightblue", color = "gray70", size = 0.2) +
  labs(title = "Piracicaba River Basin") +
  geom_sf(data = points_max_mann.kendall, color = "red", size = 2) +
  xlab(" ") + ylab(" ")  +
  geom_sf_text(data = points_max_mann.kendall, aes(label = station), color = "black", nudge_y = 0.02, size = 3) +
  theme_minimal() + theme(legend.position = "none")
ggsave("./results_figures/basin_significant_max_01_mannkendall.png", plot = plot_basin_max_mann.kendall, width = 14, height = 6, dpi = 300)