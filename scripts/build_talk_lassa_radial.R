# Radial maps of predicted Lassa incidence around Lagos, Tamale and Jos for the
# INEEID talk (presentations/2026-ineeid-arha/). A companion to
# lassa_fig8_talk.png: the profiles average each ring, these show how incidence
# varies with direction as well as distance.
#
# For visualisation only. The rasters are on a 0.05 degree grid (about 5.5 km).
# Each city is projected to a local grid in km centred on the city, using
# bilinear interpolation to a much finer cell, so the smooth surface below
# 5.5 km is interpolated, not predicted. Incidence is scaled to 0-1 within each
# city's 50 km circle, as the profiles are scaled per city.
#
# Reads the rasters from the sibling repository; no models are rerun.
#
# Run from the repository root:
#   Rscript scripts/build_talk_lassa_radial.R

if (!require("pacman")) install.packages("pacman")
pacman::p_load(terra, dplyr, readr, ggplot2, here)

# Switches
show_country  <- TRUE  # facet names "Lagos, Nigeria" rather than "Lagos"
show_shield   <- FALSE # dotted gold contour of the urban shield (night-time lights)
shield_level  <- 0.5   # contour level on night-time lights scaled 0-1 within each circle
show_peak     <- TRUE  # solid ring at each city's incidence peak distance from the profiles
max_dist_km   <- 50    # radius shown, as in the profiles
fine_res_km   <- 0.5   # interpolated cell size

lassa_dir <- here("..", "Completed Projects", "lassa-bridging-reanalysis")
out_png   <- here("presentations", "2026-ineeid-arha", "figures", "lassa_radial_talk.png")

# Load rasters --------------------------------------------------------------
incidence <- rast(file.path(lassa_dir, "results", "maps", "Incidence_Full_Custom_Lambda0.03.tif"))
ntl       <- rast(file.path(lassa_dir, "data", "processed", "final_predictor_stack_scaled.tif"))[["NTL"]]
cities    <- read_rds(file.path(lassa_dir, "data", "processed", "standardised_cities.rds"))

city_labels <- c(Lagos = "Lagos, Nigeria", Tamale = "Tamale, Ghana", Jos = "Jos, Nigeria")
if (!show_country) city_labels[] <- names(city_labels)

# Incidence peak distance per city from the saved profiles (ring with the
# highest raw mean, first ring on ties), to link the maps to the profiles
peaks <- read_rds(file.path(lassa_dir, "data", "processed", "normalised_profile_data.rds")) |>
  filter(Variable == "Incidence", City %in% names(city_labels)) |>
  group_by(City) |>
  slice_max(Mean, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(City, peak_km = Distance)

# Interpolate each city to a local km grid ----------------------------------
# Azimuthal equidistant projection centred on the city, so x and y are km from
# the centre and distance rings are true circles
scale01 <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))

city_grids <- list()
for (city in names(city_labels)) {
  centre <- cities |> filter(name == city)
  local_crs <- paste0("+proj=aeqd +lat_0=", centre$lat, " +lon_0=", centre$lon, " +units=km")
  template <- rast(xmin = -max_dist_km, xmax = max_dist_km, ymin = -max_dist_km, ymax = max_dist_km, resolution = fine_res_km, crs = local_crs)

  # Crop with a margin so interpolation at the circle edge has neighbours
  margin_deg <- (max_dist_km + 15) / 100
  box <- ext(centre$lon - margin_deg, centre$lon + margin_deg, centre$lat - margin_deg, centre$lat + margin_deg)
  local_stack <- c(crop(incidence, box), crop(ntl, box))
  names(local_stack) <- c("incidence", "ntl")
  local_grid <- project(local_stack, template, method = "bilinear")

  city_grids[[city]] <- as.data.frame(local_grid, xy = TRUE, na.rm = FALSE) |>
    mutate(City = city, dist_km = sqrt(x^2 + y^2)) |>
    filter(dist_km <= max_dist_km) |>
    mutate(incidence_scaled = scale01(incidence), ntl_scaled = scale01(ntl))
}
grid_df <- bind_rows(city_grids) |>
  mutate(city = factor(city_labels[City], levels = city_labels))

# Distance rings every 10 km, plus the peak ring ----------------------------
angle <- seq(0, 2 * pi, length.out = 361)
ring_df <- expand.grid(radius = seq(10, max_dist_km, by = 10), angle = angle) |>
  mutate(x = radius * cos(angle), y = radius * sin(angle))
ring_labels <- data.frame(radius = seq(10, max_dist_km, by = 10)) |>
  mutate(x = radius * cos(pi / 4), y = radius * sin(pi / 4), label = radius)

peak_df <- peaks |>
  mutate(city = factor(city_labels[City], levels = city_labels)) |>
  cross_join(data.frame(angle = angle)) |>
  mutate(x = peak_km * cos(angle), y = peak_km * sin(angle))

# Plot ----------------------------------------------------------------------
p <- ggplot(grid_df, aes(x = x, y = y)) +
  geom_raster(aes(fill = incidence_scaled)) +
  geom_path(data = ring_df, aes(group = radius), colour = "grey55", linewidth = 0.3, linetype = "dashed") +
  geom_path(data = ring_df |> filter(radius == max_dist_km), colour = "grey40", linewidth = 0.5) +
  geom_text(data = ring_labels, aes(label = label), colour = "grey40", size = 3.5) +
  # Sea (NA within the circle) is left white
  scale_fill_viridis_c(option = "mako", direction = -1, limits = c(0, 1), breaks = c(0, 0.5, 1), na.value = "white",
                       name = "Predicted incidence (scaled to city maximum)") +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5)) +
  facet_wrap(~ city, nrow = 1) +
  coord_equal(expand = FALSE) +
  theme_void(base_size = 16) +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        legend.title = element_text(size = 13),
        strip.text = element_text(face = "bold", hjust = 0, margin = margin(b = 6)),
        plot.background = element_rect(fill = "white", colour = NA))

if (show_shield) {
  p <- p + geom_contour(aes(z = ntl_scaled), breaks = shield_level, colour = "#E69F00", linewidth = 1.2, linetype = "dotted")
}

if (show_peak) {
  p <- p + geom_path(data = peak_df, colour = "black", linewidth = 0.8)
}

ggsave(out_png, p, width = 12, height = 5, dpi = 300, bg = "white")
