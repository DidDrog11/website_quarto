# Redraw Lassa Figure 8 (urban paradox radial profiles) for the INEEID talk
# (presentations/2026-ineeid-arha/), slide size and type, cities stacked.
#
# Reads the saved, normalised ring profiles from the sibling repository
# (written by R/5_urban_rural_incidence.R there), so no models are rerun. The
# smooth is the paper's (loess, span = 0.3); only the styling changes. The
# site figure lassa/images/urban_shield_decoupling.png is not touched.
#
# Run from the repository root:
#   Rscript scripts/build_talk_lassa_fig8.R

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, readr, ggplot2, here)

# Switches
show_country    <- TRUE   # facet names "Lagos, Nigeria" rather than "Lagos"
show_ring_means <- TRUE   # points for the 2 km ring means behind the incidence smooth
show_peaks      <- TRUE   # mark and label the ring with the highest incidence in each city
legend_rows     <- 1

profile_path <- here("..", "Completed Projects", "lassa-bridging-reanalysis", "data", "processed", "normalised_profile_data.rds")
out_png      <- here("presentations", "2026-ineeid-arha", "figures", "lassa_fig8_talk.png")

# Keep the three plotted variables (Population has no label and was the NA
# legend entry) and the three main cities, with talk labels
city_labels <- c(Lagos = "Lagos, Nigeria", Tamale = "Tamale, Ghana", Jos = "Jos, Nigeria")
if (!show_country) city_labels[] <- names(city_labels)

variable_labels <- c(Risk_Dx = "Ecological hazard", Incidence = "Predicted incidence", Shield_NTL = "Urban shield")

profiles <- read_rds(profile_path) |>
  filter(City %in% names(city_labels), Variable %in% names(variable_labels)) |>
  mutate(city = factor(city_labels[City], levels = city_labels),
         variable = factor(variable_labels[Variable], levels = variable_labels))

incidence_rings <- profiles |> filter(Variable == "Incidence")

# Incidence peak in each city: the ring with the highest raw mean, first ring
# on ties, as in the paper. Its normalised value is 1 by construction.
incidence_peaks <- incidence_rings |>
  group_by(city) |>
  slice_max(Mean, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(label = paste0("Peak ", Distance, " km"))

# Plot
line_colours <- c("#D55E00", "black", "#E69F00") |> setNames(variable_labels)
line_types   <- c("solid", "solid", "dotted") |> setNames(variable_labels)

p <- ggplot(profiles, aes(x = Distance, y = Norm_Mean, colour = variable, linetype = variable)) +
  geom_smooth(linewidth = 1.2, se = FALSE, span = 0.3) +
  scale_colour_manual(values = line_colours) +
  scale_linetype_manual(values = line_types) +
  facet_wrap(~ city, ncol = 1) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  expand_limits(y = 1.25) +
  labs(x = "Distance from city centre (km)", y = "Normalised intensity", colour = NULL, linetype = NULL) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold", hjust = 0),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", colour = NA)) +
  guides(colour = guide_legend(nrow = legend_rows), linetype = guide_legend(nrow = legend_rows))

# Ring means sit under the black incidence key rather than adding a fourth key
if (show_ring_means) {
  p <- p +
    geom_point(data = incidence_rings, alpha = 0.3, size = 1.5) +
    guides(colour = guide_legend(nrow = legend_rows, override.aes = list(shape = c(NA, 16, NA))))
}

# Peak ring drawn solid, with its distance above it
if (show_peaks) {
  p <- p +
    geom_point(data = incidence_peaks, aes(x = Distance, y = Norm_Mean), inherit.aes = FALSE, size = 2.5) +
    geom_text(data = incidence_peaks, aes(x = Distance, y = Norm_Mean, label = label), inherit.aes = FALSE, size = 4.5, hjust = 0.2, vjust = -0.7, fontface = "bold")
}

ggsave(out_png, p, width = 6, height = 5.5, dpi = 300, bg = "white")
