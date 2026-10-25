# Build the point data for the ArHa globe in the INEEID talk
# (presentations/2026-ineeid-arha/, slide "The holes").
#
# Unlike the website globe (build_globe_data.R), locations are not binned: each
# distinct sampled location is one point. Decisions (David, 2026-10-16):
#
# - Detection: a location counts as a detection for a virus family if any
#   arenavirus or hantavirus measurement there has measurementValue > 0, by any
#   assay. A serology positive is exposure, not virus, and counts the same.
# - Classes: arenavirus, hantavirus, both, tested but none detected, and not
#   tested (a host record with no arenavirus or hantavirus measurement). A
#   measurement with a missing value counts as tested, not as a detection.
# - Precision: points whose coordinates are a country or adm1 centroid are
#   drawn hollow; everything finer is filled. The 10 "unspecified" records are
#   also hollow, since nothing says they are finer.
# - Other virus families in pathogen_mof (a few hepe-, pox- and
#   peribunyavirus rows) are ignored.
#
# Reads the ArHa parquet export in the sibling repository and writes one JSON
# file beside the deck, so the deck never reads the sibling repository. Land
# comes from assets/data/land_110m.json, written by build_globe_data.R.
#
# Run from the repository root:
#   Rscript scripts/build_talk_globe_data.R

if (!require("pacman")) install.packages("pacman")
pacman::p_load(arrow, dplyr, jsonlite, here)

parquet_dir <- here("..", "arenavirus_hantavirus", "arha_app", "data", "parquet")
out_points  <- here("presentations", "2026-ineeid-arha", "data", "arha_points.json")
dir.create(dirname(out_points), recursive = TRUE, showWarnings = FALSE)

coarse_precision <- c("country", "adm1", "unspecified")
class_labels     <- c("Arenavirus", "Hantavirus", "Both", "None detected", "Not tested")

# ---- Read ------------------------------------------------------------------

src_hosts <- file.path(parquet_dir, "host_occurrences.parquet")
if (!file.exists(src_hosts)) stop("Parquet not found: ", src_hosts, "\nThe ArHa repository must sit beside this one.")

hosts <- read_parquet(src_hosts) |>
  filter(coord_status == "valid", !is.na(decimalLatitude), !is.na(decimalLongitude))

measurements <- read_parquet(file.path(parquet_dir, "pathogen_mof.parquet")) |>
  filter(family_pathogen %in% c("Arenaviridae", "Hantaviridae"))

# ---- Detection per host record ---------------------------------------------

# One row per host record: was each family tested, and was any test positive.
detection_by_record <- measurements |>
  summarise(arena_tested = any(family_pathogen == "Arenaviridae"),
            hanta_tested = any(family_pathogen == "Hantaviridae"),
            arena_pos = any(family_pathogen == "Arenaviridae" & measurementValue > 0, na.rm = TRUE),
            hanta_pos = any(family_pathogen == "Hantaviridae" & measurementValue > 0, na.rm = TRUE),
            .by = occurrenceID)

hosts_detection <- hosts |>
  select(occurrenceID, decimalLatitude, decimalLongitude, coordinate_resolution_processed) |>
  left_join(detection_by_record, by = "occurrenceID") |>
  mutate(across(c(arena_tested, hanta_tested, arena_pos, hanta_pos), \(x) coalesce(x, FALSE)),
         hollow = as.character(coordinate_resolution_processed) %in% coarse_precision)

# ---- Collapse to locations -------------------------------------------------

# A location is a distinct coordinate pair at one precision class, so a fine
# site that happens to share a centroid's coordinates keeps its own marker.
locations <- hosts_detection |>
  summarise(n_records = n(),
            tested = any(arena_tested | hanta_tested),
            arena_pos = any(arena_pos),
            hanta_pos = any(hanta_pos),
            .by = c(decimalLatitude, decimalLongitude, hollow)) |>
  mutate(class = case_when(arena_pos & hanta_pos ~ "Both",
                           arena_pos ~ "Arenavirus",
                           hanta_pos ~ "Hantavirus",
                           tested ~ "None detected",
                           .default = "Not tested"),
         class_id = match(class, class_labels) - 1)

# ---- Write -----------------------------------------------------------------

# Points as compact arrays: lon, lat, class index, hollow (0/1), records.
# Not-tested and none-detected points come first so detections draw on top.
locations <- arrange(locations, desc(class_id))

class_counts <- count(locations, class) |>
  mutate(class = factor(class, levels = class_labels)) |>
  arrange(class)

out <- list(
  meta = list(
    source = "host_occurrences.parquet, pathogen_mof.parquet",
    source_modified = format(as.Date(file.mtime(src_hosts))),
    generated = format(Sys.Date()),
    n_records = nrow(hosts),
    n_locations = nrow(locations),
    n_locations_hollow = sum(locations$hollow),
    classes = class_labels,
    class_locations = setNames(as.list(class_counts$n), as.character(class_counts$class)),
    fields = c("lon", "lat", "class", "hollow", "n_records")),
  points = Map(function(x, y, k, h, n) list(x, y, k, h, n),
               round(locations$decimalLongitude, 3), round(locations$decimalLatitude, 3),
               locations$class_id, as.integer(locations$hollow), locations$n_records) |> unname())

writeLines(toJSON(out, auto_unbox = TRUE, digits = NA), out_points)

# ---- Report ----------------------------------------------------------------

message(sprintf("%s records at %s locations (%s hollow).",
                format(nrow(hosts), big.mark = ","), format(nrow(locations), big.mark = ","),
                format(sum(locations$hollow), big.mark = ",")))
print(class_counts)
message(sprintf("Wrote %s (%s KB).", basename(out_points), round(file.size(out_points) / 1024)))
