# Build the static data assets for the ArHa sampling globe on arha.qmd.
#
# Reads host occurrences from the ArHa app's parquet export in the sibling
# repository, bins them to H3 hexagons on the sphere, and writes a small JSON
# file into this repository. The page reads only that file, never the sibling
# repository, so the site renders without it.
#
# Host side only: where small mammals have been sampled, not where pathogens
# were detected. A high-level view by design: coordinates of every resolution
# are used as they are, from trap site down to country centroid.
#
# Also writes Natural Earth 110m land, with coordinates rounded to shrink it.
# Both files go to assets/data/, which _quarto.yml copies into _site/.
#
# h3jsr returns sf objects; sf is used here only to pass points in and read
# polygon coordinates out, not for spatial analysis.
#
# Run from the repository root:
#   Rscript scripts/build_globe_data.R

if (!require("pacman")) install.packages("pacman")
pacman::p_load(arrow, dplyr, h3jsr, sf, jsonlite, countrycode, here)

h3_res <- 2  # H3 resolution. 2 is ~86,700 km2 per cell; 3 is ~12,400 km2.

src      <- here("..", "arenavirus_hantavirus", "arha_app", "data", "parquet", "host_occurrences.parquet")
land_url <- "https://cdn.jsdelivr.net/gh/nvkelso/natural-earth-vector@v5.1.2/geojson/ne_110m_land.geojson"
land_raw <- here("data", "cache", "ne_110m_land.geojson")
out_hex  <- here("assets", "data", "arha_host_hex.json")
out_land <- here("assets", "data", "land_110m.json")
dir.create(dirname(out_hex), recursive = TRUE, showWarnings = FALSE)

# ---- Host occurrences to H3 cells ------------------------------------------

if (!file.exists(src)) stop("Parquet not found: ", src, "\nThe ArHa repository must sit beside this one.")

hosts <- read_parquet(src) |>
  filter(coord_status == "valid", !is.na(decimalLatitude), !is.na(decimalLongitude))

pts <- st_as_sf(hosts, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
hosts$cell <- point_to_cell(pts, res = h3_res)

cells <- hosts |>
  summarise(n_records = n(),
            n_individuals = sum(individualCount, na.rm = TRUE),
            n_species = n_distinct(scientificName[taxonRank %in% "species"], na.rm = TRUE),
            .by = cell) |>
  arrange(desc(n_records))

# Boundary of each cell as a flat vector lon, lat, lon, lat, ... (closed ring).
# Winding is normalised in the browser with d3.geoArea, so none is enforced here.
ring <- function(p) as.vector(t(round(st_coordinates(p)[, c("X", "Y")], 3)))
bounds <- lapply(cell_to_polygon(cells$cell, simple = TRUE), ring)

hex <- list(
  meta = list(
    source = basename(src),
    source_modified = format(as.Date(file.mtime(src))),
    generated = format(Sys.Date()),
    h3_resolution = h3_res,
    n_records = nrow(hosts),
    n_locations = nrow(distinct(hosts, decimalLatitude, decimalLongitude)),
    n_individuals = sum(hosts$individualCount, na.rm = TRUE),
    n_countries = n_distinct(hosts$countryCode, na.rm = TRUE),
    n_cells = nrow(cells),
    fields = c("n_records", "n_individuals", "n_species", "boundary")),
  cells = Map(function(r, i, s, b) list(r, i, s, b), cells$n_records, cells$n_individuals, cells$n_species, bounds) |> unname())

# Records by country, for the table view beneath the globe. Hover alone
# should never be the only way to read the data.
# Grouped by country code, not name: names vary in spelling between source
# studies, which would split one country into several rows and disagree with
# the caption's country count. Each code takes its most common name.
by_country <- hosts |>
  filter(!is.na(countryCode)) |>
  summarise(country = names(sort(table(country), decreasing = TRUE))[1],
            n_records = n(),
            n_locations = n_distinct(decimalLatitude, decimalLongitude),
            n_individuals = sum(individualCount, na.rm = TRUE),
            .by = countryCode) |>
  mutate(continent = countrycode::countrycode(countryCode, "iso3c", "continent")) |>
  arrange(continent, desc(n_records))
if (anyNA(by_country$continent)) stop("No continent for: ", paste(by_country$countryCode[is.na(by_country$continent)], collapse = ", "))
hex$countries <- Map(function(k, c, r, l, i) list(k, c, r, l, i),
                     by_country$continent, by_country$country, by_country$n_records, by_country$n_locations, by_country$n_individuals) |> unname()
hex$meta$country_fields <- c("continent", "country", "n_records", "n_locations", "n_individuals")
hex$meta$n_records_no_country <- nrow(hosts) - sum(by_country$n_records)   # coordinates but no country code

writeLines(toJSON(hex, auto_unbox = TRUE, digits = NA), out_hex)

# ---- Natural Earth 110m land -----------------------------------------------

if (!file.exists(land_raw)) {
  dir.create(dirname(land_raw), recursive = TRUE, showWarnings = FALSE)
  download.file(land_url, land_raw, mode = "wb", quiet = TRUE)
}

round_coords <- function(x) if (is.numeric(x)) round(x, 2) else if (is.list(x)) lapply(x, round_coords) else x
land <- fromJSON(land_raw, simplifyVector = FALSE)
land$features <- lapply(land$features, function(f) list(type = "Feature", properties = setNames(list(), character()), geometry = round_coords(f$geometry)))
land[setdiff(names(land), c("type", "features"))] <- NULL
writeLines(toJSON(land, auto_unbox = TRUE, digits = NA), out_land)

# ---- Report ----------------------------------------------------------------

m <- hex$meta
message(sprintf("H3 res %d: %s records at %s locations in %s countries -> %s cells.",
                h3_res, format(m$n_records, big.mark = ","), format(m$n_locations, big.mark = ","),
                m$n_countries, format(m$n_cells, big.mark = ",")))
message(sprintf("Records per cell: median %s, max %s.", median(cells$n_records), max(cells$n_records)))
message(sprintf("Wrote %s (%s KB) and %s (%s KB).",
                basename(out_hex), round(file.size(out_hex) / 1024), basename(out_land), round(file.size(out_land) / 1024)))
