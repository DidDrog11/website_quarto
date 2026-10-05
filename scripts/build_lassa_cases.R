# Build data/lassa_cases.csv, annual confirmed Lassa fever cases by country,
# for the case figure and download on research/lassa-epidemiology.qmd.
#
# Two sources:
#   Nigeria, 2018 on: NCDC's published year-to-date total from the last
#     situation report of each season. These include NCDC's retrospective
#     reclassifications and do not depend on every weekly report surviving, so
#     they are the right figure for an annual total (owner's decision,
#     2026-10-05). Values copied from the hand-read table in
#     ../lassa-cfr/R/11_year_end.R; lassa-cfr is read, never written.
#   Everything else: lassa/data/confirmed_cases.csv, compiled by hand from WHO,
#     ECDC, ProMED, publications and national reports, one source per row.
#
# Run from the repository root:
#   Rscript scripts/build_lassa_cases.R

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, stringr, readr, here)

compiled <- read_csv(here("lassa", "data", "confirmed_cases.csv"), show_col_types = FALSE)
out <- here("data", "lassa_cases.csv")

ncdc_sitreps <- "https://ncdc.gov.ng/diseases/sitreps/?cat=5&name=An%20update%20of%20Lassa%20fever%20outbreak%20in%20Nigeria"

# NCDC year-to-date confirmed cases at each season's final sitrep. Update this
# table when a new season closes, or the current one moves on a week.
nigeria_year_end <- tribble(
  ~year, ~epi_week, ~confirmed_cases, ~complete, ~sitrep,
  2018L, 52L,  633, TRUE,  "lassa_sitrep_2018_w52_20181224.pdf",
  2019L, 52L,  833, TRUE,  "lassa_sitrep_2019_w52_20191228.pdf",
  2020L, 53L, 1189, TRUE,  "lassa_sitrep_2020_w53_20201231.pdf",
  2021L, 52L,  510, TRUE,  "lassa_sitrep_2021_w52_20211225.pdf",
  2022L, 52L, 1067, TRUE,  "lassa_sitrep_2022_w52_20221222.pdf",
  2023L, 52L, 1270, TRUE,  "lassa_sitrep_2023_w52_20231221.pdf",
  2024L, 52L, 1309, TRUE,  "2024 year-end report",
  2025L, 52L, 1148, TRUE,  "lassa_sitrep_2025_w52_20251227.pdf",
  2026L, 36L, 1086, FALSE, "lassa_sitrep_2026_w36_20260905.pdf"
)

nigeria_recent <- nigeria_year_end |>
  mutate(country = "Nigeria",
         coverage = if_else(complete, "", paste("to week", epi_week)),
         source = paste0("NCDC situation report (", sitrep, ")"),
         reference = ncdc_sitreps) |>
  select(country, year, confirmed_cases, complete, coverage, source, reference)

# Nigeria before 2018 uses the national row; other countries sum their
# subnational rows. Regional estimates for Nigeria are not used.
compiled_annual <- compiled |>
  filter(if_else(country == "nigeria", region == "all" & year < min(nigeria_year_end$year), confirmed_cases > 0)) |>
  summarise(confirmed_cases = sum(confirmed_cases),
            complete = all(complete_year == "y"),
            source = paste(sort(unique(source)), collapse = "; "),
            reference = paste(unique(reference), collapse = " ; "),
            .by = c(country, year)) |>
  mutate(country = recode(str_to_title(country), "Cote D'ivoire" = "Côte d'Ivoire", "Sierra Leone" = "Sierra Leone"),
         source = recode(source, situation_report = "Situation report", data_table = "Data table", promed = "ProMED",
                         publication = "Publication", who = "WHO", ecdc = "ECDC"),
         coverage = "")

cases <- bind_rows(compiled_annual, nigeria_recent) |>
  arrange(country, year)

write_csv(cases, out, na = "")
message(sprintf("Wrote %s: %d country-years, %d countries, %d to %d.",
                basename(out), nrow(cases), n_distinct(cases$country), min(cases$year), max(cases$year)))
