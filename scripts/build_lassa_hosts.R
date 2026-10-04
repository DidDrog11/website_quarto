# Build data/lassa_hosts.csv, the Lassa virus host table on
# research/lassa-epidemiology.qmd.
#
# Source: lassa/data/lassa_records_harmonised.xlsx, copied unchanged from the
# Lassa book chapter (lassa_chapter on the PC, written by its figures.R). It is
# the ArHa extract harmonised with reviewed corrections and additions, and runs
# to 2026, past the ArHa search. Published with the owner's agreement,
# 2026-10-04.
#
# The site reports which species have tested positive, by which assay, and
# where that was published. No counts. A study enters the table for a species
# and assay when it reports at least one positive animal.
#
# Run from the repository root:
#   Rscript scripts/build_lassa_hosts.R

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, stringr, readr, openxlsx, here)

src <- here("lassa", "data", "lassa_records_harmonised.xlsx")
out <- here("data", "lassa_hosts.csv")

records <- read.xlsx(src)

# Link corrections, checked against Crossref on 2026-10-04. Keyed on a fragment
# of the title, which is unique per study. The chapter's file is left as it is.
link_corrections <- tribble(
  ~title_contains,                                   ~link,
  # Science 1974; the file gives the JSTOR page
  "Lassa Virus Isolation from Mastomys",             "https://doi.org/10.1126/science.185.4147.263",
  # Bull WHO 1975, no DOI; the file gives a Penn State proxy URL
  "Recent isolations of Lassa virus",                "https://pubmed.ncbi.nlm.nih.gov/1085216/",
  # Vector Borne Zoonotic Dis 2007; the file gives a 2018 PNAS paper
  "Fluctuation of abundance and Lassa",              "https://doi.org/10.1089/vbz.2006.0520",
  # Vector Borne Zoonotic Dis 2014; the file gives a Zenodo record
  "Lassa serology in natural populations",           "https://doi.org/10.1089/vbz.2013.1484",
  # Sci Rep 2016; no link in the file
  "Spatial and temporal evolution of Lassa",         "https://doi.org/10.1038/srep21977",
  # Emerg Microbes Infect 2023; the file gives a 2007 hantavirus paper
  "Circulation of Lassa virus across the endemic",   "https://doi.org/10.1080/22221751.2023.2219350"
)

# Every correction must match exactly one study, so a changed title stops the
# build rather than leaving a wrong link in place.
for (k in link_corrections$title_contains) {
  n <- n_distinct(records$title[str_detect(records$title, fixed(k, ignore_case = TRUE))])
  if (n != 1) stop("Link correction '", k, "' matches ", n, " titles, not 1.")
}

# Positive records only, one row per species, assay, country and study
hosts <- records |>
  filter(number_positive > 0) |>
  distinct(host_genus, host_species, assay, country, author_key, publication_year, title, doi) |>
  rowwise() |>
  mutate(fix = link_corrections$link[str_detect(title, fixed(link_corrections$title_contains, ignore_case = TRUE))][1]) |>
  ungroup() |>
  mutate(doi = str_remove(str_remove(doi, "^https?://(dx\\.)?doi\\.org/"), "\\.$"),
         link = coalesce(fix, if_else(str_starts(doi, "10\\."), paste0("https://doi.org/", doi), doi)),
         reference = paste(author_key, publication_year)) |>
  select(host_genus, host_species, assay, country, publication_year, reference, title, link) |>
  arrange(host_genus, host_species, assay, publication_year)

if (any(is.na(hosts$link))) stop("Some references have no link:\n", paste(unique(hosts$reference[is.na(hosts$link)]), collapse = "\n"))

write_csv(hosts, out)
message(sprintf("Wrote %s: %d rows, %d species, %d references.",
                basename(out), nrow(hosts), n_distinct(hosts$host_species), n_distinct(hosts$link)))
