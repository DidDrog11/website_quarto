# Collect open-access PDFs for every entry in data/publications.csv.
#
# For each entry the script looks, in order, for:
#   1. an open-access copy of the published version (Unpaywall),
#   2. a Crossref full-text link, only when the licence is Creative Commons,
#   3. an open-access copy of the preprint (Unpaywall).
# The first PDF found is saved to papers/<year>_<doi-slug>.pdf, with a
# `_preprint` suffix when only the preprint was available. Nothing is fetched
# from behind a paywall. For a paper with no open copy, put the author
# accepted manuscript in papers/ by hand using the same file name and the
# script records it as source "manual".
#
# Progress is logged to data/pdfs.csv (one row per entry). Entries that
# already have a file are skipped, so re-running only tries the gaps.
#
# Run from the repository root, after scripts/update_publications.R:
#   Rscript scripts/fetch_pdfs.R
#
# Unpaywall requires a contact email: set CONTACT_EMAIL or add `email:` to
# _variables.yml.

if (!require("pacman")) install.packages("pacman")
pacman::p_load(httr2, jsonlite, dplyr, purrr, stringr, readr, tibble, tidyr, here, yaml)
source(here::here("scripts", "publications_utils.R"))

pubs_path <- here("data", "publications.csv")
log_path  <- here("data", "pdfs.csv")
pdf_dir   <- here("papers")
dir.create(pdf_dir, showWarnings = FALSE)

email <- contact_email()
if (!nzchar(email)) stop("Unpaywall needs a contact email. Set CONTACT_EMAIL or add `email:` to _variables.yml.")

pubs <- read_csv(pubs_path, show_col_types = FALSE,
                 col_types = cols(.default = col_character(), year = col_integer()))

# ---- Finding candidates -----------------------------------------------------

unpaywall <- function(doi) {
  if (is.na(doi) || doi == "") return(NULL)
  fetch_json(sprintf("https://api.unpaywall.org/v2/%s?email=%s", doi, utils::URLencode(email, reserved = TRUE)))
}

# Best PDF URL from an Unpaywall record, with the version it represents.
unpaywall_pdf <- function(u) {
  if (is.null(u) || !isTRUE(u$is_oa)) return(NULL)
  locs <- c(list(u$best_oa_location), u$oa_locations %||% list())
  locs <- Filter(function(l) !is.null(l) && !is.null(l$url_for_pdf), locs)
  if (!length(locs)) return(NULL)
  l <- locs[[1]]
  list(url = l$url_for_pdf, version = l$version %||% NA_character_,
       licence = l$license %||% NA_character_, host = l$host_type %||% NA_character_)
}

candidates <- function(row) {
  out <- list()
  # 1. Published version, open access.
  p <- unpaywall_pdf(unpaywall(row$doi))
  if (!is.null(p)) out[[length(out) + 1]] <- c(p, source = "unpaywall", which = "published")
  # 2. Crossref full-text link under a CC licence.
  if (!is.na(row$pdf_link) && row$pdf_link != "" &&
      !is.na(row$licence) && grepl("creativecommons.org", row$licence)) {
    out[[length(out) + 1]] <- list(url = row$pdf_link, version = "publishedVersion",
                                   licence = row$licence, host = "publisher",
                                   source = "crossref", which = "published")
  }
  # 3. Preprint, open access.
  if (!is.na(row$preprint_doi) && row$preprint_doi != "") {
    q <- unpaywall_pdf(unpaywall(row$preprint_doi))
    if (!is.null(q)) out[[length(out) + 1]] <- c(q, source = "unpaywall", which = "preprint")
  }
  # A preprint-only entry: its own DOI is the preprint.
  if (row$kind == "preprint" && length(out)) out[[1]]$which <- "preprint"
  out
}

# ---- Downloading ------------------------------------------------------------

download_pdf <- function(url, dest) {
  tmp <- tempfile(fileext = ".pdf")
  ok <- tryCatch({
    polite_request(url) |>
      httr2::req_headers(Accept = "application/pdf") |>
      httr2::req_perform(path = tmp)
    TRUE
  }, error = function(e) {
    message("    download failed: ", conditionMessage(e))
    FALSE
  })
  Sys.sleep(1)
  if (!ok || !file.exists(tmp)) return(FALSE)
  magic <- readBin(tmp, "raw", n = 4)
  if (!identical(rawToChar(magic), "%PDF")) {
    message("    not a PDF (probably an HTML landing page); discarded")
    unlink(tmp)
    return(FALSE)
  }
  file.copy(tmp, dest, overwrite = TRUE)
  unlink(tmp)
  TRUE
}

# ---- Main loop --------------------------------------------------------------

log <- if (file.exists(log_path)) {
  read_csv(log_path, show_col_types = FALSE, col_types = cols(.default = col_character()))
} else {
  tibble(id = character(), doi = character(), file = character(), which = character(),
         version = character(), source = character(), url = character(),
         licence = character(), fetched = character())
}

# Files already in papers/ (fetched earlier, or dropped in by hand).
existing_file <- function(id, year) {
  stem <- paste0(year, "_", id)
  hits <- list.files(pdf_dir, pattern = paste0("^", stem, "(_preprint)?\\.pdf$"))
  if (length(hits)) hits[[1]] else NA_character_
}

rows <- pubs |> filter(kind %in% c("article", "preprint"))
message(sprintf("%d entries to check; %d already logged", nrow(rows), sum(rows$id %in% log$id)))

for (i in seq_len(nrow(rows))) {
  row <- rows[i, ]
  have <- existing_file(row$id, row$year)
  if (!is.na(have)) {
    if (!row$id %in% log$id) {
      log <- bind_rows(log, tibble(id = row$id, doi = row$doi, file = have,
                                   which = if (grepl("_preprint", have)) "preprint" else "published",
                                   version = NA, source = "manual", url = NA, licence = NA,
                                   fetched = as.character(Sys.Date())))
    }
    next
  }
  message(sprintf("[%d/%d] %s", i, nrow(rows), str_trunc(row$title, 70)))
  got <- FALSE
  for (cand in candidates(row)) {
    suffix <- if (cand$which == "preprint") "_preprint" else ""
    dest <- file.path(pdf_dir, paste0(row$year, "_", row$id, suffix, ".pdf"))
    message(sprintf("    trying %s copy from %s", cand$which, cand$source))
    if (download_pdf(cand$url, dest)) {
      log <- log |> filter(id != row$id) |>
        bind_rows(tibble(id = row$id, doi = row$doi, file = basename(dest), which = cand$which,
                         version = cand$version %||% NA_character_, source = cand$source, url = cand$url,
                         licence = cand$licence %||% NA_character_, fetched = as.character(Sys.Date())))
      got <- TRUE
      break
    }
  }
  if (!got) {
    log <- log |> filter(id != row$id) |>
      bind_rows(tibble(id = row$id, doi = row$doi, file = NA, which = "none", version = NA,
                       source = NA, url = NA, licence = NA, fetched = as.character(Sys.Date())))
  }
  write_csv(log, log_path, na = "")  # save progress after every entry
}

write_csv(log, log_path, na = "")

# ---- Summary ----------------------------------------------------------------

tally <- log |> filter(id %in% rows$id) |> count(which)
print(tally)
none <- log |> filter(id %in% rows$id, which == "none") |>
  left_join(rows |> select(id, title, year), by = "id")
if (nrow(none)) {
  message("No open-access copy found. Add the accepted manuscript by hand as papers/<year>_<id>.pdf:")
  walk2(none$id, none$title, ~ message("  - ", .x, "  ", str_trunc(.y %||% "", 60)))
}
