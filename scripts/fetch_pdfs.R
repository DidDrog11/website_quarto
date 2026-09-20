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

# bioRxiv and medRxiv (prefixes 10.1101 and, from 2025, 10.64898) serve the
# PDF at a predictable URL. Unpaywall lags on the new prefix, so ask the
# bioRxiv API directly for the latest version.
biorxiv_pdf <- function(doi) {
  if (is.na(doi) || !grepl("^10\\.(1101|64898)/", doi)) return(NULL)
  for (server in c("biorxiv", "medrxiv")) {
    r <- fetch_json(sprintf("https://api.biorxiv.org/details/%s/%s", server, doi))
    coll <- r$collection %||% list()
    if (!length(coll)) next
    latest <- coll[[length(coll)]]
    v <- latest$version %||% "1"
    return(list(url = sprintf("https://www.%s.org/content/%sv%s.full.pdf", server, doi, v),
                version = "submittedVersion", licence = latest$license %||% NA_character_,
                host = server, source = "biorxiv-api", which = "preprint"))
  }
  NULL
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
    b <- biorxiv_pdf(row$preprint_doi)
    if (!is.null(b)) out[[length(out) + 1]] <- b
  }
  # 4. A preprint-only entry hosted on bioRxiv or medRxiv.
  if (row$kind == "preprint") {
    b <- biorxiv_pdf(row$doi)
    if (!is.null(b)) out[[length(out) + 1]] <- b
  }
  # A preprint-only entry: its own DOI is the preprint.
  if (row$kind == "preprint") for (k in seq_along(out)) out[[k]]$which <- "preprint"
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
    # A file on disk wins over whatever the log said last time (including a
    # failed or none outcome), so hand-added copies get logged as manual.
    logged_file <- log$file[log$id == row$id][1]
    if (is.na(logged_file) || logged_file != have) {
      log <- log |> filter(id != row$id) |>
        bind_rows(tibble(id = row$id, doi = row$doi, file = have,
                         which = if (grepl("_preprint", have)) "preprint" else "published",
                         version = NA, source = "manual", url = NA, licence = NA,
                         fetched = as.character(Sys.Date())))
      write_csv(log, log_path, na = "")
    }
    next
  }
  # pdf_url in the overrides file means "do not fetch": either a publisher-hosted
  # PDF the site links to, or "none" for a paywalled paper with no author copy.
  if (!is.na(row$pdf_url) && row$pdf_url != "") {
    if (!row$id %in% log$id || !log$which[log$id == row$id][1] %in% c("external", "skipped")) {
      log <- log |> filter(id != row$id) |>
        bind_rows(tibble(id = row$id, doi = row$doi, file = NA,
                         which = if (row$pdf_url == "none") "skipped" else "external",
                         version = NA, source = "overrides",
                         url = if (row$pdf_url == "none") NA else row$pdf_url,
                         licence = NA, fetched = as.character(Sys.Date())))
      write_csv(log, log_path, na = "")
    }
    next
  }
  message(sprintf("[%d/%d] %s", i, nrow(rows), str_trunc(row$title, 70)))
  got <- FALSE
  tried <- character()
  for (cand in candidates(row)) {
    suffix <- if (cand$which == "preprint") "_preprint" else ""
    dest <- file.path(pdf_dir, paste0(row$year, "_", row$id, suffix, ".pdf"))
    message(sprintf("    trying %s copy from %s", cand$which, cand$source))
    tried <- c(tried, cand$url)
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
    # "failed": an open copy exists but the download did not work (bot
    # protection, redirect to a landing page). The URLs are logged so it can
    # be fetched in a browser. "none": no open copy was found anywhere.
    log <- log |> filter(id != row$id) |>
      bind_rows(tibble(id = row$id, doi = row$doi, file = NA,
                       which = if (length(tried)) "failed" else "none", version = NA,
                       source = NA, url = if (length(tried)) paste(tried, collapse = " ") else NA,
                       licence = NA, fetched = as.character(Sys.Date())))
  }
  write_csv(log, log_path, na = "")  # save progress after every entry
}

# Drop rows for entries no longer in publications.csv (for example a preprint
# that has since been paired with its paper and is now part of that row).
stale <- log |> filter(!id %in% rows$id)
if (nrow(stale)) {
  message(sprintf("  %d stale log rows removed (entries no longer listed): %s",
                  nrow(stale), paste(stale$id, collapse = ", ")))
  log <- log |> filter(id %in% rows$id)
}
write_csv(log, log_path, na = "")

# ---- Summary ----------------------------------------------------------------

tally <- log |> filter(id %in% rows$id) |> count(which)
print(tally)
failed <- log |> filter(id %in% rows$id, which == "failed") |>
  left_join(rows |> select(id, title, year), by = "id")
if (nrow(failed)) {
  message("An open copy exists but the download failed. Fetch it in a browser and save as papers/<year>_<id>.pdf (add _preprint if it is the preprint):")
  pwalk(list(failed$year, failed$id, failed$title, failed$url),
        function(y, i, t, u) message("  - ", y, "_", i, "  ", str_trunc(t %||% "", 50), "\n      ", u))
}
none <- log |> filter(id %in% rows$id, which == "none") |>
  left_join(rows |> select(id, title, year), by = "id")
if (nrow(none)) {
  message("No open-access copy found. Add the accepted manuscript by hand as papers/<year>_<id>.pdf:")
  walk2(paste0(none$year, "_", none$id), none$title, ~ message("  - ", .x, "  ", str_trunc(.y %||% "", 60)))
}
