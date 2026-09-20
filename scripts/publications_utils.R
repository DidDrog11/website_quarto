# Shared helpers for scripts/update_publications.R and scripts/fetch_pdfs.R.
# Sourced by both. Not run directly.

`%||%` <- function(a, b) if (is.null(a)) b else a

# Contact email for polite API use (Crossref polite pool, Unpaywall).
# Read from the CONTACT_EMAIL environment variable, else from `email:` in
# _variables.yml, else empty. Never hard-code it here.
contact_email <- function() {
  e <- Sys.getenv("CONTACT_EMAIL", unset = "")
  if (nzchar(e)) return(e)
  vars <- here::here("_variables.yml")
  if (file.exists(vars)) {
    v <- yaml::read_yaml(vars)
    if (!is.null(v$email) && nzchar(v$email)) return(v$email)
  }
  ""
}

# ---- DOI handling -----------------------------------------------------------

doi_norm <- function(x) {
  x <- tolower(trimws(x))
  x <- sub("^https?://(dx\\.)?doi\\.org/", "", x)
  x <- sub("^doi:\\s*", "", x)
  x
}

# Servers that mint a new DOI per version as <base>.<n>. For these the
# unsuffixed DOI (where it exists) resolves to the latest version.
versioned_prefixes <- c("10.32388",  # Qeios
                        "10.12688")  # F1000 / Wellcome Open Research / Gates OR

doi_prefix <- function(x) sub("/.*$", "", doi_norm(x))

doi_base <- function(x) {
  x <- doi_norm(x)
  ifelse(doi_prefix(x) %in% versioned_prefixes, sub("\\.\\d+$", "", x), x)
}

# Version number for versioned servers. Unsuffixed counts as newest.
doi_version <- function(x) {
  x <- doi_norm(x)
  v <- stringr::str_extract(x, "(?<=\\.)\\d+$")
  v <- ifelse(doi_prefix(x) %in% versioned_prefixes, v, NA_character_)
  out <- suppressWarnings(as.numeric(v))
  ifelse(doi_prefix(x) %in% versioned_prefixes & is.na(out), Inf, out)
}

doi_slug <- function(x) gsub("^_|_$", "", gsub("[^a-z0-9]+", "_", doi_norm(x)))

doi_url <- function(x) ifelse(is.na(x) | x == "", NA_character_, paste0("https://doi.org/", doi_norm(x)))

# ---- HTTP -------------------------------------------------------------------

polite_request <- function(url) {
  email <- contact_email()
  ua <- paste0("dsimons.org publications script (https://www.dsimons.org",
               if (nzchar(email)) paste0("; mailto:", email) else "", ")")
  httr2::request(url) |>
    httr2::req_user_agent(ua) |>
    httr2::req_timeout(60) |>
    httr2::req_retry(max_tries = 3, backoff = function(i) 2 * i)
}

# GET a JSON document. Returns NULL on 404 or failure. Caches the raw body
# when cache_file is given, so re-runs do not re-hit the API.
fetch_json <- function(url, cache_file = NULL, headers = list()) {
  if (!is.null(cache_file) && file.exists(cache_file)) {
    return(jsonlite::fromJSON(cache_file, simplifyVector = FALSE))
  }
  req <- polite_request(url)
  if (length(headers)) req <- httr2::req_headers(req, !!!headers)
  resp <- tryCatch(
    httr2::req_perform(req),
    httr2_http_404 = function(e) NULL,
    error = function(e) {
      message("  request failed for ", url, ": ", conditionMessage(e))
      NULL
    }
  )
  Sys.sleep(0.5)  # stay well inside every service's rate limit
  if (is.null(resp)) return(NULL)
  txt <- httr2::resp_body_string(resp)
  if (!is.null(cache_file)) {
    dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
    writeLines(txt, cache_file, useBytes = TRUE)
  }
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

# ---- Crossref ---------------------------------------------------------------

cr_year <- function(m) {
  for (f in c("issued", "published-online", "published-print", "created")) {
    dp <- m[[f]]$`date-parts`
    if (length(dp) && length(dp[[1]]) && !is.null(dp[[1]][[1]])) {
      return(as.integer(dp[[1]][[1]]))
    }
  }
  NA_integer_
}

cr_authors <- function(m) {
  a <- m$author
  if (!length(a)) return(NA_character_)
  one <- function(p) {
    if (!is.null(p$name)) return(p$name)  # consortium author
    fam <- p$family %||% ""
    giv <- p$given %||% ""
    parts <- strsplit(giv, "[ .-]+")[[1]]
    initials <- paste(substr(parts, 1, 1), collapse = "")
    trimws(paste(fam, initials))
  }
  paste(vapply(a, one, character(1)), collapse = ", ")
}

cr_relation <- function(m, kind) {
  r <- m$relation[[kind]]
  if (!length(r)) return(NA_character_)
  ids <- vapply(r, function(z) {
    if (identical(tolower(z$`id-type` %||% ""), "doi")) z$id else NA_character_
  }, character(1))
  ids <- ids[!is.na(ids)]
  if (length(ids)) doi_norm(ids[[1]]) else NA_character_
}

cr_licence <- function(m) {
  l <- m$license
  if (!length(l)) return(NA_character_)
  urls <- vapply(l, function(z) z$URL %||% NA_character_, character(1))
  cc <- urls[grepl("creativecommons.org", urls)]
  if (length(cc)) cc[[1]] else urls[[1]]
}

cr_pdf_link <- function(m) {
  l <- m$link
  if (!length(l)) return(NA_character_)
  pdf <- vapply(l, function(z) {
    if (identical(z$`content-type`, "application/pdf")) z$URL else NA_character_
  }, character(1))
  pdf <- pdf[!is.na(pdf)]
  if (length(pdf)) pdf[[1]] else NA_character_
}

cr_tidy <- function(m) {
  title <- if (length(m$title)) stringr::str_squish(m$title[[1]]) else NA_character_
  journal <- if (length(m$`container-title`)) m$`container-title`[[1]] else NA_character_
  server <- if (length(m$institution)) m$institution[[1]]$name %||% NA_character_ else NA_character_
  if (is.na(journal)) journal <- server %||% m$publisher %||% NA_character_
  tibble::tibble(
    doi = doi_norm(m$DOI),
    resolved = TRUE,
    source_api = "crossref",
    title = title,
    journal = journal,
    year = cr_year(m),
    authors = cr_authors(m),
    cr_type = m$type %||% NA_character_,
    cr_subtype = m$subtype %||% NA_character_,
    dc_type = NA_character_,
    is_preprint_of = cr_relation(m, "is-preprint-of"),
    has_preprint = cr_relation(m, "has-preprint"),
    licence = cr_licence(m),
    pdf_link = cr_pdf_link(m)
  )
}

# ---- DataCite (Zenodo, OSF, Dryad and other data/software DOIs) -------------

dc_tidy <- function(d) {
  a <- d$attributes
  title <- if (length(a$titles)) stringr::str_squish(a$titles[[1]]$title) else NA_character_
  creators <- vapply(a$creators %||% list(), function(p) {
    if (!is.null(p$familyName)) {
      giv <- p$givenName %||% ""
      initials <- paste(substr(strsplit(giv, "[ .-]+")[[1]], 1, 1), collapse = "")
      trimws(paste(p$familyName, initials))
    } else p$name %||% ""
  }, character(1))
  tibble::tibble(
    doi = doi_norm(d$id),
    resolved = TRUE,
    source_api = "datacite",
    title = title,
    journal = a$publisher %||% NA_character_,
    year = suppressWarnings(as.integer(a$publicationYear %||% NA)),
    authors = if (length(creators)) paste(creators, collapse = ", ") else NA_character_,
    cr_type = NA_character_,
    cr_subtype = NA_character_,
    dc_type = a$types$resourceTypeGeneral %||% NA_character_,
    is_preprint_of = NA_character_,
    has_preprint = NA_character_,
    licence = if (length(a$rightsList)) a$rightsList[[1]]$rightsUri %||% NA_character_ else NA_character_,
    pdf_link = NA_character_
  )
}

# Resolve one DOI: Crossref first, DataCite second. Cached under data/cache/.
enrich_doi <- function(doi, cache_dir) {
  slug <- doi_slug(doi)
  m <- fetch_json(paste0("https://api.crossref.org/works/", doi),
                  cache_file = file.path(cache_dir, "crossref", paste0(slug, ".json")))
  if (!is.null(m$message)) return(cr_tidy(m$message))
  d <- fetch_json(paste0("https://api.datacite.org/dois/", doi),
                  cache_file = file.path(cache_dir, "datacite", paste0(slug, ".json")))
  if (!is.null(d$data)) return(dc_tidy(d$data))
  tibble::tibble(doi = doi, resolved = FALSE, source_api = NA_character_)
}
