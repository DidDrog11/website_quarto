# Build data/publications.csv from ORCID.
#
# 1. Pull the public ORCID works record (no token needed).
# 2. Apply data/publications_overrides.csv: `add` DOIs ORCID lacks, `hide`
#    DOIs to drop, and the promotion tier (page / card / line) per DOI.
# 3. Drop Zenodo/OSF "other" deposits (posters, protocols) unless overridden.
# 4. Collapse versioned preprints (Qeios, F1000) to the latest version.
# 5. Enrich every DOI from Crossref, falling back to DataCite for data and
#    software DOIs. Responses are cached under data/cache/ (gitignored).
# 6. Pair each preprint with its published version using Crossref's
#    is-preprint-of / has-preprint relations, then by exact title match.
#    A pair becomes one row with both DOIs. If ORCID holds the preprint but
#    not the paper, the paper is fetched and added.
# 7. Write data/publications.csv, sorted newest first.
#
# Run from the repository root:
#   Rscript scripts/update_publications.R
#
# Optional: set CONTACT_EMAIL (or `email:` in _variables.yml) so Crossref
# serves the request from its polite pool. The script works without it.

if (!require("pacman")) install.packages("pacman")
pacman::p_load(httr2, jsonlite, dplyr, purrr, stringr, readr, tibble, tidyr, here, yaml)
source(here::here("scripts", "publications_utils.R"))

orcid_id       <- "0000-0001-9655-1656"
cache_dir      <- here("data", "cache")
overrides_path <- here("data", "publications_overrides.csv")
out_path       <- here("data", "publications.csv")

# ORCID work types that belong on the site. "other" is dropped unless the DOI
# is in the overrides file.
keep_types <- c("journal-article", "preprint", "working-paper", "book-chapter",
                "book", "report", "data-set", "software")

# ---- 1. ORCID ---------------------------------------------------------------

message("Fetching ORCID works for ", orcid_id)
orcid <- fetch_json(sprintf("https://pub.orcid.org/v3.0/%s/works", orcid_id),
                    headers = list(Accept = "application/json"))
if (is.null(orcid) || !length(orcid$group)) stop("ORCID returned no works. Is the API up?")

# ORCID groups every DOI it considers the same work: a preprint alongside its
# paper, and every version of a versioned DOI. Keep one DOI per base (the
# newest version) and let distinct bases through as separate works that share
# the group's summary metadata. group_id is kept so a group holding both a
# preprint and an article becomes a known pair later.
orcid_works <- imap_dfr(orcid$group, function(g, idx) {
  ids  <- g$`external-ids`$`external-id` %||% list()
  dois <- vapply(ids, function(i) {
    if (identical(tolower(i$`external-id-type` %||% ""), "doi")) i$`external-id-value` else NA_character_
  }, character(1))
  dois <- unique(doi_norm(dois[!is.na(dois)]))
  s <- g$`work-summary`[[1]]
  meta <- tibble(
    group_id     = as.integer(idx),
    orcid_title  = s$title$title$value %||% NA_character_,
    orcid_type   = s$type %||% NA_character_,
    orcid_year   = suppressWarnings(as.integer(s$`publication-date`$year$value %||% NA)),
    put_code     = as.integer(s$`put-code` %||% NA),
    orcid_source = s$source$`source-name`$value %||% NA_character_
  )
  if (!length(dois)) return(bind_cols(tibble(doi = NA_character_), meta))
  tibble(doi = dois, base = doi_base(dois), version = doi_version(dois)) |>
    group_by(base) |>
    arrange(desc(version), .by_group = TRUE) |>
    slice(1) |>
    ungroup() |>
    select(doi) |>
    bind_cols(meta)
})

message(sprintf("  %d ORCID work groups, %d DOIs after keeping one per version",
                length(orcid$group), sum(!is.na(orcid_works$doi))))

no_doi <- orcid_works |> filter(is.na(doi))
if (nrow(no_doi)) {
  message("  Works without a DOI are skipped (add a DOI on ORCID to include them):")
  walk(no_doi$orcid_title, ~ message("    - ", .x))
}
orcid_works <- orcid_works |> filter(!is.na(doi))

# ---- 2. Overrides -----------------------------------------------------------

overrides <- read_csv(overrides_path, col_types = cols(.default = col_character()),
                      show_col_types = FALSE)
for (col in c("tier", "page", "note", "action", "comment", "preprint_doi", "pdf_url")) {
  if (!col %in% names(overrides)) overrides[[col]] <- ""
}
overrides <- overrides |>
  mutate(doi = doi_norm(doi)) |>
  mutate(across(c(tier, page, note, action, comment, preprint_doi, pdf_url), ~ replace_na(.x, ""))) |>
  mutate(preprint_doi = ifelse(preprint_doi == "", "", doi_norm(preprint_doi)))

bad_tier <- overrides |> filter(!tier %in% c("", "page", "card", "line"))
if (nrow(bad_tier)) stop("Unknown tier in overrides: ", paste(bad_tier$tier, collapse = ", "))

added <- overrides |>
  filter(action == "add", !doi %in% orcid_works$doi) |>
  transmute(doi, group_id = NA_integer_, orcid_title = NA_character_, orcid_type = "added",
            orcid_year = NA_integer_, put_code = NA_integer_, orcid_source = "overrides")
if (nrow(added)) message(sprintf("  %d DOIs added from overrides (missing from ORCID)", nrow(added)))

hidden <- overrides |> filter(action == "hide") |> pull(doi)

works <- bind_rows(orcid_works, added) |>
  filter(!doi %in% hidden)

# ---- 3. Type filter ---------------------------------------------------------

dropped <- works |> filter(!(orcid_type %in% c(keep_types, "added") | doi %in% overrides$doi))
if (nrow(dropped)) {
  message(sprintf("  %d works of type 'other' dropped (posters, protocols, deposits). Add a DOI to overrides with action=add to keep one:", nrow(dropped)))
  walk2(dropped$doi, dropped$orcid_title, ~ message("    - ", .x, "  ", str_trunc(.y %||% "", 60)))
}
works <- works |> filter(orcid_type %in% c(keep_types, "added") | doi %in% overrides$doi)

# ---- 4. Collapse versioned preprints ----------------------------------------

works <- works |>
  mutate(base = doi_base(doi), version = doi_version(doi)) |>
  group_by(base) |>
  arrange(desc(version), .by_group = TRUE) |>
  mutate(n_versions = n()) |>
  slice(1) |>
  ungroup()
collapsed <- works |> filter(n_versions > 1)
if (nrow(collapsed)) {
  message(sprintf("  %d versioned works collapsed to their latest version", nrow(collapsed)))
}

# ---- 5. Enrich --------------------------------------------------------------

message(sprintf("Resolving %d DOIs via Crossref / DataCite (cached in %s)", nrow(works), cache_dir))
meta <- map_dfr(seq_len(nrow(works)), function(i) {
  d <- works$doi[[i]]
  message(sprintf("  [%d/%d] %s", i, nrow(works), d))
  enrich_doi(d, cache_dir)
})
pubs <- works |> left_join(meta, by = "doi")

unresolved <- pubs |> filter(!resolved)
if (nrow(unresolved)) {
  message(sprintf("  %d DOIs did not resolve at Crossref or DataCite; ORCID metadata used:", nrow(unresolved)))
  walk(unresolved$doi, ~ message("    - ", .x))
  pubs <- pubs |> mutate(title = coalesce(title, orcid_title), year = coalesce(year, orcid_year))
}

# ---- 6. Pair preprints with published versions ------------------------------

classify <- function(df) {
  df |> mutate(kind = case_when(
    cr_type == "posted-content"                                        ~ "preprint",
    cr_type %in% c("journal-article", "book-chapter", "proceedings-article",
                   "book", "monograph", "report", "report-component")  ~ "article",
    cr_type == "dataset" | dc_type == "Dataset"                        ~ "data",
    dc_type == "Software"                                              ~ "software",
    orcid_type %in% c("preprint", "working-paper")                     ~ "preprint",
    orcid_type %in% c("journal-article", "book-chapter", "book", "report") ~ "article",
    orcid_type == "data-set"                                           ~ "data",
    orcid_type == "software"                                           ~ "software",
    TRUE                                                               ~ "other"
  ))
}
pubs <- classify(pubs)

# Map any versioned DOI onto the DOI we kept for that base.
to_kept <- function(d) {
  idx <- match(doi_base(d), pubs$base)
  ifelse(is.na(idx), doi_norm(d), pubs$doi[idx])
}

# Pair sources, in priority order:
# 1. preprint_doi set on an article's row in the overrides file,
# 2. an ORCID group that holds both a preprint and an article,
# 3. Crossref is-preprint-of / has-preprint relations,
# 4. exact title match (added further down).
pair_manual <- overrides |>
  filter(preprint_doi != "") |>
  transmute(preprint_doi = to_kept(preprint_doi), published_doi = to_kept(doi))

pair_orcid <- pubs |>
  filter(!is.na(group_id)) |>
  group_by(group_id) |>
  filter(n() > 1, any(kind == "preprint"), any(kind == "article")) |>
  summarise(preprint_doi = first(doi[kind == "preprint"]),
            published_doi = first(doi[kind == "article"]), .groups = "drop") |>
  select(-group_id)

pair_rel <- bind_rows(
  pubs |> filter(kind == "preprint", !is.na(is_preprint_of)) |>
    transmute(preprint_doi = doi, published_doi = to_kept(is_preprint_of)),
  pubs |> filter(kind == "article", !is.na(has_preprint)) |>
    transmute(preprint_doi = to_kept(has_preprint), published_doi = doi)
)

# Published versions ORCID does not hold: fetch and add them.
missing_pub <- setdiff(c(pair_manual$published_doi, pair_rel$published_doi), pubs$doi)
missing_pre <- setdiff(pair_manual$preprint_doi, pubs$doi)
if (length(missing_pre)) {
  message("  preprint_doi values in overrides that are not in ORCID (pair ignored until added):")
  walk(missing_pre, ~ message("    - ", .x))
}
if (length(missing_pub)) {
  message(sprintf("  %d published versions found via preprint relations but absent from ORCID; adding:", length(missing_pub)))
  walk(missing_pub, ~ message("    - ", .x, "  (add it to ORCID via search-and-link)"))
  extra <- map_dfr(missing_pub, enrich_doi, cache_dir = cache_dir) |>
    mutate(orcid_type = "crossref-relation", orcid_source = "crossref relation",
           base = doi_base(doi), version = doi_version(doi), n_versions = 1L)
  pubs <- bind_rows(pubs, extra) |> classify()
}

# Title match as a fallback for pairs Crossref does not know about.
norm_title <- function(x) str_squish(str_replace_all(tolower(x), "[^a-z0-9 ]", ""))
pair_title <- inner_join(
  pubs |> filter(kind == "preprint") |> transmute(preprint_doi = doi, key = norm_title(title)),
  pubs |> filter(kind == "article")  |> transmute(published_doi = doi, key = norm_title(title)),
  by = "key", na_matches = "never"
) |> select(-key)

pairs <- bind_rows(pair_manual, pair_orcid, pair_rel, pair_title) |>
  filter(!is.na(preprint_doi), !is.na(published_doi), preprint_doi != published_doi) |>
  distinct(preprint_doi, .keep_all = TRUE) |>
  distinct(published_doi, .keep_all = TRUE)
message(sprintf("  %d preprints paired with a published version", nrow(pairs)))

entries <- pubs |>
  left_join(pairs |> rename(doi = published_doi), by = "doi", na_matches = "never") |>
  filter(!doi %in% pairs$preprint_doi)

# Carry the preprint's year so the card can show the progression.
entries <- entries |>
  left_join(pubs |> transmute(preprint_doi = doi, preprint_year = year, preprint_server = journal),
            by = "preprint_doi", na_matches = "never")

# ---- 7. Tier and output -----------------------------------------------------

ov <- overrides |> select(doi, tier, page, note, pdf_url) |>
  mutate(across(c(tier, page, note, pdf_url), ~ na_if(.x, "")))
entries <- entries |>
  left_join(ov, by = "doi", na_matches = "never") |>
  left_join(ov |> rename(preprint_doi = doi, tier_pp = tier, page_pp = page,
                         note_pp = note, pdf_url_pp = pdf_url),
            by = "preprint_doi", na_matches = "never") |>
  mutate(tier = coalesce(tier, tier_pp, "line"),
         page = coalesce(page, page_pp, ""),
         note = coalesce(note, note_pp, ""),
         pdf_url = coalesce(pdf_url, pdf_url_pp))

# A page tier needs a page file. Warn rather than stop so the CSV still builds.
missing_pages <- entries |> filter(tier == "page", page == "" | !file.exists(here(page)))
if (nrow(missing_pages)) {
  message("  Tier 'page' rows whose page file is blank or missing:")
  walk2(missing_pages$doi, missing_pages$page, ~ message("    - ", .x, "  page=", .y))
}

out <- entries |>
  transmute(
    id       = doi_slug(doi),
    doi,
    kind,
    tier,
    page,
    note,
    title,
    authors,
    journal,
    year,
    url      = doi_url(doi),
    preprint_doi,
    preprint_url = doi_url(preprint_doi),
    preprint_year,
    preprint_server,
    licence,
    pdf_link,
    pdf_url,      # publisher-hosted open PDF set by hand in overrides; used when no local file
    orcid_type,
    orcid_source,
    put_code
  ) |>
  arrange(desc(year), title)

if (file.exists(out_path)) {
  prev <- read_csv(out_path, show_col_types = FALSE, col_types = cols(.default = col_character()))
  new_ids  <- setdiff(out$id, prev$id)
  gone_ids <- setdiff(prev$id, out$id)
  if (length(new_ids))  message(sprintf("  %d new entries since last run", length(new_ids)))
  if (length(gone_ids)) message(sprintf("  %d entries no longer present: %s", length(gone_ids), paste(gone_ids, collapse = ", ")))
}

write_csv(out, out_path, na = "")
message(sprintf("Wrote %d entries to %s", nrow(out), out_path))

# Unpaired preprints whose title resembles an article's. These are candidates
# only; confirm a pair by putting the preprint DOI in the article's
# preprint_doi column in the overrides file.
title_words <- function(x) unique(str_split(norm_title(x), " ")[[1]])
unpaired <- out |> filter(kind == "preprint", !is.na(title))
articles <- out |> filter(kind == "article", !is.na(title))
if (nrow(unpaired) && nrow(articles)) {
  suggestions <- map_dfr(seq_len(nrow(unpaired)), function(i) {
    w <- title_words(unpaired$title[i])
    sim <- vapply(articles$title, function(t) {
      v <- title_words(t)
      length(intersect(w, v)) / length(union(w, v))
    }, numeric(1))
    k <- which.max(sim)
    tibble(preprint_doi = unpaired$doi[i], published_doi = articles$doi[k],
           similarity = round(sim[k], 2), preprint_title = str_trunc(unpaired$title[i], 45))
  }) |>
    filter(similarity >= 0.4) |>
    arrange(desc(similarity))
  if (nrow(suggestions)) {
    message(sprintf("  %d unpaired preprints resemble an article title. Confirm by adding preprint_doi to the article's overrides row:", nrow(suggestions)))
    print(suggestions, n = Inf, width = 200)
  }
}
print(count(out, kind, tier) |> arrange(kind, tier), n = Inf)
