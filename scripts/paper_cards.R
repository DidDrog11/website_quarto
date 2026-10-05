# Shared rendering of publication entries for the site.
#
# Sourced by publications.qmd (the numbered list) and by each programme page
# under research/ (its own papers, summaries folded). Reads data/publications.csv,
# data/pdfs.csv and the explainer notes in data/paper_notes/, and writes
# Pandoc markdown with cat(), so call it from a chunk with `results: asis`.
#
# Freeze caveat: Quarto's freeze hashes the .qmd, not this file. After editing
# this script, re-render publications.qmd and the programme pages by hand.

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, readr, stringr, purrr, here, yaml)

# ---- Data ---------------------------------------------------------------------

pubs <- read_csv(here("data", "publications.csv"), col_types = cols(.default = col_character()), show_col_types = FALSE)

pdf_log_path <- here("data", "pdfs.csv")
pdfs <- if (file.exists(pdf_log_path)) {
  read_csv(pdf_log_path, col_types = cols(.default = col_character()), show_col_types = FALSE)
} else {
  tibble(id = character(), which = character(), source = character(), url = character())
}

notes_dir <- here("data", "paper_notes")

# Extra pages a card appears on, beyond its home `page`. Read straight from the
# overrides file, so a change needs no re-run of update_publications.R.
# `also_on` holds page paths separated by semicolons.
also_on <- read_csv(here("data", "publications_overrides.csv"), col_types = cols(.default = col_character()),
                    show_col_types = FALSE)
also_on <- if ("also_on" %in% names(also_on)) {
  also_on |> filter(!is.na(also_on), also_on != "") |> transmute(doi = str_to_lower(doi), also_on)
} else {
  tibble(doi = character(), also_on = character())
}

# ---- Helpers ------------------------------------------------------------------

owner_pattern <- "\\bSimons D\\b"

# Long author lists are cut to six, keeping the owner visible, then "et al.".
format_authors <- function(a, max_show = 8) {
  if (is.na(a) || a == "") return("")
  parts <- str_split(a, ",\\s*")[[1]]
  if (length(parts) > max_show) {
    keep <- seq_len(6)
    me <- which(str_detect(parts, owner_pattern))
    shown <- parts[keep]
    if (length(me) && !me[1] %in% keep) shown <- c(shown, "…", parts[me[1]])
    parts <- c(shown, "et al.")
  }
  str_replace_all(paste(parts, collapse = ", "), owner_pattern, "**Simons D**")
}

# A note is data/paper_notes/<id>.md with YAML front matter (figure, alt,
# optional caption) and a short body.
read_note <- function(id) {
  f <- file.path(notes_dir, paste0(id, ".md"))
  if (!file.exists(f)) return(NULL)
  txt <- readLines(f, warn = FALSE, encoding = "UTF-8")
  fm_end <- which(txt == "---")
  meta <- list(); body <- txt
  if (length(fm_end) >= 2 && fm_end[1] == 1) {
    meta <- yaml::yaml.load(paste(txt[2:(fm_end[2] - 1)], collapse = "\n"))
    body <- txt[(fm_end[2] + 1):length(txt)]
  }
  list(meta = meta, body = paste(body, collapse = "\n"))
}

# Open PDF link: a publisher link set in the overrides file, else the
# open-access URL the fetch script found. Hand-added files are not served.
pdf_link <- function(row) {
  if (!is.na(row$pdf_url) && row$pdf_url != "" && row$pdf_url != "none") return(sprintf("[PDF](%s)", row$pdf_url))
  hit <- pdfs |> filter(id == row$id, !is.na(url), url != "", source %in% c("unpaywall", "crossref", "biorxiv-api"))
  if (!nrow(hit)) return(NULL)
  label <- if (hit$which[1] == "preprint") "PDF (preprint)" else "PDF"
  sprintf("[%s](%s)", label, hit$url[1])
}

# Preprint, then published version, then PDF, so the progression reads left to right.
venue_links <- function(row) {
  links <- character()
  has_pre <- !is.na(row$preprint_doi) && row$preprint_doi != ""
  if (row$kind == "preprint") {
    links <- c(links, sprintf("[Preprint, %s](%s)", row$year, row$url))
  } else {
    if (has_pre) {
      pre_year <- if (is.na(row$preprint_year)) "" else row$preprint_year
      links <- c(links, sprintf("[Preprint, %s](%s)", pre_year, row$preprint_url))
    }
    venue <- if (!is.na(row$journal) && row$journal != "") sprintf("*%s*, %s", row$journal, row$year) else sprintf("Published, %s", row$year)
    links <- c(links, sprintf("[%s](%s)", venue, row$url))
  }
  p <- pdf_link(row)
  if (!is.null(p)) links <- c(links, p)
  paste(links, collapse = " · ")
}

# The disclosure holding a note's figure and text. The citation and its links
# sit outside <summary>, so no link is nested inside the toggle.
render_note <- function(row, note, open = FALSE, read_more = TRUE) {
  fig <- note$meta$figure %||% NULL
  cat(sprintf('<details class="pub-more"%s>\n<summary>Summary</summary>\n\n', if (open) " open" else ""))
  cat("::: {.pub-more-body}\n")
  if (!is.null(fig)) {
    cap <- note$meta$caption %||% ""
    alt <- note$meta$alt %||% row$title
    cat(sprintf('![%s](%s){.pub-fig fig-alt="%s"}\n\n', cap, fig, gsub('"', "'", alt)))
  }
  cat(note$body, "\n\n")
  # Link from the publications list to the programme page; root-absolute so
  # it works from any folder.
  if (read_more && row$tier == "page" && nzchar(row$page %||% "")) {
    cat(sprintf("[Read more](/%s)\n\n", sub("\\.qmd$", ".html", row$page)))
  }
  cat(":::\n\n</details>\n\n")
}

# One entry. With a number (publications list) it gets the number column;
# without (programme pages) it is a plain single-column entry.
render_entry <- function(row, n = NULL, open = FALSE, read_more = TRUE) {
  note <- if (row$tier %in% c("card", "page")) read_note(row$id) else NULL
  if (is.null(n)) {
    cat(sprintf("::: {.pub-entry .pub-entry-plain #pub-%s}\n", row$id))
  } else {
    cat(sprintf("::: {.pub-entry #pub-%s}\n", row$id))
    cat(sprintf("::: {.pub-num}\n%d\n:::\n", n))
  }
  cat("::: {.pub-body}\n")
  cat(sprintf("**%s**  \n", row$title))
  auth <- format_authors(row$authors)
  if (auth != "") cat(auth, "  \n", sep = "")
  cat(venue_links(row), "\n\n")
  if (!is.null(note)) render_note(row, note, open = open, read_more = read_more)
  cat(":::\n:::\n\n")
}

# The papers assigned to one programme page in data/publications_overrides.csv,
# newest first, with summaries folded. `page` is the page's path from the
# project root, e.g. "research/arha.qmd".
render_programme_papers <- function(page) {
  # Papers whose home is this page, plus those listed for it in `also_on`.
  # A visiting card links back to its home page from its summary.
  visiting <- also_on |>
    filter(map_lgl(str_split(also_on, ";"), \(p) page %in% str_trim(p))) |>
    pull(doi)
  mine <- pubs |>
    filter(page == !!page | str_to_lower(doi) %in% visiting) |>
    mutate(year_n = suppressWarnings(as.integer(year)),
           home = page == !!page) |>
    arrange(desc(year_n), title)
  if (!nrow(mine)) {
    cat("::: {.callout-note appearance=\"minimal\"}\nNo papers are assigned to this page yet. Set its path in the `page` column of `data/publications_overrides.csv`.\n:::\n\n")
    return(invisible(NULL))
  }
  for (i in seq_len(nrow(mine))) render_entry(mine[i, ], open = FALSE, read_more = !mine$home[i])
}
