# QR codes for the INEEID talk (presentations/2026-ineeid-arha/).
# Each links to a DOI resolver URL and is written as SVG beside the deck.
#
# Run from the repository root:
#   Rscript scripts/build_talk_qr.R

if (!require("pacman")) install.packages("pacman")
pacman::p_load(qrcode, here)

out_dir <- here("presentations", "2026-ineeid-arha", "figures")

qr_links <- c(qr_protocol = "https://doi.org/10.12688/wellcomeopenres.24037.2",
              qr_macroecology = "https://doi.org/10.64898/2026.06.03.729761")

for (name in names(qr_links)) {
  code <- qr_code(qr_links[[name]], ecl = "M")
  svg_file <- here(out_dir, paste0(name, ".svg"))
  generate_svg(code, svg_file, size = 300, foreground = "#1e2420", background = "#ffffff", show = FALSE)
  # Crisp edges, or anti-aliasing leaves hairline gaps between the modules.
  svg <- readLines(svg_file)
  svg <- sub("^<svg ", "<svg shape-rendering=\"crispEdges\" ", svg)
  writeLines(svg, svg_file)
}
