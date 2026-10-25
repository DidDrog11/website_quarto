# Split the four-panel host-status figure (manuscript Figure 2) into one PNG
# per panel for the INEEID talk, so the panels can be laid out to fill a
# slide. Finds the white gutters between panels, then trims each panel's
# white margin.
#
# Run from the repository root:
#   Rscript scripts/split_talk_figure.R

if (!require("pacman")) install.packages("pacman")
pacman::p_load(png, here)

fig_dir <- here("presentations", "2026-ineeid-arha", "figures")
src     <- file.path(fig_dir, "arha_fig2_host_status.png")
img     <- readPNG(src)

# A pixel is white when all three channels are near 1.
ink <- apply(img, c(1, 2), function(p) any(p < 0.97))

# The gutter is the longest run of blank rows (or columns) near the middle.
gutter <- function(blank) {
  n <- length(blank); mid <- round(n * 0.3):round(n * 0.7)
  runs <- rle(blank[mid]); ends <- cumsum(runs$lengths)
  i <- which.max(ifelse(runs$values, runs$lengths, 0))
  mid[1] - 1 + ends[i] - runs$lengths[i] %/% 2
}
split_row <- gutter(rowSums(ink) == 0)
split_col <- gutter(colSums(ink) == 0)

trim <- function(x) {
  k <- apply(x, c(1, 2), function(p) any(p < 0.97))
  r <- range(which(rowSums(k) > 0)); c <- range(which(colSums(k) > 0))
  pad <- 20
  x[max(1, r[1] - pad):min(nrow(x), r[2] + pad), max(1, c[1] - pad):min(ncol(x), c[2] + pad), , drop = FALSE]
}

panels <- list(
  a = img[1:split_row, 1:split_col, ],
  b = img[1:split_row, (split_col + 1):ncol(img), ],
  c = img[(split_row + 1):nrow(img), 1:split_col, ],
  d = img[(split_row + 1):nrow(img), (split_col + 1):ncol(img), ])

for (p in names(panels)) {
  out <- trim(panels[[p]])
  writePNG(out, file.path(fig_dir, paste0("arha_fig2_panel_", p, ".png")))
  message(sprintf("panel %s: %d x %d", p, ncol(out), nrow(out)))
}
