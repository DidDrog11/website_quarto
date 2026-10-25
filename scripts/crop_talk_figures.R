# Crop manuscript figures for the INEEID talk (presentations/2026-ineeid-arha/)
# so they can be laid out to fill a slide.
#
# - Figure 1 (surveillance residuals): world map, colour key and the three
#   regional insets as separate PNGs. The figure's own title is dropped; the
#   slide title replaces it.
# - Figure 3 (community host probability): title dropped, white margins
#   trimmed.
#
# Each box is a rough crop in source pixels (x from, x to, y from, y to); the
# white margin inside it is then trimmed, so the boxes need not be exact. If a
# source figure changes, check the boxes against it.
#
# Run from the repository root:
#   Rscript scripts/crop_talk_figures.R

if (!require("pacman")) install.packages("pacman")
pacman::p_load(png, here)

fig_dir <- here("presentations", "2026-ineeid-arha", "figures")

# Trim white margins, keeping a small pad.
trim <- function(x, pad = 16) {
  ink <- apply(x, c(1, 2), function(p) any(p[1:3] < 0.97))
  r <- range(which(rowSums(ink) > 0)); c <- range(which(colSums(ink) > 0))
  x[max(1, r[1] - pad):min(nrow(x), r[2] + pad), max(1, c[1] - pad):min(ncol(x), c[2] + pad), , drop = FALSE]
}

crop <- function(img, box) img[box[3]:box[4], box[1]:box[2], , drop = FALSE]

# ---- Figure 1: surveillance residuals (3600 x 3000) ------------------------

fig1 <- readPNG(file.path(fig_dir, "arha_fig1_surveillance_residuals.png"))
fig1_boxes <- list(
  map           = c(560, 3200, 230, 1640),
  legend        = c(1380, 2220, 1680, 1870),
  inset_west    = c(180, 850, 1900, 2990),
  inset_wafrica = c(860, 2385, 1900, 2990),
  inset_casia   = c(2390, 3420, 1900, 2990))

for (part in names(fig1_boxes)) {
  out <- trim(crop(fig1, fig1_boxes[[part]]))
  writePNG(out, file.path(fig_dir, paste0("arha_fig1_", part, ".png")))
  message(sprintf("fig1 %s: %d x %d", part, ncol(out), nrow(out)))
}

# ---- Figure 3: community host probability (3600 x 2400) --------------------

fig3 <- readPNG(file.path(fig_dir, "arha_fig3_community_host_probability.png"))
out <- trim(crop(fig3, c(1, ncol(fig3), 340, nrow(fig3))))
writePNG(out, file.path(fig_dir, "arha_fig3_cropped.png"))
message(sprintf("fig3: %d x %d", ncol(out), nrow(out)))

# ---- Lassa spillover paper, Figure 1a and 1b -------------------------------
# Simons 2026, Epidemiol Infect, 10.1017/S0950268826101824. No source image is
# in this repository, so page 4 of the published PDF is rendered at 300 dpi
# from papers/ (the local, gitignored archive). Panel letters are painted out;
# the panel titles stay. Boxes are in pixels of the 300 dpi page.

pdf_file <- here("papers", "2026_10_1017_s0950268826101824.pdf")
p4 <- pdftools::pdf_render_page(pdf_file, page = 4, dpi = 300, numeric = TRUE)[, , 1:3]
p4[270:315, 180:233, ] <- 1     # letter A
p4[270:315, 1245:1277, ] <- 1   # letter B
lassa_boxes <- list(
  a = c(180, 1250, 270, 1070),
  b = c(1250, 2330, 270, 1070))

for (part in names(lassa_boxes)) {
  out <- trim(crop(p4, lassa_boxes[[part]]))
  writePNG(out, file.path(fig_dir, paste0("lassa_fig1_", part, ".png")))
  message(sprintf("lassa fig1 %s: %d x %d", part, ncol(out), nrow(out)))
}

# ---- Reassortment preprint, Figure 2b ---------------------------------------
# Rivero et al. 2026, bioRxiv, 10.64898/2026.06.10.731004 (CC BY 4.0). Page 25
# of the preprint PDF from papers/, rendered at 300 dpi. The box drops the
# panel letter above and panel d's legend below.

rr_file <- here("papers", "2026_10_64898_2026_06_10_731004_preprint.pdf")
p25 <- pdftools::pdf_render_page(rr_file, page = 25, dpi = 300, numeric = TRUE)[, , 1:3]
out <- trim(crop(p25, c(590, 2080, 1080, 2285)))
writePNG(out, file.path(fig_dir, "rivero_fig2b.png"))
message(sprintf("rivero fig2b: %d x %d", ncol(out), nrow(out)))
