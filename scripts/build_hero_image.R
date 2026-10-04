# Build the landing-page hero landscape from the source illustration.
#
# Source: images/source/hero_landscape_gemini.jpg, generated with Gemini from a
# prompt written for this site (side-on cross-section, five zones, one ground
# line, flat pale background). This script crops it to the drawing, makes the
# flat background transparent so the image sits on the site's own ground in
# light and dark mode, halves the resolution, and writes a WebP.
#
# The axis, species and labels are drawn in SVG in _hero.qmd, not in the image.
# If the source is replaced, re-run this script and update the placement
# numbers it prints (image height, ground line) in _hero.qmd.
#
# Run from the repository root:
#   Rscript scripts/build_hero_image.R

if (!require("pacman")) install.packages("pacman")
pacman::p_load(jpeg, webp, here)

src <- here("images", "source", "hero_landscape_gemini.jpg")
out <- here("images", "hero_landscape.webp")

img <- readJPEG(src)

# ---- Crop to the drawing ------------------------------------------------------
# Rows measured on the source: the first drawn pixel is row 85 and the ground
# line is rows 891 to 894. Keep a little air above and stop just below the line.
top <- 61
bottom <- 896
img <- img[top:bottom, , ]

# ---- Key out the background ---------------------------------------------------
# The background is a flat peach. Distance from it sets transparency, with a
# soft ramp so edges stay smooth. Edge pixels are then decontaminated: the
# peach that was mixed into them is subtracted, so no light halo shows on a
# dark page.
background <- c(0xFD, 0xEB, 0xE1) / 255
distance <- sqrt((img[, , 1] - background[1])^2 + (img[, , 2] - background[2])^2 + (img[, , 3] - background[3])^2)
inner <- 0.035   # at or below this distance, fully transparent
outer <- 0.14    # at or above this distance, fully opaque
alpha <- pmin(pmax((distance - inner) / (outer - inner), 0), 1)

for (k in 1:3) {
  channel <- img[, , k]
  unmixed <- (channel - (1 - alpha) * background[k]) / pmax(alpha, 1e-3)
  img[, , k] <- ifelse(alpha > 0, pmin(pmax(unmixed, 0), 1), 0)
}

# ---- Halve the resolution -----------------------------------------------------
# A 2 x 2 average on premultiplied colour, so transparent pixels do not darken
# the edges they border. The result is about 1.4 times the hero's display width.
halve <- function(m) {
  r <- seq(1, nrow(m) - 1, by = 2); c <- seq(1, ncol(m) - 1, by = 2)
  (m[r, c] + m[r + 1, c] + m[r, c + 1] + m[r + 1, c + 1]) / 4
}
alpha_small <- halve(alpha)
rgba <- array(0, dim = c(length(seq(1, nrow(alpha) - 1, by = 2)), length(seq(1, ncol(alpha) - 1, by = 2)), 4))
for (k in 1:3) rgba[, , k] <- halve(img[, , k] * alpha) / pmax(alpha_small, 1e-3)
rgba[, , 4] <- alpha_small
rgba[, , 1:3][rgba[, , 1:3] > 1] <- 1

write_webp(rgba, out, quality = 82)

# ---- Report placement numbers for _hero.qmd -----------------------------------
ground_row_out <- (892 - top + 1) / 2
cat(sprintf("Wrote %s: %d x %d px, %.0f KB\n", basename(out), ncol(rgba[, , 1]), nrow(rgba[, , 1]), file.size(out) / 1024))
cat(sprintf("Ground line at output row %.1f of %d (%.3f of the height)\n", ground_row_out, nrow(rgba[, , 1]), ground_row_out / nrow(rgba[, , 1])))
