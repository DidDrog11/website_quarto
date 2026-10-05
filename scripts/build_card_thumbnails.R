# Build the small images on the programme cards (landing page and research
# overview) into images/cards/. Each card shows its programme's strongest
# figure or photograph, scaled down so the landing page stays light.
#
# Re-run when a source image changes. Sources are files already in the repo.
#
# Run from the repository root:
#   Rscript scripts/build_card_thumbnails.R

if (!require("pacman")) install.packages("pacman")
pacman::p_load(png, jpeg, webp, rsvg, here)

out_dir <- here("images", "cards")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
target_width <- 640

sources <- c(
  `sierra-leone`           = "lassa/images/rodent_trapping.jpg",
  scapes                   = "lassa/files/scapes/scapes_training.webp",
  `bank-voles`             = "rodent_ecology/figures/microbial_sensor.png",
  surveillance             = "policy/images/india_casestudy.png",
  vaccines                 = "policy/images/ohtd_graphic.svg",
  `rodent-disease-ecology` = "rodent_ecology/figures/lancet_preview.jpg",
  arha                     = "arha/figures/fig3_community_host_probability.webp",
  `lassa-cities`           = "lassa/images/urban_shield_decoupling.png",
  `lassa-epidemiology`     = "_freeze/research/lassa-epidemiology/figure-html/lassa-cases-1.png",
  tools                    = "open_science/images/map_liberator_app.webp",
  consultancy              = "images/hero_landscape.webp",
  `earlier-work`           = "others/images/asf_map.png"
)

# Read any of the source formats as an RGB array in [0, 1], flattening any
# transparency onto white.
read_rgb <- function(f) {
  ext <- tolower(tools::file_ext(f))
  a <- switch(ext,
    png  = png::readPNG(f),
    jpg  = , jpeg = jpeg::readJPEG(f),
    webp = webp::read_webp(f),
    svg  = aperm(rsvg::rsvg(f, width = target_width), c(1, 2, 3)),
    stop("Unsupported image: ", f))
  if (length(dim(a)) == 2) a <- array(a, c(dim(a), 3))
  if (dim(a)[3] == 4) {
    alpha <- a[, , 4]
    a <- a[, , 1:3]
    for (k in 1:3) a[, , k] <- a[, , k] * alpha + (1 - alpha)
  }
  a[, , 1:3]
}

# Downscale by averaging k x k blocks, with k chosen so the result is close to
# the target width. Good enough for thumbnails and needs no extra packages.
shrink <- function(a) {
  k <- max(1, round(dim(a)[2] / target_width))
  if (k == 1) return(a)
  h <- dim(a)[1] %/% k
  w <- dim(a)[2] %/% k
  out <- array(0, c(h, w, 3))
  for (i in seq_len(k)) for (j in seq_len(k)) {
    out <- out + a[seq(i, by = k, length.out = h), seq(j, by = k, length.out = w), ]
  }
  out / k^2
}

for (name in names(sources)) {
  src <- here(sources[[name]])
  if (!file.exists(src)) stop("Missing source for ", name, ": ", sources[[name]])
  thumb <- shrink(read_rgb(src))
  dest <- file.path(out_dir, paste0(name, ".webp"))
  webp::write_webp(thumb, dest, quality = 78)
  message(sprintf("%-24s %4d x %-4d %4.0f KB", name, dim(thumb)[2], dim(thumb)[1], file.size(dest) / 1024))
}
