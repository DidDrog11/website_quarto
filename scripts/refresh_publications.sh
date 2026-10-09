#!/bin/sh
# Re-render every page that reads data/publications.csv, after running
#   Rscript scripts/update_publications.R
#
# Quarto's freeze re-runs a page's R code only when the page's own .qmd
# changes, so a plain `quarto render` would keep the old lists. Rendering a
# page on its own always re-runs it. The CV is rebuilt too.
#
# Run from the repository root (Git Bash):
#   sh scripts/refresh_publications.sh
# Then commit data/publications.csv, _freeze/ and cv.pdf, and push main.
set -e

quarto render publications.qmd

# Every programme page that lists papers.
for page in $(grep -l "paper_cards.R" research/*.qmd); do
  quarto render "$page"
done

quarto render cv.qmd

# New papers need a topic row for the publications filter; untagged ones show
# as Other until they get one.
echo
echo "Done. Check the output above for 'Papers without topic tags', and add a row"
echo "for each to data/publication_topics.csv, then run this script again."
git status --short data/ _freeze/ cv.pdf
