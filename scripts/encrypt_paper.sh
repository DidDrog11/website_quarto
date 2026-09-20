#!/usr/bin/env bash
# Encrypt a paywalled PDF so it can sit in the public repository and be
# decrypted only by the Netlify function netlify/functions/request-paper.mjs.
#
# Usage (from the repository root, in Git Bash):
#   PAPER_KEY=<64 hex chars> scripts/encrypt_paper.sh path/to/accepted_manuscript.pdf <slug>
#
# First time: run without PAPER_KEY set. A key is generated and printed. Add it
# to Netlify (Site configuration > Environment variables > PAPER_KEY) and keep
# it somewhere safe outside the repo. Reuse the same key for every paper.
#
# Output: private/<slug>.pdf.enc and a line to append to data/private_papers.csv.
# The IV is per file and not secret; it goes in the CSV.

set -euo pipefail

in="${1:?path to the PDF}"
slug="${2:?short slug for the file name, e.g. lassa_urbanisation}"
[ -f "$in" ] || { echo "No such file: $in" >&2; exit 1; }

mkdir -p private

if [ -z "${PAPER_KEY:-}" ]; then
  PAPER_KEY=$(openssl rand -hex 32)
  echo "No PAPER_KEY in the environment. Generated a new one."
  echo "Add this to Netlify as PAPER_KEY and store it safely; it is not saved anywhere by this script:"
  echo
  echo "  PAPER_KEY=$PAPER_KEY"
  echo
fi

iv=$(openssl rand -hex 16)
out="private/${slug}.pdf.enc"
openssl enc -aes-256-cbc -K "$PAPER_KEY" -iv "$iv" -in "$in" -out "$out"

echo "Wrote $out ($(wc -c < "$out") bytes)."
echo
echo "Append this row to data/private_papers.csv, filling in the DOI and title:"
echo
echo "  10.xxxx/yyyy,\"Paper title\",$out,$iv,$(basename "$in")"
echo
echo "Commit $out and the CSV. Never commit the plain PDF; private/*.pdf is gitignored."
