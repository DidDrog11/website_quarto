# Vendored JavaScript

Self-hosted so that no page makes a request to a third-party CDN.

| File | Library | Licence | Source |
| --- | --- | --- | --- |
| `d3.v7.9.0.min.js` | d3 7.9.0 | ISC | `https://cdn.jsdelivr.net/npm/d3@7.9.0/dist/d3.min.js`, fetched 2026-09-27 |

Used by the ArHa sampling globe (`_globe.qmd`). Only d3-geo, d3-drag, d3-selection and d3-format are needed; the full bundle (273 KB, about 90 KB gzipped) is kept for simplicity. A custom build would cut it to roughly a third.

The globe's data files in `assets/data/` are build artefacts of `scripts/build_globe_data.R`. `land_110m.json` is Natural Earth 110m land (public domain), v5.1.2. `arha_host_hex.json` is derived from the Project ArHa host occurrence release.

| File | Library | Licence | Source |
| --- | --- | --- | --- |
| `goatcounter-count.js` | GoatCounter count.js | ISC | `https://gc.zgo.at/count.js`, fetched 2026-10-05 |

Cookieless page-view and country counting, included on every page through `assets/goatcounter.html`. Counts go to `dsimons.goatcounter.com`. Re-fetch occasionally; a self-hosted copy does not update itself.
