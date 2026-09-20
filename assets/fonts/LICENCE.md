# Fonts

Both families are licensed under the SIL Open Font License 1.1, which permits
self-hosting and redistribution with the licence notice. Neither is served from
Google Fonts, so no request leaves the visitor's browser to a third party.

| Family | Role | Source | Licence |
| --- | --- | --- | --- |
| Newsreader | Display serif: headings, species names, the hero | [Production Type](https://github.com/productiontype/Newsreader) | SIL OFL 1.1 |
| Public Sans | Body sans | [USWDS](https://github.com/uswds/public-sans) | SIL OFL 1.1 |

Files are the variable-weight Latin subsets from the Fontsource distribution,
fetched from jsDelivr:

```
https://cdn.jsdelivr.net/npm/@fontsource-variable/newsreader@5/files/newsreader-latin-wght-normal.woff2
https://cdn.jsdelivr.net/npm/@fontsource-variable/newsreader@5/files/newsreader-latin-wght-italic.woff2
https://cdn.jsdelivr.net/npm/@fontsource-variable/public-sans@5/files/public-sans-latin-wght-normal.woff2
https://cdn.jsdelivr.net/npm/@fontsource-variable/public-sans@5/files/public-sans-latin-wght-italic.woff2
```

About 178 KB in total. One variable file per family and style covers every
weight, so adding a weight costs nothing. The `@font-face` rules are in
`scss/custom.scss` and reference the files by root-absolute URL; `resources:`
in `_quarto.yml` is what copies them into `_site/`.
