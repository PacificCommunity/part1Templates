# part1Templates

Report templates for the WCPFC SciData Part 1 report, built on `tufman2R`.

## Install

```r
devtools::install_github("PacificCommunity/part1Templates")
# or, testing a local copy:
devtools::install("path/to/part1Templates")
```

Make sure `tufman2R` and `TUF_USER` / `TUF_PASSWORD` / `TUF_COUNTRY` are
already set up (per `tufman2R`'s own README) — this package depends on
it for the actual data download.

## Create a new report for the year

```r
library(part1Templates)
new_scidata_report(report_year = 2027)
```

This creates a project folder (e.g. `vu_2027_scidata`, using your own
`TUF_COUNTRY`) with everything copied in and `report_year` already
baked into `part1-report.qmd`'s default params. Then:

```r
setwd("vu_2027_scidata")
quarto::quarto_render("part1-report.qmd")
```

### Special cases: Niue, Tokelau (and soon Wallis & Futuna)

Niue and Tokelau don't have a licensed foreign-flagged fleet, so their
Part 1 report skips the flag-state reporting section entirely — it's
a different report structure, not just different data. `new_scidata_report()`
picks the right skeleton automatically based on country code, so you
don't need a separate function:

```r
new_scidata_report(report_year = 2027, country_code = "nu")
new_scidata_report(report_year = 2027, country_code = "tk")
```

If you're running this as the country's own user (`TUF_COUNTRY` set to
`NU` or `TK`), you can omit `country_code` — it's read from
`TUF_COUNTRY` the same way `report_year` is.

Wallis & Futuna will work the same way once its skeleton is added —
see `R/special_cases.R`, which is the single place that list of
special-case codes lives.

## Rendering multiple countries at once (admin)

```r
render_all_countries(
  path = "vu_2027_scidata",
  countries = c("FJ", "TO", "WS"),
  report_year = 2027,
  refresh_data = FALSE   # TRUE re-downloads every country's data fresh
)
```

Special-case countries (Niue, Tokelau) can't be included in the same
batch as standard countries — their report body is different, so
they need their own project folder and their own `render_all_countries()`
call:

```r
render_all_countries(path = "nu_2027_scidata", countries = "nu", report_year = 2027)
```

To render every standard member in one go:

```r
standard_countries <- setdiff(get_all_country_codes(), get_special_case_country_codes())
render_all_countries("vu_2027_scidata", standard_countries, 2027)
```

`refresh_data = FALSE` (default) reuses cached CSVs under
`data/scidata_<year>_<country>/` if present; `TRUE` re-downloads
everything fresh regardless of what's cached.

## Optional: Ikasavea credentials

Ikasavea report downloads are optional — skip this if you don't use
Ikasavea. If you do, set `IKA_USER_NAME` and `IKA_PASSWORD` the same
way as the `TUF_*` variables:

```r
usethis::edit_r_environ()
```

Then add two lines to the file that opens:

```
IKA_USER_NAME=your_username
IKA_PASSWORD=your_password
```

Save and restart R for the new variables to take effect.

## Files to update every report cycle

| File | What to update |
|---|---|
| `inst/extdata/sc_session.yml` | SC session number, CCM number, meeting location, session dates — one edit here updates the cover page for every report (standard, NU, TK). |
| `R/scidata_bundle.yaml` (inside each skeleton) | The list of Tufman2 `user_report_id`s, if the set of reports needed changes. **Each skeleton (`scidata-report`, `scidata-report-nu`, `scidata-report-tk`) has its own copy** — NU/TK deliberately pull fewer reports (no flag-state ones), so check whether a change applies to one, some, or all three. |
| `sections/*.qmd` (inside each skeleton) | The actual yearly narrative content (Abstract, Background, Addendum, etc.) that gets filled in per country once the folder is created. |

`R/config.yaml`'s country names, species labels, and gear codes rarely
change — only touch it if WCPFC membership or reportable species
change.
