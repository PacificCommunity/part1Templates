# part1Templates

Report templates for the WCPFC SciData Part 1 report, built on `tufman2R`.

# Installation

``` r
devtools::install_github("PacificCommunity/part1Templates")
# or, testing a local copy:
devtools::install("path/to/part1Templates")
```

After installation, run `usethis::edit_r_environ()` to make sure `TUF_USER` / `TUF_PASSWORD` / `TUF_COUNTRY` are set up (per [`tufman2R`'s](https://github.com/PacificCommunity/tufman2R) own README). If IKASAVEA is used by your CCM to record tuna catches and you want to include those catches in your Artisanal ACE table, set `IKA_USER_NAME` and `IKA_PASSWORD` the same way as the `TUF_*` variables:

```         
IKA_USER_NAME=your_username
IKA_PASSWORD=your_password
```

You can update your environment variables at any time using `usethis::edit_r_environ()`. You will have to save and restart R for the new variables to take effect.

# Setup

1.  (Optional) [Create a project](https://support.posit.co/hc/en-us/articles/200526207-Using-RStudio-Projects) called `part_1_reports`.

Open your project and run all the next steps inside it.

2.  Load the library, specify report year (in this example `2025`) and your country code (in this example the `TUF_COUNTRY` specified in the `.Renv` ).

``` r
library(part1Templates)

report_yr = 2025
country_cd = Sys.getenv("TUF_COUNTRY")
```

3.  Create a project folder for the report year and country specified in the step above.

```         
new_scidata_report(report_year = report_yr, country_code = country_cd)
```

This creates a project folder (e.g. `[country_cd]_[report_yr]_scidata`) and downloads all the data from required from Tufman 2 reports to your computer. You can check `R/scidata_bundle.yaml` if you want to know the list of Tufman 2 reports used for Part1.

4.  Set your working environment

Set the working environment to the folder you just created.

``` r
setwd(paste0(country_cd, "_", report_yr, "_scidata"))
```

5.  Generate your Part1 report

``` r
quarto::quarto_render(
  "part1-report.qmd",
  execute_params = list(refresh_data = TRUE)
)
```

The line above will re-download all Tufman 2 reports fresh to your computer as CSVs under data/scidata\_<year>\_<country>/ 
before running your report. If after running the first time you want to reuse the CSVs downloaded in previous run, just set
refresh_data to `FALSE`.

## Special cases: Niue, Tokelau (and soon Wallis & Futuna)

Niue and Tokelau don't have a licensed foreign-flagged fleet, so their Part 1 report skips the flag-state reporting section entirely, it's a different report structure, not just different data (see `R/special_cases.R`). `new_scidata_report()` picks the right skeleton automatically based on country code, so you don't need a separate function:

``` r
new_scidata_report(report_year = 2025, country_code = "nu")
new_scidata_report(report_year = 2025, country_code = "tk")
````

## Files to update every report cycle

-   `inst/extdata/sc_session.yml`: SC session number, CCM number, meeting location, session dates — one edit here updates the cover page for every report (standard, NU, TK). \|

-   `sections/*.qmd`: The actual yearly narrative content (Abstract, Background, Addendum, etc.) that gets filled in per country once the folder is created.

## Marking Addendum CMMs as not applicable

`sections/Addendum.qmd` walks through a fixed list of CMMs (2009-03, observer coverage, 2009-06, 2011-03, 2018-03, 2006-04, 2015-02, 2019-03, 2023-03, ...). For a given country, some of CMMS won't apply. Which ones are marked not-applicable is controlled per project folder, in `R/config.yaml`, under `addendum.not_applicable_measures`:

``` yaml
addendum:
  not_applicable_measures:
    "2009-03": "{{country_code}} flagged vessels do not target SWORDFISH nor fish south of 20°S."
    "2011-03": ""   # blank -> falls back to the generic "This measure does not apply to {country_name}."
```

-   Leave it as the default `not_applicable_measures`: `none` to treat every CMM as applicable (nothing suppressed).
-   CMM codes must be quoted.
-   A blank reason ("") falls back to a generic "This measure does not apply to {country_name}." sentence.
-   A CMM code left out of the map entirely is treated as applicable — you only need to list the ones you're suppressing.

This only applies to the standard skeleton. *Niue* and *Tokelau's* `Addendum.qmd` hardcodes a blanket "not applicable, no domestic commercial fishing industry" under every CMM directly in the qmd — there's nothing to configure there.

**One caveat:** a handful of CMM subsections in the standard skeleton's `Addendum.qmd` (the shark measures — 2013-08, 2012-04, 2011-04, 2010-07) are currently gated to Vanuatu only `(#| eval: !expr tolower(country_code) == "vu"`) rather than going through `config.yaml` like the rest of the section — for any other country they don't render at all, not even as "not applicable".

## Rendering multiple countries at once (admin only)

``` r
render_all_countries(
  path = "vu_2025_scidata",
  countries = c("FJ", "TO", "WS"),
  report_year = 2025,
  refresh_data = FALSE   # TRUE re-downloads every country's data fresh
)
```

Special-case countries (Niue, Tokelau) can't be included in the same batch as standard countries since their report body is different, so they need their own project folder and their own `render_all_countries()` call:

``` r
render_all_countries(path = "nu_2025_scidata", countries = "nu", report_year = 2025)
```
