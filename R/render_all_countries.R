#' All WCPFC member/CCM two-letter country codes that this report Part1 applies to.
#'
#' Convenience list for use with [render_all_countries()] when you want
#' every member rendered, not a hand-picked subset. Includes Niue and
#' Tokelau (see `special_case_countries`) — but note that
#' [render_all_countries()] can't render them in the same batch as
#' standard countries; call it separately for those two (and once it
#' exists, Wallis & Futuna).
#'
#' @return A lowercase character vector of country codes.
#' @export
get_all_country_codes <- function() {
  tolower(c(
    "CK", "FJ", "FM", "KI", "MH", "NC", "NU", "TK",
    "PF", "PG", "PW", "SB", "TO", "TV", "VU", "WS"
  ))
}

#' Country codes that use a special-case Part 1 report skeleton
#'
#' Subset of [get_all_country_codes()] whose report structure differs
#' from the standard template (currently: no flag-state reporting
#' section). Use this to split a full-member render into two batches,
#' e.g. `setdiff(get_all_country_codes(), get_special_case_country_codes())`
#' for the standard batch.
#'
#' @return A lowercase character vector of country codes.
#' @export
get_special_case_country_codes <- function() {
  special_case_countries
}

#' Render the Part 1 report for multiple countries at once
#'
#' Intended for admin use: renders `part1-report.qmd` once per country
#' in `countries`, overriding params for each render rather than
#' requiring a separate project folder per country. A regular member
#' never needs this — it's for producing many countries' reports from
#' a single project folder in one pass.
#'
#' All `countries` passed in one call must match the report structure
#' already present at `path` — i.e. don't mix special-case countries
#' (Niue, Tokelau; see `special_case_countries`) with standard ones in
#' the same batch, since their skeletons use a different report body.
#' Render special-case countries from their own project folder,
#' created via `new_scidata_report(country_code = "nu")`, in a
#' separate call.
#'
#' @param path Path to the report project (containing `part1-report.qmd`).
#' @param countries Character vector of two-letter country codes. Use
#'   [get_all_country_codes()] to render every member.
#' @param report_year The report year to use for every render.
#' @param refresh_data Whether to re-download data for every country
#'   (`TRUE`), or reuse whatever's already cached under each country's
#'   `data/scidata_<year>_<country>/` folder (`FALSE`, default).
#' @return Invisibly, a named list of output filenames (or `NULL` for
#'   any country that failed), named by country code.
#' @export
render_all_countries <- function(path, countries, report_year, refresh_data = FALSE) {
  countries <- tolower(countries)

  is_special <- countries %in% special_case_countries
  if (any(is_special) && !all(is_special)) {
    cli::cli_abort(c(
      "Can't mix special-case countries with standard ones in the same batch.",
      "x" = "Special-case: {.val {countries[is_special]}}. Standard: {.val {countries[!is_special]}}.",
      "i" = "Special-case reports (Niue, Tokelau, and soon Wallis & Futuna) use a different report structure and need their own project folder — create one with {.code new_scidata_report(country_code = ...)} and render it separately."
    ))
  }

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(path)
  results <- lapply(countries, function(cc) {
    cli::cli_alert_info("Rendering report for {.val {cc}}...")
    output_file <- paste0("part1_report_", cc, ".pdf")
    tryCatch({
      quarto::quarto_render(
        "part1-report.qmd",
        output_file = output_file,
        execute_params = list(
          country_code = cc,
          report_year = report_year,
          refresh_data = refresh_data
        )
      )
      cli::cli_alert_success("Rendered {.val {cc}} -> {.file {output_file}}")
      output_file
    }, error = function(e) {
      cli::cli_alert_danger("Failed for {.val {cc}}: {conditionMessage(e)}")
      NULL
    })
  })
  names(results) <- countries
  invisible(results)
}
