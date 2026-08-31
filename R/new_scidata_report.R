#' Create a new SciData Part 1 report project
#'
#' Copies the report skeleton (report structure, section files, data
#' prep script, and configs) into a new project folder, and bakes the
#' given `report_year` into that copy's `part1-report.qmd`.
#'
#' `country_code` is used only to pick the right skeleton and to build
#' the default `path` — it is deliberately **not** baked into the
#' `.qmd` itself. The qmd's `params` default to the running user's own
#' `TUF_COUNTRY`, so the same project folder renders correctly for
#' whoever opens it, the same reasoning used for the bundle YAML files
#' in `tufman2R`.
#'
#' Niue (NU) and Tokelau (TK) use their own skeleton (no flag-state
#' reporting section) — this is handled automatically based on
#' `country_code`. See `special_case_countries` if a new one needs
#' adding (e.g. Wallis & Futuna).
#'
#' @param path Directory to create the report project in. Defaults to
#'   `"<country_code>_<report_year>_scidata"`.
#' @param report_year The report year to bake into `part1-report.qmd`'s
#'   default params.
#' @param country_code Two-letter country code, used to pick the
#'   matching skeleton and (if `path` isn't supplied) the default
#'   folder name. Defaults to `TUF_COUNTRY`.
#' @return Invisibly, the path to the created project directory.
#' @export
new_scidata_report <- function(path = NULL,
                                report_year = as.integer(format(Sys.Date(), "%Y")),
                                country_code = NULL) {
  if (is.null(country_code)) {
    country_code <- Sys.getenv("TUF_COUNTRY")
  }
  cc <- tolower(country_code)

  if (!nzchar(cc)) {
    cli::cli_abort(c(
      "No `country_code` supplied and {.envvar TUF_COUNTRY} isn't set.",
      "i" = "Either pass `country_code` explicitly, or set {.envvar TUF_COUNTRY} via {.code usethis::edit_r_environ()}."
    ))
  }

  if (is.null(path)) {
    path <- paste0(cc, "_", report_year, "_scidata")
  }

  if (dir.exists(path)) {
    cli::cli_abort("{.path {path}} already exists — choose a new path, or remove the existing folder first.")
  }

  template_name <- if (cc %in% special_case_countries) paste0("scidata-report-", cc) else "scidata-report"

  skeleton <- system.file("rmarkdown", "templates", template_name, "skeleton", package = "part1Templates")
  if (!nzchar(skeleton)) {
    cli::cli_abort("Could not find the {.val {template_name}} template — is part1Templates installed correctly?")
  }

  dir.create(path, recursive = TRUE)
  file.copy(
    list.files(skeleton, full.names = TRUE, all.files = FALSE, no.. = TRUE),
    path,
    recursive = TRUE
  )

  # Bake report_year into this copy's default params. country_code
  # stays as Sys.getenv("TUF_COUNTRY") in the file, unchanged, so it
  # resolves to whoever actually runs it.
  qmd_path <- file.path(path, "part1-report.qmd")
  lines <- readLines(qmd_path, warn = FALSE)
  lines <- sub("^(\\s*report_year:\\s*)[0-9]+\\s*$", paste0("\\1", report_year), lines)
  writeLines(lines, qmd_path)

  cli::cli_alert_success("Created {.path {path}} (using the {.val {template_name}} template)")
  cli::cli_alert_info("Next: setwd({.val {path}}); quarto::quarto_render(\"part1-report.qmd\")")

  invisible(path)
}
