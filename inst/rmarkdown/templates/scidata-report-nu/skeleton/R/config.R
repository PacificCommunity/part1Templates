#' Generate the country-specific LaTeX cover page preamble
#'
#' Downloads the country's flag, substitutes placeholders into the
#' preamble template, and writes the result to a **fixed filename**
#' (`preamble_active.tex`) rather than one that varies per country.
#' This matters: `part1-report.qmd`'s `include-in-header` list
#' references that fixed path directly in its YAML, so it needs to
#' exist under a name that doesn't change — this function runs (via
#' `data_prep.R`) early in the render, before the final PDF compile
#' step reads that file.
#'
#' @param country_code Two-letter country code.
#' @param report_year The report year.
#' @param sc_session,ccm_num,report_date,location,session_dates Cover
#'   page text fields — adjust per report cycle.
#' @param output_dir Directory to write the preamble and flag image to.
#' @param template_path Path to the preamble LaTeX template.
#' @return Invisibly, the path to the generated preamble file.
#' @export
generate_preamble <- function(country_code,
                              report_year,
                              sc_session    = NULL,
                              ccm_num       = NULL,
                              report_date   = format(Sys.Date(), "%d %B %Y"),
                              location      = NULL,
                              session_dates = NULL,
                              output_dir    = "R/preambles",
                              template_path = "R/preambles/preamble_template.tex") {
  
  # Fill any un-supplied session fields from the package's shared config.
  # Keeps a single source of truth (sc_session.yml) while still letting
  # callers override individual fields for testing.
  if (is.null(sc_session) || is.null(ccm_num) || is.null(location) || is.null(session_dates)) {
    session_meta <- yaml::read_yaml(
      system.file("extdata", "sc_session.yml", package = "part1Templates")
    )
    sc_session    <- sc_session    %||% session_meta$sc_session
    ccm_num       <- ccm_num       %||% session_meta$ccm_num
    location      <- location      %||% session_meta$location
    session_dates <- session_dates %||% session_meta$session_dates
  }
  
  country_names <- c(
    fj = "FIJI",
    fm = "FEDERATED STATES OF MICRONESIA",
    gu = "GUAM",
    ki = "KIRIBATI",
    mh = "MARSHALL ISLANDS",
    mp = "NORTHERN MARIANA IS.",
    nc = "NEW CALEDONIA",
    nr = "NAOERO",
    nu = "NIUE",
    pf = "FRENCH POLYNESIA",
    pg = "PAPUA NEW GUINEA",
    pn = "PITCAIRN",
    pw = "PALAU",
    sb = "SOLOMON ISLANDS",
    tk = "TOKELAU",
    to = "TONGA",
    tv = "TUVALU",
    vu = "VANUATU",
    wf = "WALLIS AND FUTUNA ISLANDS",
    ws = "SAMOA",
    ck = "COOK ISLANDS"
  )
  
  country_cd   <- tolower(country_code)
  country_name <- country_names[[country_cd]]
  
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  flag_path <- file.path(output_dir, paste0("flag_", country_cd, ".png"))
  tryCatch(
    utils::download.file(
      paste0("https://flagcdn.com/w320/", country_cd, ".png"),
      flag_path, mode = "wb", quiet = TRUE
    ),
    error = function(e) {
      cli::cli_warn("Could not download flag for {.val {country_cd}}: {conditionMessage(e)}")
    }
  )
  
  preamble <- readLines(template_path) |>
    paste(collapse = "\n") |>
    gsub("__REPORT_YEAR__",   report_year,    x = _, fixed = TRUE) |>
    gsub("__SC_SESSION__",    sc_session,     x = _, fixed = TRUE) |>
    gsub("__CCM_NUM__",       ccm_num,        x = _, fixed = TRUE) |>
    gsub("__REPORT_DATE__",   report_date,    x = _, fixed = TRUE) |>
    gsub("__COUNTRY_NAME__",  country_name,   x = _, fixed = TRUE) |>
    gsub("__FLAG_PATH__",     flag_path,      x = _, fixed = TRUE) |>
    gsub("__LOCATION__",      location,       x = _, fixed = TRUE) |>
    gsub("__SESSION_DATES__", session_dates,  x = _, fixed = TRUE)
  
  # Fixed filename - overwritten every render, not named per country -
  # so part1-report.qmd's YAML can reference it as a static path.
  preamble_path <- file.path(output_dir, "preamble_active.tex")
  writeLines(c(preamble, ""), preamble_path)
  
  invisible(preamble_path)
}
