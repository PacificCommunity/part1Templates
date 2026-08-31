library(tidyverse)
library(flextable)
library(tufman2R)
library(scales)
library(countrycode)
library(mregions2)
library(scatterpie)

# set theme for all plots
theme_set(
  theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.text     = element_text(size = 8),
      legend.title    = element_text(size = 9),
      legend.key.size = unit(0.4, "cm")
    )
)

flextable::set_flextable_defaults(font.family = "Times New Roman")

vars <- list(report_year = report_year, country_code = tolower(country_code))
config_yaml <- "R/config.yaml"

raw <- paste(readLines(config_yaml, warn = FALSE), collapse = "\n")
rendered <- glue::glue(raw, .envir = list2env(vars), .open = "{{", .close = "}}")
config <- yaml::yaml.load(as.character(rendered))

ps_species_labels <- unlist(config$species_labels$ps)
ll_species_labels <- unlist(config$species_labels$ll)
pl_species_labels <- unlist(config$species_labels$pl)
species_labels_all <- unlist(config$species_labels$all)
gear_codes <- unlist(config$gear_codes)
region_lookup <- unlist(config$region_lookup)
ps_species <- unlist(config$ps_species)
ll_species <- unlist(config$ll_species)
base_dir <- config$base_dir
fig_dir <- config$folders$figures
tbl_dir <- config$folders$tables
not_applicable_measures <- config$not_applicable_measures
country_name <- config$country_names[tolower(country_code)]
ref_data_folder <- config$folders$ref_data
ikasavea_folder <- config$folders$ikasavea
additional_folder <- config$folders$others
art_est_trips_folder <- config$folders$artisanal_est_trips

# create folders
for (folder_name in config$folders) {
  dir.create(folder_name, recursive = TRUE, showWarnings = FALSE)
}

# More config
local_knowl_trips_file <-  str_c(base_dir,  'additional_files/est_trips.csv')
yrs_long <- (report_year-4):report_year
yr_range <- paste(min(yrs_long),max(yrs_long), sep="-")

sp_colours <- setNames(object = c("seagreen3","red3","black","cyan1","steelblue","orange",
                                  "pink","wheat","purple3","grey40","dodgerblue4","purple4",
                                  "#66CCFF","salmon4","magenta3","gold", "grey80"),
                       nm = c("Albacore","Bigeye tuna","Black marlin","Blue marlin","Blue shark",
                              "Hammerhead sharks nei","Mako sharks","Oceanic whitetip shark",
                              "Pacific bluefin tuna","Silky shark","Skipjack tuna",
                              "Striped marlin","Swordfish","Thresher sharks nei",
                              "Whale shark","Yellowfin tuna", "Other"))

# read ref data
flag_lookup = read.csv(paste0(ref_data_folder, "/flag_names.csv")) |>
  select(country_code, country_name, country_short) |>
  mutate(country_name = na_if(country_name, "NULL"),
         country_short = na_if(country_short, "NULL")) |>
  # use short name when available, fall back to long
  mutate(name_pretty = coalesce(str_to_title(tolower(str_trim(country_short))), str_to_title(country_name)))

flag_labels <- flag_lookup |>
  mutate(flag_code_lower = tolower(country_code)) |>
  select(flag_code_lower, name_pretty) |>
  deframe()  # from tibble: turns 2-col df into a named vector


# Plot of vessel numbers for only Longline, Purse seine and Pole Line, aggregated over size class
vessel_colours <- c(
  "0 - 50"    = "#A1DAB4",
  "51 - 200"  = "#41B6C4",
  "201 - 500" = "#2C7FB8",
  "500+"      = "#253494"
)

# seabirds measure
area_defs_default <- c(
  south = "south_of_30s",
  mids  = "between_25s_30s",
  midn  = "between_25s_23n",
  north = "north_of_23n"
)

# 2018-03
default_area_prefixes <- c(south = "s30s",
                           mids = "b25s_30s",
                           midn = "b25s_23n",
                           north = "n23n")


## Functions: data availability ##########################################

# A single, safe "is there anything to render" check, used everywhere
# instead of the old scattered variants (`!("data_X" %in% names(all_data))
# | length(data) == 0`, etc.). Those patterns had two bugs: `|` doesn't
# short-circuit in R, so both sides always evaluate - if `data` is NULL
# (report never downloaded), `length(NULL) == 0` is fine, but
# `nrow(NULL) == 0` returns NULL, and `TRUE | NULL` is `logical(0)`,
# which makes `if(...)` crash instead of skipping. Also, `length()` on a
# data frame counts columns, not rows - a genuinely empty report with
# several columns would slip past a `length(data) == 0` check entirely.
# This checks NULL-ness first, then row count for data frames.
has_data <- function(data) {
  !is.null(data) && length(data) > 0 && (!is.data.frame(data) || nrow(data) > 0)
}

## Functions: data grooming #############################################

groom_data <- function(all_data, report_year) {
  if (has_data(all_data$data_3605)) {
    
    data_3605 <- all_data$data_3605 |>
      mutate(year = as.numeric(year)) |>
      filter(year <= report_year) |>
      filter(year >= (report_year - 4))
    
    # separate gears
    data_3605_ps <- data_3605 |>
      filter(gear_code == "s") |>
      filter(sp_code %in% ps_species) |>
      select(sp_code, year, catch)
    
    data_3605_ll <- data_3605 |>
      filter(gear_code == "l") |>
      filter(sp_code %in% ll_species) |>
      select(sp_code, year, catch)
    
    # Prepare vessel data once
    ves_raw <-
      data_3605 |>
      filter(year %in% yrs_long, gear_code %in% c("s", "l")) |>
      select(flag, year, gear_code, vessels_sml, vessels_med, vessels_lge, vessels_hge) |>
      distinct() |>
      mutate(gear = case_when(
        gear_code == "s" ~ "Purse seine",
        gear_code == "l" ~ "Longline"
      ))
    
    # Prepare vessel data once - addendum
    ves_calcs <-
      data_3605 |>
      filter(gear_code %in% c("L", "l")) |>
      select(flag, year, gear_code, vessels_sml, vessels_med, vessels_lge, vessels_hge) |>
      distinct() |>
      mutate(Vessels = rowSums(across(starts_with("vessels_")), na.rm = TRUE)) |>
      group_by(year, gear_code) |>
      summarise(Vessels = sum(Vessels), .groups = "drop") |>
      select(year, nb_vessel = Vessels)
    
    # Coastal dfs
    if (has_data(all_data$data_2904)) {
      data_2904 <- all_data$data_2904 |>
        rename(year = t2col0) |>
        filter(year == report_year) 
    }
    
    if (has_data(all_data$data_2900)) {
      data_2900 <- all_data$data_2900 |>
        mutate(year = as.numeric(substr(t2col1, 1, 4))) |>
        filter(year >= report_year - 4) |>
        filter(year <= report_year) |>
        select(-fish_days_aws_only, - t2col1)
    }
    
    if (has_data(all_data$data_2894)) {
      data_2894 <- all_data$data_2894 |>
        mutate(year = as.numeric(substr(t2col4, 1, 4))) |>
        filter(year >= report_year - 4) |>
        filter(year <= report_year) |>
        select(flag, year, vessels = vessel_name, trips, days, days, alb_mt, bet_mt, yft_mt, oth_mt, tot_mt)
    }
    if (has_data(data_2900)) {
      # Table - overall per year
      data_2900_summary <-data_2900 |>
        group_by(year) |>
        summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) |>
        ungroup() |>
        mutate(across(year:fish_days, ~ round(.x, 0)),
               gear = "Purse seine") |>
        select(-sea_days)
      
      plot_data1ps <- data_2900 |>
        mutate(tot_mt = replace_na(tot_mt, 0)) |>          
        mutate(flag = fct_lump_n(flag, n = 5, w = tot_mt, other_level = "Other")) |>
        group_by(year, flag) |>
        summarise(tot_mt = sum(tot_mt, na.rm = TRUE), .groups = "drop") |>
        mutate(flag = fct_reorder(flag, tot_mt, .fun = sum, .desc = TRUE),
               gear = "Purse seine") |> 
        select(year, flag, tot_mt, gear)
      
      # Country and sp catch 
      plot_data2ps <- data_2900 |>
        filter(year == report_year) |>
        mutate(across(c(skj_mt, bet_mt, yft_mt, oth_mt), \(x) replace_na(x, 0))) |>
        pivot_longer(c(skj_mt, bet_mt, yft_mt, oth_mt),
                     names_to = "species", values_to = "mt") |>
        mutate(species = recode(species,
                                skj_mt = "Skipjack tuna",
                                bet_mt = "Bigeye tuna",
                                yft_mt = "Yellowfin tuna",
                                oth_mt = "Other")) |>
        mutate(flag    = fct_reorder(flag, mt, .fun = sum, .desc = TRUE),
               species = fct_reorder(species, mt, .fun = sum, .desc = TRUE),
               gear = "Purse seine") |> 
        select(year, flag, species, mt, gear)
    }
    if (exists("data_2894") && has_data(data_2894)){
      # Table - overall per year
      data_2894_summary <- data_2894 |>
        group_by(year) |>
        summarise(vessels = n_distinct(vessels), 
                  across(where(is.numeric), ~ sum(.x, na.rm = TRUE))
        ) |>
        mutate(across(year:days, ~ round(.x, 0)),
               gear = "Longline") |>
        rename(fish_days = days)
      
      plot_data1ll <- data_2894 |>
        mutate(tot_mt = replace_na(tot_mt, 0)) |>          
        mutate(flag = fct_lump_n(flag, n = 5, w = tot_mt, other_level = "Other")) |>
        group_by(year, flag) |>
        summarise(tot_mt = sum(tot_mt, na.rm = TRUE), .groups = "drop") |>
        mutate(flag = fct_reorder(flag, tot_mt, .fun = sum, .desc = TRUE),
               gear = "Longline") |> 
        select(year, flag, tot_mt, gear)
      
      # Country and sp catch 
      plot_data2ll <- data_2894 |>
        filter(year == report_year) |>
        mutate(across(c(alb_mt, bet_mt, yft_mt, oth_mt), \(x) replace_na(x, 0))) |>
        pivot_longer(c(alb_mt, bet_mt, yft_mt, oth_mt),
                     names_to = "species", values_to = "mt") |>
        mutate(species = recode(species,
                                yft_mt = "Yellowfin tuna",
                                alb_mt = "Albacore",
                                bet_mt = "Bigeye tuna",
                                oth_mt = "Other")) |>
        mutate(flag    = fct_reorder(flag, mt, .fun = sum, .desc = TRUE),
               species = fct_reorder(species, mt, .fun = sum, .desc = TRUE),
               gear = "Longline") |>
        select(year, flag, species, mt, gear)
    }
    
    has_2894 <- exists("data_2894_summary")
    has_2900 <- exists("data_2900_summary")
    
    if (has_2894 && has_2900) {
      flag_summary <- bind_rows(data_2894_summary, data_2900_summary)
      plot_data1   <- bind_rows(plot_data1ll, plot_data1ps)
      plot_data2   <- bind_rows(plot_data2ll, plot_data2ps)
    } else if (has_2894) {
      flag_summary <- data_2894_summary   # longline only
      plot_data1   <- plot_data1ll
      plot_data2   <- plot_data2ll
    } else if (has_2900) {
      flag_summary <- data_2900_summary   # purse seine only
      plot_data1   <- plot_data1ps
      plot_data2   <- plot_data2ps
    }else{
      flag_summary <- data.frame()
      plot_data1   <- data.frame()
      plot_data2   <- data.frame()
    }
    # remove any rows where fishing did not happen
    if(has_data(flag_summary)){
      flag_summary <- flag_summary |>
        filter(fish_days > 0) |>
        select(-report_id)
    }
    if(has_data(plot_data1)){
      plot_data1 <- plot_data1 |>
        filter(tot_mt > 0)
    }
    if(has_data(plot_data2)){
      plot_data2 <- plot_data2 |>
        filter(mt > 0) 
    }
    
    # add to all_data
    all_data$data_3605_ps <- data_3605_ps
    all_data$data_3605_ll <- data_3605_ll
    all_data$ves_raw <- ves_raw
    all_data$ves_calcs <- ves_calcs
    # coastal
    if (has_2894) {
      all_data$data_2894 <- data_2894
      }
    
    if (has_2900) {
      all_data$data_2900 <- data_2900
      }
    
    if (exists("data_2904")) {
      all_data$data_2904 <- data_2904
      }
    # coastal plot and summary data
    all_data$plot_data1 <- plot_data1
    all_data$plot_data2 <- plot_data2
    all_data$flag_summary <- flag_summary
    
  }
  return(all_data)
}

#' Download Ikasavea report data for a country/year and cache to CSV
download_ikasavea_data <- function(country_code, report_ids, folder_path, r_year, baseurl_ika = "https://www.spc.int/coastalfisheries/") {
  
  if (!nzchar(Sys.getenv("IKA_USER_NAME")) || !nzchar(Sys.getenv("IKA_PASSWORD"))) {
    message("IKA_USER_NAME/IKA_PASSWORD not set - skipping Ikasavea download.")
    return(invisible(NULL))
  }
  
  connection <- httr::POST(
    paste0(baseurl_ika, "account/SignIn"),
    body = list(
      username = Sys.getenv("IKA_USER_NAME"),
      password = Sys.getenv("IKA_PASSWORD")
    ),
    encode = "form"
  )
  
  if (!(httr::status_code(connection) == 200 && length(httr::content(connection)) > 0)) {
    cat("\u274c Authentication failed!\n")
    cat("Please check your credentials in the .env file\n")
    cat(paste("Status Code:", httr::status_code(connection), "\n"))
    message("Authentication for Ikasavea failed. If you are expecting data sourced from Ikasavea please check your credentials in the .env file, otherwise, you can ignore this message.")
    return(invisible(NULL))  # don't proceed to request data unauthenticated
  }
  
  auths <- read.csv(paste0(ref_data_folder, "/", "list_authorities.csv")) |>
    dplyr::filter(flag %in% tolower(country_code))
  
  if (nrow(auths) == 0) {
    cat("No data from Ikasavea for country_code:", country_code, "\n")
    return(invisible(NULL))
  }
  
  authority_ids <- auths |> dplyr::pull(Id)
  
  for (report_id in report_ids) {
    body <- list(reportId = report_id, includeIdColumns = "false", includeIgnoredData = "false")
    for (i in seq_along(authority_ids)) body[[sprintf("authorityIds[%d]", i - 1)]] <- authority_ids[i]
    
    resp <- httr::POST(paste0(baseurl_ika, "FieldSurveys/LdsStatistics/ExportDataAsJson"),
                       body = body, encode = "form")
    response_data <- httr::content(resp, "parsed")
    
    if (!"data" %in% names(response_data)) {
      cat("\u26a0\ufe0f IKASAVEA: No 'data' field found in response for report", report_id, "year", r_year, "\n")
      next
    }
    
    df_result <- dplyr::bind_rows(response_data$data)
    cat("IKASAVEA: Retrieved", nrow(df_result), "data records for report", report_id, "year", r_year, "\n")
    
    report_name <- if (report_id == "b1559368-b7a3-464e-883a-34fe3d2cd7c0") "trips_landing_site" else report_id
    filename_csv <- paste0(folder_path, "/", report_name, ".csv")
    write.csv(df_result, file = filename_csv, row.names = FALSE)
  }
}

#' Read the single ikasavea export in a folder and reshape into artisanal-ACE inputs
#'
#' @param ikasavea_folder Folder expected to contain exactly one ikasavea export csv
#' @param report_year Year to filter the export to
#' @return list(df_ika_wide = <trips, wide by month>, data_ika_catch_kg = <catch, long>)
#'         or NULL if no usable file is found
prep_ikasavea_artisanal_inputs <- function(ikasavea_folder, report_year, data_source_lst = c()) {
  
  files <- list.files(ikasavea_folder)
  if (length(files) != 1) {
    if (length(files) > 1) {
      warning("Expected exactly one file in ", ikasavea_folder, ", found ", length(files), " - skipping ikasavea data.")
    }
    return(NULL)
  }
  
  data_ika_raw <- tryCatch(
    read.csv(file.path(ikasavea_folder, files[1])),
    error = function(e) NULL
  )
  if (is.null(data_ika_raw) || nrow(data_ika_raw) == 0) return(NULL)
  
  if (!"LandingSite" %in% colnames(data_ika_raw)) {
    warning("Ikasavea file in ", ikasavea_folder, " is missing 'LandingSite' - skipping ikasavea data.")
    return(NULL)
  }
  
  data_ika_raw <- data_ika_raw |>
    dplyr::rename(landing_site_name = LandingSite) |>
    dplyr::group_by(Country, landing_site_name, Year, Month) |>
    dplyr::summarise(dplyr::across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop") |>
    dplyr::filter(Year == report_year)
  
  if (nrow(data_ika_raw) == 0) return(NULL)
  
  # add to source list
  data_source_lst = append(data_source_lst, "ikasavea")
  
  data_ika_trips <- data_ika_raw |>
    dplyr::mutate(dplyr::across(where(is.character), tolower)) |>
    janitor::clean_names() |>
    dplyr::select(landing_site_name, month, number_trips)
  
  df_ika_wide <- data_ika_trips |>
    tidyr::complete(landing_site_name, month = 1:12, fill = list(number_trips = 0)) |>
    dplyr::mutate(month = month.abb[month]) |>
    tidyr::pivot_wider(names_from = month, values_from = number_trips) |>
    dplyr::select(landing_site_name, dplyr::all_of(month.abb))
  
  data_ika_catch_kg <- data_ika_raw |>
    dplyr::mutate(dplyr::across(where(is.character), tolower)) |>
    janitor::clean_names() |>
    dplyr::select(landing_site_name, month, skj = skj_kg, yft = yft_kg, bet = bet_kg) |>
    tidyr::pivot_longer(cols = c("skj", "bet", "yft"), names_to = "sp_code", values_to = "sp_kg")
  
  list(df_ika_wide = df_ika_wide, data_ika_catch_kg = data_ika_catch_kg, data_source_lst = data_source_lst)
}

#' Prepare artisanal tuna ACE (raised & unraised) summary tables
#'
#' @param data_3615 Tails trips-per-landing-site data (all_data$data_3615)
#' @param data_3614 Tails catch-per-landing-site data (all_data$data_3614)
#' @param df_ika_wide Ikasavea trips-per-landing-site data, wide format (or NULL)
#' @param data_ika_catch_kg Ikasavea catch data, long, with sp_code/month/sp_kg (or NULL)
#' @param local_knowl_trips_file Path to member-reported ("local knowledge") trips csv, can be NULL
#'
#' @return list(ace_summary = <raised/unraised mt table>, ace_perc = <species % table>)
prep_artisanal_ace <- function(data_3615,
                               data_3614,
                               df_ika_wide = NULL,
                               data_ika_catch_kg = NULL,
                               local_knowl_trips_file = NULL) {
  
  # Check if data exists
  if (!has_data(data_3615) && !has_data(data_3614) && !has_data(df_ika_wide) && !has_data(data_ika_catch_kg)) {
    return(invisible(NULL))
  }

  month_labels <- c("Jan","Feb","Mar","Apr","May","Jun",
                    "Jul","Aug","Sep","Oct","Nov","Dec")
  species <- c("skj", "bet", "yft")
  
  ## --- Trips: clean Tails, combine with Ikasavea --------------------------
  if (nrow(data_3615) > 0) {
    names(data_3615) <- gsub("_trips$", "", names(data_3615))
    data_3615 <- data_3615 |>
      dplyr::rename_with(~ month.abb[match(., tolower(month.abb))],
                         .cols = dplyr::any_of(tolower(month.abb))) |>
      dplyr::select(landing_site_name, dplyr::any_of(month.abb))  # <- keep only what's needed
  }
  
  prep_trips <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df %>%
      dplyr::mutate(landing_site_name = tolower(trimws(landing_site_name))) %>%
      dplyr::select(landing_site_name, dplyr::any_of(c(month.abb, tolower(month.abb)))) %>%
      tidyr::pivot_longer(-landing_site_name, names_to = "month", values_to = "trips") %>%
      dplyr::mutate(month = tolower(month))
  }
  
  ereporting_trips <- dplyr::bind_rows(prep_trips(data_3615), prep_trips(df_ika_wide)) %>%
    dplyr::group_by(landing_site_name, month) %>%
    dplyr::summarise(trips = sum(trips, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(month = factor(month, levels = tolower(month.abb))) %>%
    tidyr::pivot_wider(names_from = month, values_from = trips, values_fill = 0) %>%
    dplyr::arrange(landing_site_name) %>%
    dplyr::select(landing_site_name, dplyr::all_of(tolower(month.abb))) %>%
    data.frame()
  names(ereporting_trips)[-1] <- month.abb
  
  ## --- Best estimate trips = max(ereporting, local knowledge) -------------
  if (nrow(ereporting_trips) > 0 && !is.null(local_knowl_trips_file) && file.exists(local_knowl_trips_file)) {
    local_knowl_trips <- read.csv(local_knowl_trips_file)
    
    if (all(c("landing_site_name", "est.trips") %in% colnames(local_knowl_trips))) {
      local_knowl_trips <- local_knowl_trips |>
        dplyr::mutate(dplyr::across(dplyr::where(is.character), tolower)) |>
        dplyr::mutate(landing_site_name = stringr::str_squish(landing_site_name))
      
      best_estimate_trips <- ereporting_trips |>
        dplyr::full_join(local_knowl_trips |> dplyr::select(landing_site_name, est.trips),
                         by = "landing_site_name")
      best_estimate_trips[is.na(best_estimate_trips)] <- 0
      
      best_estimate_trips <- best_estimate_trips |>
        dplyr::mutate(dplyr::across(2:13, ~ pmax(., est.trips, na.rm = TRUE))) |>
        dplyr::filter(rowSums(dplyr::across(2:13)) > 0) |>
        dplyr::select(-est.trips)
    } else {
      warning("local_knowl_trips_file missing 'landing_site_name'/'est.trips' - using ereporting trips only.")
      best_estimate_trips <- ereporting_trips
    }
  } else {
    best_estimate_trips <- ereporting_trips
  }
  
  ## --- Catch: clean Tails + Ikasavea, per species --------------------------
  make_wide <- function(data) {
    data %>%
      dplyr::group_by(landing_site_name, month) %>%
      dplyr::summarise(sp_kg = sum(sp_kg, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(month = month_labels[month]) %>%
      tidyr::complete(landing_site_name, month = month_labels, fill = list(sp_kg = 0)) %>%
      tidyr::pivot_wider(names_from = month, values_from = sp_kg, values_fill = 0) %>%
      dplyr::select(landing_site_name, dplyr::any_of(month_labels))
  }
  
  combine_wide <- function(df1, df2) {
    if (is.null(df1) && is.null(df2)) return(NULL)
    drop_zero_rows <- function(df) {
      if (is.null(df)) return(NULL)
      df |> dplyr::filter(dplyr::if_any(dplyr::all_of(month_labels), ~ . != 0))
    }
    df1 <- drop_zero_rows(df1); df2 <- drop_zero_rows(df2)
    if (is.null(df1) || nrow(df1) == 0) return(df2)
    if (is.null(df2) || nrow(df2) == 0) return(df1)
    
    joined <- dplyr::full_join(df1, df2, by = "landing_site_name", suffix = c("_a", "_b")) |>
      dplyr::mutate(dplyr::across(dplyr::all_of(paste0(month_labels, "_a")), ~ dplyr::coalesce(., 0L))) |>
      dplyr::mutate(dplyr::across(dplyr::all_of(paste0(month_labels, "_b")), ~ dplyr::coalesce(., 0L)))
    
    for (m in month_labels) {
      joined[[m]] <- joined[[paste0(m, "_a")]] + joined[[paste0(m, "_b")]]
    }
    
    joined |> dplyr::select(landing_site_name, dplyr::all_of(month_labels))
  }
  
  get_species_wide <- function(data, sp) {
    if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) return(NULL)
    data %>%
      dplyr::filter(sp_code == sp) %>%
      dplyr::mutate(landing_site_name = stringr::str_squish(landing_site_name)) %>%
      make_wide()
  }
  
  catch_by_species <- setNames(lapply(species, function(sp) {
    combine_wide(get_species_wide(data_3614, sp), get_species_wide(data_ika_catch_kg, sp))
  }), species)
  
  ## --- Unraised / raised catch per species ----------------------------------
  tot_trips_ereport <- data.frame(
    landing_site_name = ereporting_trips$landing_site_name,
    tot_trips = rowSums(ereporting_trips[, -1], na.rm = TRUE)
  ) |> dplyr::mutate(landing_site_name = stringr::str_squish(landing_site_name))
  
  best_estimate_trips_long <- best_estimate_trips |>
    janitor::clean_names() |>
    dplyr::mutate(landing_site_name = stringr::str_squish(landing_site_name)) |>
    tidyr::pivot_longer(cols = 2:13, names_to = "months", values_to = "tot_trips_best")
  
  ereporting_trips_long <- ereporting_trips |>
    janitor::clean_names() |>
    dplyr::mutate(landing_site_name = stringr::str_squish(landing_site_name)) |>
    tidyr::pivot_longer(cols = 2:13, names_to = "months", values_to = "tot_trips_spc")
  
  unr_list <- list(); raised_list <- list()
  
  for (sp in species) {
    df <- catch_by_species[[sp]]
    if (is.null(df) || nrow(df) == 0) next
    
    avg_sp <- data.frame(
      landing_site_name = df$landing_site_name,
      tot_catch = rowSums(df[, -1], na.rm = TRUE)
    ) |>
      dplyr::left_join(tot_trips_ereport, by = "landing_site_name") |>
      dplyr::mutate(cpd = round(tot_catch / tot_trips, 0))
    
    unr_sp <- df |> dplyr::left_join(avg_sp |> dplyr::select(landing_site_name, cpd), by = "landing_site_name")
    unr_list[[sp]] <- unr_sp
    
    unr_sp_long <- unr_sp |>
      janitor::clean_names() |>
      dplyr::mutate(landing_site_name = stringr::str_squish(landing_site_name)) |>
      tidyr::pivot_longer(cols = 2:13, names_to = "months", values_to = "unr_catch")
    
    raised_df <- unr_sp_long |>
      dplyr::left_join(ereporting_trips_long, by = c("landing_site_name", "months")) |>
      dplyr::left_join(best_estimate_trips_long, by = c("landing_site_name", "months")) |>
      dplyr::mutate(raised = round(dplyr::case_when(
        is.na(tot_trips_spc) | tot_trips_spc == 0 ~ cpd * tot_trips_best,
        .default = (unr_catch / tot_trips_spc) * tot_trips_best
      ), 0))
    
    raised_list[[sp]] <- raised_df |>
      dplyr::select(landing_site_name, months, raised) |>
      tidyr::pivot_wider(id_cols = 1, names_from = "months", values_from = "raised")
  }
  
  ## --- Final summary tables --------------------------------------------------
  ace <- data.frame()
  for (sp in species) {
    if (sp %in% names(unr_list)) {
      unraised <- unr_list[[sp]] |> data.frame() |> dplyr::ungroup() |>
        dplyr::select(-c(1, 14)) |>
        dplyr::summarise(dplyr::across(dplyr::everything(), \(x) sum(x, na.rm = TRUE))) |>
        rowSums()
      raised <- raised_list[[sp]] |> data.frame() |> dplyr::ungroup() |>
        dplyr::select(-1) |>
        dplyr::summarise(dplyr::across(dplyr::everything(), \(x) sum(x, na.rm = TRUE))) |>
        rowSums()
    } else {
      unraised <- 0; raised <- 0
    }
    ace <- dplyr::bind_rows(ace, data.frame(
      sp = toupper(sp), unraised = round(unraised / 1000, 2), raised = round(raised / 1000, 2)
    ))
  }
  
  ace_summary <- ace |>
    tidyr::gather(key = "Method", value = "ACE", -1) |>
    tidyr::spread(key = sp, value = ACE)
  
  ace_perc <- ace_summary
  ace_perc$tot <- rowSums(ace_perc[, -1])
  ace_perc <- ace_perc |>
    dplyr::mutate(SKJ = round(SKJ / tot * 100, 2),
                  YFT = round(YFT / tot * 100, 2),
                  BET = round(BET / tot * 100, 2)) |>
    dplyr::mutate(dplyr::across(BET:YFT, ~ tidyr::replace_na(., 0))) |>
    dplyr::select(-tot)
  
  list(ace_summary = ace_summary, ace_perc = ace_perc)
}

