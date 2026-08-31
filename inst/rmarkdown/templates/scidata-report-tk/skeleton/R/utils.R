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

parse_coord <- function(x, type = c("lat", "lon")) {
  type <- match.arg(type)
  deg_width <- if (type == "lat") 2L else 3L
  
  hem <- str_sub(x, -1)
  num <- str_sub(x, 1, -2)
  
  degs <- as.numeric(str_sub(num, 1, deg_width))
  mins <- as.numeric(str_sub(num, deg_width + 1))
  
  decimal <- degs + mins / 60
  
  sign <- dplyr::case_when(
    hem %in% c("n", "e") ~  1,
    hem %in% c("s", "w") ~ -1,
    TRUE                 ~ NA_real_
  )
  
  decimal * sign
}

## Functions: data grooming #############################################

groom_data <- function(all_data, report_year) {
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
    
  return(all_data)
}


## Functions: outputs ####################################################

# Define size category functions for each gear type
ll_size_fn <- function(data) {
  data |>
    summarise(
      '0 - 50' = sum(vessels_sml),
      '51 - 200' = sum(vessels_med),
      '201 - 500' = sum(vessels_lge),
      '500+' = sum(vessels_hge),
      .groups = "drop"
    )
}

ps_size_fn <- function(data) {
  data |>
    summarise(
      '0 - 50' = sum(vessels_sml),
      '51 - 200' = sum(vessels_med),
      '201 - 500' = sum(vessels_lge),
      '500+' = sum(vessels_hge),
      .groups = "drop"
    )
}

shape_catch_tbl <- function(data) {
  data |>
    pivot_wider(names_from = year, values_from = catch, values_fill = 0) |>
    mutate(across(where(is.numeric), ~ replace_na(., 0))) |>
    arrange(sp_code) |>
    rename(Species = sp_code) |>
    janitor::adorn_totals("row") |>
    select(Species, sort(tidyselect::peek_vars()))
}

# Renders a gear-specific catch table as a flextable AND saves the
# flextable object itself (as an .rds) for the .qmd to load and print
# directly. `refresh_data = FALSE` skips regenerating an .rds that
# already exists (both the download step above and this step respect
# the same flag, for the same reason: don't redo expensive work if the
# person just wants to re-render the .qmd text without touching the
# data).
#
# Saving the flextable object (rather than a rasterised PNG via
# save_as_image()) is deliberate: PNGs of long/wide tables scaled
# unpredictably once embedded in the PDF, producing tiny or blurry
# text. Loading the flextable directly with read_rds() and letting
# Quarto/LaTeX typeset it natively renders crisp, correctly-sized
# tables for every member, regardless of table length.
#
# save_path is always derived from tbl_dir + gear_code here — every
# .qmd section referencing this table must use this same function
# (or its resulting path) rather than typing the filename directly,
# to avoid the kind of typo mismatch (tbl_ace_ll.rds vs tbl_ace_l.rds)
# that silently hid the longline section in an earlier draft.
render_gear_tbl_section3 <- function(data, gear_code, species_labels, refresh_data = TRUE) {
  save_path <- file.path(tbl_dir, paste0("tbl_ace_", gear_code, ".rds"))
  
  if (!refresh_data && file.exists(save_path)) {
    return(invisible(save_path))
  }
  
  if (!has_data(data)) {
    return(invisible(NULL))
  }
  
  data_tbl <- shape_catch_tbl(data)
  
  ft <- flextable(data_tbl) |>
    labelizor(labels = species_labels, part = "all") |>
    set_table_properties(width = 0.7, align = "center") |>
    bold(j = ncol(data_tbl), part = "body") |>
    width(j = 1, width = 2)
  
  saveRDS(ft, save_path)
  
  # Return the flextable explicitly - previously this function had no
  # final expression and relied on save_as_image()'s own (invisible)
  # return value, which happened to work but was fragile.
  ft
}

# Function to render gear-specific figures in section 3 with header and plot
render_gear_figs_section3 <- function(data, gear_code, species_list,
                                      species_labels,
                                      effort_df_ll = all_data$data_3597,
                                      effort_df_ps = all_data$data_3590,
                                      country_cd,
                                      yrs_long = yrs_long) {
  
  # Check if data exists
  if (!has_data(data)) {
    return(invisible(NULL))
  }
  
  # ── Prepare data for plotting ──────────────────────────────────────────────
  data_fig <- data |>
    group_by(sp_code, year) |>
    summarise(catch = sum(catch, na.rm = TRUE), .groups = "drop")
  
  ref_df <- expand_grid(year = as.numeric(yrs_long), sp_code = species_list)
  
  plot_data <- left_join(ref_df, data_fig, by = c("year", "sp_code")) |>
    mutate(
      catch   = replace_na(catch, 0),
      Species = factor(recode(sp_code, !!!species_labels),
                       levels = unname(species_labels[species_list]))
    )
  
  # ── Base plot (shared by all gears) ────────────────────────────────────────
  p <- ggplot(plot_data, aes(x = year, y = catch, fill = Species)) +
    geom_col(position = "stack") +
    scale_fill_manual(values = sp_colours) +
    scale_x_continuous(breaks = seq(min(plot_data$year), max(plot_data$year), 1)) +
    labs(x = NULL, y = "Catch (mt)") +
    theme_bw() +
    theme(
      axis.title   = element_text(size = 16),
      axis.text    = element_text(size = 16),
      axis.text.x  = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text  = element_text(size = 13)
    )
  
  # ── Longline: overlay effort (hooks) on a secondary axis ──────────────────
  if (gear_code == "l") {
    
    if (!has_data(effort_df_ll)) {
      message("No usable effort data for gear '", gear_code,
              "' — rendering catch only.")
      p <- p + scale_y_continuous(labels = comma)
      
    } else {
      
      effort_annual <- effort_df_ll |>
        rename(year = yr) |>
        filter(flag_id == country_cd) |>
        filter(year %in% plot_data$year) |>          # align with plotted years
        group_by(year) |>
        summarise(effort = sum(hooks_total_logsheet, na.rm = TRUE), .groups = "drop")
      
      max_catch  <- max(tapply(plot_data$catch, plot_data$year, sum))
      max_effort <- max(effort_annual$effort, 0)
      
      if (nrow(effort_annual) > 0 && max_effort > 0 && max_catch > 0) {
        
        scale_factor <- max_catch / max_effort
        
        p <- p +
          geom_line(data = effort_annual,
                    aes(x = year, y = effort * scale_factor),
                    inherit.aes = FALSE, colour = "darkorange",
                    linewidth = 1.2, linetype = "dashed") +
          geom_point(data = effort_annual,
                     aes(x = year, y = effort * scale_factor),
                     inherit.aes = FALSE, colour = "darkorange", size = 3) +
          scale_y_continuous(
            labels = comma,
            sec.axis = sec_axis(~ . / scale_factor,
                                name = "Effort (hooks)", labels = comma)
          ) +
          theme(
            axis.title.y.right = element_text(colour = "darkorange"),
            axis.text.y.right  = element_text(colour = "darkorange")
          )
      } else {
        message("No usable effort data for gear '", gear_code,
                "' — rendering catch only.")
        p <- p + scale_y_continuous(labels = comma)
      }
    }
    
  } else if (gear_code == "s") {
    
    if (!has_data(effort_df_ps)) {
      message("No usable effort data for gear '", gear_code,
              "' — rendering catch only.")
      p <- p + scale_y_continuous(labels = comma)
      
    } else {
      
      effort_annual <- effort_df_ps |>
        filter(flag == country_cd) |>
        filter(year %in% plot_data$year) |>          # align with plotted years
        group_by(year) |>
        summarise(effort = sum(trips, na.rm = TRUE), .groups = "drop")
      
      max_catch  <- max(tapply(plot_data$catch, plot_data$year, sum))
      max_effort <- max(effort_annual$effort, 0)
      
      if (nrow(effort_annual) > 0 && max_effort > 0 && max_catch > 0) {
        
        scale_factor <- max_catch / max_effort
        
        p <- p +
          geom_line(data = effort_annual,
                    aes(x = year, y = effort * scale_factor),
                    inherit.aes = FALSE, colour = "darkorange",
                    linewidth = 1.2, linetype = "dashed") +
          geom_point(data = effort_annual,
                     aes(x = year, y = effort * scale_factor),
                     inherit.aes = FALSE, colour = "darkorange", size = 3) +
          scale_y_continuous(
            labels = comma,
            sec.axis = sec_axis(~ . / scale_factor,
                                name = "Effort (trips)", labels = comma)
          ) +
          theme(
            axis.title.y.right = element_text(colour = "darkorange"),
            axis.text.y.right  = element_text(colour = "darkorange")
          )
      } else {
        message("No usable effort data for gear '", gear_code,
                "' — rendering catch only.")
        p <- p + scale_y_continuous(labels = comma)
      }
    }
    
  } else {
    p <- p + scale_y_continuous(labels = comma)
  }
  
  save_path = paste0(fig_dir, "/fig_ace_", gear_code, ".png")
  ggsave(save_path, plot = p,  width = 12, height = 7, dpi = 300, bg = "white")
  
  invisible(NULL)
}

render_vessels_figs_section3 <- function(data, yrs_long){
  
  # Check if data exists
  if (!has_data(data)) {
    return(invisible(NULL))
  }
  
  ves_raw <-
    data |>
    filter(year %in% yrs_long, gear_code %in% c("s", "l"))
  
  if (has_data(ves_raw)) {
    ves_raw <- ves_raw |>
      select(flag, year, gear_code, vessels_sml, vessels_med, vessels_lge, vessels_hge) |>
      distinct() |>
      mutate(gear = case_when(
        gear_code == "s" ~ "Purse seine",
        gear_code == "l" ~ "Longline"
      ))
    
    vessel_plot <- ves_raw |>
      pivot_longer(
        cols = c(vessels_sml, vessels_med, vessels_lge, vessels_hge),
        names_to = "vessel_class",
        values_to = "count"
      ) |>
      mutate(vessel_class = recode(vessel_class,
                                   "vessels_sml" = "0 - 50",
                                   "vessels_med" = "51 - 200",
                                   "vessels_lge" = "201 - 500",
                                   "vessels_hge" = "500+"
      )) |>
      mutate(vessel_class = factor(vessel_class, levels = c("0 - 50", "51 - 200", "201 - 500", "500+")))
    
    p_vessels <- ggplot(vessel_plot, aes(x = year, y = count, fill = vessel_class)) +
      geom_col(position = "stack") +
      scale_fill_manual(values = vessel_colours) +
      scale_x_continuous(breaks = seq(min(vessel_plot$year), max(vessel_plot$year), 1)) +
      scale_y_continuous(breaks = scales::pretty_breaks()) +
      labs(x = NULL, y = "Vessels", fill = "Size category (GRT)") +
      theme_bw() +
      facet_wrap(~gear) +
      theme(
        axis.title   = element_text(size = 16),
        axis.text    = element_text(size = 16),
        axis.text.x  = element_text(angle = 45, hjust = 1),
        strip.text   = element_text(size = 16),
        legend.position = "bottom",
        legend.title = element_text(size = 13),
        legend.text  = element_text(size = 13)
      )
    
    save_path = paste0(fig_dir, "/fig_ace_vessels.png")
    ggsave(save_path, plot = p_vessels, width = 12, height = 7, dpi = 300, bg = "white")
    
    invisible(NULL)
  }
}

render_other_areas_tbl <- function(data_01, data_02, data_03) {
  save_path <- file.path(tbl_dir, "tbl_other_areas.rds")
  
  if (!has_data(data_01) && !has_data(data_02) && !has_data(data_03)) {
    return(invisible(NULL))
  }
  
  empty_cols <- function(...) {
    tibble(year = integer(), sp_code = character(), !!!setNames(rep(list(numeric()), length(list(...))), c(...)))
  }
  
  if (has_data(data_01)) {
    data_01 <- data_01 |>
      filter(sp_code %in% ll_species) |>
      select(year = yy, sp_code,
             `WCPFC Convention Area (S of Equator)` = log_sx_raised,
             `WCPFC Convention Area (N of Equator)` = log_nx_raised)
  } else {
    data_01 <- empty_cols("WCPFC Convention Area (S of Equator)", "WCPFC Convention Area (N of Equator)")
  }
  
  if (has_data(data_02)) {
    data_02 <- data_02 |>
      mutate(across(where(is.character), tolower)) |>
      filter(sp_code %in% ll_species) |>
      mutate(year = report_year) |>
      pivot_wider(names_from = area, values_from = mt) |>
      select(year, sp_code, `South Pacific Ocean` = spac, `North Pacific Ocean` = npac)
  } else {
    data_02 <- empty_cols("South Pacific Ocean", "North Pacific Ocean")
  }
  
  if (has_data(data_03)) {
    data_03 <- data_03 |>
      filter(sp_code %in% ll_species) |>
      select(year = yy, sp_code, `WCPO` = log_wcpo_raised)
  } else {
    data_03 <- empty_cols("WCPO")
  }
  
  data_other_areas <- data_01 |>
    left_join(data_02, by = c("year", "sp_code")) |>
    left_join(data_03, by = c("year", "sp_code")) |>
    mutate(across(where(is.numeric), ~replace_na(., 0))) |>
    select(-year) |>
    rename(Species = sp_code)
  
  if (!has_data(data_other_areas)) {
    return(invisible(NULL))
  }
  
  ft <- flextable(data_other_areas) |>
    labelizor(labels = ll_species_labels, part = "all") |>
    set_table_properties(width = 0.7, align = "center") |>
    width(j = 1, width = 2)
  
  saveRDS(ft, save_path)
  
  ft
}

# Function to render vessel table for a specific gear
render_vessel_table <- function(ves_data, gear_cd, size_categories_fn) {
  
  if (!has_data(ves_data)) {
    return(invisible(NULL))
  }
  
  save_path <- file.path(tbl_dir, paste0("tbl_vess_categ_", gear_cd, ".rds"))
  
  # Filter for specific gear
  gear_data <- ves_data |> filter(gear_code == gear_cd)
  
  # Check if data exists
  if (!has_data(gear_data)) {
    return(invisible(NULL))
  }
  
  # Apply the size category function to summarize data
  ves_summary <- gear_data |>
    group_by(year, gear) |>
    size_categories_fn()
  
  # Check if there's any actual vessel data (not all zeros)
  if (sum(ves_summary[, -c(1:2)]) == 0) {
    return(invisible(NULL))
  }
  
  # Create and return flextable
  ft <- ves_summary |>
    pivot_longer(-c(year, gear), names_to = "Size Category (GRT)", values_to = "Vessels") |>
    pivot_wider(names_from = year, values_from = Vessels, values_fill = 0) |>
    group_by(gear) |>
    mutate(gear = ifelse(row_number() == 1, gear, "")) |>
    flextable() |>
    set_table_properties(width = 0.7, align = "center") |>
    width(j = 1:2, width = 1.2)
  
  saveRDS(ft, save_path)
  return(invisible(NULL))
}

save_spatial_maps_dynamic <- function(data, newmap, year_range) {
  
  gears <- list(
    s = list(code = "s",  name = "purse seine", species = c("bet", "skj", "yft"), cell_size = 1),
    l = list(code = "l",  name = "longline",    species = c("bet", "alb", "yft"), cell_size = 5)
  )
  
  saved_paths <- list()
  
  # buffer applied independently to lon/lat instead of one shared value —
  # this is the actual lever for panel shape, not the ggsave width/height
  buffer_lon_deg <- 10
  buffer_lat_deg <- 5     
  edge_pad       <- 1.5
  
  all_gear_species <- unique(unlist(lapply(gears, function(x) x$species)))
  
  extent_data <- data |>
    filter(
      between(year, year_range[1], year_range[2]),
      sp_code %in% all_gear_species
    ) |>
    mutate(half_cell = ifelse(gear_code == "s", gears$s$cell_size, gears$l$cell_size) / 2)
  
  lon_rng <- range(c(extent_data$lon - extent_data$half_cell,
                     extent_data$lon + extent_data$half_cell), na.rm = TRUE)
  lat_rng <- range(c(extent_data$lat - extent_data$half_cell,
                     extent_data$lat + extent_data$half_cell), na.rm = TRUE)
  
  safety_xlim <- c(90, 260)
  safety_ylim <- c(-60, 60)
  
  lon_rng <- lon_rng + c(-buffer_lon_deg, buffer_lon_deg)
  lat_rng <- lat_rng + c(-buffer_lat_deg, buffer_lat_deg)
  lon_rng <- c(max(lon_rng[1], safety_xlim[1]), min(lon_rng[2], safety_xlim[2]))
  lat_rng <- c(max(lat_rng[1], safety_ylim[1]), min(lat_rng[2], safety_ylim[2]))
  
  shared_xlim <- lon_rng + c(-edge_pad, edge_pad)
  shared_ylim <- lat_rng + c(-edge_pad, edge_pad)
  
  shared_lon_breaks <- pretty(shared_xlim, n = 4)
  shared_lat_breaks <- pretty(shared_ylim, n = 4)
  
  lat_labels <- scales::label_number(accuracy = 1)
  
  lon_labels_180 <- function(x) {
    x <- ifelse(x > 180, x - 360, x)
    scales::label_number(accuracy = 1)(x)
  }
  
  # --- Shared canvas size, computed ONCE so both gears save identically ---
  n_years    <- length(seq(year_range[1], year_range[2]))
  n_species  <- max(vapply(gears, function(x) length(x$species), integer(1)))
  
  panel_asp  <- diff(shared_xlim) / diff(shared_ylim)  # true geographic aspect
  
  target_width_in <- 6.27   # matches SCreport's \textwidth (A4, geometry: margin=1in)
  extra_h <- 1.4             # title + bottom legend (unchanged from your original)
  extra_w <- 0.3
  
  panel_w_in <- (target_width_in - extra_w) / n_species
  panel_h_in <- panel_w_in / panel_asp
  
  out_w <- target_width_in
  out_h <- n_years * panel_h_in + extra_h
  
  message(glue::glue(
    "  panel_asp = {round(panel_asp, 2)} | panel {round(panel_w_in,2)}x{round(panel_h_in,2)}in | ",
    "canvas {round(out_w,2)}x{round(out_h,2)}in"
  ))
  
  for (g in names(gears)) {
    
    gear_code_val  <- gears[[g]][["code"]]
    gear_name_val  <- gears[[g]][["name"]]
    gear_species   <- gears[[g]][["species"]]
    cell_size      <- gears[[g]][["cell_size"]]
    
    gear_data <- data |> filter(gear_code == gear_code_val)
    
    if (!has_data(gear_data)) {
      message("  No data for ", gear_name_val, " - skipping")
      next
    }
    
    spatial_data <- gear_data |>
      filter(
        between(year, year_range[1], year_range[2]),
        sp_code %in% gear_species
      ) |>
      group_by(lat, lon, year, species) |>
      summarise(catch = sum(catch), .groups = "drop") |>
      filter(catch > 0)
    
    if (!has_data(spatial_data)) {
      message("  No spatial data for ", gear_name_val, " - skipping")
      next
    }
    
    p <- ggplot() +
      geom_sf(data = newmap, fill = "grey75", colour = "grey55", linewidth = 0.15) +
      geom_tile(
        data = spatial_data,
        aes(lon, lat, fill = catch),
        width = cell_size, height = cell_size,
        colour = "white", linewidth = 0.05
      ) +
      scale_fill_distiller(
        palette = "Spectral",
        na.value = "transparent",
        guide = guide_colourbar(barwidth = unit(8, "cm"), barheight = unit(0.4, "cm"))
      ) +
      scale_x_continuous(breaks = shared_lon_breaks, labels = lon_labels_180) +
      scale_y_continuous(breaks = shared_lat_breaks, labels = lat_labels) +
      facet_grid(year ~ species) +
      coord_sf(xlim = shared_xlim, ylim = shared_ylim, expand = FALSE) +
      xlab(NULL) + ylab(NULL) + labs(fill = "Catch (mt)") +
      ggtitle(glue::glue("Spatial patterns in catch for the {gear_name_val} fishery")) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text          = element_text(size = 9, colour = "grey30"),
        axis.text.x        = element_text(size = 8),
        axis.ticks         = element_line(colour = "grey70", linewidth = 0.2),
        legend.text        = element_text(size = 11),
        legend.title       = element_text(size = 12, face = "bold"),
        strip.background   = element_rect(fill = "grey95", colour = NA),
        strip.text         = element_text(size = 12, face = "bold"),
        plot.title         = element_text(size = 16, face = "bold", margin = margin(b = 8)),
        panel.grid.major   = element_line(colour = "grey90", linewidth = 0.2),
        panel.grid.minor   = element_blank(),
        panel.spacing      = unit(0.25, "lines"),
        panel.border       = element_rect(colour = "grey85", fill = NA, linewidth = 0.3),
        legend.position    = "bottom",
        legend.margin      = margin(t = 4),
        plot.margin        = margin(t = 4, r = 6, b = 2, l = 2)
      )
    
    output_dir <- file.path(fig_dir, glue::glue("fig_map_{g}.png"))
    ggsave(output_dir, plot = p, width = out_w, height = out_h, dpi = 400, bg = "white")
    message("  Saved: ", output_dir)
    
    saved_paths[[g]] <- output_dir
    rm(p, spatial_data, gear_data); gc()
  }
  
  return(saved_paths)
}

save_spatial_maps_dynamic2 <- function(data, newmap, year_range) {
  
  gears <- list(
    s = list(code = "s",  name = "purse seine", species = c("bet", "skj", "yft"), cell_size = 1),
    l = list(code = "l",  name = "longline",    species = c("bet", "alb", "yft"), cell_size = 5)
  )
  
  saved_paths <- list()
  
  buffer_deg <- 10
  edge_pad   <- 1.5
  
  all_gear_species <- unique(unlist(lapply(gears, function(x) x$species)))
  
  extent_data <- data |>
    filter(
      between(year, year_range[1], year_range[2]),
      sp_code %in% all_gear_species
    ) |>
    mutate(half_cell = ifelse(gear_code == "s", gears$s$cell_size, gears$l$cell_size) / 2)
  
  lon_rng <- range(c(extent_data$lon - extent_data$half_cell,
                     extent_data$lon + extent_data$half_cell), na.rm = TRUE)
  lat_rng <- range(c(extent_data$lat - extent_data$half_cell,
                     extent_data$lat + extent_data$half_cell), na.rm = TRUE)
  
  # Safety clamp — generous, just guards against a rogue/erroneous outlier
  # point blowing the extent out absurdly. Must stay wide enough to never
  # clip legitimate data (that was the earlier bug).
  safety_xlim <- c(90, 260)
  safety_ylim <- c(-60, 60)
  
  lon_rng <- lon_rng + c(-buffer_deg, buffer_deg)
  lat_rng <- lat_rng + c(-buffer_deg, buffer_deg)
  lon_rng <- c(max(lon_rng[1], safety_xlim[1]), min(lon_rng[2], safety_xlim[2]))
  lat_rng <- c(max(lat_rng[1], safety_ylim[1]), min(lat_rng[2], safety_ylim[2]))
  
  shared_xlim <- lon_rng + c(-edge_pad, edge_pad)
  shared_ylim <- lat_rng + c(-edge_pad, edge_pad)
  
  shared_lon_breaks <- pretty(shared_xlim, n = 4)
  shared_lat_breaks <- pretty(shared_ylim, n = 4)
  
  lat_labels <- scales::label_number(accuracy = 1)
  
  lon_labels_180 <- function(x) {
    x <- ifelse(x > 180, x - 360, x)
    scales::label_number(accuracy = 1)(x)
  }
  
  # --- Shared canvas size, computed ONCE so both gears save identically ---
  # facet_grid is now year ~ species, i.e. ROWS = year, COLUMNS = species.
  # So width scales with n_species (columns) and height scales with
  # n_years (rows) — the reverse of the old species~year layout.
  n_years    <- length(seq(year_range[1], year_range[2]))
  n_species  <- max(vapply(gears, function(x) length(x$species), integer(1)))
  
  panel_asp  <- diff(shared_xlim) / diff(shared_ylim)  # true geographic aspect
  target_width_in <- 6.27  # matches SCreport's \textwidth (A4, geometry: margin=1in)
  
  extra_h <- 1.4   # title + bottom legend
  extra_w <- 0.3   # right-hand year strip labels
  
  panel_w_in <- (target_width_in - extra_w) / n_species   # derived from page width, not fixed
  panel_h_in <- panel_w_in / panel_asp                      # true geographic aspect preserved
  
  out_w <- target_width_in
  out_h <- n_years * panel_h_in + extra_h
  
  for (g in names(gears)) {
    
    gear_code_val  <- gears[[g]][["code"]]
    gear_name_val  <- gears[[g]][["name"]]
    gear_species   <- gears[[g]][["species"]]
    cell_size      <- gears[[g]][["cell_size"]]
    
    gear_data <- data |> filter(gear_code == gear_code_val)
    
    if (!has_data(gear_data)) {
      message("  No data for ", gear_name_val, " - skipping")
      next
    }
    
    spatial_data <- gear_data |>
      filter(
        between(year, year_range[1], year_range[2]),
        sp_code %in% gear_species
      ) |>
      group_by(lat, lon, year, species) |>
      summarise(catch = sum(catch), .groups = "drop") |>
      filter(catch > 0)
    
    if (!has_data(spatial_data)) {
      message("  No spatial data for ", gear_name_val, " - skipping")
      next
    }
    
    p <- ggplot() +
      geom_sf(data = newmap, fill = "grey75", colour = "grey55", linewidth = 0.15) +
      geom_tile(
        data = spatial_data,
        aes(lon, lat, fill = catch),
        width = cell_size, height = cell_size,
        colour = "white", linewidth = 0.05
      ) +
      scale_fill_distiller(
        palette = "Spectral",
        na.value = "transparent",
        guide = guide_colourbar(barwidth = unit(8, "cm"), barheight = unit(0.4, "cm"))
      ) +
      scale_x_continuous(breaks = shared_lon_breaks, labels = lon_labels_180) +
      scale_y_continuous(breaks = shared_lat_breaks, labels = lat_labels) +
      facet_grid(year ~ species) +
      coord_sf(xlim = shared_xlim, ylim = shared_ylim, expand = FALSE) +
      xlab(NULL) + ylab(NULL) + labs(fill = "Catch (mt)") +
      ggtitle(glue::glue("Spatial patterns in catch for the {gear_name_val} fishery")) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text          = element_text(size = 9, colour = "grey30"),
        axis.text.x        = element_text(size = 8),   # horizontal now — rows carry the years, so no angled labels needed
        axis.ticks         = element_line(colour = "grey70", linewidth = 0.2),
        legend.text        = element_text(size = 11),
        legend.title       = element_text(size = 12, face = "bold"),
        strip.background   = element_rect(fill = "grey95", colour = NA),
        strip.text         = element_text(size = 12, face = "bold"),
        plot.title         = element_text(size = 16, face = "bold", margin = margin(b = 8)),
        panel.grid.major   = element_line(colour = "grey90", linewidth = 0.2),
        panel.grid.minor   = element_blank(),
        panel.spacing      = unit(0.25, "lines"),       # tighter — less dead space between panels
        panel.border       = element_rect(colour = "grey85", fill = NA, linewidth = 0.3),
        legend.position    = "bottom",
        legend.margin      = margin(t = 4),
        plot.margin        = margin(t = 4, r = 6, b = 2, l = 2)  # trims the outer white frame
      )
    
    output_dir <- file.path(
      fig_dir,
      glue::glue("fig_map_{g}.png")
    )
    
    ggsave(output_dir, plot = p, width = out_w, height = out_h, dpi = 400, bg = "white")
    message("  Saved: ", output_dir)
    
    saved_paths[[g]] <- output_dir
    rm(p, spatial_data, gear_data); gc()
  }
  
  return(saved_paths)
}
save_spatial_maps <- function(data, newmap, year_range) {
  
  gears <- list(
    ps = list(code = "s",  name = "purse seine", species = c("bet", "skj", "yft")),
    ll = list(code = "l",  name = "longline",    species = c("bet", "alb", "yft"))
  )
  
  saved_paths <- list()
  
  for (g in names(gears)) {
    
    gear_code_val  <- gears[[g]][["code"]]
    gear_name_val  <- gears[[g]][["name"]]
    gear_species   <- gears[[g]][["species"]]
    
    gear_data <- data |> filter(gear_code == gear_code_val)
    
    if (!has_data(gear_data)) {
      message("  No data for ", gear_name_val, " - skipping")
      next
    }
    
    spatial_data <- gear_data |>
      filter(
        between(year, as.numeric(year_range[1]), as.numeric(year_range[2])),
        sp_code %in% gear_species
      ) |>
      group_by(lat, lon, year, species) |>
      summarise(catch = sum(catch), .groups = "drop") |>
      filter(catch > 0)
    
    if (!has_data(spatial_data)) {
      message("  No spatial data for ", gear_name_val, " - skipping")
      next
    }
    
    p <- ggplot() +
      geom_sf(data = newmap, fill = 'bisque1', colour = 'gray') +
      geom_tile(data = spatial_data, aes(lon, lat, fill = catch)) +
      scale_fill_distiller(palette = "Spectral") +
      scale_x_continuous(breaks = seq(140, 220, 40), expand = c(0, 0)) +
      scale_y_continuous(breaks = seq(-40, 20, 20), expand = c(0, 0)) +
      facet_grid(year ~ species) +
      coord_sf(xlim = c(120, 230), ylim = c(-40, 30), expand = FALSE) +
      xlab("Longitude") + ylab("Latitude") + labs(fill = "Catch (mt)") +
      ggtitle(glue::glue("Spatial patterns in catch for the {gear_name_val} fishery")) +
      theme_bw() +
      theme(
        axis.text        = element_text(size = 14),
        legend.text      = element_text(size = 14),
        legend.title     = element_text(size = 14),
        strip.background = element_rect(fill = 'white'),
        strip.text       = element_text(size = 14),
        axis.title       = element_text(size = 16),
        title            = element_text(size = 16),
        panel.grid.major = element_line(colour = "grey85", linewidth = 0.3),
        panel.grid.minor = element_blank()
      )
    
    output_dir <- file.path(
      fig_dir,
      glue::glue("fig_map_{g}.png")
    )
    
    ggsave(output_dir, plot = p, width = 9, height = 9, dpi = 150, bg = "white")
    message("  Saved: ", output_dir)
    
    saved_paths[[g]] <- output_dir
    rm(p, spatial_data, gear_data); gc()
  }
  
  return(saved_paths)
}

create_maps <- function(data, yrs_range){
  
  # Check if data exists
  if (!has_data(data)) {
    return(invisible(NULL))
  }
  
  sf::sf_use_s2(FALSE)
  
  newmap <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Keep only continents relevant to the Pacific region
  newmap <- newmap[newmap$continent %in% c("Oceania", "Asia"), ]
  
  newmap <- st_make_valid(newmap)
  newmap <- st_shift_longitude(newmap)
  newmap <- st_make_valid(newmap)
  newmap <- st_crop(newmap, xmin = 120, xmax = 230, ymin = -45, ymax = 35)
  
  # Plot just the map to see what's actually there
  plot(st_geometry(newmap), col = 'darkgray', border = 'lightgray')
  
  # Load spatial data
  data <- data |>
    mutate(
      lon = ifelse(lon < 0, lon + 360, lon),
      lat = ifelse(gear_code == "s", lat + 0.5, lat + 2.5),
      lon = ifelse(gear_code == "s", lon + 0.5, lon + 2.5)
    )
  
  # Only generate if data exists
  if (has_data(data)) {
    save_spatial_maps_dynamic(
      data   = data,
      newmap      = newmap,
      year_range = yrs_range
    )
    message("Maps saved for: ", country_code)
  } else {
    message("No spatial data found for: ", country_code, " - skipping maps")
  }
  
  rm(data, newmap); gc()
}

# Function to render SSI (Species of Special Interest) table for a specific gear
render_ssi_table <- function(data, gear_code, r_year) {
  
  if (!has_data(data)) {
    return(invisible(NULL))
  }
  
  save_path <- file.path(tbl_dir, paste0("data_tbl_ssi_", gear_code, ".rds"))
  
  # Filter data for specific gear
  data_clean <- data |>
    filter(gear == gear_code) |>
    select(year, species, category, number, alive, dead) |>
    filter(year >= (r_year - 4)) |>
    filter(year <= r_year) |>
    arrange(year)
  
  # Check if data exists
  if (!has_data(data_clean)) {
    return(invisible(NULL))
  }
  
  saveRDS(data_clean, save_path)
  return(invisible(NULL))
}

render_non_target_table  <- function(data, gear_cd, yrs_long, lst_species) {
  
  if (!has_data(data)) {
    return(invisible(NULL))
  }
  
  save_path <- file.path(tbl_dir, paste0("tbl_non_target_", gear_cd, ".rds"))
  
  data_clean <- data |>
    filter(gear_code == gear_cd,
           year %in% yrs_long,
           !sp_code %in% lst_species) |>  # Exclude only the key species
    group_by(sp_code, year) |>
    summarise(catch = sum(catch, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = year, values_from = catch, values_fill = 0) |>
    arrange(sp_code)
  
  if (has_data(data_clean)) {
    ft <- flextable(data_clean |>
                      rename(Species = sp_code)) |>
      set_table_properties(layout = "autofit", width = 0.95, align = "center") |>
      fontsize(size = 9, part = "all") |>
      width(j = 1, width = 2) |>
      colformat_num(big.mark = ",") |>
      align(align = "center", part = "all") |>
      labelizor(labels = species_labels_all, part = "all") |>
      padding(padding.left = 5, padding.right = 5, part = "all")
    
    saveRDS(ft, save_path)
  }
  
  return(invisible(NULL))
}

# addendum
render_tbl_2009_03 <- function(data, no_rep = "2918", no_cmm = "2009_03") {
  
  if (!has_data(data)) {
    return(invisible(NULL))
  }
  
  save_path <- file.path(tbl_dir, paste0("tbl_", no_cmm, ".rds"))
  
  data <- data |>
    select(-guid, -report_id, -attrs_query)
  
  # Create and return flextable
  ft <- flextable(data |>
                    mutate(swo_mt = round(swo_mt, 0))) |>
    colformat_num(j = "yr", big.mark='') |>
    fontsize(size = 8, part = "all") |>
    set_header_labels(values = list(
                        flag = "Flag",
                        yr = "Year",
                        vessels = "Vessels (n)",
                        swo_n = "SWO (n)",
                        swo_mt = "SWO (mt)"
                      ))
  
  saveRDS(ft, save_path)
  return(invisible(NULL))
}

render_tbl_observer <- function(data, no_rep = "2986", no_cmm = "observer", yy = report_year) {
  
  if (!has_data(data)) {
    return(invisible(NULL))
  }
  
  save_path <- file.path(tbl_dir, paste0("tbl_", no_cmm, ".rds"))
  
  data <- data |>
    select(-guid, -report_id, -attrs_query)
  
  # Create and return flextable
  ft <- flextable(data |>
                    mutate(gear = "L", year = as.character(yy),
                           tot_effort = round(tot_effort/100, 0),
                           hks_obsv = round(hks_obsv/100, 0)) |>
                    select(year, flag = vessel_flag, gear,
                           tot_effort, hks_obsv, hooks_cov,
                           days_fishing, obs_fish_days, fishday_cov,
                           days_at_sea, obs_sea_days, sea_cov,
                           trips_est, obs_trips, trip_cov)) |>
    set_header_labels(
      tot_effort = "hooks",
      hks_obsv = "obs",
      hooks_cov = "cov %",
      days_fishing = 'fish days',
      obs_fish_days = 'obs',
      fishday_cov = 'cov %',
      days_at_sea = 'sea days',
      obs_sea_days = 'obs',
      sea_cov = 'cov %',
      trips_est = 'trips',
      obs_trips = 'obs',
      trip_cov = 'cov %') |>
    add_header_row(top = TRUE, values = c("", "Hooks", "Fishing days", "Sea days", "Trips"),
                   colwidths = c(3,3,3,3,3)) |>
    vline(j = c(3,6,9,12)) |>
    colformat_num(j = "year", big.mark='') |>
    fontsize(size = 8, part = "all") |>
    fit_to_width(8)
  
  saveRDS(ft, save_path)
  return(invisible(NULL))
}

render_tbl_2011_03 <- function(data, no_rep = "3222", no_cmm = "2011_03") {
  
  if (!has_data(data)) {
    return(invisible(NULL))
  }
  
  save_path <- file.path(tbl_dir, paste0("tbl_", no_cmm, ".rds"))
  
  data <- data |>
    select(-guid, -report_id, -attrs_query)
  
  # Create and return flextable
  ft <- flextable(data) |>
    fontsize(size = 8, part = "all")
  
  saveRDS(ft, save_path)
  return(invisible(NULL))
}

### x
groom_2018_03x_df <- function(data, data_compl, ves_calcs, country_cd, min_year, max_year){ # 3317 a6, 3612 a6_1
  
  if (!has_data(data)) {
    return(invisible(NULL))
  }
  if (!has_data(data_compl)) {
    return(invisible(NULL))
  }
  if (!has_data(ves_calcs)) {
    return(invisible(NULL))
  }
  
  res <- data |>
    left_join(data_compl, by = "year", suffix = c("_obs", "_tot")) |>
    select(-total_vessels) |>
    left_join(ves_calcs, by = "year") |>
    mutate(flag_id = country_cd) |>
    select(year, flag_id, nb_vessel,
           hhooks = totalhooks,
           hks_south_of_30s = hks_south_of30s_tot,
           hks_north_of_23n = hks_north_of23n_tot,
           hks_between_25s_30s = hks_between25s30s_tot,
           hks_between_25s_23n = hks_between25s23n_tot,
           south_of_30s = south_of30s,
           north_of_23n = north_of23n,
           between_25s_23n = between25s23n,
           between_25s_30s = between25s30s,
           hooks_obs = hooks_observed,
           n_birds = brd_no,
           hks_obs_south_of_30s = hks_south_of30s_obs,
           hks_obs_north_of_23n = hks_north_of23n_obs,
           hks_obs_between_25s_30s = hks_between25s30s_obs,
           hks_obs_between_25s_23n = hks_between25s23n_obs,
           capture_rate,
           capture_rate_south_of_30s = capt_rate_south_of30s,
           capture_rate_north_of_23n = capt_rate_north_of23n,
           capture_rate_between_25s_23n = capt_rate_between25s23n,
           capture_rate_between_25s_30s = capt_rate_between25s30s) |>
    filter(year >= min_year, year <= max_year) |>
    arrange(year)
  
  return(res)
}

extract_2018_03_area_x <- function(data, suffix) {
  hks_col     <- paste0("hks_", suffix)
  hks_obs_col <- paste0("hks_obs_", suffix)
  birds_col   <- suffix
  cr_col      <- paste0("capture_rate_", suffix)
  
  data |>
    select(year, flag_id, nb_vessel,
           hhooks       = all_of(hks_col),
           hhooks_obs   = all_of(hks_obs_col),
           n_birds      = all_of(birds_col),
           capture_rate = all_of(cr_col)) |>
    mutate(capture_rate = round(capture_rate, 4))
}

render_tbl_2018_03x <- function(data, no_cmm = "2018_03x"){
  
  if (!has_data(data)) {
    return(invisible(NULL))
  }
  
  save_path <- file.path(tbl_dir, paste0("tbl_", no_cmm, ".rds"))
  
  ft <- flextable(data |>  mutate(capture_rate = round(capture_rate, 4)) |>
                    select(year, flag_id, nb_vessel, hhooks, hhooks_obs = hooks_obs, n_birds, capture_rate)) |> 
    colformat_num(j = "year", big.mark='') |> 
    fontsize(size = 8, part = "all") |>
    set_header_labels(values = list(
      flag_id = "Flag",
      year = "Year",
      nb_vessel = "Vessels (n)",
      hhooks = "Hhooks (logbook)",
      hhooks_obs = "Hhooks (observer)",
      n_birds = "Birds (n)",
      capture_rate = "Capture rate"
    ))
  
  saveRDS(ft, save_path)
  return(invisible(NULL))
}

render_tbl_2018_03x_areas <- function(data, area_defs = area_defs_default) {
  if (!has_data(data)) {
    return(invisible(NULL))
  }
  
  seabird_areas <- purrr::map(area_defs, ~ extract_2018_03_area_x(data, .x))
  
  for (i in 1:length(seabird_areas)){
    
    save_path <- file.path(tbl_dir, paste0("tbl_2018_03_x_", as.character(names(seabird_areas[i])), ".rds"))
    
    ft <- flextable(seabird_areas[[i]]) |>
      colformat_num(j = "year", big.mark='') |>
      width(j = 7, width = 1.2) |>
      set_header_labels(values = list(
        flag_id = "Flag",
        year = "Year",
        nb_vessel = "Vessels (n)",
        hhooks = "Hhooks (logbook)",
        hhooks_obs = "Hhooks (observer)",
        n_birds = "Birds (n)",
        capture_rate = "Capture rate"
      )) |>
      fontsize(size = 8, part = "all")
    
    saveRDS(ft, save_path)
  }
  return(invisible(NULL))
}

### y
groom_2018_03y_df <- function(data) {
  data |>
    rename(reqs = requirements,
           mitigation = comb_mitigation,
           s30s = south_of30s,             s30s_pct = south_of30s_pct,
           b25s_30s = between25s30s,       b25s_30s_pct = between25s30s_pct,
           b25s_23n = between25s23n,       b25s_23n_pct = between25s23n_pct,
           n23n = north_of23n,             n23n_pct = north_of23n_pct) |>
    mutate(across(ends_with("_pct"), ~ round(., 1))) |>
    mutate(across(6:13, ~ ifelse(is.na(.), 0, .)))
}

extract_2018_03_area_y <- function(data, prefix) {
  sets_col <- prefix
  pct_col  <- paste0(prefix, "_pct")
  
  data |>
    select(year, fleet, reqs, mitigation,
           sets    = all_of(sets_col),
           percent = all_of(pct_col))
}

render_tbl_2018_03y_areas <- function(data, area_prefixes = default_area_prefixes) {
  
  if (!has_data(data)) {
    return(invisible(NULL))
  }
  
  data_2018_03 <- purrr::map(area_prefixes, ~ extract_2018_03_area_y(data, .x))
  
  for (i in 1:length(data_2018_03)){
    save_path <- file.path(tbl_dir, paste0("tbl_2018_03_y_", as.character(names(data_2018_03[i])), ".rds"))
    
    ft <- flextable(data_2018_03[[i]]) |>
      colformat_num(j = "year", big.mark='') |>
      width(j = c(3,4), width = c(1.2, 1.2)) |>
      fontsize(size = 8, part = "all") |>
      set_header_labels(values = list(
        fleet = "Fleet",
        year = "Year",
        reqs = "Requirements",
        mitigation = "Mitigation",
        sets = "Sets",
        percent = "Percent"
      ))
    
    saveRDS(ft, save_path)
  }
  return(invisible(NULL))
}

### others
render_tbl_2018_03z <- function(data, no_rep = "3314", no_cmm = "2018_03z"){
  
  if (!has_data(data)) {
    return(invisible(NULL))
  }
  
  save_path <- file.path(tbl_dir, paste0("tbl_", no_cmm, ".rds"))
  
  data <- data |>
    select(-guid, -report_id, -attrs_query)
  
  ft <- flextable(data |> rename(n_birds = total_number)) |>
    colformat_num(j = "year", big.mark='') |>
    fontsize(size = 8, part = "all") |>
    set_header_labels(values = list(
      species = "Species",
      year = "Year",
      south30s = "South of 30S",
      btwn25s30s = "25S - 30S",
      btwn23n25s = "23N - 25S",
      north23n = "North of 23N",
      n_birds = "Birds (n)"
    ))  |>
    fit_to_width(8)
  
  saveRDS(ft, save_path)
  return(invisible(NULL))
}

# 2917
render_tbl_2006_04 <- function(data, no_rep = "2917", no_cmm = "2006_04"){
  
  if (!has_data(data)) {
    return(invisible(NULL))
  }
  
  save_path <- file.path(tbl_dir, paste0("tbl_", no_cmm, ".rds"))
  
  data <- data |>
    select(-guid, -report_id, -attrs_query)
  
  ft <- flextable(data |>
                    mutate(weight_kg = round(weight_kg, 1))) |>
    set_header_labels(
      values = list(
        flag = "Flag",
        yr = "Year",
        numvessels = "Vessels",
        # daysfishing = "Fishing days",
        numcaught = "MLS (n)",
        weight_kg = 'MLS (Kg)')) |>
    fontsize(size = 8, part = "all")
  
  saveRDS(ft, save_path)
  return(invisible(NULL))
}

# 3602 - 2019-03
render_tbl_2019_03 <- function(data, no_rep = "3602", no_cmm = "2019_03"){
  
  if (!has_data(data)) {
    return(invisible(NULL))
  }
  
  save_path <- file.path(tbl_dir, paste0("tbl_", no_cmm, ".rds"))
  
  data <- data |>
    select(-guid, -report_id, -attrs_query)
  
  ft <- flextable(data) |>
    set_header_labels(
      values = list(
        flag = "Flag",
        yr = "Year",
        gear = "Gear",
        numvessels = "Vessels",
        daysfishing = "Fishing days",
        numcaught = "ALB (n)",
        weightcaught_mt = 'ALB (Kg)')) |>
    fontsize(size = 8, part = "all")
  
  saveRDS(ft, save_path)
  return(invisible(NULL))
}

# 3513 - 2023-03
render_tbl_2023_03 <- function(data, no_rep = "3513", no_cmm = "2023_03"){
  
  if (!has_data(data)) {
    return(invisible(NULL))
  }
  
  data <- data |>
    select(-guid, -report_id, -attrs_query)
  
  save_path <- file.path(tbl_dir, paste0("tbl_", no_cmm, ".rds"))
  
  ft <- flextable(data) |>
    
    set_header_labels(values = list(
      flag = "Flag",
      yr = "Year",
      vessels = "Vessels (n)",
      swo_n = "SWO (n)",
      swo_mt = "SWO (mt)"
    )) |>
    fontsize(size = 8, part = "all")
  
  saveRDS(ft, save_path)
  return(invisible(NULL))
}

# 2955 - vu - 2013-08
render_tbl_2013_08 <- function(data, no_rep = "2955", no_cmm = "2013_08", member = country_code){
  
  if (!has_data(data)) {
    return(invisible(NULL))
  }
  
  save_path <- file.path(tbl_dir, paste0("tbl_", no_cmm, ".rds"))
  
  data <- data |>
    select(-guid, -report_id, -attrs_query)
  
  ft <- flextable(data |>
                    mutate(across(where(is.character), ~tolower(.))) |>
                    filter(flag == tolower(member)) |>
                    select(-flag, -number_est, -weight_est, -lat, -lon)) |>
    set_header_labels(values = list(
      gear = "Gear",
      species = "Species",
      number = "FAL (n)",
      weight = "FAL (Kg)",
      date = "Date",
      eez = "EEZ",
      fate = "Fate"
      )) |>
    
    fontsize(size = 8, part = "all")
  
  saveRDS(ft, save_path)
  return(invisible(NULL))
}

# 2942 - vu - 2012-04
render_tbl_2942 <- function(data, no_rep = "2942", no_cmm = "2012_04", member = country_code){
  
  if (!has_data(data)) {
    return(invisible(NULL))
  }
  
  save_path <- file.path(tbl_dir, paste0("tbl_", no_cmm, ".rds"))
  
  data <- data |>
    select(-guid, -report_id, -attrs_query)
  
  ft <- flextable(data |>
                    mutate(across(where(is.character), ~tolower(.))) |>
                    filter(flag == tolower(member)) |>
                    mutate(alive = coalesce(alive_healthy, alive_injured, alive_unknown)) |>
                    rename(number = individuals) |>
                    select(-flag, -lat, -lon, -activity_code, -species, -vesselname, -n_estimate,
                           -alive_healthy, -alive_injured, -alive_unknown, -fate, -type)) |>
    set_header_labels(values = list(
      gear = "Gear",
      number = "RHN (n)",
      mt = "RHN (mt)",
      date = "Date",
      eez = "EEZ",
      dead = "Dead",
      unknown = "Unknown",
      alive = "Alive"
    )) |>
    fontsize(size = 8, part = "all")
  
  saveRDS(ft, save_path)
  return(invisible(NULL))
}

# 2956 - vu - 2011-04
render_tbl_2956 <- function(data, no_rep = "2956", no_cmm = "2011_04", member = country_code){
  
  if (!has_data(data)) {
    return(invisible(NULL))
  }
  
  save_path <- file.path(tbl_dir, paste0("tbl_", no_cmm, ".rds"))
  
  data <- data |>
    select(-guid, -report_id, -attrs_query)
  ft <- flextable(data |>
                    mutate(across(where(is.character), ~tolower(.))) |>
                    filter(flag == tolower(member)) |>
                    mutate(alive = coalesce(alive_healthy, alive_injured, alive_unknown)) |>
                    rename(number = individuals) |>
                    select(-flag, -lat, -lon, -activity_code, -species, -vesselname, -n_estimate,
                           -alive_healthy, -alive_injured, -alive_unknown)) |>
    fontsize(size = 8, part = "all")

  saveRDS(ft, save_path)
  return(invisible(NULL))
}

# 2939 - vu - 2010-07
render_tbl_2010_07 <- function(data, no_rep = "2939", no_cmm = "2010_07", member = country_code){
  
  if (!has_data(data)) {
    return(invisible(NULL))
  }
  
  save_path <- file.path(tbl_dir, paste0("tbl_", no_cmm, ".rds"))
  
  data <- data |>
    select(-guid, -report_id, -attrs_query)
  
  ft <- flextable(data |>
                    mutate(across(where(is.character), ~tolower(.))) |>
                    select(-aws)) |>
    set_header_labels(values = list(
      gear = "Gear",
      species = "Species",
      number = "SKH (n)",
      retain = "Retain",
      discard = "Discard",
      unknown = "Unknown",
      finned_retain = "Finned retain",
      finned_trunk_discard = "Finned trunk discard"
    )) |>
    fontsize(size = 8, part = "all")
  
  saveRDS(ft, save_path)
  return(invisible(NULL))
}

## Functions: CMM applicability (addendum.qmd) ###########################

# `not_applicable_measures` in config.yaml is a named map: CMM code ->
# reason text (e.g. "2009-03": "..."). A code absent from the map, or
# the whole map absent/"none", is treated as applicable - matching the
# YAML tag's stated default of "none means all CMMs apply".
cmm_status <- function(cmm_code) {
  reasons <- config$not_applicable_measures
  
  if (is.null(reasons) || identical(reasons, "none") || !(cmm_code %in% names(reasons))) {
    return(list(applicable = TRUE, reason = NULL))
  }
  
  reason <- reasons[[cmm_code]]
  if (is.null(reason) || !nzchar(reason)) {
    reason <- glue::glue("This measure does not apply to {country_name}.")
  } else {
    reason <- glue::glue(reason)  # lets the YAML text itself use {country_name}
  }
  
  list(applicable = FALSE, reason = reason)
}

# Prints a single sub-item's "not applicable" note (the a/b/c bullets
# under a CMM paragraph). `text` is the reason specific to *this*
# sub-item - sub-items under one CMM can describe different things, so
# this isn't reused from cmm_status()'s whole-CMM reason.
cmm_subitem_note <- function(cmm_code, text) {
  if (!cmm_status(cmm_code)$applicable) {
    cat(glue::glue("- **No report available** — {text}\n\n"))
  }
  invisible(NULL)
}

# Prints exactly one of three things for a CMM's supporting figure:
# the not-applicable reason, a generic "no data" note (applicable but
# the file isn't there), or the image itself. Never prints nothing,
# never emits a broken image tag, and never references a variable that
# might not exist - which is what the old eval-gated chunk + separate
# `r msg` line could do.
cmm_render_figure <- function(cmm_code, img_path, caption, ref_id) {
  status <- cmm_status(cmm_code)

  if (!status$applicable) {
    cat(glue::glue("**{status$reason}**\n\n"))
    return(invisible(NULL))
  }
  
  if (!file.exists(img_path)) {
    cat("*No data reported for this measure in the current reporting year.*\n\n")
    return(invisible(NULL))
  }
  
  cat(glue::glue("![{caption}]({img_path}){{#{ref_id} fig-align='center' width=90%}}\n\n"))
  invisible(NULL)
}

# Companion to cmm_render_figure(), for CMM tables that have been moved
# off PNGs and onto flextable .rds files (see the "PNG -> rds" note on
# render_gear_tbl_section3() above for why).
#
# The actual table now lives in its own labelled chunk in the .qmd
# (`#| label: tbl-xxx` / `#| tbl-cap: ...`), evaluated only when
# `cmm_status(cmm_code)$applicable && file.exists(rds_path)` — that's
# what gives the table a proper Quarto/LaTeX crossref instead of a
# hand-built markdown image tag. This function covers the other two
# branches (not applicable / applicable but no data), and is called
# from a second chunk with the *inverse* `eval` condition, exactly
# mirroring how cmm_subitem_note() already handles the a/b/c bullets
# elsewhere in this document. It only ever prints text — never a table
# — so it doesn't need (or use) a tbl-cap/label itself.
cmm_table_note <- function(cmm_code, rds_path, msg = "No data available for this reporting year.") {
  status <- cmm_status(cmm_code)

  if (!status$applicable) {
    cat(glue::glue("**{status$reason}**\n\n"))
    return(invisible(NULL))
  }

  if (!file.exists(rds_path)) {
    
    cat(glue::glue("**{msg}**\n\n"))
    
    return(invisible(NULL))
  }

  invisible(NULL)
}

# Coastal funcs ####
get_country_eez <- function(iso2_code) {
  iso3 <- countrycode(iso2_code, "iso2c", "iso3c", warn = FALSE)
  
  if (is.na(iso3)) {
    stop("Could not resolve ISO-2 code '", iso2_code, "' to ISO-3.")
  }
  
  eez <- mrp_get(
    "eez",
    cql_filter = sprintf("iso_ter1 = '%s' AND pol_type = '200NM'", iso3)
  )
  
  if (nrow(eez) == 0) {
    warning("No EEZ found for '", iso2_code, "' (ISO-3: ", iso3, ").")
  }
  
  eez
}

# Helper: build a gridded species-composition catch summary for one EEZ
grid_catch <- function(data, eez_code, cell_size = 1,
                       species = c("skj", "yft", "bet")) {
  
  species_cols <- paste0(species, "_mt")
  half <- cell_size / 2
  
  data |>
    # filter(eez == eez_code) |>
    mutate(
      lat_bin = floor(latd / cell_size) * cell_size + half,
      lon_bin = floor(lond / cell_size) * cell_size + half
    ) |>
    group_by(lon_bin, lat_bin) |>
    summarise(across(all_of(species_cols), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") |>
    rename_with(\(x) str_remove(x, "_mt$"), all_of(species_cols)) |>
    mutate(total = rowSums(across(all_of(species)))) |>
    filter(total > 0)
}

render_coastal_eez_all_flags <- function(data){
  
  if (!has_data(data)) {
    return(invisible(NULL))
  }
  
  save_path <- file.path(tbl_dir, paste0("tbl_coastal_eez_all_flags.rds"))
  
  # Define the full set of possible columns and their labels
  all_mt_cols  <- c("alb_mt", "bet_mt", "yft_mt", "skj_mt", "oth_mt", "tot_mt")
  all_int_cols <- c("year", "vessels", "trips", "fish_days")
  
  all_labels <- list(
    gear      = "Gear",
    year      = "Year",
    vessels   = "Vessels",
    trips     = "Trips",
    fish_days = "Days",
    alb_mt    = "Alb (mt)",
    bet_mt    = "Bet (mt)",
    yft_mt    = "Yft (mt)",
    skj_mt    = "Skj (mt)",
    oth_mt    = "Others (mt)",
    tot_mt    = "Total (mt)"
  )
  
  # Filter to only columns that actually exist in this run's data
  mt_cols     <- intersect(all_mt_cols,  names(data))
  int_cols    <- intersect(all_int_cols, names(data))
  labels_use  <- all_labels[intersect(names(all_labels), names(data))]
  
  # Build relocate list dynamically — only push columns to the end if they exist
  relocate_cols <- intersect(c("oth_mt", "tot_mt"), names(data))
  
  ft <- data |>
    relocate(all_of(relocate_cols), .after = last_col()) |>
    mutate(year = as.integer(year)) |>
    # apply sign based on hemisphere
    mutate(gear = dplyr::case_when(
      gear %in% c("Longline") ~  "LL",
      gear %in% c("Purse seine") ~ "PS",
      TRUE ~ gear
    )) |>
    as_grouped_data(groups = "gear") |>
    flextable() |>
    set_header_labels(values = labels_use) |>
    bold(i = ~ !is.na(gear)) |>
    fontsize(size = 8, part = "all") |>
    fit_to_width(max_width = 6.5) |>
    theme_booktabs() |>
    align(align = "right", part = "all") |>
    align(j = "gear", align = "left", part = "all")
  
  ft <- data |>
    relocate(all_of(relocate_cols), .after = last_col()) |>
    mutate(year = as.integer(year)) |>
    # mutate(gear = dplyr::case_when(
    #   gear %in% c("Longline")    ~ "LL",
    #   gear %in% c("Purse seine") ~ "PS",
    #   TRUE ~ gear
    # )) |>
    select(gear, everything()) |>
    flextable() |>
    set_header_labels(values = labels_use) |>
    merge_v(j = "gear") |>
    valign(j = "gear", valign = "top", part = "body") |>
    fix_border_issues() |>
    fontsize(size = 8, part = "all") |>
    fit_to_width(max_width = 6.5) |>
    theme_booktabs() |>
    align(align = "right", part = "all") |>
    align(j = "gear", align = "left", part = "all")
  
  if (length(mt_cols) > 0) {
    ft <- ft |> colformat_double(j = mt_cols, digits = 0, na_str = "–")
  }
  if (length(int_cols) > 0) {
    ft <- ft |> colformat_int(j = int_cols)
  }
  
  ft
  
  saveRDS(ft, save_path)
  return(invisible(NULL))
}

render_coastal_eez_perc_catch <- function(data){
  
  if (!has_data(data)) {
    return(invisible(NULL))
  }
  
  save_path <- file.path(fig_dir, paste0("tbl_coastal_eez_perc_catch.png"))
  
  plot_data <- data |> 
    mutate(flag = toupper(flag),
           flag = forcats::fct_reorder(flag, tot_mt, .fun = sum, .desc = TRUE)
    )
  
  
  flags <- sort(unique(plot_data$flag))
  
  flag_labels <- setNames(stringr::str_to_title(flag_lookup$name_pretty),
                          flag_lookup$country_code)
  flag_labels <- c(flag_labels, OTHER = "Other")
  
  # fall back to the code itself for anything not in the lookup (e.g. OTHER)
  flag_labels[is.na(flag_labels)] <- names(flag_labels)[is.na(flag_labels)]
  
  p <- ggplot(plot_data, aes(x = factor(year), y = tot_mt, fill = flag)) +
    geom_col(position = "fill", colour = "white", linewidth = 0.3) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_hue(labels = flag_labels, c = 50) +
    facet_wrap(~ gear) +
    labs(x = NULL, y = "Share of catch", fill = "Country") +
    theme(
    axis.title   = element_text(size = 16),
    axis.text    = element_text(size = 16),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    strip.text   = element_text(size = 16),
    legend.position = "bottom",
    legend.title = element_text(size = 13),
    legend.text  = element_text(size = 13)
  )
  
  ggsave(save_path, plot = p,  width = 12, height = 7, dpi = 300, bg = "white")
  
  return(invisible(NULL))
}

render_coastal_eez_mt_catch <- function(data){
  
  if (!has_data(data)) {
    return(invisible(NULL))
  }
  
  save_path <- file.path(fig_dir, paste0("tbl_coastal_eez_mt_catch.png"))
  
  # labels built from the full lookup, so every flag in any plot is covered
  flag_labels <- setNames(stringr::str_to_title(flag_lookup$name_pretty),
                          flag_lookup$country_code)
  # quick guard: any species missing from the colour palette?
  missing_sp <- setdiff(unique(data$species), names(sp_colours))
  if (length(missing_sp) > 0) {
    warning("Species missing from sp_colours: ", paste(missing_sp, collapse = ", "))
  }
  
  p <- data |>
    mutate(
      flag    = toupper(flag),
      # order flags by total catch (ascending = biggest at top after flip)
      flag    = forcats::fct_reorder(flag, mt, .fun = sum),
      # push "Other" to the end of the legend
      species = forcats::fct_relevel(factor(species), "Other", after = Inf)
    ) |>
    ggplot(aes(x = mt, y = flag, fill = species)) +
    geom_col(position = position_stack(reverse = TRUE)) +
    scale_y_discrete(labels = flag_labels) +
    scale_x_continuous(labels = scales::comma) +
    scale_fill_manual(values = sp_colours, na.value = "grey80") +
    facet_wrap(~ gear) +
    labs(x = "Catch (mt)", y = NULL, fill = "Species") +
    theme(
      axis.title   = element_text(size = 16),
      axis.text    = element_text(size = 16),
      axis.text.x  = element_text(angle = 45, hjust = 1),
      strip.text   = element_text(size = 16),
      legend.position = "bottom",
      legend.title = element_text(size = 13),
      legend.text  = element_text(size = 13)
    )
  
  ggsave(save_path, plot = p,  width = 12, height = 7, dpi = 300, bg = "white")
  
  return(invisible(NULL))
}

render_coastal_map <- function(data, 
                               country_cd = country_code, 
                               gear_code = "s", 
                               additional_data_l = NULL){ # data_02 = all_data$data_2894
  
  old_s2 <- sf_use_s2()
  sf_use_s2(FALSE)
  on.exit(sf_use_s2(old_s2), add = TRUE)
  
  if (!has_data(data)) {
    return(invisible(NULL))
  }
  
  if (!is.null(additional_data_l)){
    if (!has_data(additional_data_l)) {
      return(invisible(NULL))
    }
  }
  
  if (!(gear_code %in% c("s", "l"))){
    stop("Accepted gear_code : l, s")
  }else if(gear_code == "l"){
    species_map <- c(
      alb = "Albacore",
      skj = "Skipjack tuna",
      yft = "Yellowfin tuna",
      bet = "Bigeye tuna"
    )
    
    data <- data |>
      mutate(
        latd = parse_coord(latitude,  "lat"),
        lond = parse_coord(longitude, "lon")
      ) |>
      select(flag, vessel_name, log_date, latd, lond, species, mt) |>
      distinct() |>
      filter(species %in% names(species_map)) |>
      summarise(mt = sum(mt, na.rm = TRUE), .by = c(flag, vessel_name, log_date, latd, lond, species)) |>
      pivot_wider(names_from = species, values_from = mt) |>
      mutate(year = substr(log_date, 7, 10)) |>
      filter(year %in% report_year) |>
      rename_with(\(x) paste0(x, "_mt"), any_of(names(species_map)))
    
  }else if(gear_code == "s"){
    species_map <- c(
      skj = "Skipjack tuna",
      yft = "Yellowfin tuna",
      bet = "Bigeye tuna"
    )
  }
  
  save_path <- file.path(fig_dir, paste0("tbl_coastal_map_", gear_code ,".png"))
  
  eez <- get_country_eez(country_cd) |>
    st_make_valid() |>
    st_shift_longitude() |>
    st_set_crs(NA) |>
    st_union() |>
    st_make_valid()
  
  bbox <- st_bbox(eez)
  
  land <- suppressWarnings(
    ne_countries(scale = "medium", returnclass = "sf") |>
      filter(continent %in% c("Oceania", "Asia", "North America", "South America")) |>
      st_make_valid() |>
      st_shift_longitude() |>
      st_make_valid() |>
      st_crop(bbox + c(-2, -2, 2, 2)) |>
      st_set_crs(NA)
  )
  
  missing_sp <- setdiff(species_map, names(sp_colours))
  if (length(missing_sp) > 0) {
    warning("Species missing from sp_colours: ",
            paste(missing_sp, collapse = ", "))
  }
  
  catch_grid <- grid_catch(data,
                           eez_code  = tolower(country_cd),
                           cell_size = 1,
                           species   = names(species_map))
  
  
  # Align pie longitudes with the 0-360 convention (no-op if already 0-360)
  catch_grid <- catch_grid |>
    mutate(lon_bin = ifelse(lon_bin < 0, lon_bin + 360, lon_bin)) |>
    filter(total > 0)
  
  # only draw if there is anything to draw (grid_catch can return zero
  # usable cells even when data_2904 has rows)
  if (nrow(catch_grid) > 0) {
    
    max_r <- 0.45
    catch_grid <- catch_grid |>
      mutate(radius = max_r * sqrt(total / max(total)))
    
    # sanity check: pies should fall within the map extent; if not, the
    # longitude conventions of grid_catch and the shifted EEZ disagree
    out_of_extent <- catch_grid |>
      filter(lon_bin < bbox["xmin"] - 2 | lon_bin > bbox["xmax"] + 2 |
               lat_bin < bbox["ymin"] - 2 | lat_bin > bbox["ymax"] + 2)
    if (nrow(out_of_extent) > 0) {
      warning(country_cd, ": ", nrow(out_of_extent),
              " grid cell(s) fall outside the EEZ map extent and will not be visible.")
    }
    
    # Fill values and labels from the lookup
    fill_values <- setNames(sp_colours[species_map], names(species_map))
    fill_labels <- setNames(species_map, names(species_map))
    
    x_span <- as.numeric(bbox["xmax"] - bbox["xmin"])
    step   <- max(1, round(x_span / 8))
    xbr    <- seq(floor(bbox["xmin"]), ceiling(bbox["xmax"]), by = step)
    lon_lab <- function(x) {
      x <- x %% 360
      ifelse(x == 180, "180\u00B0",
             ifelse(x > 180, paste0(360 - x, "\u00B0W"),
                    paste0(x, "\u00B0E")))
    }
    lat_lab <- function(y) {
      ifelse(y < 0, paste0(abs(y), "\u00B0S"),
             ifelse(y > 0, paste0(y, "\u00B0N"), "0\u00B0"))
    }
    
    # ── Aspect ratio: with no CRS, coord_sf assumes projected coordinates
    #    (1 deg lon = 1 deg lat on screen). Correct for longitude
    #    convergence at the EEZ's mid-latitude so shapes aren't stretched
    #    E-W — matters for the higher-latitude PICTs (CK, PF, NU, ...).
    mid_lat <- mean(c(bbox["ymin"], bbox["ymax"]))
    y_span  <- as.numeric(bbox["ymax"] - bbox["ymin"])
    asp     <- (y_span / x_span) / cos(mid_lat * pi / 180)
    
    p <- ggplot() +
      geom_sf(data = land, fill = "grey90", colour = "grey60", linewidth = 0.2) +
      geom_sf(data = eez,  fill = "grey50", colour = "black", alpha = 0.4) +
      geom_scatterpie(
        data   = catch_grid,
        aes(x = lon_bin, y = lat_bin, r = radius),
        cols   = names(species_map),
        colour = NA,
        alpha  = 0.9
      ) +
      scale_fill_manual(
        values = fill_values,
        labels = fill_labels,
        name   = "Species"
      ) +
      scale_x_continuous(breaks = xbr, labels = lon_lab) +
      scale_y_continuous(labels = lat_lab) +
      coord_sf(
        xlim   = c(bbox["xmin"], bbox["xmax"]),
        ylim   = c(bbox["ymin"], bbox["ymax"]),
        expand = FALSE
      ) +
      labs(x = "Longitude", y = "Latitude") +
      theme_bw() +
      theme(legend.position = "bottom",
            aspect.ratio = asp,
            axis.title   = element_text(size = 16),
            axis.text    = element_text(size = 16),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 16),
            axis.text.y = element_text(size = 16),
            panel.background = element_rect(fill = "white"),
            legend.title = element_text(size = 13),
            legend.text  = element_text(size = 13)
      )
    
    ggsave(save_path, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
    
    
  } else {
    message(country_cd, ": no positive-catch grid cells — spatial figure skipped.")
  }
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

