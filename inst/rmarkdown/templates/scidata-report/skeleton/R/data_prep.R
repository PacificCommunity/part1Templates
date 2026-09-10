# Expects country_code, report_year, refresh_data to already be set
# (part1-report.qmd's setup chunk does this from its `params`).
library(rnaturalearth)
library(sf)
source("R/utils.R")

## Download (or reuse cached) raw report data ##########################
# refresh_data controls tuf_get_report_bundle()'s overwrite argument:
bundle <- tuf_read_bundle(
  "R/scidata_bundle.yaml",
  report_year = report_year,
  country_code = country_code
)
tuf_get_report_bundle(bundle, overwrite = refresh_data)

## Read data ############################################################
all_files <- paste0(base_dir, list.files(base_dir, recursive = TRUE))
all_files <- all_files[tools::file_ext(all_files) == "csv"]

all_data <- lapply(all_files, read.csv)
names(all_data) <- sub("^[a-z]+_", "data_", tools::file_path_sans_ext(basename(all_files)))

all_data <- lapply(all_data, function(data) {
  data |>
    janitor::clean_names() |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character), tolower))
})

## Apply functions and generate outputs ##################################

all_data <- groom_data(all_data = all_data, report_year = report_year)

### Flag state reporting ####
ft_ps <- render_gear_tbl_section3(all_data$data_3605_ps, "s", ps_species_labels, refresh_data = refresh_data)

ft_ll <- render_gear_tbl_section3(all_data$data_3605_ll, "l", ll_species_labels, refresh_data = refresh_data)

acef_ps <- render_gear_figs_section3(data = all_data$data_3605_ps,
                                     gear_code = "s",
                                     species_list = ps_species,
                                     species_labels = ps_species_labels,
                                     country_cd = tolower(country_code),
                                     yrs_long = yrs_long)
acef_ll <- render_gear_figs_section3(data = all_data$data_3605_ll,
                                     gear_code = "l",
                                     species_list = ll_species,
                                     species_labels = ll_species_labels,
                                     country_cd = tolower(country_code),
                                     yrs_long = yrs_long)
acef_vessels <- render_vessels_figs_section3(data = all_data$data_3605,
                                             yrs_long = yrs_long)

ace_other_areas <- render_other_areas_tbl(data_01 = all_data$data_3609,
                                      data_02 = all_data$data_3083,
                                      data_03 = all_data$data_3616
                                      )

render_vessel_table(
  ves_data = all_data$ves_raw,
  gear_cd = "l",
  size_categories_fn = ll_size_fn
)

render_vessel_table(
  ves_data = all_data$ves_raw,
  gear_cd = "s",
  size_categories_fn = ps_size_fn
)

create_maps(data = all_data$data_3608, yrs_range = c(report_year - 4, report_year))

render_ssi_table(data = all_data$data_2953, gear_code = "s", r_year = report_year)
render_ssi_table(data = all_data$data_2953, gear_code = "l", r_year= report_year)

render_non_target_table(all_data$data_3605, gear_cd = "l", yrs_long, lst_species = ll_species)
render_non_target_table(all_data$data_3605, gear_cd = "s", yrs_long, lst_species = ps_species)

### Coastal State reporting ####

render_coastal_eez_all_flags(data = all_data$flag_summary)
render_coastal_eez_perc_catch(data = all_data$plot_data1)
render_coastal_eez_mt_catch(data = all_data$plot_data2)

render_coastal_map(data =  all_data$data_2904, 
                   country_cd = country_code, 
                   gear_code = "s", 
                   additional_data_l = NULL)

render_coastal_map(data =  all_data$data_3375, 
                   country_cd = country_code, 
                   gear_code = "l", 
                   additional_data_l = all_data$data_2894)

#### Artisanal ACE

report_ids_ikasavea <- c("b1559368-b7a3-464e-883a-34fe3d2cd7c0")

if (length(report_ids_ikasavea) > 0 &&
    nzchar(Sys.getenv("IKA_USER_NAME")) &&
    nzchar(Sys.getenv("IKA_PASSWORD"))) {
  download_ikasavea_data(
    country_code = country_code,
    report_ids = report_ids_ikasavea,
    folder_path = ikasavea_folder,
    r_year = report_year
  )
}

# prepare ikasavea data if it exists
if (is.null(ika_data) && nrow(all_data$data_3615) == 0) {
  data_source_lst = c()
}else{
  data_source_lst = c("tufman2")
}

ika_data <- prep_ikasavea_artisanal_inputs(ikasavea_folder, report_year, data_source_lst)

if (is.null(ika_data) && nrow(all_data$data_3615) == 0) {
  cat("No Ikasavea or Tails trip data found for this country/year. Document generation stopped.\n")
  knitr::knit_exit()
}else{
  # try to extract the local knowledge table
  if (length(list.files(art_est_trips_folder)) == 1){
    local_knowl_trips_file = paste0(art_est_trips_folder, "/", list.files(art_est_trips_folder)[1])
  }else{
    local_knowl_trips_file = NULL
  }
  
  if (is.null(ika_data)) {
    artisanal_data_sources <- data_source_lst        # ikasavea failed/absent - just tufman2 (or empty)
  } else {
    artisanal_data_sources <- ika_data$data_source_lst # ikasavea succeeded - has both, if applicable
  }
  
  # Calculate ACE
  artisanal_ace <- prep_artisanal_ace(
    data_3615 = all_data$data_3615,
    data_3614 = all_data$data_3614,
    df_ika_wide = ika_data$df_ika_wide,
    data_ika_catch_kg = ika_data$data_ika_catch_kg,
    local_knowl_trips_file = local_knowl_trips_file
  )
  
}

### Addendum ####

render_tbl_2009_03(data = all_data$data_2918, no_rep = "2918", no_cmm = "2009_03")
render_tbl_observer(data = all_data$data_2986, no_rep = "2986", no_cmm = "observer")
render_tbl_2011_03(data = all_data$data_3222, no_rep = "3222", no_cmm = "2011_03")

# Seabirds
## Table x
data_tbl_x <- groom_2018_03x_df(data = all_data$data_3317,
                               data_compl = all_data$data_3612,
                               ves_calcs = all_data$ves_calcs,
                               country_cd = country_code,
                               min_year = min(yrs_long),
                               max_year = max(yrs_long))

# render_tbl_2018_03x(data = data_tbl_x, no_cmm = "2018_03x") # maybe include this, and a similar table for y and z too

### Table x - by area
render_tbl_2018_03x_areas(data_tbl_x)

# Table y  
data_tbl_y <- groom_2018_03y_df(all_data$data_3315)

render_tbl_2018_03y_areas(data = data_tbl_y)

# 3314
render_tbl_2018_03z(data = all_data$data_3314, no_rep = "3314", no_cmm = "2018_03z")

# 2917
render_tbl_2006_04(data = all_data$data_2917, no_rep = "2917", no_cmm = "2006_04")

# 2015-02 - always missing... 

# 3602 - 2019-03
render_tbl_2019_03(data = all_data$data_3602, no_rep = "3602", no_cmm = "2019_03")

# 3513 - 2023-03
render_tbl_2023_03(data = all_data$data_3513, no_rep = "3513", no_cmm = "2023_03")

if(tolower(country_code) == "vu"){
  # 2955 - vu - 2013-08
  render_tbl_2013_08(data = all_data$data_2955, no_rep = "2955", no_cmm = "2013_08")
  
  # 2942 - vu - 2012-04
  render_tbl_2942(data = all_data$data_2942, no_rep = "2942", no_cmm = "2012_04")
  
  # 2956 - vu - 2011-04
  render_tbl_2956(data = all_data$data_2956, no_rep = "2956", no_cmm = "2011_04")
  
  # 2939 - vu - 2010-07
  render_tbl_2010_07(data = all_data$data_2939, no_rep = "2939", no_cmm = "2010_07")
}


