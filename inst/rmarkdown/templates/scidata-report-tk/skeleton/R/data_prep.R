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
# prepare ikasavea data if it exists
has_tufman_data <- !is.null(all_data$data_3615) && nrow(all_data$data_3615) > 0

if (!has_tufman_data) {
  data_source_lst <- c()
} else {
  data_source_lst <- c("tufman2")
}

ika_data <- prep_ikasavea_artisanal_inputs(ikasavea_folder, report_year, data_source_lst)

if (is.null(ika_data) && !has_tufman_data) {
  cat("No Ikasavea or Tails trip data found for this country/year. Document generation stopped.\n")
  knitr::knit_exit()
} else {
  # try to extract the local knowledge table
  if (length(list.files(art_est_trips_folder)) == 1) {
    local_knowl_trips_file <- paste0(art_est_trips_folder, "/", list.files(art_est_trips_folder)[1])
  } else {
    local_knowl_trips_file <- NULL
  }
  
  if (is.null(ika_data)) {
    artisanal_data_sources <- data_source_lst
  } else {
    artisanal_data_sources <- ika_data$data_source_lst
  }
  
  artisanal_ace <- prep_artisanal_ace(
    data_3615 = all_data$data_3615,
    data_3614 = all_data$data_3614,
    df_ika_wide = ika_data$df_ika_wide,
    data_ika_catch_kg = ika_data$data_ika_catch_kg,
    local_knowl_trips_file = local_knowl_trips_file
  )
}
