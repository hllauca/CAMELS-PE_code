# Create sample CAMELS-PE dataset for package examples

library(readr)
library(dplyr)
library(sf)
library(terra)

# ============================================================
# Paths
# ============================================================

camels_real <- "./2_results/CAMELS-PE"  # change to your real CAMELS-PE path
pkg_root    <- "./2_results/RCamelsPE"

out_dir <- file.path(
  pkg_root,
  "inst",
  "extdata",
  "sample_camels_pe"
)

gauges_keep <- c(
  "PE_212900",
  "PE_200907"
)

# ============================================================
# Create folders
# ============================================================

# start from an empty folder so files renamed in the full dataset do not linger
unlink(out_dir, recursive = TRUE, force = TRUE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

create_dir <- function(path) {
  dir.create(
    file.path(out_dir, path),
    recursive = TRUE,
    showWarnings = FALSE
  )
}

create_dir("01_metadata")
create_dir("02_attributes")
create_dir("03_timeseries/by_catchment")
create_dir("04_geospatial/by_catchment")

# ============================================================
# Helpers
# ============================================================

filter_gauge <- function(x) {

  if ("gauge_id" %in% names(x)) {
    x <- dplyr::filter(
      x,
      .data$gauge_id %in% gauges_keep
    )
  }

  x
}

safe_write_gpkg <- function(x, out_file) {

  if (file.exists(out_file)) {
    unlink(out_file, force = TRUE)
  }

  terra::writeVector(
    x,
    out_file,
    filetype = "GPKG",
    overwrite = TRUE
  )
}

# ============================================================
# 01_metadata
# ============================================================

metadata_files <- c(
  "stations.csv",
  "data_dictionary.csv"
)

for (f in metadata_files) {

  in_file  <- file.path(camels_real, "01_metadata", f)
  out_file <- file.path(out_dir, "01_metadata", f)

  if (file.exists(in_file)) {

    x <- read_csv(
      in_file,
      show_col_types = FALSE
    )

    x <- filter_gauge(x)

    write_csv(x, out_file)
  }
}

# ============================================================
# 02_attributes
# ============================================================

attribute_files <- list.files(
  file.path(camels_real, "02_attributes"),
  pattern = "\\.csv$"
)

for (f in attribute_files) {

  in_file  <- file.path(camels_real, "02_attributes", f)
  out_file <- file.path(out_dir, "02_attributes", f)

  if (file.exists(in_file)) {

    x <- read_csv(
      in_file,
      show_col_types = FALSE
    )

    x <- filter_gauge(x)

    write_csv(x, out_file)
  }
}

# ============================================================
# 03_timeseries/global
# ============================================================

ts_file <- file.path(
  camels_real,
  "03_timeseries",
  "timeseries.csv"
)

if (file.exists(ts_file)) {

  x <- read_csv(
    ts_file,
    show_col_types = FALSE
  ) %>%
    filter(.data$gauge_id %in% gauges_keep) %>%
    group_by(.data$gauge_id) %>%
    slice_head(n = 365) %>%
    ungroup()

  write_csv(
    x,
    file.path(
      out_dir,
      "03_timeseries",
      "timeseries.csv"
    )
  )
}

# ============================================================
# 03_timeseries/by_catchment
# ============================================================

for (g in gauges_keep) {

  in_file <- file.path(
    camels_real,
    "03_timeseries",
    "by_catchment",
    paste0(g, ".csv")
  )

  out_file <- file.path(
    out_dir,
    "03_timeseries",
    "by_catchment",
    paste0(g, ".csv")
  )

  if (file.exists(in_file)) {

    x <- read_csv(
      in_file,
      show_col_types = FALSE
    ) %>%
      slice_head(n = 365)

    write_csv(x, out_file)
  }
}

# ============================================================
# 04_geospatial/global
# ============================================================

geo_files <- c(
  "camels_pe_gauges.gpkg",
  "camels_pe_catchments.gpkg"
)

for (f in geo_files) {

  in_file <- file.path(
    camels_real,
    "04_geospatial",
    f
  )

  out_file <- file.path(
    out_dir,
    "04_geospatial",
    f
  )

  if (file.exists(in_file)) {

    x <- terra::vect(in_file)

    x <- x[x$gauge_id %in% gauges_keep, ]

    safe_write_gpkg(x, out_file)
  }
}

# ============================================================
# 04_geospatial/by_catchment
# ============================================================

for (g in gauges_keep) {

  in_folder <- file.path(
    camels_real,
    "04_geospatial",
    "by_catchment",
    g
  )

  out_folder <- file.path(
    out_dir,
    "04_geospatial",
    "by_catchment",
    g
  )

  dir.create(
    out_folder,
    recursive = TRUE,
    showWarnings = FALSE
  )

  geo_files <- c(
    paste0(g, "_outlet.gpkg"),
    paste0(g, "_catchment.gpkg")
  )

  for (f in geo_files) {

    in_file  <- file.path(in_folder, f)
    out_file <- file.path(out_folder, f)

    if (file.exists(in_file)) {

      x <- terra::vect(in_file)

      safe_write_gpkg(x, out_file)
    }
  }
}

message(
  "Sample CAMELS-PE dataset created at:\n",
  normalizePath(out_dir)
)
