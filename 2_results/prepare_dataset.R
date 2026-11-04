# Create CAMELS-PE dataset
rm(list=ls())
cat('\f')
# Run this script from the root of the CAMELS-PE project folder (see README.md).
library(data.table)
library(terra)

# Redondear solo las columnas numéricas
round_num <- function(df, digits = 3) {
  num <- sapply(df, is.numeric)
  df[num] <- round(df[num], digits)
  df
}

# Ordenar las filas como en stations.csv (gauge_id con prefijo 'PE_')
by_meta <- function(df) {
  df$gauge_id <- ifelse(grepl('^PE_', df$gauge_id), df$gauge_id, paste0('PE_', df$gauge_id))
  stopifnot(all(meta$gauge_id %in% df$gauge_id))
  df[match(meta$gauge_id, df$gauge_id), , drop = FALSE]
}

save_attr <- function(df, file) {
  write.csv(df, file.path('./2_results/CAMELS-PE/02_attributes', file), row.names = FALSE, na = 'NA')
}


# 01_metadata
#---------------------------------------------------------------------------
meta <- read.csv('./1_data/location/gauge_location.csv')
meta$gauge_perc_obs <- round(meta$gauge_perc_obs,2)

# Resumen del control de calidad del caudal observado por estación (flow_qc.R)
qc_sta <- read.csv('./1_data/hydrometeorological/qc/qc_summary_station.csv')
qc_sta <- qc_sta[match(meta$gauge_id, qc_sta$gauge_id), ]
stopifnot(!anyNA(qc_sta$gauge_id))
meta$flow_obs_n_suspect <- qc_sta$n_suspect
meta$flow_obs_n_fail    <- qc_sta$n_fail
meta$flow_obs_perc_fail <- round(qc_sta$perc_fail_over_observed, 2)

# Reorder columns
meta <- meta[, c(
  "gauge_id",
  "gauge_name",
  "gauge_region",
  "gauge_lat",
  "gauge_lon",
  "gauge_elev",
  "gauge_record_start",
  "gauge_record_end",
  "gauge_perc_obs",
  "flow_obs_n_suspect",
  "flow_obs_n_fail",
  "flow_obs_perc_fail",
  "name_cat",
  "is_nested",          
  "nested_group_id",    
  "nested_group_size",
  "downstream_gauge_id",
  "upstream_gauge_id" 
)]

# Save metadata
write.csv(meta, file = "./2_results/CAMELS-PE/01_metadata/stations.csv",
  row.names = FALSE, na = "NA")


# 02_attributes
#---------------------------------------------------------------------------

# Todas las columnas se seleccionan por nombre (no por posición)

# topography (gauge_elev ya está en stations.csv)
topo <- by_meta(read.csv('./1_data/topography/catchment_topography.csv'))
topo <- topo[, c('gauge_id', 'area', 'perimeter', 'elev_min', 'elev_max', 'elev_mean', 'elev_median', 'slope_mean')]
save_attr(round_num(topo), 'topographic_attributes.csv')

# climatic indices (P de PISCOp; PET de Hargreaves-Samani, Priestley-Taylor y Oudin)
sig_all <- by_meta(read.csv('./1_data/signatures/catchment_signatures.csv', colClasses = c(gauge_id = 'character')))
clim <- sig_all[, c('gauge_id', 'p_mean', 'pet_mean', 'aridity', 'pet_mean_pt', 'pet_mean_ou', 'aridity_pt',
                    'aridity_ou', 'p_seasonality', 'high_prec_freq', 'high_prec_dur', 'high_prec_timing',
                    'low_prec_freq', 'low_prec_dur', 'low_prec_timing')]
# El sufijo indica el método de PET, como en pet_mean_pt y pet_mean_ou
setnames(clim, c('pet_mean', 'aridity'), c('pet_mean_hs', 'aridity_hs'))
save_attr(round_num(clim), 'climatic_indices.csv')

# hydrological signatures from observed streamflow (NA where < 5 valid hydrological years)
sig_cols <- c('q_mean', 'runoff_ratio', 'stream_elas', 'baseflow_index', 'slope_fdc', 'hfd_mean', 'Q5', 'Q95',
              'high_q_freq', 'high_q_dur', 'low_q_freq', 'low_q_dur', 'zero_q_freq')
sign <- sig_all[, c('gauge_id', sig_cols, 'q_obs_years', 'q_obs_start', 'q_obs_end', 'q_obs_frac')]
save_attr(round_num(sign), 'hydrological_signatures.csv')

# hydrological signatures from simulated streamflow (PISCO-ARNOVIC, 1981–2025)
sign_sim <- by_meta(read.csv('./1_data/signatures/catchment_signatures_sim.csv', colClasses = c(gauge_id = 'character')))
sign_sim <- sign_sim[, c('gauge_id', sig_cols, 'q_sim_years', 'q_sim_start', 'q_sim_end')]
save_attr(round_num(sign_sim), 'hydrological_signatures_sim.csv')

# land-cover attributes (MapBiomas 2024) and annual series 1985–2024 (formato largo)
land <- by_meta(read.csv('./1_data/land_cover/land_cover.csv', colClasses = c(gauge_id = 'character')))
names(land)[names(land) == 'dominant_class'] <- 'land_dominant_class'
land$land_dominant_class <- sub('_perc$', '', land$land_dominant_class)   # igual que en las series anuales
save_attr(round_num(land), 'land_cover_attributes.csv')

for (lvl in c('class', 'subclass')) {
  lc <- read.csv(sprintf('./1_data/land_cover/land_cover_%s_annual.csv', lvl))
  stopifnot(setequal(lc$gauge_id, meta$gauge_id))
  lc <- lc[order(match(lc$gauge_id, meta$gauge_id), lc$year), ]
  names(lc)[names(lc) == 'dominant_class'] <- 'land_dominant_class'
  save_attr(round_num(lc), sprintf('land_cover_%s_annual.csv', lvl))
}
save_attr(read.csv('./1_data/land_cover/land_cover_classification.csv'), 'land_cover_classification.csv')

# geologic attributes (GLiM v1.1 y GLHYMPS 2.0)
geol <- by_meta(read.csv('./1_data/geology/Geologic characteristics - CAMELSPERU.csv', colClasses = c(gauge_id = 'character')))
save_attr(round_num(geol), 'geologic_attributes.csv')

# soil attributes (DSOLMap): nombres en minúscula y clase dominante sin sufijo
soil <- by_meta(read.csv('./1_data/soil/Soil characteristics - CAMELSPERU.csv', colClasses = c(gauge_id = 'character')))
names(soil) <- tolower(names(soil))
names(soil)[names(soil) == 'dominant'] <- 'soil_dominant_class'
soil$soil_dominant_class <- sub('_perc$', '', soil$soil_dominant_class)
# Suelos: suborden, gran grupo y textura de los 3 horizontes dominantes (1_data/soil/process_soil_types.R)
soil_dom <- by_meta(read.csv('./1_data/soil/soil_types_dominant.csv'))
soil <- cbind(soil, soil_dom[, setdiff(names(soil_dom), 'gauge_id')])
save_attr(round_num(soil, 2), 'soil_attributes.csv')

# Suelos: % de área de cada suborden, gran grupo y clase textural por horizonte (formato largo) y leyenda
soil_cls <- read.csv('./1_data/soil/soil_classes.csv')
soil_cls$gauge_id <- ifelse(grepl('^PE_', soil_cls$gauge_id), soil_cls$gauge_id, paste0('PE_', soil_cls$gauge_id))
stopifnot(setequal(soil_cls$gauge_id, meta$gauge_id))
lv <- c('suborder', 'great_group', 'texture_h1', 'texture_h2', 'texture_h3')
soil_cls <- soil_cls[order(match(soil_cls$gauge_id, meta$gauge_id), match(soil_cls$level, lv)), ]
save_attr(round_num(soil_cls, 2), 'soil_classes.csv')
save_attr(read.csv('./1_data/soil/soil_classification.csv'), 'soil_classification.csv')

# human intervention attributes (6 decimales: los volúmenes acreditados en mm d-1 son del orden de 1e-6)
hatt <- by_meta(read.csv('./1_data/human_intervention/human_intervention.csv', colClasses = c(gauge_id = 'character')))
save_attr(round_num(hatt, 6), 'human_intervention_attributes.csv')


# 03_timeseries
#---------------------------------------------------------------------------
dir_hm <- './1_data/hydrometeorological'
ids    <- sub('^PE_', '', meta$gauge_id)
dates  <- seq(as.Date('1981-01-01'), as.Date('2025-12-31'), by = 'day')

# Matriz días x cuencas (en el orden de stations.csv) de un CSV con columnas year, month, day, <gauge_id>
read_ts <- function(file) {
  df <- fread(file.path(dir_hm, file), colClasses = 'numeric')
  stopifnot(all(ids %in% names(df)), nrow(df) == length(dates),
            all(as.Date(sprintf('%04d-%02d-%02d', df$year, df$month, df$day)) == dates))
  as.matrix(df[, ..ids])
}

# Caudal observado en m3 s-1 (con control de calidad), escrito por flow_qc.R
q_m3s <- fread(file.path(dir_hm, 'raw/flow_OBS_1981_2025_m3s.csv'), colClasses = c(Fecha = 'character'))
stopifnot(all(as.Date(q_m3s$Fecha, '%d/%m/%Y') == dates))
q_m3s <- as.matrix(q_m3s[, ..ids])

# Área de las cuencas para pasar el caudal simulado a m3 s-1 (la misma que se usó para convertir a mm d-1)
area <- topo$area[match(meta$gauge_id, topo$gauge_id)]

flow_sim <- read_ts('flow_PISCO_ARNOVIC_1981_2025.csv')

# Variables en el orden de las columnas de timeseries.csv
ts_list <- list(
  prec_pisco    = read_ts('prec_PISCO_1981_2025.csv'),
  prec_sd_pisco = sqrt(read_ts('prec_var_PISCO_1981_2025.csv')),   # desviación estándar espacial (mm d-1)
  prec_chirps   = read_ts('prec_CHIRPS3_1981_2025.csv'),
  prec_era5land = read_ts('prec_ERA5LAND_1981_2025.csv'),
  pet_hs        = read_ts('evap_HS_1981_2025.csv'),
  pet_pt        = read_ts('evap_PT_1981_2025.csv'),
  pet_ou        = read_ts('evap_OU_1981_2025.csv'),
  tmin          = read_ts('tmin_PISCO_1981_2025.csv'),
  tmean         = read_ts('tmed_PISCO_1981_2025.csv'),
  tmax          = read_ts('tmax_PISCO_1981_2025.csv'),
  srad          = read_ts('srad_ERA5LAND_1981_2025.csv'),
  vprp          = read_ts('vprp_ERA5LAND_1981_2025.csv'),
  flow_obs      = read_ts('flow_OBS_1981_2025.csv'),
  flow_obs_m3s  = q_m3s,
  flow_obs_raw  = read_ts('flow_OBS_raw_1981_2025.csv'),
  flow_obs_flag = read_ts('qc/flow_OBS_qc_flags_1981_2025.csv'),
  flow_sim      = flow_sim,
  flow_sim_m3s  = sweep(flow_sim, 2, area / 86.4, '*')
)

# Los caudales en mm d-1 van con 6 decimales para no convertir en cero los caudales bajos de las cuencas áridas
digits <- c(flow_obs = 6, flow_obs_raw = 6, flow_sim = 6, flow_obs_flag = 0)

# Formato largo: filas ordenadas por fecha y luego por estación (orden de stations.csv)
n_d <- length(dates); n_g <- length(ids)
timeseries_daily <- data.table(date = rep(format(dates), each = n_g), gauge_id = rep(meta$gauge_id, times = n_d))
for (v in names(ts_list)) {
  x <- as.vector(t(ts_list[[v]]))
  set(timeseries_daily, j = v, value = if (v == 'flow_obs_flag') as.integer(x) else round(x, ifelse(v %in% names(digits), digits[v], 3)))
}

# Prueba de control de calidad que marcó cada valor (flag 3 o 4); vacío en el resto
qc_log <- fread(file.path(dir_hm, 'qc/flow_OBS_qc_log.csv'))
qc_codes <- c('Gross range' = 'gross_range', 'Spike' = 'spike', 'Spike (rain-supported)' = 'spike_rain_supported',
              'Flat line' = 'flat_line', 'Flat line (suspect)' = 'flat_line_suspect',
              'Linear interpolation' = 'linear_interpolation')
stopifnot(all(qc_log$test %in% names(qc_codes)), !anyDuplicated(qc_log[, .(gauge_id, date)]))
row_qc <- (match(as.Date(qc_log$date), dates) - 1L) * n_g + match(qc_log$gauge_id, meta$gauge_id)
stopifnot(!anyNA(row_qc))
timeseries_daily[, flow_obs_qc_test := NA_character_]
timeseries_daily[row_qc, flow_obs_qc_test := unname(qc_codes[qc_log$test])]
setcolorder(timeseries_daily, c(setdiff(names(timeseries_daily), c('flow_sim', 'flow_sim_m3s')), 'flow_sim', 'flow_sim_m3s'))

# Consistencia: el caudal con QC solo existe donde el flag es 1 (aprobado) o 3 (sospechoso),
# y la prueba está indicada exactamente en los valores con flag 3 o 4
stopifnot(with(timeseries_daily, all(is.na(flow_obs) == !(flow_obs_flag %in% c(1L, 3L)))),
          with(timeseries_daily, all(is.na(flow_obs) == is.na(flow_obs_m3s))),
          with(timeseries_daily, all(is.na(flow_obs_qc_test) == !(flow_obs_flag %in% c(3L, 4L)))))

out_ts_dir <- './2_results/CAMELS-PE/03_timeseries'
out_bc_dir <- file.path(out_ts_dir, 'by_catchment')
dir.create(out_bc_dir, recursive = TRUE, showWarnings = FALSE)

# Archivo global en formato largo
fwrite(timeseries_daily, file.path(out_ts_dir, 'timeseries.csv'), na = 'NA')

# Un archivo por cuenca
for (gid in meta$gauge_id) {
  fwrite(timeseries_daily[gauge_id == gid, !'gauge_id'], file.path(out_bc_dir, paste0(gid, '.csv')), na = 'NA')
}

# El control de calidad va completo en las series (flow_obs_raw, flow_obs_flag, flow_obs_qc_test) y el
# resumen por estación en stations.csv: se elimina la carpeta quality_control de versiones anteriores
unlink(file.path(out_ts_dir, 'quality_control'), recursive = TRUE)


# 04_geoespatial
#---------------------------------------------------------------------------
geo_dir         <- "./2_results/CAMELS-PE/04_geospatial"
gauges_file     <- file.path(geo_dir, "camels_pe_gauges.gpkg")
catchments_file <- file.path(geo_dir, "camels_pe_catchments.gpkg")

# Capas actualizadas (cuencas corregidas, área y perímetro geodésicos) desde 1_data/sig
gauges     <- vect('./1_data/sig/camels_pe_gauges.gpkg')
catchments <- vect('./1_data/sig/camels_pe_catchments.gpkg')
stopifnot(setequal(gauges$gauge_id, meta$gauge_id), setequal(catchments$gauge_id, meta$gauge_id))
writeVector(gauges, gauges_file, overwrite = TRUE)
writeVector(catchments, catchments_file, overwrite = TRUE)

# # Read layers
# UH <- vect("./1_data/sig/UnidadesHidrograficas.shp")
# UH <- UH[, "NOMBRE"]
# gauges <- vect(gauges_file)
# catchments <- vect(catchments_file)
# UH   <- makeValid(UH)
# UH_g <- project(UH, crs(gauges))
# gauge_name <- extract(UH_g, gauges)
# gauges$name_cat <- gauge_name$NOMBRE
# catchments$name_cat <- gauges$name_cat[
#   match(catchments$gauge_id, gauges$gauge_id)
# ]
# catchments$area <- round(expanse(catchments, unit = "km"),4)
# writeVector(gauges, gauges_file, overwrite = TRUE)
# writeVector(catchments, catchments_file, overwrite = TRUE)

# topologia de estaciones y cuencas
# library(terra)
# library(dplyr)
# 
# gauges <- vect(gauges_file)
# catchments <- vect(catchments_file)
# 
# gauges_rel <- project(gauges, crs(catchments))
# 
# catchments$area <- expanse(catchments, unit = "km")
# catchments$perimeter <- perim(catchments) / 1000
# 
# rel <- relate(gauges_rel, catchments, relation = "within", pairs = TRUE)
# 
# if (is.null(rel) || nrow(rel) == 0) {
#   nested_pairs <- data.frame(
#     upstream_gauge_id = character(),
#     downstream_gauge_id = character(),
#     upstream_area_km2 = numeric(),
#     downstream_area_km2 = numeric()
#   )
# } else {
#   nested_pairs <- data.frame(
#     upstream_gauge_id = gauges$gauge_id[rel[, 1]],
#     downstream_gauge_id = catchments$gauge_id[rel[, 2]],
#     upstream_area_km2 = catchments$area[
#       match(gauges$gauge_id[rel[, 1]], catchments$gauge_id)
#     ],
#     downstream_area_km2 = catchments$area[rel[, 2]]
#   ) |>
#     filter(upstream_gauge_id != downstream_gauge_id) |>
#     filter(downstream_area_km2 > upstream_area_km2)
# }
# 
# immediate_downstream <- nested_pairs |>
#   group_by(upstream_gauge_id) |>
#   slice_min(downstream_area_km2, n = 1, with_ties = FALSE) |>
#   ungroup() |>
#   select(gauge_id = upstream_gauge_id, downstream_gauge_id)
# 
# upstream_summary <- nested_pairs |>
#   group_by(downstream_gauge_id) |>
#   slice_max(
#     upstream_area_km2,
#     n = 1,
#     with_ties = FALSE
#   ) |>
#   ungroup() |>
#   select(
#     gauge_id = downstream_gauge_id,
#     upstream_gauge_id
#   )
# 
# find_outlet <- function(id, lookup) {
#   current <- id
#   visited <- character()
#   
#   repeat {
#     if (current %in% visited) return(current)
#     
#     visited <- c(visited, current)
#     
#     next_id <- lookup$downstream_gauge_id[
#       match(current, lookup$gauge_id)
#     ]
#     
#     if (length(next_id) == 0 || is.na(next_id) || next_id == "NONE") {
#       return(current)
#     }
#     
#     current <- next_id
#   }
# }
# 
# nested_groups <- data.frame(
#   gauge_id = gauges$gauge_id,
#   nested_group_id = sapply(
#     gauges$gauge_id,
#     find_outlet,
#     lookup = immediate_downstream
#   )
# ) |>
#   group_by(nested_group_id) |>
#   mutate(nested_group_size = n()) |>
#   ungroup()
# 
# catchment_attributes <- data.frame(
#   gauge_id = catchments$gauge_id,
#   area = catchments$area,
#   perimeter = catchments$perimeter
# )
# 
# network_attributes <- data.frame(gauge_id = gauges$gauge_id) |>
#   left_join(immediate_downstream, by = "gauge_id") |>
#   left_join(upstream_summary, by = "gauge_id") |>
#   left_join(nested_groups, by = "gauge_id") |>
#   left_join(catchment_attributes, by = "gauge_id") |>
#   mutate(
#     downstream_gauge_id = ifelse(is.na(downstream_gauge_id), "NONE", downstream_gauge_id),
#     upstream_gauge_id = ifelse(is.na(upstream_gauge_id), "NONE", upstream_gauge_id),
#     nested_group_id = ifelse(is.na(nested_group_id), gauge_id, nested_group_id),
#     nested_group_size = ifelse(is.na(nested_group_size), 1L, nested_group_size),
#     is_nested = nested_group_size > 1
#   ) |>
#   select(
#     gauge_id,
#     area,
#     perimeter,
#     is_nested,
#     downstream_gauge_id,
#     upstream_gauge_id,
#     nested_group_id,
#     nested_group_size
#   )
# 
# idx_gauges <- match(gauges$gauge_id, network_attributes$gauge_id)
# idx_catchments <- match(catchments$gauge_id, network_attributes$gauge_id)
# 
# gauges$area <- network_attributes$area[idx_gauges]
# gauges$perimeter <- network_attributes$perimeter[idx_gauges]
# gauges$is_nested <- ifelse(network_attributes$is_nested[idx_gauges],"TRUE","FALSE")
# gauges$downstream_gauge_id <- network_attributes$downstream_gauge_id[idx_gauges]
# gauges$upstream_gauge_id <- network_attributes$upstream_gauge_id[idx_gauges]
# gauges$nested_group_id <- network_attributes$nested_group_id[idx_gauges]
# gauges$nested_group_size <- network_attributes$nested_group_size[idx_gauges]
# 
# catchments$area <- network_attributes$area[idx_catchments]
# catchments$perimeter <- network_attributes$perimeter[idx_catchments]
# catchments$is_nested <- ifelse(network_attributes$is_nested[idx_catchments],"TRUE","FALSE")
# catchments$downstream_gauge_id <- network_attributes$downstream_gauge_id[idx_catchments]
# catchments$upstream_gauge_id <- network_attributes$upstream_gauge_id[idx_catchments]
# catchments$nested_group_id <- network_attributes$nested_group_id[idx_catchments]
# catchments$nested_group_size <- network_attributes$nested_group_size[idx_catchments]
# 
# writeVector(gauges, gauges_file, overwrite = TRUE)
# writeVector(catchments, catchments_file, overwrite = TRUE)


# Create individual folders and files by catchment
out_dir <- file.path(geo_dir, "by_catchment")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (gid in gauges$gauge_id) {
  
  gid_dir <- file.path(out_dir, gid)
  dir.create(gid_dir, recursive = TRUE, showWarnings = FALSE)
  
  gauge_i <- gauges[gauges$gauge_id == gid, ]
  catch_i <- catchments[catchments$gauge_id == gid, ]
  
  writeVector(
    gauge_i,
    file.path(gid_dir, paste0(gid, "_outlet.gpkg")),
    overwrite = TRUE
  )
  
  writeVector(
    catch_i,
    file.path(gid_dir, paste0(gid, "_catchment.gpkg")),
    overwrite = TRUE
  )
}







