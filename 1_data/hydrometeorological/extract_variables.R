# Preparar bases de datos hidroclimaticos para las cuencas de CAMELS-PE
# By Harold Llauca
cat('\f')
rm(list=ls())

# Librerías
library(terra)
library(sf)
library(openxlsx)
library(datetimeutils)
library(exactextractr)
library(zoo)
library(tidyverse)
library(lubridate)
library(ncdf4)
library(hyfo)

# Directorio de trabajo
# Run this script from the root of the CAMELS-PE project folder (see README.md).
# Datos geoespaciales
cat <- vect('./1_data/sig/camels_pe_catchments.gpkg')
riv <- vect('./1_data/sig/camels_pe_gauges.gpkg')

# variables auxiliares
# gauge_id sin prefijo 'PE_' en las columnas de los CSV (prepare_dataset.R lo agrega)
area      <- terra::expanse(cat, unit = "km")
comid_riv <- riv$COMID[match(cat$gauge_id, riv$gauge_id)]
gauge_id  <- sub('^PE_', '', cat$gauge_id)
cat_sf    <- st_as_sf(cat)

# Extracción areal rápida: los pesos de cobertura de cada celda en cada cuenca (exactextractr)
# se calculan una sola vez y el NetCDF se lee por bloques de tiempo. Equivale a
# exact_extract(..., c('mean', 'variance')) pero sin releer el archivo completo por cada cuenca.
build_weights <- function(f, polys) {
  r  <- rast(f, lyrs = 1)
  nc <- nc_open(f)
  lon <- ncvar_get(nc, names(nc$dim)[1])
  lat <- ncvar_get(nc, names(nc$dim)[2])
  nc_close(nc)
  cov <- exact_extract(r, polys, include_cell = TRUE, progress = FALSE)
  W <- matrix(0, length(lon) * length(lat), length(cov))
  for (k in seq_along(cov)) {
    cc <- cov[[k]]$cell
    i  <- match(round(xFromCell(r, cc), 4), round(lon, 4))
    j  <- match(round(yFromCell(r, cc), 4), round(lat, 4))
    stopifnot(!anyNA(i), !anyNA(j))
    W[i + (j - 1) * length(lon), k] <- cov[[k]]$coverage_fraction
  }
  list(W = W, lon = lon, lat = lat)
}

extract_weighted <- function(f, var, wts, block = 2740) {
  nc <- nc_open(f)
  on.exit(nc_close(nc))
  stopifnot(identical(as.numeric(ncvar_get(nc, names(nc$dim)[1])), as.numeric(wts$lon)),
            identical(as.numeric(ncvar_get(nc, names(nc$dim)[2])), as.numeric(wts$lat)))
  nt <- nc$var[[var]]$varsize[3]
  keep <- rowSums(wts$W) > 0
  W  <- wts$W[keep, , drop = FALSE]
  mu <- s2 <- matrix(NA_real_, nt, ncol(W))
  for (s in seq(1, nt, by = block)) {
    n  <- min(block, nt - s + 1)
    x  <- matrix(ncvar_get(nc, var, start = c(1, 1, s), count = c(-1, -1, n)), ncol = n)[keep, , drop = FALSE]
    ok <- !is.na(x)   # celdas sin dato no cuentan en el peso, como en exactextractr
    x[!ok] <- 0
    sw <- crossprod(ok * 1, W)
    m  <- crossprod(x, W) / sw
    mu[s:(s + n - 1), ] <- m
    s2[s:(s + n - 1), ] <- crossprod(x^2, W) / sw - m^2
  }
  list(mean = mu, variance = pmax(s2, 0))
}

# ==============================================================================================
# PISCOp v3.0 (1981–2025)
# ==============================================================================================
# El NetCDF no tiene coordenada de tiempo: cada capa es un día desde 1981-01-01
time_sub <- seq(as.Date('1981-01-01'), as.Date('2025-12-31'), by = 'days')
f_pr     <- './1_data/hydrometeorological/raw/pisco_dpr_v3.0.nc'
pr_ext   <- extract_weighted(f_pr, 'precipitation', build_weights(f_pr, cat_sf))
stopifnot(nrow(pr_ext$mean) == length(time_sub))
pr_cat   <- zoo(pr_ext$mean, time_sub)
colnames(pr_cat) <- gauge_id
df_pr <- data.frame(
  year = year(time_sub),
  month = month(time_sub),
  day = day(time_sub),
  pr_cat,
  check.names = FALSE
)
write.csv(df_pr, './1_data/hydrometeorological/prec_PISCO_1981_2025.csv', row.names = FALSE)

# varianza
pr_var_cat <- zoo(pr_ext$variance, time_sub)
colnames(pr_var_cat) <- gauge_id
df_pr_var <- data.frame(
  year = year(time_sub),
  month = month(time_sub),
  day = day(time_sub),
  pr_var_cat,
  check.names = FALSE
)
write.csv(df_pr_var, './1_data/hydrometeorological/prec_var_PISCO_1981_2025.csv', row.names = FALSE)


# ==============================================================================================
# PISCOt (archivos anuales 1981–2025 en raw/tmax y raw/tmin)
# ==============================================================================================
# Cada archivo <año>.nc tiene una capa por día del año, sin coordenada de tiempo
time_full <- seq(as.Date('1981-01-01'), as.Date('2025-12-31'), by = 'days')

extract_temp <- function(var, years = 1981:2025) {
  nc_var <- c(tmax = 'maximum temperature', tmin = 'minimum temperature')[[var]]
  f_y    <- sprintf('./1_data/hydrometeorological/raw/%s/%d.nc', var, years)
  wts    <- build_weights(f_y[1], cat_sf)   # misma grilla en todos los años (se verifica al leer)
  out <- lapply(seq_along(years), function(k) {
    y  <- years[k]
    tt <- seq(as.Date(paste0(y, '-01-01')), as.Date(paste0(y, '-12-31')), by = 'days')
    cat(" Extrayendo:", var, y, "\n")
    m  <- extract_weighted(f_y[k], nc_var, wts, block = 366)$mean
    stopifnot(nrow(m) == length(tt))
    zoo(m, tt)
  })
  out <- do.call(rbind, out)
  colnames(out) <- gauge_id
  out
}
tx_cat    <- extract_temp('tmax')
tn_cat    <- extract_temp('tmin')
stopifnot(identical(time(tx_cat), time_full), identical(time(tn_cat), time_full))
tm_cat    <- 0.5 * (tx_cat + tn_cat)
dates_df  <- data.frame(
  year  = year(time_full),
  month = month(time_full),
  day   = day(time_full)
)
write.csv(
  data.frame(dates_df, round(coredata(tx_cat), 3), check.names = FALSE),
  './1_data/hydrometeorological/tmax_PISCO_1981_2025.csv',
  row.names = FALSE
)
write.csv(
  data.frame(dates_df, round(coredata(tn_cat), 3), check.names = FALSE),
  './1_data/hydrometeorological/tmin_PISCO_1981_2025.csv',
  row.names = FALSE
)
write.csv(
  data.frame(dates_df, round(coredata(tm_cat), 3), check.names = FALSE),
  './1_data/hydrometeorological/tmed_PISCO_1981_2025.csv',
  row.names = FALSE
)


# ==============================================================================================
# Evapotranspiración potencial: Hargreaves-Samani original (1981–2025)
# ==============================================================================================
# ET0 = 0.0023 * (Tmean + 17.8) * (Tmax - Tmin)^0.5 * 0.408 * Ra   (Allen et al., 1998, FAO-56 Ec. 52)
# Ra: radiación extraterrestre (FAO-56 Ec. 21–25) con la latitud de un punto interior de la cuenca.
# Tmax y Tmin areales de cada cuenca (sección PISCOt). Días con Tmax < Tmin quedan como NA.
lat_cat <- crds(centroids(cat, inside = TRUE))[, 'y'] * pi / 180
J       <- yday(time_full)
dr      <- 1 + 0.033 * cos(2 * pi * J / 365)
dec     <- 0.409 * sin(2 * pi * J / 365 - 1.39)
ws      <- acos(outer(-tan(dec), tan(lat_cat)))
Ra      <- (24 * 60 / pi) * 0.0820 * dr *
  (ws * outer(sin(dec), sin(lat_cat)) + outer(cos(dec), cos(lat_cat)) * sin(ws))  # MJ m-2 d-1
tx_m    <- coredata(tx_cat)
tn_m    <- coredata(tn_cat)
dT      <- tx_m - tn_m
dT[dT < 0] <- NA
eo_cat  <- 0.0023 * (0.5 * (tx_m + tn_m) + 17.8) * sqrt(dT) * 0.408 * Ra
colnames(eo_cat) <- gauge_id
df_eo <- data.frame(
  year  = year(time_full),
  month = month(time_full),
  day   = day(time_full),
  round(eo_cat, 3),
  check.names = FALSE
)
write.csv(df_eo, "./1_data/hydrometeorological/evap_HS_1981_2025.csv", row.names = FALSE)


#==============================================================================================
# Caudales observados (1981–2025): control de calidad en mm/día (flow_qc.R), que escribe
# flow_OBS_1981_2025.csv (mm d-1), raw/flow_OBS_1981_2025_m3s.csv y los flags en qc/
#==============================================================================================
# entorno propio para no sobrescribir variables de este script; usa prec_PISCO_1981_2025.csv (calculado arriba)
source('./1_data/hydrometeorological/flow_qc.R', local = new.env())


#==============================================================================================
# PISCO-Arnovic (1981–2025)
#==============================================================================================
nc    <- nc_open('./1_data/hydrometeorological/raw/PISCO_ARNOVIC_v1.0.nc')
comid <- ncvar_get(nc, 'comid')
tsim  <- as.Date(as.POSIXct(ncvar_get(nc, 'time'), origin = "1970-01-01"))
raw   <- t(ncvar_get(nc, 'flow'))
nc_close(nc)
qsim <- raw[, match(comid_riv, comid)]  # asegurar que 'comid_riv' esté definido
qsim <- sapply(1:length(gauge_id), function(i) qsim[,i]*86.4/area[i])
qsim <- zoo(qsim, tsim)
qsim <- window(qsim, start = '1981-01-01', end = '2025-12-31')
stopifnot(length(time(qsim)) == length(time_full), all(time(qsim) == time_full))
index(qsim) <- time_full
colnames(qsim) <- gauge_id
coredata(qsim)[coredata(qsim) < 0] <- 0   # valores negativos residuales del ruteo (RAPID)

df_qsim <- data.frame(
  year  = year(time(qsim)),
  month = month(time(qsim)),
  day   = day(time(qsim)),
  coredata(qsim),
  check.names = FALSE
)
write.csv(df_qsim, './1_data/hydrometeorological/flow_PISCO_ARNOVIC_extracted_1981_2025.csv', row.names = FALSE)



#==============================================================================================
# ERA5-Land (vprp, srad, P) y CHIRPS v3 (P), 1981–2025, exportados de Google Earth Engine
#==============================================================================================
# Exportaciones de GEE por año (gee/*.js): un CSV por año con columnas gauge_id y <var>_YYYYMMDD
# (formato ancho por fecha). Se ordenan por el gauge_id de las cuencas.
read_gee_yearly <- function(dir, prefix, out, label) {
  files <- list.files(dir, pattern = paste0('^', prefix, '\\d{4}\\.csv$'), full.names = TRUE)
  if (length(files) == 0) return(FALSE)
  m <- do.call(cbind, lapply(files, function(f) {
    x <- read.csv(f, check.names = FALSE)
    v <- t(as.matrix(x[, grep('^[a-z]+_\\d{8}$', names(x))]))
    colnames(v) <- sub('^PE_', '', as.character(x$gauge_id))
    t(v[, gauge_id, drop = FALSE])            # filas = cuencas en el orden de gauge_id
  }))
  dd <- as.Date(sub('^[a-z]+_', '', colnames(m)), '%Y%m%d')
  m  <- t(m[, match(time_full, dd), drop = FALSE])
  if (anyNA(m)) warning(label, ": ", sum(is.na(m)), " valores faltantes (días o cuencas sin dato).")
  m  <- pmax(m, 0)                            # quita valores negativos mínimos de redondeo (P de ERA5)
  colnames(m) <- gauge_id
  write.csv(data.frame(dates_df, round(m, 3), check.names = FALSE), out, row.names = FALSE)
  TRUE
}

# Exportaciones anteriores en formato largo (date, gauge_id, mean), usadas si no están las anuales
era5_to_wide <- function(f) {
  raw <- read.csv(f)
  raw$gauge_id <- sub('^PE_', '', as.character(raw$gauge_id))
  dd  <- as.Date(raw[, 1])
  vals <- sapply(gauge_id, function(g) raw$mean[raw$gauge_id == g][match(time_full, dd[raw$gauge_id == g])])
  zoo(vals, time_full)
}

dir_raw <- './1_data/hydrometeorological/raw/'
dir_out <- './1_data/hydrometeorological/'
gee_vars <- data.frame(
  label  = c('ERA5-Land vprp', 'ERA5-Land srad', 'CHIRPS v3', 'ERA5-Land P'),
  dir    = c('vprp_era5land', 'srad_era5land', 'chirpsv3_rnl', 'prec_era5land'),
  prefix = c('vprp_era5land_', 'srad_era5land_', 'chirps3_rnl_', 'prec_era5land_'),
  long   = c('vprp_era5land.csv', 'sr_era5land.csv', NA, NA),          # respaldo en formato largo
  out    = c('vprp_ERA5LAND_1981_2025.csv',                            # presión de vapor (hPa)
             'srad_ERA5LAND_1981_2025.csv',                            # radiación solar incidente (MJ m-2 d-1)
             'prec_CHIRPS3_1981_2025.csv',                             # precipitación (mm d-1)
             'prec_ERA5LAND_1981_2025.csv'))                           # precipitación (mm d-1)

for (k in seq_len(nrow(gee_vars))) {
  r <- gee_vars[k, ]
  if (read_gee_yearly(paste0(dir_raw, r$dir), r$prefix, paste0(dir_out, r$out), r$label)) next
  if (!is.na(r$long) && file.exists(paste0(dir_raw, r$long))) {
    x <- era5_to_wide(paste0(dir_raw, r$long))
    write.csv(data.frame(dates_df, coredata(x), check.names = FALSE), paste0(dir_out, r$out), row.names = FALSE)
  } else {
    warning("No se encontraron los CSV de ", r$label, "; se mantiene ", r$out, " sin actualizar.")
  }
}


# ==============================================================================================
# PET adicional: Priestley-Taylor y Oudin (a partir de los CSV de PISCOt y ERA5-Land)
# ==============================================================================================
source('./1_data/hydrometeorological/compute_pet.R')
