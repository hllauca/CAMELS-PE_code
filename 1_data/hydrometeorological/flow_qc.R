# Control de calidad de los caudales diarios observados de CAMELS-PE (1981–2025)
# By Harold Llauca
#
# Pruebas adaptadas de QARTOD (U.S. IOOS, 2018: Manual for Real-Time Quality Control of Stream Flow
# Observations, v1.0, doi:10.25923/gszc-ha43), aplicadas a cada estación sobre el caudal expresado como
# lámina de escorrentía de la cuenca (mm d-1), para usar los mismos umbrales en cuencas de distinto tamaño.
#
# Flags: 1 = pasa, 3 = sospechoso (se conserva), 4 = falla (se elimina), 9 = faltante en el registro original
#   1. Rango bruto:          q < 0                                                         -> 4
#   2. Interpolación lineal: >= 5 días seguidos con pendiente constante no nula; se eliminan los días
#                            interiores (los extremos son observaciones)                    -> 4
#   3. Línea plana:          >= 7 días seguidos con |q(t) - q(t-1)| < min(0.001 mm d-1, 0.1 % de q(t)); el
#                            umbral relativo evita marcar recesiones suaves en cuencas grandes, donde
#                            0.001 mm d-1 equivale a varios m3 s-1. No se aplica a las rachas de caudal
#                            cero (ríos intermitentes). Las rachas de hasta 30 días pueden ser lecturas
#                            reales de estiaje (nivel sin cambio a la resolución de la mira), por lo que
#                            solo se marcan; las más largas indican un registro detenido
#                              7–30 días                                                     -> 3 (toda la racha)
#                              > 30 días                                                     -> 4 (toda la racha)
#   4. Pico (spike):         |q(t) - (q(t-1) + q(t+1)) / 2| > 4 sd(q):
#                            sin lluvia que lo respalde (P(t) y P(t-1) <= p90 de la cuenca) -> 4
#                            con lluvia que lo respalde                                     -> 3 (crecida plausible)
#
# Entradas: raw/observations/database_flow_1981_2025_raw.xlsx (m3 s-1, sin control de calidad),
#           prec_PISCO_1981_2025.csv (P media de la cuenca) y las cuencas corregidas (área).
# Salidas:  flow_OBS_1981_2025.csv (mm d-1), flow_OBS_raw_1981_2025.csv (mm d-1, sin control de calidad,
#           para que el usuario pueda aplicar su propio criterio), raw/flow_OBS_1981_2025_m3s.csv (m3 s-1),
#           qc/flow_OBS_qc_flags_1981_2025.csv (flag diario), qc/flow_OBS_qc_log.csv (valores no aprobados)
#           y qc/qc_summary_*.csv (resúmenes para el Apéndice A).

library(terra)
library(openxlsx)
# Run this script from the root of the CAMELS-PE project folder (see README.md).
dir_hm <- './1_data/hydrometeorological/'
dir_qc <- file.path(dir_hm, 'qc')
dir.create(dir_qc, showWarnings = FALSE)

# Estaciones (orden de las cuencas) y área de drenaje, como en extract_variables.R
cat_v    <- vect('./1_data/sig/camels_pe_catchments.gpkg')
gauge_id <- sub('^PE_', '', cat_v$gauge_id)
area     <- terra::expanse(cat_v, unit = 'km')
time_full <- seq(as.Date('1981-01-01'), as.Date('2025-12-31'), by = 'day')

# Caudal original (m3 s-1) y precipitación de la cuenca (mm d-1)
raw <- read.xlsx(file.path(dir_hm, 'raw/observations/database_flow_1981_2025_raw.xlsx'), sheet = 'data',
                 check.names = FALSE, detectDates = TRUE)
stopifnot(all(as.Date(raw[, 1]) == time_full), all(gauge_id %in% names(raw)))
q_m3s <- as.matrix(raw[, gauge_id])
q_mm  <- sweep(q_m3s, 2, area, function(q, a) q * 86.4 / a)

prec <- read.csv(file.path(dir_hm, 'prec_PISCO_1981_2025.csv'), check.names = FALSE)
stopifnot(nrow(prec) == length(time_full))
P <- as.matrix(prec[, gauge_id])

# Parámetros
SPIKE_K    <- 4       # múltiplo de la desviación estándar
P_SUPPORT  <- 0.90    # percentil de P de la cuenca que respalda una crecida
FLAT_DAYS  <- 7
FLAT_FAIL  <- 30      # días; rachas más largas se eliminan
FLAT_EPS   <- 0.001   # mm d-1 (umbral absoluto)
FLAT_REL   <- 0.001   # fracción de q (umbral relativo)
INTERP_LEN <- 5       # días

# Rachas de valores lógicos: longitud de la racha a la que pertenece cada posición
run_length <- function(x) { r <- rle(x); rep(r$lengths * r$values, r$lengths) }

qc_station <- function(q, p) {
  n    <- length(q)
  flag <- ifelse(is.na(q), 9L, 1L)
  test <- ifelse(is.na(q), 'Missing', 'Pass')
  set  <- function(idx, f, lab) {           # una falla no se sobrescribe con una prueba posterior
    idx <- idx[flag[idx] %in% c(1L, 3L) & (f == 4L | flag[idx] == 1L)]
    flag[idx] <<- f; test[idx] <<- lab
  }
  d1 <- c(NA, diff(q))                      # q(t) - q(t-1)

  # 1. Rango bruto
  set(which(q < 0), 4L, 'Gross range')

  # 2. Interpolación lineal: pendiente constante no nula entre días consecutivos
  same_slope <- c(NA, diff(d1))
  lin <- !is.na(same_slope) & abs(same_slope) <= 1e-6 * pmax(abs(q), 1e-3) & abs(d1) > 1e-9
  # una racha de k pendientes iguales consecutivas cubre k + 2 días; se marcan los k días interiores
  rl  <- run_length(lin)
  set(which(lin & rl >= INTERP_LEN - 2) - 1L, 4L, 'Linear interpolation')

  # 3. Línea plana (sin rachas de caudal cero)
  flat_step <- !is.na(d1) & abs(d1) < pmin(FLAT_EPS, FLAT_REL * abs(q)) & q != 0
  rl  <- run_length(flat_step)              # pasos iguales; la racha de días es rl + 1
  idx <- which(flat_step & rl + 1 > FLAT_FAIL)
  set(sort(unique(c(idx, idx - 1L))), 4L, 'Flat line')
  idx <- which(flat_step & rl + 1 >= FLAT_DAYS & rl + 1 <= FLAT_FAIL)
  set(sort(unique(c(idx, idx - 1L))), 3L, 'Flat line (suspect)')

  # 4. Pico, respaldado o no por la lluvia de la cuenca
  ref <- (c(NA, q[-n]) + c(q[-1], NA)) / 2
  mag <- abs(q - ref)
  sp  <- which(!is.na(mag) & mag > SPIKE_K * sd(q, na.rm = TRUE))
  p90 <- quantile(p, P_SUPPORT, na.rm = TRUE)
  wet <- pmax(p, c(NA, p[-n]), na.rm = TRUE) > max(p90, 0)
  set(sp[!wet[sp]], 4L, 'Spike')
  set(sp[wet[sp]], 3L, 'Spike (rain-supported)')

  list(flag = flag, test = test)
}

res  <- lapply(seq_along(gauge_id), function(j) qc_station(q_mm[, j], P[, j]))
flag <- sapply(res, `[[`, 'flag')
test <- sapply(res, `[[`, 'test')
colnames(flag) <- colnames(test) <- gauge_id

# Series finales: se eliminan solo las fallas (flag 4)
q_mm_qc  <- q_mm;  q_mm_qc[flag == 4L]  <- NA
q_m3s_qc <- q_m3s; q_m3s_qc[flag == 4L] <- NA

dates_df <- data.frame(year  = as.integer(format(time_full, '%Y')),
                       month = as.integer(format(time_full, '%m')),
                       day   = as.integer(format(time_full, '%d')))
write.csv(data.frame(dates_df, q_mm_qc, check.names = FALSE), file.path(dir_hm, 'flow_OBS_1981_2025.csv'),
          row.names = FALSE)
write.csv(data.frame(dates_df, q_mm, check.names = FALSE), file.path(dir_hm, 'flow_OBS_raw_1981_2025.csv'),
          row.names = FALSE)
write.csv(data.frame(Fecha = format(time_full, '%d/%m/%Y'), q_m3s_qc, check.names = FALSE),
          file.path(dir_hm, 'raw/flow_OBS_1981_2025_m3s.csv'), row.names = FALSE, na = '')
write.csv(data.frame(dates_df, flag, check.names = FALSE), file.path(dir_qc, 'flow_OBS_qc_flags_1981_2025.csv'),
          row.names = FALSE)

# Registro de los valores no aprobados
k <- which(flag == 3L | flag == 4L, arr.ind = TRUE)
qc_log <- data.frame(gauge_id = paste0('PE_', gauge_id[k[, 2]]), date = time_full[k[, 1]],
                     q_mm = round(q_mm[k], 6), q_m3s = q_m3s[k], flag = flag[k], test = test[k])
qc_log <- qc_log[order(qc_log$gauge_id, qc_log$date), ]
write.csv(qc_log, file.path(dir_qc, 'flow_OBS_qc_log.csv'), row.names = FALSE)

# Resúmenes (Apéndice A)
n_obs <- sum(!is.na(q_mm))
qc_summary_global <- data.frame(
  n_total = length(q_mm), n_observed_raw = n_obs, n_pass = sum(flag == 1L), n_suspect = sum(flag == 3L),
  n_fail = sum(flag == 4L), n_missing = sum(flag == 9L),
  perc_fail_over_observed = 100 * sum(flag == 4L) / n_obs,
  perc_missing_over_total = 100 * sum(flag == 9L) / length(q_mm))
tt <- table(test[flag %in% c(3L, 4L)])
qc_summary_by_test <- data.frame(test = names(tt), n = as.integer(tt), perc_of_observed_raw = 100 * as.integer(tt) / n_obs)
qc_summary_station <- data.frame(
  gauge_id = paste0('PE_', gauge_id), n_observed_raw = colSums(!is.na(q_mm)), n_pass = colSums(flag == 1L),
  n_suspect = colSums(flag == 3L), n_fail = colSums(flag == 4L),
  perc_fail_over_observed = 100 * colSums(flag == 4L) / pmax(colSums(!is.na(q_mm)), 1))
write.csv(qc_summary_global,  file.path(dir_qc, 'qc_summary_global.csv'),  row.names = FALSE)
write.csv(qc_summary_by_test, file.path(dir_qc, 'qc_summary_by_test.csv'), row.names = FALSE)
write.csv(qc_summary_station, file.path(dir_qc, 'qc_summary_station.csv'), row.names = FALSE)

print(qc_summary_global)
print(qc_summary_by_test)
cat('Fallas por estación (%): mediana', round(median(qc_summary_station$perc_fail_over_observed), 2),
    '| p25–p75', paste(round(quantile(qc_summary_station$perc_fail_over_observed, c(.25, .75)), 2), collapse = '–'),
    '| máx', round(max(qc_summary_station$perc_fail_over_observed), 2), '\n')
