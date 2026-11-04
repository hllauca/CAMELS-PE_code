# Comparar firmas hidrológicas simuladas (PISCO-ARNOVIC) y observadas para CAMELS-PE
# By Harold Llauca
#
# Las firmas se calculan con la misma función calc_signatures() de compute_indices_signatures.R.
# Para que la comparación sea consistente, el caudal simulado se usa solo en los días con
# caudal observado dentro de los años hidrológicos válidos de cada estación.
cat('\f')
rm(list=ls())

# Directorio de trabajo
# Run this script from the root of the CAMELS-PE project folder (see README.md).
# Librerías
library(dplyr)
library(lubridate)
library(tidyr)
library(readr)

# Cargar calc_signatures() y los criterios de disponibilidad sin ejecutar el resto del script
src <- parse('./1_data/signatures/compute_indices_signatures.R', keep.source = FALSE)
for (e in src) {
  if (is.call(e) && identical(e[[1]], as.name('<-')) &&
      as.character(e[[2]]) %in% c('calc_signatures', 'min_frac_year', 'min_years')) {
    eval(e)
  }
}

# Leer bases de datos
read_series <- function(f, name) {
  read_csv(f, show_col_types = FALSE) %>%
    mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>%
    dplyr::select(-year, -month, -day) %>%
    pivot_longer(-date, names_to = "gauge_id", values_to = name)
}

camels_pe <- read_series("./1_data/hydrometeorological/prec_PISCO_1981_2025.csv", "prec") %>%
  left_join(read_series("./1_data/hydrometeorological/tmed_PISCO_1981_2025.csv", "temp"), by = c("gauge_id", "date")) %>%
  left_join(read_series("./1_data/hydrometeorological/evap_HS_1981_2025.csv", "evap"), by = c("gauge_id", "date")) %>%
  left_join(read_series("./1_data/hydrometeorological/flow_OBS_1981_2025.csv", "flow_obs"), by = c("gauge_id", "date")) %>%
  left_join(read_series("./1_data/hydrometeorological/flow_PISCO_ARNOVIC_1981_2025.csv", "flow_sim"), by = c("gauge_id", "date")) %>%
  filter(date >= as.Date("1981-01-01") & date <= as.Date("2025-12-31"))

sign_vars <- c("q_mean", "runoff_ratio", "stream_elas", "baseflow_index", "slope_fdc", "hfd_mean",
               "Q5", "Q95", "high_q_freq", "high_q_dur", "low_q_freq", "low_q_dur", "zero_q_freq")


#=========================================================================================
# Firmas observadas y simuladas por estación
#=========================================================================================
gauge_ids <- unique(camels_pe$gauge_id)
res <- list()

for (i in seq_along(gauge_ids)) {
  gid <- gauge_ids[i]
  cat("Procesando estación", gid, "(", i, "de", length(gauge_ids), ")\n")

  df_est <- camels_pe %>% filter(gauge_id == gid)

  # Firmas observadas (aplica los criterios de años hidrológicos válidos)
  obs <- calc_signatures(df_est %>% mutate(flow = flow_obs))
  if (is.na(obs$q_obs_years) || obs$q_obs_years < min_years) next

  # Días usados para las firmas observadas: con dato dentro de los años válidos
  wy    <- ifelse(month(df_est$date) >= 9, year(df_est$date) + 1, year(df_est$date))
  d_ini <- as.Date(obs$q_obs_start); d_fin <- as.Date(obs$q_obs_end)
  frac  <- tapply(!is.na(df_est$flow_obs), wy, mean)
  valid <- as.numeric(names(frac)[frac >= min_frac_year])
  used  <- !is.na(df_est$flow_obs) & wy %in% valid & df_est$date >= d_ini & df_est$date <= d_fin

  # Firmas simuladas en exactamente los mismos días
  sim <- calc_signatures(df_est %>% mutate(flow = ifelse(used, flow_sim, NA_real_)))
  stopifnot(identical(sim$q_obs_years, obs$q_obs_years))

  res[[gid]] <- bind_rows(obs %>% mutate(source = "obs"), sim %>% mutate(source = "sim")) %>%
    dplyr::select(gauge_id, source, all_of(sign_vars), q_obs_years, q_obs_start, q_obs_end)
}

res <- bind_rows(res)


#=========================================================================================
# Métricas de comparación por firma
#=========================================================================================
wide <- res %>%
  pivot_longer(all_of(sign_vars), names_to = "signature", values_to = "value") %>%
  pivot_wider(names_from = source, values_from = value)

summary_tab <- wide %>%
  filter(is.finite(obs), is.finite(sim)) %>%
  group_by(signature) %>%
  summarise(
    n            = n(),
    obs_median   = median(obs),
    sim_median   = median(sim),
    pearson_r    = cor(obs, sim),
    spearman_r   = cor(obs, sim, method = "spearman"),
    bias_median  = median(sim - obs),
    # Errores relativos solo donde el valor observado no es (casi) cero
    rel_bias_median_pct = median(100 * (sim - obs)[abs(obs) > 1e-6] / abs(obs[abs(obs) > 1e-6])),
    rel_abs_err_median_pct = median(100 * abs(sim - obs)[abs(obs) > 1e-6] / abs(obs[abs(obs) > 1e-6])),
    rmse         = sqrt(mean((sim - obs)^2)),
    .groups = "drop"
  ) %>%
  mutate(signature = factor(signature, levels = sign_vars)) %>%
  arrange(signature) %>%
  mutate(across(where(is.numeric), ~ signif(.x, 3)))

write.csv(wide, "./1_data/signatures/signatures_sim_vs_obs.csv", row.names = FALSE)
write.csv(summary_tab, "./1_data/signatures/signatures_sim_vs_obs_summary.csv", row.names = FALSE)
print(summary_tab, width = Inf)

