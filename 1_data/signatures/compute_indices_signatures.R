# Calcular firmas hidrológicas y climáticas para CAMELS-PE
# By Harold Llauca
cat('\f')
rm(list=ls())

# Directorio de trabajo
# Run this script from the root of the CAMELS-PE project folder (see README.md).
# Instalar y cargar paquetes
library(dplyr)
library(lubridate)
library(zoo)
library(tidyr)
library(readr)
library(purrr)

# Leer bases de datos
prec <- read_csv("./1_data/hydrometeorological/prec_PISCO_1981_2025.csv")
evap <- read_csv("./1_data/hydrometeorological/evap_HS_1981_2025.csv")
temp <- read_csv("./1_data/hydrometeorological/tmed_PISCO_1981_2025.csv")
flow <- read_csv("./1_data/hydrometeorological/flow_OBS_1981_2025.csv")   # caudal observado

# Criterios de disponibilidad del caudal observado para las firmas hidrológicas
min_frac_year <- 0.90   # fracción mínima de días con dato para que un año hidrológico sea válido
min_years     <- 5      # número mínimo de años hidrológicos válidos por estación

# Reconstruir fecha y limpiar
prec <- prec %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>%
  dplyr::select(-year, -month, -day)

temp <- temp %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>%
  dplyr::select(-year, -month, -day)

evap <- evap %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>%
  dplyr::select(-year, -month, -day)

flow <- flow %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>%
  dplyr::select(-year, -month, -day)


# Pivotear a formato largo
prec_long <- prec %>%
  pivot_longer(-date, names_to = "gauge_id", values_to = "prec")

temp_long <- temp %>%
  pivot_longer(-date, names_to = "gauge_id", values_to = "temp")

evap_long <- evap %>%
  pivot_longer(-date, names_to = "gauge_id", values_to = "evap")

flow_long <- flow %>%
  pivot_longer(-date, names_to = "gauge_id", values_to = "flow")


# Unir datasets
camels_pe <- prec_long %>%
  left_join(temp_long, by = c("gauge_id","date")) %>%
  left_join(evap_long, by = c("gauge_id","date")) %>%
  left_join(flow_long, by = c("gauge_id","date")) %>%
  filter(date >= as.Date("1981-01-01") & date <= as.Date("2025-12-31"))


#=========================================================================================
# Función para calcular las firmas hidroclimáticas e hidrológicas por estación
#=========================================================================================
calc_signatures <- function(df_station) {
  
  station_id <- unique(df_station$gauge_id)
  P   <- df_station$prec
  PET <- df_station$evap
  Q   <- df_station$flow

  
  # Climate indices
  #-----------------------------------------------------------------------------
  p_mean   <- mean(P)
  pet_mean <- mean(PET, na.rm = TRUE)
  aridity  <- ifelse(p_mean > 0, pet_mean / p_mean, NA)
  years    <- unique(year(df_station$date))
  
  # Auxiliary function to compute the frequency and duration of events over the whole series,
  # following Addor et al. (2017) and https://github.com/naddor/camels:
  # frequency = event days / available days * 365.25 (days per year)
  # duration  = mean length of consecutive event days
  # Both are 0 when no event occurs, and NA when there are no data
  calc_runs <- function(x, condition) {
    avail <- !is.na(x)
    if (!any(avail)) return(c(freq = NA_real_, dur = NA_real_))

    cond_vals <- condition(x)
    cond_vals[is.na(cond_vals)] <- FALSE
    if (!any(cond_vals)) return(c(freq = 0, dur = 0))

    runs <- rle(cond_vals)
    c(freq = sum(cond_vals) / sum(avail) * 365.25,
      dur  = mean(runs$lengths[runs$values]))
  }

  # Season with the highest number of event days. As in naddor/camels (clim_indices.R),
  # NA when any two seasons with events have the same number of days (rank() ties)
  calc_timing <- function(dates) {
    if (length(dates) == 0) return(NA_character_)
    season <- c("DJF", "DJF", "MAM", "MAM", "MAM", "JJA",
                "JJA", "JJA", "SON", "SON", "SON", "DJF")[month(dates)]
    n_sea <- table(season)
    if (max(rank(n_sea) %% 1 != 0)) return(NA_character_)
    names(which.max(n_sea))
  }

  # High precipitation frequency and duration
  # High precipitation days: P >= 5 times the mean daily precipitation
  highP_stats <- calc_runs(P, function(v) v >= 5 * p_mean)
  high_prec_freq <- highP_stats["freq"]
  high_prec_dur  <- highP_stats["dur"]

  # Timing of high precipitation
  # Defined as the season with the highest occurrence of high precipitation days
  high_prec_timing <- calc_timing(df_station$date[which(P >= 5 * p_mean)])

  # Low precipitation frequency and duration
  # Low precipitation days: P < 1 mm/day
  lowP_stats <- calc_runs(P, function(v) v < 1)
  low_prec_freq <- lowP_stats["freq"]
  low_prec_dur  <- lowP_stats["dur"]

  # Timing of low precipitation
  # Defined as the season with the highest occurrence of low precipitation days
  low_prec_timing <- calc_timing(df_station$date[which(P < 1)])

  # Precipitation seasonality
  # Computed following Woods (2009) and Addor et al. (2017):
  # p_seasonality = delta_p * sign(delta_t) * cos(2*pi*(s_p - s_t)/tau)
  # where delta_p is the amplitude of the annual precipitation cycle relative to mean precipitation
  # and s_p, s_t are the phases of the precipitation and temperature cycles. The sine curves are
  # fitted as linear models in sin/cos terms; their amplitudes are non-negative, so sign(delta_t) = 1
  df_tmp <- df_station %>%
    filter(!(month(date) == 2 & day(date) == 29)) %>%
    mutate(
      doy = yday(date),
      sin_term = sin(2 * pi * doy / 365),
      cos_term = cos(2 * pi * doy / 365)
    ) %>%
    filter(!is.na(prec), !is.na(temp))
  
  if (nrow(df_tmp) > 10) {
    
    fit_p <- lm(prec ~ sin_term + cos_term, data = df_tmp)
    fit_t <- lm(temp ~ sin_term + cos_term, data = df_tmp)
    
    coef_p <- coef(fit_p)
    coef_t <- coef(fit_t)
    
    bp <- coef_p["sin_term"]
    cp <- coef_p["cos_term"]
    bt <- coef_t["sin_term"]
    ct <- coef_t["cos_term"]
    
    if (all(is.finite(c(bp, cp, bt, ct))) && coef_p["(Intercept)"] > 0) {
      phase_p <- atan2(cp, bp)
      phase_t <- atan2(ct, bt)
      delta_phase <- phase_p - phase_t
      delta_p <- sqrt(bp^2 + cp^2) / coef_p["(Intercept)"]
      p_seasonality <- unname(delta_p * cos(delta_phase))
    } else {
      p_seasonality <- NA
    }
    
  } else {
    p_seasonality <- NA
  }
  

  #------------------------------------------------------------
  # Hydrological signatures (observed streamflow)
  #------------------------------------------------------------
  # Only complete hydrological years (1 September to 31 August) with at least min_frac_year of
  # days with observations are used; the remaining days are set to NA. Signatures are NA when the
  # station has fewer than min_years valid years. Each station uses its own observation period.
  water_year <- ifelse(month(df_station$date) >= 9, year(df_station$date) + 1, year(df_station$date))
  n_days_hy  <- table(water_year)
  frac_hy    <- tapply(!is.na(Q), water_year, mean)
  valid_hy   <- as.numeric(names(frac_hy)[frac_hy >= min_frac_year & n_days_hy[names(frac_hy)] >= 365])

  Q[!(water_year %in% valid_hy)] <- NA
  q_obs_years <- length(valid_hy)
  if (q_obs_years < min_years) Q[] <- NA

  q_obs_start <- if (q_obs_years >= min_years) as.character(as.Date(paste0(min(valid_hy) - 1, "-09-01"))) else NA_character_
  q_obs_end   <- if (q_obs_years >= min_years) as.character(as.Date(paste0(max(valid_hy), "-08-31"))) else NA_character_
  q_obs_frac  <- if (q_obs_years >= min_years) mean(!is.na(Q[water_year %in% valid_hy])) else NA_real_
  df_station$flow <- Q

  # Mean daily discharge
  q_mean <- mean(Q, na.rm = TRUE)

  # Median daily discharge
  q_median <- median(Q, na.rm = TRUE)

  # Runoff ratio
  # Computed as the ratio of mean daily discharge to mean daily precipitation,
  # over the days with both precipitation and streamflow data
  ok_pq <- !is.na(P) & !is.na(Q)
  runoff_ratio <- if (any(ok_pq)) mean(Q[ok_pq]) / mean(P[ok_pq]) else NA_real_
  
  # Streamflow precipitation elasticity
  # Computed at the annual scale following Sankarasubramanian et al. (2001)
  # using the hydrological year adopted for Peru (1 September to 31 August), as in Addor et al. (2017):
  # median of the annual anomalies of mean daily Q and P relative to their long-term means
  df_annual <- df_station[ok_pq, ] %>%
    mutate(water_year = ifelse(month(date) >= 9, year(date) + 1, year(date))) %>%
    group_by(water_year) %>%
    summarise(
      P = mean(prec),
      Q = mean(flow),
      .groups = "drop"
    )

  if (nrow(df_annual) > 1) {

    P_bar <- mean(P[ok_pq])
    Q_bar <- mean(Q[ok_pq])

    elas_vals <- ((df_annual$Q - Q_bar) / Q_bar) /
      ((df_annual$P - P_bar) / P_bar)

    stream_elas <- median(elas_vals)

  } else {
    stream_elas <- NA
  }
  
  # Flow duration curve slope
  # Computed following Sawicz et al. (2011), as in Addor et al. (2017):
  # slope_fdc = (ln Q33 - ln Q66) / (0.66 - 0.33)
  # where Q33 and Q66 are the flows exceeded 33% and 66% of the time (NA if Q66 = 0)
  Q33 <- unname(quantile(Q, probs = 1 - 0.33, na.rm = TRUE))
  Q66 <- unname(quantile(Q, probs = 1 - 0.66, na.rm = TRUE))

  if (is.finite(Q33) && is.finite(Q66) && Q66 > 0) {
    slope_fdc <- (log(Q33) - log(Q66)) / (0.66 - 0.33)
  } else {
    slope_fdc <- NA
  }
  
  # Baseflow index
  # Computed as the ratio of baseflow volume to total discharge volume, with hydrograph
  # separation performed using the Lyne and Hollick filter as standardised by Ladson et al. (2013)
  # (alpha = 0.925, 3 passes forward-backward-forward, 30 values reflected at each end),
  # following the CAMELS definition (Addor et al., 2017)

  calc_bfi <- function(Q, a = 0.925, passes = 3, n_reflect = 30) {
    Q <- as.numeric(Q)
    N <- length(Q)

    if (anyNA(Q) || N <= n_reflect + 1 || any(Q < 0)) return(NA_real_)

    # reflect the first and last n_reflect values to avoid warm-up effects
    Qr <- c(Q[(n_reflect + 1):2], Q, Q[(N - 1):(N - n_reflect)])

    # one pass of the filter; quickflow is initialised with init_qf and may be negative
    lh_pass <- function(qb, init_qf, forward) {
      n   <- length(qb)
      idx <- if (forward) seq_len(n) else rev(seq_len(n))
      qf  <- numeric(n)
      qf[idx[1]] <- init_qf
      for (k in 2:n) {
        i <- idx[k]; j <- idx[k - 1]
        qf[i] <- a * qf[j] + 0.5 * (1 + a) * (qb[i] - qb[j])
      }
      ifelse(qf > 0, qb - qf, qb)
    }

    bf <- lh_pass(Qr, Qr[1], forward = TRUE)
    for (p in seq_len(round((passes - 1) / 2))) {
      bf <- lh_pass(bf, bf[length(bf)], forward = FALSE)
      bf <- lh_pass(bf, bf[1], forward = TRUE)
    }

    bf <- bf[(n_reflect + 1):(n_reflect + N)]
    bf[bf < 0] <- 0
    sum(bf) / sum(Q)
  }
  
  # With missing data, the BFI is computed for each continuous segment longer than n_reflect days
  # and the station value is the length-weighted mean of the segment BFIs (as in Ladson et al., 2013)
  calc_bfi_segments <- function(Q, n_reflect = 30) {
    rl  <- rle(!is.na(Q))
    end <- cumsum(rl$lengths)
    ok  <- which(rl$values & rl$lengths > n_reflect)
    if (length(ok) == 0) return(NA_real_)
    bfi <- sapply(ok, function(k) calc_bfi(Q[(end[k] - rl$lengths[k] + 1):end[k]], n_reflect = n_reflect))
    weighted.mean(bfi, rl$lengths[ok], na.rm = TRUE)
  }

  baseflow_index <- tryCatch(
    calc_bfi_segments(Q),
    error = function(e) NA_real_
  )

  # Mean half-flow date
  # Defined as the day of the hydrological year when cumulative discharge
  # reaches half of the annual discharge total (valid hydrological years, available days)
  df_water <- df_station %>%
    mutate(water_year = ifelse(month(date) >= 9, year(date) + 1, year(date))) %>%
    filter(water_year %in% valid_hy, !is.na(flow))

  hfd_per_year <- if (nrow(df_water) == 0) NULL else df_water %>%
    group_by(water_year) %>%
    arrange(date, .by_group = TRUE) %>%
    mutate(
      cumQ = cumsum(flow),
      totalQ = sum(flow)
    ) %>%
    filter(totalQ > 0) %>%
    summarise(hfd_date = date[which(cumQ >= 0.5 * totalQ)[1]], .groups = "drop") %>%
    mutate(
      hydro_start = as.Date(paste0(water_year - 1, "-09-01")),
      hfd_doy = as.numeric(hfd_date - hydro_start) + 1
    )
  
  hfd_mean <- if (nrow(df_water) > 0) mean(hfd_per_year$hfd_doy, na.rm = TRUE) else NA_real_
  
  # Flow quantiles
  # Q5 represents low flows and Q95 represents high flows
  Q5  <- quantile(Q, probs = 0.05, na.rm = TRUE)
  Q95 <- quantile(Q, probs = 0.95, na.rm = TRUE)
  
  # High-flow frequency and duration
  # high_q_freq is the frequency of high-flow days
  # high_q_dur is the average duration of high-flow events
  # High-flow threshold is defined as 9 times the median daily flow
  highQ_stats <- calc_runs(Q, function(v) v > 9 * q_median)
  high_q_freq <- highQ_stats["freq"]
  high_q_dur  <- highQ_stats["dur"]
  
  # Low-flow frequency and duration
  # low_q_freq is the frequency of low-flow days
  # low_q_dur is the average duration of low-flow events
  # Low-flow threshold is defined as 0.2 times the mean daily flow
  lowQ_stats <- calc_runs(Q, function(v) v <= 0.2 * q_mean)
  low_q_freq <- lowQ_stats["freq"]
  low_q_dur  <- lowQ_stats["dur"]

  # Zero-flow frequency
  # Computed as the percentage of days with zero discharge (Q <= 0)
  zero_q_freq <- mean(Q <= 0, na.rm = TRUE) * 100
  
  #------------------------------------------------------------
  # Consolidar resultados en tibble
  #------------------------------------------------------------
  tibble(
    gauge_id = station_id,
    
    # Climate indices
    p_mean,
    pet_mean,
    aridity,
    p_seasonality,
    high_prec_freq,
    high_prec_dur,
    high_prec_timing,
    low_prec_freq,
    low_prec_dur,
    low_prec_timing,
    
    # Hydrological signatures
    q_mean,
    runoff_ratio,
    stream_elas,
    baseflow_index,
    slope_fdc,
    hfd_mean,
    Q5,
    Q95,
    high_q_freq,
    high_q_dur,
    low_q_freq,
    low_q_dur,
    zero_q_freq,

    # Observed streamflow period used for the hydrological signatures
    q_obs_years,
    q_obs_start,
    q_obs_end,
    q_obs_frac
  ) %>%
    mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA_real_, .x)))
}


#=========================================================================================
# Loop sobre todas las estaciones CAMELS-PE
#=========================================================================================

# Lista de estaciones únicas
gauge_ids <- unique(camels_pe$gauge_id)
indices_all <- list()

# Bucle sobre todas las estaciones
for (i in seq_along(gauge_ids)) {
  gid <- gauge_ids[i]
  cat('\f')
  cat("Procesando estación", gid, "(", i, "de", length(gauge_ids), ")\n")
  
  df_est <- camels_pe %>% 
    filter(gauge_id == gid)
  
  result <- calc_signatures(df_est)
  
  indices_all[[i]] <- result
}

# Unir resultados
indices_all <- bind_rows(indices_all)

# Guardar resultado
indices_signatures <- indices_all

# PET media e índice de aridez con cada formulación de PET (1981–2025, mismo periodo que
# pet_mean/aridity). Hargreaves-Samani es la referencia (pet_mean, aridity); Priestley-Taylor y
# Oudin representan la incertidumbre asociada al método (ver compute_pet.R)
pet_alt <- lapply(c(pt = "evap_PT_1981_2025.csv", ou = "evap_OU_1981_2025.csv"), function(f) {
  x <- read_csv(file.path("./1_data/hydrometeorological", f), show_col_types = FALSE)
  colMeans(as.matrix(x[, -(1:3)]), na.rm = TRUE)
})
indices_signatures <- indices_signatures %>%
  mutate(
    pet_mean_pt = unname(pet_alt$pt[gauge_id]),
    pet_mean_ou = unname(pet_alt$ou[gauge_id]),
    aridity_pt  = ifelse(p_mean > 0, pet_mean_pt / p_mean, NA),
    aridity_ou  = ifelse(p_mean > 0, pet_mean_ou / p_mean, NA)
  ) %>%
  relocate(pet_mean_pt, pet_mean_ou, aridity_pt, aridity_ou, .after = aridity)
stopifnot(!anyNA(indices_signatures$pet_mean_pt), !anyNA(indices_signatures$pet_mean_ou))

write.csv(indices_signatures, "./1_data/signatures/catchment_signatures.csv", row.names = FALSE)
print(indices_all)


#=========================================================================================
# Firmas hidrológicas con caudal simulado (PISCO-ARNOVIC), archivo aparte
#=========================================================================================
# Mismo cálculo que las firmas observadas, sobre la serie simulada completa (años
# hidrológicos completos dentro de 1981–2025) para las 136 cuencas
flow_sim_long <- read_csv("./1_data/hydrometeorological/flow_PISCO_ARNOVIC_1981_2025.csv") %>%
  mutate(date = as.Date(paste(year, month, day, sep = "-"))) %>%
  dplyr::select(-year, -month, -day) %>%
  pivot_longer(-date, names_to = "gauge_id", values_to = "flow")

camels_pe_sim <- camels_pe %>%
  dplyr::select(-flow) %>%
  left_join(flow_sim_long, by = c("gauge_id", "date"))

signatures_sim <- list()

for (i in seq_along(gauge_ids)) {
  gid <- gauge_ids[i]
  cat('\f')
  cat("Procesando estación (simulado)", gid, "(", i, "de", length(gauge_ids), ")\n")

  df_est <- camels_pe_sim %>%
    filter(gauge_id == gid)

  signatures_sim[[i]] <- calc_signatures(df_est)
}

signatures_sim <- bind_rows(signatures_sim) %>%
  dplyr::select(gauge_id, q_mean:zero_q_freq,
                q_sim_years = q_obs_years, q_sim_start = q_obs_start, q_sim_end = q_obs_end)

write.csv(signatures_sim, "./1_data/signatures/catchment_signatures_sim.csv", row.names = FALSE)
