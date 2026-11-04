# =============================================================================
# Human intervention attributes for CAMELS-PE
# Water availability accreditations and reservoirs
# Data source: ANA (https://snirh.ana.gob.pe/ConsultaIDE/)
# =============================================================================

cat("\f")
rm(list = ls())

library(tidyverse)
library(terra)
library(stringr)
library(tidyr)
# Run this script from the root of the CAMELS-PE project folder (see README.md).
path <- "./1_data/human_intervention/"

cat <- vect("./1_data/sig/camels_pe_catchments.gpkg")   # cuencas corregidas
cat$gauge_id <- sub("^PE_", "", cat$gauge_id)          # gauge_id sin prefijo (prepare_dataset.R lo agrega)
adh <- vect(file.path(path, "AcredDispHidrica.gpkg"))
dam <- vect(file.path(path, "Presas.gpkg"))

# Add unique IDs before spatial intersection
adh$id_adh <- seq_len(nrow(adh))
dam$id_dam <- seq_len(nrow(dam))

# Spatial intersection
adh_cat <- intersect(cat, adh)
dam_cat <- intersect(cat, dam)

# Base catchment table
df_cat <- as_tibble(cat) %>%
  select(gauge_id) %>%
  distinct(gauge_id)

# =============================================================================
# Helper function: dominant category ignoring NA
# =============================================================================

dominant_value <- function(x) {
  x_valid <- x[!is.na(x)]
  
  if (length(x_valid) == 0) {
    return(NA_character_)
  }
  
  names(which.max(table(x_valid)))
}

# =============================================================================
# Water availability accreditation processing
# =============================================================================

df_adh_cat <- as_tibble(adh_cat)

df_sel_adh <- df_adh_cat %>%
  select(gauge_id, id_adh, Uso, Fuentes, VolDer) %>%
  distinct(gauge_id, id_adh, .keep_all = TRUE)

df_adh <- df_sel_adh %>%
  mutate(
    VolDer = suppressWarnings(as.numeric(VolDer)),
    VolDer = na_if(VolDer, -99),
    
    source_type = if_else(
      str_detect(str_to_lower(coalesce(Fuentes, "")), "^acu|^mana"),
      "groundwater",
      "surface"
    )
  ) %>%
  group_by(gauge_id, source_type) %>%
  summarise(
    adh_n = n_distinct(id_adh),
    # NA si ningún registro tiene volumen (VolDer = -99): volumen desconocido, no nulo
    adh_vol = if (all(is.na(VolDer))) NA_real_ else sum(VolDer, na.rm = TRUE) / 1e6,
    adh_use = dominant_value(Uso),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = source_type,
    values_from = c(adh_n, adh_vol, adh_use),
    names_glue = "{source_type}_{.value}",
    values_fill = list(
      adh_n = 0,
      adh_vol = 0,
      adh_use = NA_character_
    )
  )

needed_adh_cols <- c(
  "surface_adh_n",
  "surface_adh_vol",
  "surface_adh_use",
  "groundwater_adh_n",
  "groundwater_adh_vol",
  "groundwater_adh_use"
)

for (col in needed_adh_cols) {
  if (!col %in% names(df_adh)) {
    df_adh[[col]] <- NA
  }
}

df_adh <- df_adh %>%
  mutate(
    surface_adh_n = replace_na(surface_adh_n, 0),
    surface_adh_vol = if_else(surface_adh_n == 0, 0, surface_adh_vol),
    groundwater_adh_n = replace_na(groundwater_adh_n, 0),
    groundwater_adh_vol = if_else(groundwater_adh_n == 0, 0, groundwater_adh_vol),

    surface_adh_use = recode(
      surface_adh_use,
      "Agrícola" = "Agricultural",
      "Poblacional" = "Domestic",
      "Doméstico - Poblacional" = "Domestic",
      "Acuícola" = "Aquaculture",
      "Energético" = "Energy",
      "Industrial" = "Industrial",
      "Minero" = "Mining",
      "Turístico" = "Tourism",
      "Otros Usos" = "Other",
      .default = surface_adh_use
    ),
    
    groundwater_adh_use = recode(
      groundwater_adh_use,
      "Agrícola" = "Agricultural",
      "Poblacional" = "Domestic",
      "Doméstico - Poblacional" = "Domestic",
      "Acuícola" = "Aquaculture",
      "Energético" = "Energy",
      "Industrial" = "Industrial",
      "Minero" = "Mining",
      "Turístico" = "Tourism",
      "Otros Usos" = "Other",
      .default = groundwater_adh_use
    )
  ) %>%
  select(
    gauge_id,
    surface_adh_n,
    surface_adh_vol,
    surface_adh_use,
    groundwater_adh_n,
    groundwater_adh_vol,
    groundwater_adh_use
  )

# =============================================================================
# Reservoir processing
# =============================================================================

df_dam_cat <- as_tibble(dam_cat)

# Tailings dams (counted separately as an indicator of mining activity; capacity is
# reported for few of them, so only the number is kept)
df_tailings <- df_dam_cat %>%
  filter(str_detect(coalesce(USO_EMBALS, ""), "Relaves")) %>%
  distinct(gauge_id, id_dam) %>%
  count(gauge_id, name = "tailings_dam_n")

df_sel_dam <- df_dam_cat %>%
  select(gauge_id, id_dam, NOM_PRESA, RIO_PRESA, CAP_RES_HM, VOL_UT_HM3, USO_EMBALS) %>%
  distinct(gauge_id, id_dam, .keep_all = TRUE) %>%
  # Tailings dams store mine tailings, not water: excluded from the reservoir attributes
  filter(!str_detect(coalesce(USO_EMBALS, ""), "Relaves"))

df_reservoir <- df_sel_dam %>%
  mutate(
    CAP_RES_HM = na_if(CAP_RES_HM, "No disponible"),
    CAP_RES_HM = na_if(CAP_RES_HM, "No aplica"),
    VOL_UT_HM3 = na_if(VOL_UT_HM3, "No disponible"),
    VOL_UT_HM3 = na_if(VOL_UT_HM3, "No aplica"),
    
    CAP_RES_HM = suppressWarnings(as.numeric(CAP_RES_HM)),
    VOL_UT_HM3 = suppressWarnings(as.numeric(VOL_UT_HM3)),
    
    reservoir_vol = coalesce(CAP_RES_HM, VOL_UT_HM3),
    is_dike = str_detect(str_to_upper(NOM_PRESA), "DIQUE")
  ) %>%
  # Los diques de cierre comparten el embalse de su presa principal (mismo río y capacidad):
  # se cuentan como un solo embalse para no sumar su capacidad varias veces
  group_by(gauge_id, RIO_PRESA, reservoir_vol) %>%
  mutate(
    reservoir_id = if_else(any(is_dike) & !is.na(reservoir_vol),
                           paste(RIO_PRESA, reservoir_vol),
                           as.character(id_dam))
  ) %>%
  ungroup() %>%
  arrange(is_dike) %>%
  distinct(gauge_id, reservoir_id, .keep_all = TRUE) %>%
  group_by(gauge_id) %>%
  summarise(
    reservoir_n = n_distinct(reservoir_id),
    reservoir_vol = ifelse(
      all(is.na(reservoir_vol)),
      NA_real_,
      sum(reservoir_vol, na.rm = TRUE)
    ),
    reservoir_use_raw = dominant_value(USO_EMBALS),
    .groups = "drop"
  ) %>%
  mutate(
    reservoir_use = case_when(
      is.na(reservoir_use_raw) ~ NA_character_,
      str_detect(reservoir_use_raw, "No disponible") ~ "Unknown",
      str_detect(reservoir_use_raw, "Min") ~ "Mining",
      str_detect(reservoir_use_raw, "Riego") &
        str_detect(reservoir_use_raw, "Suministro|Aprovechamiento") ~ "Multi_purpose",
      str_detect(reservoir_use_raw, "Suministro") &
        str_detect(reservoir_use_raw, "Aprovechamiento") ~ "Multi_purpose",
      str_detect(reservoir_use_raw, "Riego") ~ "Irrigation",
      str_detect(reservoir_use_raw, "Suministro") ~ "Water Supply",
      str_detect(reservoir_use_raw, "Aprovechamiento") ~ "Hydropower",
      TRUE ~ "Other"
    )
  ) %>%
  select(
    gauge_id,
    reservoir_n,
    reservoir_vol,
    reservoir_use
  )

# =============================================================================
# Final database
# =============================================================================

df_human_intervention <- df_cat %>%
  left_join(df_adh, by = "gauge_id") %>%
  left_join(df_reservoir, by = "gauge_id") %>%
  left_join(df_tailings, by = "gauge_id") %>%
  mutate(
    tailings_dam_n = replace_na(tailings_dam_n, 0L),
    # Sin registros -> 0; con registros pero sin volumen registrado -> NA (desconocido)
    surface_adh_n = replace_na(surface_adh_n, 0),
    surface_adh_vol = if_else(surface_adh_n == 0, 0, surface_adh_vol),
    groundwater_adh_n = replace_na(groundwater_adh_n, 0),
    groundwater_adh_vol = if_else(groundwater_adh_n == 0, 0, groundwater_adh_vol),
    reservoir_n = replace_na(reservoir_n, 0),
    
    surface_adh_use = case_when(
      surface_adh_n == 0 ~ "None",
      is.na(surface_adh_use) ~ "Unknown",
      TRUE ~ surface_adh_use
    ),
    
    groundwater_adh_use = case_when(
      groundwater_adh_n == 0 ~ "None",
      is.na(groundwater_adh_use) ~ "Unknown",
      TRUE ~ groundwater_adh_use
    ),
    
    reservoir_use = case_when(
      reservoir_n == 0 ~ "None",
      is.na(reservoir_use) ~ "Unknown",
      TRUE ~ reservoir_use
    )
  )

# =============================================================================
# Degree of regulation (DOR; Lehner et al., 2011)
# Reservoir capacity / mean annual observed streamflow volume [%]
# Observed q_mean from catchment_signatures.csv (valid hydrological years, same as the
# signatures); NA in catchments without enough observed record
# =============================================================================

sig  <- read.csv("./1_data/signatures/catchment_signatures.csv")
topo <- read.csv("./1_data/topography/catchment_topography.csv")
sig$gauge_id  <- sub("^PE_", "", sig$gauge_id)
topo$gauge_id <- sub("^PE_", "", topo$gauge_id)

df_qvol <- tibble(
  gauge_id = sig$gauge_id,
  q_mean = sig$q_mean                                                    # mm/d
) %>%
  mutate(q_vol = q_mean * 365.25 * topo$area[match(gauge_id, topo$gauge_id)] * 1e-3)   # hm3/yr

# Volumes normalised by catchment area (1 hm3 / 1 km2 = 1000 mm; ADH in mm/d, reservoir
# storage in mm) and accredited volume
# as % of the mean annual observed streamflow volume
df_human_intervention <- df_human_intervention %>%
  left_join(select(df_qvol, gauge_id, q_vol), by = "gauge_id") %>%
  mutate(
    area = topo$area[match(gauge_id, topo$gauge_id)],
    surface_adh_mm = 1e3 * surface_adh_vol / area / 365.25,              # mm/d
    groundwater_adh_mm = 1e3 * groundwater_adh_vol / area / 365.25,      # mm/d
    reservoir_mm = if_else(reservoir_n == 0, 0, 1e3 * reservoir_vol / area),   # mm
    adh_ratio = if_else(is.na(q_vol), NA_real_,
                        100 * (surface_adh_vol + groundwater_adh_vol) / q_vol),  # %
    reservoir_dor = case_when(
      is.na(q_vol) ~ NA_real_,
      reservoir_n == 0 ~ 0,
      TRUE ~ 100 * reservoir_vol / q_vol
    )
  ) %>%
  select(-area)

# =============================================================================
# Near-natural catchments: no water reservoirs, no tailings dams and anthropic land
# cover (agriculture incl. planted forest and pasture + urban + mining, MapBiomas 2024)
# below 10 % of the catchment area. ADH are not used (they are not use rights).
# =============================================================================

lc <- read.csv("./1_data/land_cover/land_cover_subclass_annual.csv")
lc <- lc[lc$year == max(lc$year), ]
lc$gauge_id <- sub("^PE_", "", lc$gauge_id)
anthro_cols <- c("planted_forest_perc", "pasture_perc", "mosaic_agri_pasture_perc", "oil_palm_perc",
                 "rice_perc", "other_crops_perc", "urban_perc", "mining_perc")
anthro_perc <- rowSums(lc[, anthro_cols])[match(df_human_intervention$gauge_id, lc$gauge_id)]
stopifnot(!anyNA(anthro_perc))

df_human_intervention <- df_human_intervention %>%
  mutate(near_natural = as.integer(reservoir_n == 0 & tailings_dam_n == 0 & anthro_perc < 10)) %>%
  select(gauge_id,
         surface_adh_n, surface_adh_vol, surface_adh_mm, surface_adh_use,
         groundwater_adh_n, groundwater_adh_vol, groundwater_adh_mm, groundwater_adh_use,
         adh_ratio,
         reservoir_n, reservoir_vol, reservoir_mm, reservoir_dor, reservoir_use,
         tailings_dam_n, near_natural)

write.csv(
  df_human_intervention,
  "./1_data/human_intervention/human_intervention.csv",
  row.names = FALSE
)