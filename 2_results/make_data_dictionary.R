# Create the CAMELS-PE data dictionary (01_metadata/data_dictionary.csv)
# Se ejecuta después de prepare_dataset.R: comprueba que todas las columnas de los archivos
# del dataset estén documentadas y que no se documenten columnas inexistentes.
# By Harold Llauca
rm(list=ls())
cat('\f')

library(data.table)
# Run this script from the root of the CAMELS-PE project folder (see README.md).
ds <- './2_results/CAMELS-PE'

dict <- list()
add <- function(folder, file, category, variable, description, unit = '-', source = '-') {
  dict[[length(dict) + 1]] <<- data.table(folder, file, category, variable, description, unit, source)
}
id_row <- function(folder, file, category) {
  add(folder, file, category, 'gauge_id', 'Unique gauge identifier (PE_xxxxx)', '-', 'SENAMHI')
}

P_SRC   <- 'PISCOp v3.0'
T_SRC   <- 'PISCOt v1.2'
PET_HS  <- 'Hargreaves-Samani (FAO-56 Eq. 52) from PISCOt v1.2'
Q_OBS   <- 'SENAMHI (quality-controlled)'
Q_SIM   <- 'PISCO-ARNOVIC v1.1'
MAPB    <- 'MapBiomas Peru, Ecuador and Colombia (Collection 4)'
GLIM    <- 'GLiM v1.1'
SIGNOTE <- ' Computed from complete hydrological years (1 September-31 August) with at least 90% of daily observations; NA for catchments with fewer than 5 valid years.'

# 01_metadata ---------------------------------------------------------------------------
f <- 'stations.csv'; m <- '01_metadata'; k <- 'metadata'
add(m, f, k, 'gauge_id', 'Unique gauge identifier (PE_xxxxx)', '-', 'SENAMHI')
add(m, f, k, 'gauge_name', 'Gauge name', '-', 'SENAMHI')
add(m, f, k, 'gauge_region', 'Drainage region (Pacific, Atlantic or Titicaca)', '-', 'ANA')
add(m, f, k, 'gauge_lat', 'Gauge latitude (WGS84): catchment outlet, i.e. the gauge location snapped to the drainage network of the 90 m SRTM DEM used for delineation', 'degN', 'SENAMHI; SRTM 90 m')
add(m, f, k, 'gauge_lon', 'Gauge longitude (WGS84): catchment outlet, i.e. the gauge location snapped to the drainage network of the 90 m SRTM DEM used for delineation', 'degE', 'SENAMHI; SRTM 90 m')
add(m, f, k, 'gauge_elev', 'Gauge elevation (DEM value at the gauge location)', 'm_asl',
    if (file.exists('./1_data/topography/raw/fabdem_gauge_elevation.csv')) 'FABDEM v1.2' else 'SRTM 90 m')
add(m, f, k, 'gauge_record_start', 'Date of the first valid streamflow observation (after quality control)', 'YYYY-MM-DD', 'Derived from data')
add(m, f, k, 'gauge_record_end', 'Date of the last valid streamflow observation (after quality control)', 'YYYY-MM-DD', 'Derived from data')
add(m, f, k, 'gauge_perc_obs', 'Percentage of days with valid streamflow observations in 1981-2025', '%', 'Derived from data')
add(m, f, k, 'flow_obs_n_suspect', 'Number of observed streamflow values flagged as suspect and kept (flow_obs_flag = 3)', 'days', 'Derived from data')
add(m, f, k, 'flow_obs_n_fail', 'Number of observed streamflow values that failed quality control and were removed (flow_obs_flag = 4)', 'days', 'Derived from data')
add(m, f, k, 'flow_obs_perc_fail', 'Percentage of the observed streamflow values removed by quality control', '%', 'Derived from data')
add(m, f, k, 'name_cat', 'Official name of the hydrographic unit', '-', 'ANA')
add(m, f, k, 'is_nested', 'Whether the catchment belongs to a group of nested catchments', 'logical', 'Derived from spatial topology')
add(m, f, k, 'nested_group_id', 'Gauge identifier of the most downstream gauge of the nested group', '-', 'Derived from spatial topology')
add(m, f, k, 'nested_group_size', 'Number of gauges in the nested group', '-', 'Derived from spatial topology')
add(m, f, k, 'downstream_gauge_id', 'Gauge identifier of the immediately downstream catchment (NONE if absent)', '-', 'Derived from spatial topology')
add(m, f, k, 'upstream_gauge_id', 'Gauge identifier of the largest immediately upstream catchment (NONE if absent)', '-', 'Derived from spatial topology')

# 02_attributes: topography -------------------------------------------------------------
a <- '02_attributes'
f <- 'topographic_attributes.csv'; k <- 'topographic'
id_row(a, f, k)
add(a, f, k, 'area', 'Catchment area (geodesic, WGS84 ellipsoid)', 'km2', 'Catchment polygons')
add(a, f, k, 'perimeter', 'Catchment perimeter (geodesic, WGS84 ellipsoid)', 'km', 'Catchment polygons')
add(a, f, k, 'elev_min', 'Minimum catchment elevation', 'm_asl', 'FABDEM v1.2')
add(a, f, k, 'elev_max', 'Maximum catchment elevation', 'm_asl', 'FABDEM v1.2')
add(a, f, k, 'elev_mean', 'Mean catchment elevation', 'm_asl', 'FABDEM v1.2')
add(a, f, k, 'elev_median', 'Median catchment elevation', 'm_asl', 'FABDEM v1.2')
add(a, f, k, 'slope_mean', 'Mean catchment slope (pixel slope at 30 m, 1000 tan(slope), averaged over the catchment)', 'm/km', 'FABDEM v1.2')

# 02_attributes: climatic indices (1981-2025) -------------------------------------------
f <- 'climatic_indices.csv'; k <- 'climatic'
id_row(a, f, k)
add(a, f, k, 'p_mean', 'Mean daily precipitation (1981-2025)', 'mm/day', P_SRC)
add(a, f, k, 'pet_mean_hs', 'Mean daily potential evapotranspiration, Hargreaves-Samani (reference; 1981-2025)', 'mm/day', PET_HS)
add(a, f, k, 'aridity_hs', 'Aridity index: pet_mean_hs / p_mean (reference)', '-', paste(P_SRC, 'and', T_SRC))
add(a, f, k, 'pet_mean_pt', 'Mean daily potential evapotranspiration, Priestley-Taylor (1981-2025)', 'mm/day', 'PISCOt v1.2 and ERA5-Land')
add(a, f, k, 'pet_mean_ou', 'Mean daily potential evapotranspiration, Oudin (1981-2025)', 'mm/day', T_SRC)
add(a, f, k, 'aridity_pt', 'Aridity index: pet_mean_pt / p_mean', '-', 'PISCOp v3.0, PISCOt v1.2 and ERA5-Land')
add(a, f, k, 'aridity_ou', 'Aridity index: pet_mean_ou / p_mean', '-', paste(P_SRC, 'and', T_SRC))
add(a, f, k, 'p_seasonality', 'Seasonality and timing of precipitation (positive: precipitation peaks in phase with temperature, i.e. in summer; negative: in winter; Woods, 2009)', '-', P_SRC)
add(a, f, k, 'high_prec_freq', 'Frequency of high-precipitation days (>= 5 times p_mean)', 'days/year', P_SRC)
add(a, f, k, 'high_prec_dur', 'Average duration of high-precipitation events (consecutive days >= 5 times p_mean)', 'days', P_SRC)
add(a, f, k, 'high_prec_timing', 'Season with most high-precipitation days (DJF, MAM, JJA, SON; NA if any two seasons have the same number of days, as in Addor et al., 2017)', 'season', P_SRC)
add(a, f, k, 'low_prec_freq', 'Frequency of dry days (< 1 mm/day)', 'days/year', P_SRC)
add(a, f, k, 'low_prec_dur', 'Average duration of dry periods (consecutive days < 1 mm/day)', 'days', P_SRC)
add(a, f, k, 'low_prec_timing', 'Season with most dry days (DJF, MAM, JJA, SON; NA if any two seasons have the same number of days, as in Addor et al., 2017)', 'season', P_SRC)

# 02_attributes: hydrological signatures (observed and simulated) -----------------------
sig <- list(
  q_mean         = c('Mean daily streamflow', 'mm/day'),
  runoff_ratio   = c('Runoff ratio: q_mean / mean daily precipitation (same days)', '-'),
  stream_elas    = c('Streamflow elasticity with respect to precipitation (Sankarasubramanian et al., 2001)', '-'),
  baseflow_index = c('Baseflow index: ratio of mean daily baseflow to mean daily streamflow (Ladson et al., 2013)', '-'),
  slope_fdc      = c('Slope of the flow duration curve between the 33rd and 66th percentiles (log space)', '-'),
  hfd_mean       = c('Mean half-flow date: day of the hydrological year (from 1 September) by which half of the annual streamflow has passed', 'day'),
  Q5             = c('5% flow quantile (low flow)', 'mm/day'),
  Q95            = c('95% flow quantile (high flow)', 'mm/day'),
  high_q_freq    = c('Frequency of high-flow days (> 9 times the median daily flow)', 'days/year'),
  high_q_dur     = c('Average duration of high-flow events (consecutive days > 9 times the median daily flow)', 'days'),
  low_q_freq     = c('Frequency of low-flow days (<= 0.2 times the mean daily flow)', 'days/year'),
  low_q_dur      = c('Average duration of low-flow events (consecutive days <= 0.2 times the mean daily flow)', 'days'),
  zero_q_freq    = c('Percentage of days with zero streamflow', '%')
)
f <- 'hydrological_signatures.csv'; k <- 'hydrological'
id_row(a, f, k)
for (v in names(sig)) add(a, f, k, v, paste0(sig[[v]][1], ', from observed streamflow.', SIGNOTE), sig[[v]][2],
                          if (v %in% c('runoff_ratio', 'stream_elas')) paste(Q_OBS, 'and', P_SRC) else Q_OBS)
add(a, f, k, 'q_obs_years', 'Number of valid hydrological years used for the observed signatures', 'years', Q_OBS)
add(a, f, k, 'q_obs_start', 'Start of the first valid hydrological year', 'YYYY-MM-DD', Q_OBS)
add(a, f, k, 'q_obs_end', 'End of the last valid hydrological year', 'YYYY-MM-DD', Q_OBS)
add(a, f, k, 'q_obs_frac', 'Fraction of days with observations within the valid hydrological years', '-', Q_OBS)

f <- 'hydrological_signatures_sim.csv'
id_row(a, f, k)
for (v in names(sig)) add(a, f, k, v, paste0(sig[[v]][1], ', from simulated streamflow (complete hydrological years 1981-2025).'),
                          sig[[v]][2], if (v %in% c('runoff_ratio', 'stream_elas')) paste(Q_SIM, 'and', P_SRC) else Q_SIM)
add(a, f, k, 'q_sim_years', 'Number of hydrological years used for the simulated signatures', 'years', Q_SIM)
add(a, f, k, 'q_sim_start', 'Start of the first hydrological year', 'YYYY-MM-DD', Q_SIM)
add(a, f, k, 'q_sim_end', 'End of the last hydrological year', 'YYYY-MM-DD', Q_SIM)

# 02_attributes: land cover --------------------------------------------------------------
cls <- list(
  forest_perc     = 'forest formation',
  agricul_perc    = 'agricultural area (including pasture and planted forest)',
  non_forest_perc = 'natural non-forest formation',
  non_veget_perc  = 'non-vegetated area',
  water_perc      = 'water bodies including glaciers (water_perc = glacier_perc + water_body_perc)',
  non_identi_perc = 'not observed area'
)
f <- 'land_cover_attributes.csv'; k <- 'land_cover'
id_row(a, f, k)
for (v in names(cls)) add(a, f, k, v, paste('Percentage of the catchment covered by', cls[[v]], 'in 2024'), '%', MAPB)
add(a, f, k, 'glacier_perc', 'Percentage of the catchment covered by glaciers in 2024', '%', MAPB)
add(a, f, k, 'water_body_perc', 'Percentage of the catchment covered by rivers, lakes and aquaculture in 2024 (excluding glaciers)', '%', MAPB)
add(a, f, k, 'land_dominant_class', 'Land cover class with the largest area in 2024', '-', MAPB)

f <- 'land_cover_class_annual.csv'
id_row(a, f, k)
add(a, f, k, 'year', 'Year of the land cover map (1985-2024)', 'year', MAPB)
for (v in names(cls)) add(a, f, k, v, paste('Percentage of the catchment covered by', cls[[v]], 'in the given year'), '%', MAPB)
add(a, f, k, 'land_dominant_class', 'Land cover class with the largest area in the given year', '-', MAPB)

lcc <- fread(file.path(ds, a, 'land_cover_classification.csv'))
f <- 'land_cover_subclass_annual.csv'
id_row(a, f, k)
add(a, f, k, 'year', 'Year of the land cover map (1985-2024)', 'year', MAPB)
for (i in seq_len(nrow(lcc))) {
  add(a, f, k, paste0(lcc$subclass_code[i], '_perc'),
      sprintf('Percentage of the catchment covered by %s (MapBiomas code %d; class: %s) in the given year',
              tolower(lcc$subclass[i]), lcc$mapbiomas_id[i], tolower(lcc$class[i])), '%', MAPB)
}

f <- 'land_cover_classification.csv'
add(a, f, k, 'mapbiomas_id', 'Original MapBiomas legend code', '-', MAPB)
add(a, f, k, 'class', 'Land cover class used in CAMELS-PE', '-', MAPB)
add(a, f, k, 'class_code', 'Class code used in the attribute names (<class_code>_perc)', '-', MAPB)
add(a, f, k, 'subclass', 'MapBiomas subclass name', '-', MAPB)
add(a, f, k, 'subclass_code', 'Subclass code used in the attribute names (<subclass_code>_perc)', '-', MAPB)

# 02_attributes: geology -----------------------------------------------------------------
glim <- c(su = 'unconsolidated sediments', ss = 'siliciclastic sedimentary rocks', py = 'pyroclastics',
          sm = 'mixed sedimentary rocks', sc = 'carbonate sedimentary rocks', ev = 'evaporites',
          va = 'acid volcanic rocks', vi = 'intermediate volcanic rocks', vb = 'basic volcanic rocks',
          pa = 'acid plutonic rocks', pi = 'intermediate plutonic rocks', pb = 'basic plutonic rocks',
          mt = 'metamorphics', wb = 'water bodies', ig = 'ice and glaciers', nd = 'no data')
f <- 'geologic_attributes.csv'; k <- 'geologic'
id_row(a, f, k)
add(a, f, k, 'geol_1st_class', 'Most common GLiM lithological class in the catchment (code, see glim_<code>_perc)', '-', GLIM)
add(a, f, k, 'glim_1st_class_perc', 'Percentage of the catchment covered by the most common lithological class', '%', GLIM)
add(a, f, k, 'geol_2nd_class', 'Second most common GLiM lithological class in the catchment', '-', GLIM)
add(a, f, k, 'glim_2nd_class_perc', 'Percentage of the catchment covered by the second most common lithological class', '%', GLIM)
add(a, f, k, 'carbonate_rocks_perc', 'Percentage of the catchment covered by carbonate sedimentary rocks (= glim_sc_perc)', '%', GLIM)
add(a, f, k, 'geol_porosity', 'Area-weighted mean subsurface porosity', '-', 'GLHYMPS v2.0')
add(a, f, k, 'geol_permeability', 'Area-weighted mean subsurface permeability', 'log10(m2)', 'GLHYMPS v2.0')
for (cd in names(glim)) add(a, f, k, paste0('glim_', cd, '_perc'),
                            sprintf('Percentage of the catchment covered by %s (GLiM class %s)', glim[[cd]], cd), '%', GLIM)

# 02_attributes: soil --------------------------------------------------------------------
f <- 'soil_attributes.csv'; k <- 'soil'
id_row(a, f, k)
for (s in c('inceptisols', 'entisols', 'alfisols', 'ultisols', 'aridisols', 'gelisols', 'oxisols', 'mollisols', 'vertisols')) {
  add(a, f, k, paste0(s, '_perc'), paste('Percentage of the catchment covered by', s), '%', 'DSOLMap')
}
add(a, f, k, 'soil_dominant_class', 'Soil order with the largest area', '-', 'DSOLMap')
TAXNOTE <- ' (USDA Soil Taxonomy as given in the DSOLMap legend, which uses pre-1999 names such as Ochrepts; see soil_classification.csv)'
HZNOTE  <- ' (horizons 1 to 3 of the DSOLMap soil profile, from the shallowest (h1) to the deepest (h3))'
DOMNOTE <- '; determined independently of the dominant order, so it may belong to a different order'
add(a, f, k, 'soil_suborder_dominant_class', paste0('Soil suborder with the largest area', TAXNOTE, DOMNOTE), '-', 'DSOLMap')
add(a, f, k, 'soil_suborder_dominant_perc', 'Percentage of the catchment covered by the dominant soil suborder', '%', 'DSOLMap')
add(a, f, k, 'soil_great_group_dominant_class', paste0('Soil great group with the largest area', TAXNOTE, DOMNOTE), '-', 'DSOLMap')
add(a, f, k, 'soil_great_group_dominant_perc', 'Percentage of the catchment covered by the dominant soil great group', '%', 'DSOLMap')
for (h in 1:3) {
  add(a, f, k, sprintf('soil_texture_h%d_dominant_class', h), paste0(sprintf('USDA texture class with the largest area in horizon %d', h), HZNOTE), '-', 'DSOLMap')
  add(a, f, k, sprintf('soil_texture_h%d_dominant_perc', h), sprintf('Percentage of the catchment covered by the dominant texture class of horizon %d', h), '%', 'DSOLMap')
}

f <- 'soil_classes.csv'
id_row(a, f, k)
add(a, f, k, 'level', 'Classification level: suborder, great_group, texture_h1, texture_h2 or texture_h3 (texture of horizons 1 to 3, from the shallowest to the deepest)', '-', 'DSOLMap')
add(a, f, k, 'class', 'Class code (see soil_classification.csv); all classes present in at least one catchment are listed, with 0 where absent', '-', 'DSOLMap')
add(a, f, k, 'perc', 'Percentage of the catchment covered by the class; the classes of each level sum to 100% in each catchment', '%', 'DSOLMap')

f <- 'soil_classification.csv'
add(a, f, k, 'level', 'Classification level: suborder, great_group or texture', '-', 'DSOLMap')
add(a, f, k, 'class', 'Class code used in soil_classes.csv and in the soil_*_dominant_class attributes', '-', 'DSOLMap')
add(a, f, k, 'name', 'Class name (USDA Soil Taxonomy or USDA texture class)', '-', 'DSOLMap')
add(a, f, k, 'order', 'Soil order of the suborder or great group (NA for texture classes)', '-', 'DSOLMap')
add(a, f, k, 'suborder', 'Soil suborder of the great group (NA for suborders and texture classes)', '-', 'DSOLMap')

# 02_attributes: human intervention ------------------------------------------------------
ADH <- ' ADH (Acreditacion de Disponibilidad Hidrica) certify water availability for a project; they are not water use rights or abstractions.'
f <- 'human_intervention_attributes.csv'; k <- 'human_intervention'
id_row(a, f, k)
for (w in c('surface', 'groundwater')) {
  wl <- c(surface = 'surface-water', groundwater = 'groundwater')[[w]]
  add(a, f, k, paste0(w, '_adh_n'), paste0('Number of ', wl, ' ADH records in the catchment.', ADH), '-', 'ANA')
  add(a, f, k, paste0(w, '_adh_vol'), paste0('Total annual volume accredited by ', wl, ' ADH records (0 if no records; NA if no record has a registered volume).', ADH), 'hm3/year', 'ANA')
  add(a, f, k, paste0(w, '_adh_mm'), paste0('Total annual volume accredited by ', wl, ' ADH records divided by the catchment area (NA if no record has a registered volume).', ADH), 'mm/day', 'ANA')
  add(a, f, k, paste0(w, '_adh_use'), paste0('Use category with the largest accredited ', wl, ' volume (None if no records)'), '-', 'ANA')
}
add(a, f, k, 'adh_ratio', 'Total accredited volume (surface and groundwater ADH) as a percentage of the mean annual observed streamflow volume (indicator of potential pressure, not of actual use); NA without observed signatures or when an accredited volume is unknown', '%', 'ANA and SENAMHI')
add(a, f, k, 'reservoir_n', 'Number of water reservoirs (dams) in the catchment, excluding tailings dams', '-', 'ANA')
add(a, f, k, 'reservoir_vol', 'Total storage capacity of the water reservoirs (lower bound: capacity is not recorded for all dams)', 'hm3', 'ANA')
add(a, f, k, 'reservoir_mm', 'Total storage capacity of the water reservoirs divided by the catchment area; NA if no dam has a recorded capacity', 'mm', 'ANA')
add(a, f, k, 'reservoir_dor', 'Degree of regulation: total storage capacity as a percentage of the mean annual observed streamflow volume (Lehner et al., 2011); NA without observed signatures or recorded capacity', '%', 'ANA and SENAMHI')
add(a, f, k, 'reservoir_use', 'Use category with the largest storage capacity (None if no reservoirs)', '-', 'ANA')
add(a, f, k, 'tailings_dam_n', 'Number of tailings dams in the catchment (not included in the reservoir attributes)', '-', 'ANA')
add(a, f, k, 'near_natural', 'Near-natural catchment (1): no water reservoirs, no tailings dams and less than 10% anthropic land cover (agriculture, pasture, planted forest, urban and mining) in 2024', '0/1', 'ANA and MapBiomas')

# 03_timeseries --------------------------------------------------------------------------
t3 <- '03_timeseries'; k <- 'timeseries'
ts_vars <- list(
  date          = c('Date. Precipitation, temperature and PET follow the Peruvian meteorological day (07:00-07:00 local time); observed streamflow the calendar day (00:00-24:00 local time); ERA5-Land variables the UTC day', 'YYYY-MM-DD', '-'),
  gauge_id      = c('Unique gauge identifier (PE_xxxxx)', '-', 'SENAMHI'),
  prec_pisco    = c('Catchment mean daily precipitation (reference product)', 'mm/day', P_SRC),
  prec_sd_pisco = c('Spatial standard deviation of daily precipitation within the catchment (area-weighted over the intersecting grid cells)', 'mm/day', P_SRC),
  prec_chirps   = c('Catchment mean daily precipitation (alternative product)', 'mm/day', 'CHIRPS v3 (daily, RNL)'),
  prec_era5land = c('Catchment mean daily precipitation (alternative product)', 'mm/day', 'ERA5-Land'),
  pet_hs        = c('Potential evapotranspiration, Hargreaves-Samani (reference)', 'mm/day', PET_HS),
  pet_pt        = c('Potential evapotranspiration, Priestley-Taylor (alpha = 1.26, FAO-56 net radiation)', 'mm/day', 'PISCOt v1.2 and ERA5-Land'),
  pet_ou        = c('Potential evapotranspiration, Oudin et al. (2005)', 'mm/day', T_SRC),
  tmin          = c('Catchment mean daily minimum air temperature', 'degC', T_SRC),
  tmean         = c('Catchment mean daily mean air temperature', 'degC', T_SRC),
  tmax          = c('Catchment mean daily maximum air temperature', 'degC', T_SRC),
  srad          = c('Catchment mean daily surface solar radiation (downwards)', 'MJ/m2/day', 'ERA5-Land'),
  vprp          = c('Catchment mean daily vapour pressure', 'hPa', 'ERA5-Land'),
  flow_obs      = c('Observed streamflow after quality control (values with flow_obs_flag = 4 removed)', 'mm/day', Q_OBS),
  flow_obs_m3s  = c('Observed streamflow after quality control, volumetric units', 'm3/s', Q_OBS),
  flow_obs_raw  = c('Observed streamflow before quality control (may contain erroneous values, e.g. negative values flagged by the gross range test)', 'mm/day', 'SENAMHI'),
  flow_obs_flag = c('Quality-control flag of the observed streamflow (QARTOD): 1 pass, 3 suspect (kept), 4 fail (removed), 9 missing', '-', 'Derived from data'),
  flow_obs_qc_test = c('Quality-control test that flagged the value (only for flow_obs_flag 3 or 4): gross_range, spike, spike_rain_supported (suspect), flat_line (> 30 days), flat_line_suspect (7-30 days), linear_interpolation', '-', 'Derived from data'),
  flow_sim     = c('Simulated streamflow', 'mm/day', Q_SIM),
  flow_sim_m3s  = c('Simulated streamflow, volumetric units (flow_sim times catchment area)', 'm3/s', Q_SIM)
)
for (v in names(ts_vars)) add(t3, 'timeseries.csv', k, v, ts_vars[[v]][1], ts_vars[[v]][2], ts_vars[[v]][3])
for (v in setdiff(names(ts_vars), 'gauge_id')) add(t3, 'by_catchment/PE_xxxxx.csv', k, v, ts_vars[[v]][1], ts_vars[[v]][2], ts_vars[[v]][3])

# 04_geospatial --------------------------------------------------------------------------
g4 <- '04_geospatial'; k <- 'geospatial'
add(g4, 'camels_pe_gauges.gpkg', k, 'gauges', 'Gauge locations at the catchment outlets (points) with name, coordinates, COMID of the PISCO-ARNOVIC river reach, area, perimeter and nesting attributes', '-', 'SENAMHI; SRTM 90 m')
add(g4, 'camels_pe_catchments.gpkg', k, 'catchments', 'Catchment boundaries (polygons) with area, perimeter and nesting attributes, delineated with D8 flow directions from the 90 m SRTM DEM', '-', 'SRTM 90 m')
add(g4, 'by_catchment/PE_xxxxx/', k, 'PE_xxxxx_outlet.gpkg, PE_xxxxx_catchment.gpkg', 'Gauge location and catchment boundary of each catchment', '-', 'Derived')

dict <- rbindlist(dict)

# Comprobación: columnas de cada CSV frente al diccionario
af  <- list.files(file.path(ds, '02_attributes'))
chk <- data.table(folder = c('01_metadata', rep('02_attributes', length(af)), '03_timeseries'),
                  file = c('stations.csv', af, 'timeseries.csv'))
for (i in seq_len(nrow(chk))) {
  cols <- names(fread(file.path(ds, chk$folder[i], chk$file[i]), nrows = 1))
  doc  <- dict[folder == chk$folder[i] & file == chk$file[i], variable]
  if (!setequal(cols, doc)) stop(sprintf('%s: sin documentar {%s}; sobran {%s}', chk$file[i],
                                         paste(setdiff(cols, doc), collapse = ', '), paste(setdiff(doc, cols), collapse = ', ')))
}
bc <- names(fread(list.files(file.path(ds, '03_timeseries/by_catchment'), full.names = TRUE)[1], nrows = 1))
stopifnot(setequal(bc, dict[file == 'by_catchment/PE_xxxxx.csv', variable]))

fwrite(dict, file.path(ds, '01_metadata/data_dictionary.csv'), bom = TRUE)
cat('Diccionario:', nrow(dict), 'filas;', nrow(chk) + 1, 'archivos comprobados\n')
