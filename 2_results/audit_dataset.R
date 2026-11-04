# Auditoría del dataset CAMELS-PE antes de publicar: estructura, consistencia interna, rangos físicos y
# coherencia con las capas geoespaciales. Se ejecuta después de prepare_dataset.R y make_data_dictionary.R.
# Termina con el número de problemas encontrados (debe ser 0).
# By Harold Llauca
suppressMessages({library(data.table); library(terra)})
ds <- "./2_results/CAMELS-PE"
setwd(ds)
issues <- character()
chk <- function(ok, msg) { if (!isTRUE(all(ok))) { issues <<- c(issues, msg); cat("  [X]", msg, "\n") } else cat("  [ok]", msg, "\n") }
info <- function(...) cat("  [i]", ..., "\n")

cat("\n== 0. Archivos\n")
all_files <- list.files(".", recursive = TRUE)
info("archivos totales:", length(all_files))
print(table(dirname(all_files))[!grepl("by_catchment", names(table(dirname(all_files))))])

st <- fread("01_metadata/stations.csv", encoding = "UTF-8"); ids <- st$gauge_id
cat("\n== 1. stations.csv\n")
chk(nrow(st) == 136 && !anyDuplicated(ids) && all(grepl("^PE_", ids)), "136 gauge_id únicos con prefijo PE_")
chk(!any(grepl("Ã|Â|\uFFFD", unlist(st[, .(gauge_name, name_cat)]))), "sin caracteres mal codificados en nombres")
chk(all(st$downstream_gauge_id %in% c("NONE", ids)) && all(st$upstream_gauge_id %in% c("NONE", ids)) && all(st$nested_group_id %in% ids), "referencias downstream/upstream/nested válidas")
chk(all((st$nested_group_size > 1) == st$is_nested), "is_nested coherente con nested_group_size")
gs <- st[, .N, by = nested_group_id]; chk(all(st$nested_group_size == gs$N[match(st$nested_group_id, gs$nested_group_id)]), "nested_group_size = n.º de estaciones del grupo")
chk(all(st$gauge_lat > -19 & st$gauge_lat < 1 & st$gauge_lon > -82 & st$gauge_lon < -68), "coordenadas en el dominio")
chk(all(st$flow_obs_perc_fail >= 0 & st$flow_obs_perc_fail <= 100), "flow_obs_perc_fail en 0-100")

cat("\n== 2. timeseries.csv\n")
ts <- fread("03_timeseries/timeseries.csv")
ts[, date := as.Date(date)]
nd <- uniqueN(ts$date)
chk(nrow(ts) == 136 * nd && nd == as.integer(as.Date("2025-12-31") - as.Date("1981-01-01")) + 1, "136 x 16436 filas, calendario 1981-2025 completo")
chk(setequal(unique(ts$gauge_id), ids) && !anyDuplicated(ts[, .(date, gauge_id)]), "mismas estaciones que stations.csv, sin duplicados")
met <- c("prec_pisco","prec_sd_pisco","prec_chirps","prec_era5land","pet_hs","pet_pt","pet_ou","tmin","tmean","tmax","srad","vprp","flow_sim","flow_sim_m3s")
chk(all(sapply(ts[, ..met], function(x) !anyNA(x))), "sin NA en meteorología y caudal simulado")
for (v in c("prec_pisco","prec_sd_pisco","prec_chirps","prec_era5land","pet_hs","pet_pt","pet_ou","srad","flow_sim","flow_obs")) chk(all(ts[[v]] >= 0, na.rm = TRUE), paste(v, ">= 0"))
chk(all(ts$vprp > 0), "vprp > 0")
chk(all(ts[flow_obs_raw < 0, flow_obs_qc_test] == "gross_range"), "valores negativos de flow_obs_raw marcados como gross_range")
info("rangos: tmin", range(ts$tmin), "| tmax", range(ts$tmax), "| prec_pisco max", max(ts$prec_pisco), "| srad", range(ts$srad), "| vprp", range(ts$vprp))
info("tmin > tmax en", sum(ts$tmin > ts$tmax + 1e-6), "filas; tmean fuera de [tmin,tmax] en", sum(ts$tmean < ts$tmin - 0.01 | ts$tmean > ts$tmax + 0.01))
chk(sum(ts$tmin > ts$tmax + 1e-6) == 0, "tmin <= tmax")
chk(all(abs(ts$tmean - (ts$tmin + ts$tmax) / 2) < 0.002), "tmean = (tmin + tmax) / 2")
chk(all(ts$flow_obs_flag %in% c(1L, 3L, 4L, 9L)), "flags solo 1/3/4/9")
chk(with(ts, all(is.na(flow_obs_raw) == (flow_obs_flag == 9L))), "flag 9 <=> flow_obs_raw NA")
chk(with(ts, all(is.na(flow_obs) == (flow_obs_flag %in% c(4L, 9L)))), "flow_obs NA <=> flag 4 o 9")
chk(with(ts[!is.na(flow_obs)], all(abs(flow_obs - flow_obs_raw) < 1e-6)), "flow_obs = flow_obs_raw donde existe")
chk(with(ts, all(is.na(flow_obs_qc_test) == !(flow_obs_flag %in% c(3L, 4L)))), "qc_test solo con flag 3/4")
topo <- fread("02_attributes/topographic_attributes.csv"); ts[, area := topo$area[match(gauge_id, topo$gauge_id)]]
r_obs <- ts[!is.na(flow_obs) & flow_obs_m3s >= 0.1, flow_obs_m3s / (flow_obs * area / 86.4)]  # debajo de 0.1 m3/s domina el redondeo a 3 decimales
r_sim <- ts[flow_sim_m3s >= 0.1, flow_sim_m3s / (flow_sim * area / 86.4)]
info("ratio m3s/(mm*area/86.4): obs", round(range(r_obs), 4), " sim", round(range(r_sim), 4))
chk(all(abs(r_obs - 1) < 0.01), "flow_obs_m3s consistente con flow_obs y área")
chk(all(abs(r_sim - 1) < 0.01), "flow_sim_m3s consistente con flow_sim y área")
# QC por estación frente a stations.csv
q <- ts[, .(sus = sum(flow_obs_flag == 3L), fail = sum(flow_obs_flag == 4L), nobs = sum(flow_obs_flag != 9L), nv = sum(!is.na(flow_obs)),
            first = min(date[!is.na(flow_obs)]), last = max(date[!is.na(flow_obs)])), by = gauge_id][match(ids, gauge_id)]
chk(all(q$sus == st$flow_obs_n_suspect & q$fail == st$flow_obs_n_fail), "conteos de QC = stations.csv")
chk(all(abs(round(100 * q$fail / q$nobs, 2) - st$flow_obs_perc_fail) < 0.011), "flow_obs_perc_fail = fail / observados")
chk(all(abs(100 * q$nv / nd - st$gauge_perc_obs) < 0.011), "gauge_perc_obs = % días con flow_obs")
chk(all(as.Date(st$gauge_record_start) == q$first & as.Date(st$gauge_record_end) == q$last), "gauge_record_start/end = primer/último flow_obs")
if (!all(as.Date(st$gauge_record_start) == q$first & as.Date(st$gauge_record_end) == q$last)) print(cbind(st[, .(gauge_id, gauge_record_start, gauge_record_end)], q[, .(first, last)])[as.Date(gauge_record_start) != first | as.Date(gauge_record_end) != last])

cat("\n== 3. by_catchment (series)\n")
bc <- list.files("03_timeseries/by_catchment", full.names = TRUE)
chk(setequal(sub("\\.csv$", "", basename(bc)), ids), "136 archivos con los gauge_id de stations.csv")
bad <- 0
for (f in bc) { g <- sub("\\.csv$", "", basename(f)); x <- fread(f); y <- ts[gauge_id == g]
  for (v in setdiff(names(x), "date")) if (!(all(is.na(x[[v]])) && all(is.na(y[[v]]))) && !isTRUE(all.equal(x[[v]], y[[v]], tolerance = 1e-9))) { bad <- bad + 1; break } }
chk(bad == 0, "cada archivo por cuenca = su subconjunto de timeseries.csv")

cat("\n== 4. Atributos: gauge_id y orden\n")
af <- c("topographic_attributes","climatic_indices","hydrological_signatures","hydrological_signatures_sim","land_cover_attributes","geologic_attributes","soil_attributes","human_intervention_attributes")
A <- lapply(setNames(af, af), function(f) fread(file.path("02_attributes", paste0(f, ".csv")), encoding = "UTF-8"))
for (f in af) chk(identical(A[[f]]$gauge_id, ids), paste(f, ": 136 filas en el orden de stations.csv"))

cat("\n== 5. Topografía\n")
tp <- A$topographic_attributes
chk(all(tp$elev_min <= tp$elev_median & tp$elev_median <= tp$elev_max & tp$elev_min <= tp$elev_mean & tp$elev_mean <= tp$elev_max), "elev_min <= mean/median <= elev_max")
chk(all(tp$area > 0 & tp$perimeter > 0 & tp$slope_mean > 0), "área, perímetro y pendiente positivos")
d_el <- st$gauge_elev - tp$elev_min; info("gauge_elev - elev_min: rango", range(d_el), "m")
chk(all(d_el > -50), "gauge_elev no muy por debajo de elev_min")
if (any(d_el <= -50)) print(data.table(st[, .(gauge_id, gauge_name, gauge_elev)], elev_min = tp$elev_min)[d_el <= -50])
cats <- vect("04_geospatial/camels_pe_catchments.gpkg"); gau <- vect("04_geospatial/camels_pe_gauges.gpkg")
a_g <- expanse(cats, unit = "km")[match(ids, cats$gauge_id)]
chk(all(abs(tp$area / a_g - 1) < 1e-3), "area = área del polígono (gpkg)")
chk(all(abs(cats$area[match(ids, cats$gauge_id)] / tp$area - 1) < 1e-3) && all(abs(gau$area[match(ids, gau$gauge_id)] / tp$area - 1) < 1e-3), "campo area de los gpkg = topographic_attributes")
chk(all(abs(cats$perimeter[match(ids, cats$gauge_id)] / tp$perimeter - 1) < 1e-3), "perímetro del gpkg = topographic_attributes")

cat("\n== 6. Índices climáticos (vs series)\n")
ci <- A$climatic_indices
m <- ts[, .(p = mean(prec_pisco), hs = mean(pet_hs), pt = mean(pet_pt), ou = mean(pet_ou)), by = gauge_id][match(ids, gauge_id)]
chk(all(abs(ci$p_mean - m$p) < 0.002), "p_mean = media de prec_pisco 1981-2025")
chk(all(abs(ci$pet_mean_hs - m$hs) < 0.002) && all(abs(ci$pet_mean_pt - m$pt) < 0.002) && all(abs(ci$pet_mean_ou - m$ou) < 0.002), "pet_mean_hs/pt/ou = medias de las series")
chk(all(abs(ci$aridity_hs - ci$pet_mean_hs / ci$p_mean) / ci$aridity_hs < 0.01) && all(abs(ci$aridity_pt - ci$pet_mean_pt / ci$p_mean) / ci$aridity_pt < 0.01) && all(abs(ci$aridity_ou - ci$pet_mean_ou / ci$p_mean) / ci$aridity_ou < 0.01), "aridity = pet_mean / p_mean")
chk(all(ci$high_prec_freq >= 0 & ci$low_prec_freq <= 366 & ci$low_prec_freq >= 0), "frecuencias en rango")
info("NA en climatic_indices:", paste(names(ci)[sapply(ci, anyNA)], collapse = ", "), "| timing:", paste(names(table(ci$high_prec_timing, useNA = "ifany")), collapse = ","))

cat("\n== 7. Firmas observadas\n")
hs <- A$hydrological_signatures
ok5 <- !is.na(hs$q_obs_years) & hs$q_obs_years >= 5
chk(sum(ok5) == 109 && all(is.na(hs$q_mean) == !ok5), "109 cuencas con firmas, NA en el resto")
chk(all(is.na(hs$q_obs_start) == !ok5) && all(is.na(hs$q_obs_frac) == !ok5), "q_obs_* NA donde no hay firmas")
h <- hs[ok5]
chk(all(h$q_obs_frac >= 0.9 & h$q_obs_frac <= 1), "q_obs_frac en [0.9, 1]")
chk(all(h$baseflow_index >= 0 & h$baseflow_index <= 1, na.rm = TRUE), "baseflow_index en [0, 1]")
chk(all(h$Q5 <= h$q_mean & h$q_mean <= h$Q95 * 10 & h$Q5 <= h$Q95, na.rm = TRUE), "Q5 <= Q95")
chk(all(h$zero_q_freq >= 0 & h$zero_q_freq <= 100), "zero_q_freq en 0-100")
chk(all(h$hfd_mean >= 1 & h$hfd_mean <= 366), "hfd_mean en 1-366")
info("runoff_ratio: rango", range(h$runoff_ratio), "| > 1 en", sum(h$runoff_ratio > 1), "cuencas:", paste(h$gauge_id[h$runoff_ratio > 1], collapse = ", "))
# q_mean recalculado con los años válidos (días con dato dentro de q_obs_start..q_obs_end, solo años con >= 90 %)
rq <- sapply(seq_len(nrow(h)), function(i) { x <- ts[gauge_id == h$gauge_id[i] & date >= as.Date(h$q_obs_start[i]) & date <= as.Date(h$q_obs_end[i])]
  x[, hy := year(date) + (month(date) >= 9)]; v <- x[, .(f = mean(!is.na(flow_obs))), by = hy][f >= 0.9, hy]; x[hy %in% v, mean(flow_obs, na.rm = TRUE)] })
info("q_mean recalculado vs atributo: máx. dif. relativa", signif(max(abs(rq / h$q_mean - 1)), 3))
chk(all(abs(rq / h$q_mean - 1) < 0.005), "q_mean = media de flow_obs en los años válidos")
hsim <- A$hydrological_signatures_sim
chk(all(!is.na(hsim$q_mean)), "firmas simuladas en las 136 cuencas")
rqs <- ts[date >= as.Date("1981-09-01") & date <= as.Date("2025-08-31"), mean(flow_sim), by = gauge_id][match(ids, gauge_id), V1]
chk(all(abs(rqs - hsim$q_mean) < 0.0006), "q_mean simulado = media de flow_sim 1981/09-2025/08 (a 3 decimales)")

cat("\n== 8. Cobertura del suelo\n")
lc <- A$land_cover_attributes; cl <- c("forest_perc","agricul_perc","non_forest_perc","non_veget_perc","water_perc","non_identi_perc")
info("suma de clases: rango", range(rowSums(lc[, ..cl])))
chk(all(abs(rowSums(lc[, ..cl]) - 100) < 0.1), "clases suman 100")
chk(all(abs(lc$glacier_perc + lc$water_body_perc - lc$water_perc) < 0.01), "glacier + water_body = water")
chk(all(sub("_perc", "", cl)[max.col(as.matrix(lc[, ..cl]), "first")] == lc$land_dominant_class), "land_dominant_class = clase máxima")
la <- fread("02_attributes/land_cover_class_annual.csv"); ls_ <- fread("02_attributes/land_cover_subclass_annual.csv")
chk(nrow(la) == 136 * 40 && all(la[, .N, by = gauge_id]$N == 40) && setequal(unique(la$year), 1985:2024), "serie anual de clases: 136 x 40 años")
chk(nrow(ls_) == 136 * 40, "serie anual de subclases: 136 x 40 años")
l24 <- la[year == 2024][match(ids, gauge_id)]
chk(all(abs(as.matrix(l24[, ..cl]) - as.matrix(lc[, ..cl])) < 0.01), "atributos estáticos = año 2024 de la serie anual")
chk(all(sub("_perc","",cl)[max.col(as.matrix(la[, ..cl]), "first")] == la$land_dominant_class), "land_dominant_class anual = clase máxima")
lcc <- fread("02_attributes/land_cover_classification.csv")
sc <- paste0(lcc$subclass_code, "_perc"); chk(setequal(sc, setdiff(names(ls_), c("gauge_id","year"))), "subclases = land_cover_classification")
M <- as.matrix(ls_[, ..sc]); agg <- sapply(unique(lcc$class_code), function(k) rowSums(M[, sc[lcc$class_code == k], drop = FALSE]))
la2 <- la[match(paste(ls_$gauge_id, ls_$year), paste(gauge_id, year))]
info("subclases agregadas vs clases: máx. dif.", max(abs(agg - as.matrix(la2[, paste0(colnames(agg), "_perc"), with = FALSE]))))
chk(max(abs(agg - as.matrix(la2[, paste0(colnames(agg), "_perc"), with = FALSE]))) < 0.05, "subclases suman sus clases")
chk(all(abs(ls_[year == 2024][match(ids, gauge_id)]$glacier_perc - lc$glacier_perc) < 0.01), "glacier_perc = subclase glaciar 2024")

cat("\n== 9. Geología y suelos\n")
ge <- A$geologic_attributes; gc <- grep("^glim_.._perc$", names(ge), value = TRUE)
chk(all(abs(rowSums(ge[, ..gc]) - 100) < 0.1), "clases GLiM suman 100")
chk(all(sub("glim_(..)_perc", "\\1", gc)[max.col(as.matrix(ge[, ..gc]), "first")] == ge$geol_1st_class), "geol_1st_class = clase máxima")
chk(all(abs(ge$carbonate_rocks_perc - ge$glim_sc_perc) < 0.01), "carbonate = glim_sc")
chk(all(ge$geol_porosity > 0 & ge$geol_porosity < 1 & ge$geol_permeability < -9 & ge$geol_permeability > -18), "porosidad y permeabilidad en rango")
so <- A$soil_attributes; sc2 <- grep("^[a-z]+sols_perc$", names(so), value = TRUE)
info("suma suelos: rango", range(rowSums(so[, ..sc2])))
chk(all(abs(rowSums(so[, ..sc2]) - 100) < 0.1), "órdenes de suelo suman 100")
chk(all(sub("_perc", "", sc2)[max.col(as.matrix(so[, ..sc2]), "first")] == so$soil_dominant_class), "soil_dominant_class = orden máximo")
# Suelos: subórdenes, grandes grupos y texturas por horizonte
scl <- fread("02_attributes/soil_classes.csv"); scf <- fread("02_attributes/soil_classification.csv")
chk(setequal(scl$gauge_id, ids) && all(scl[, .N, by = .(gauge_id, level)][, .N, by = level]$N == 136), "soil_classes: 136 cuencas en cada nivel")
chk(all(abs(scl[, sum(perc), by = .(gauge_id, level)]$V1 - 100) < 0.1), "soil_classes: cada nivel suma 100")
chk(all(scl[level %in% c("suborder", "great_group"), paste(level, class)] %in% scf[, paste(level, class)]) &&
    all(scl[grepl("^texture", level), class] %in% scf[level == "texture", class]), "soil_classes: clases en soil_classification")
so_s <- merge(scl[level == "suborder"], scf[level == "suborder", .(class, order = tolower(order))], by = "class")[, .(p = sum(perc)), by = .(gauge_id, order)]
so_o <- melt(so[, c("gauge_id", sc2), with = FALSE], id.vars = "gauge_id", variable.name = "order", value.name = "perc")[, order := sub("_perc$", "", order)]
d_so <- merge(so_s, so_o, by = c("gauge_id", "order"), all = TRUE)[, abs(fifelse(is.na(p), 0, p) - fifelse(is.na(perc), 0, perc))]
info("subórdenes agregados vs órdenes publicados: máx. dif.", round(max(d_so), 3), "puntos porcentuales")
chk(max(d_so) < 0.1, "subórdenes suman sus órdenes (< 0.1 pp)")
dm <- scl[order(-perc), .SD[1], by = .(gauge_id, level)]
for (lv in c("suborder", "great_group", "texture_h1", "texture_h2", "texture_h3")) {
  col <- if (lv == "suborder") "soil_suborder_dominant_class" else if (lv == "great_group") "soil_great_group_dominant_class" else sprintf("soil_%s_dominant_class", lv)
  chk(all(dm[level == lv][match(ids, gauge_id), class] == so[[col]]), paste(col, "= clase máxima de soil_classes"))
}

cat("\n== 10. Intervención humana\n")
hu <- A$human_intervention_attributes
for (w in c("surface", "groundwater")) {
  r <- hu[[paste0(w, "_adh_mm")]] / (hu[[paste0(w, "_adh_vol")]] / tp$area * 1000 / 365.25)
  e <- hu[[paste0(w, "_adh_mm")]] - hu[[paste0(w, "_adh_vol")]] / tp$area * 1000 / 365.25; chk(all(abs(e) < 2e-6, na.rm = TRUE) && all(is.na(e) == is.na(hu[[paste0(w, "_adh_vol")]])), paste0(w, "_adh_mm = vol / área / 365.25 (a 6 decimales)"))
  chk(all(hu[[paste0(w, "_adh_vol")]][hu[[paste0(w, "_adh_n")]] == 0] == 0) && all(hu[[paste0(w, "_adh_vol")]][hu[[paste0(w, "_adh_n")]] > 0] > 0, na.rm = TRUE), paste0(w, ": n = 0 -> vol = 0; n > 0 -> vol > 0 o NA (sin volumen registrado)"))
  chk(all((hu[[paste0(w, "_adh_n")]] == 0) == (hu[[paste0(w, "_adh_use")]] == "None")), paste0(w, ": n = 0 <=> use = None"))
}
rr <- hu$reservoir_mm / (hu$reservoir_vol / tp$area * 1000); chk(all(abs(rr[!is.na(rr) & hu$reservoir_vol > 0] - 1) < 0.01), "reservoir_mm = vol / área")
chk(all((hu$reservoir_n == 0) == (hu$reservoir_use == "None")), "reservoir_n = 0 <=> use = None")
qv <- hs$q_mean * tp$area * 365.25 / 1000  # hm3/año
dor <- 100 * hu$reservoir_vol / qv; ok <- !is.na(hu$reservoir_dor) & hu$reservoir_dor > 0
chk(all(abs(dor[ok] / hu$reservoir_dor[ok] - 1) < 0.01), "reservoir_dor = vol / volumen medio anual observado")
adr <- 100 * (hu$surface_adh_vol + hu$groundwater_adh_vol) / qv; ok <- !is.na(hu$adh_ratio) & hu$adh_ratio > 0.01
chk(all(abs(adr[ok] / hu$adh_ratio[ok] - 1) < 0.01), "adh_ratio = vol acreditado / volumen medio anual observado")
chk(all(is.na(hu$adh_ratio) == (is.na(hs$q_mean) | is.na(hu$surface_adh_vol) | is.na(hu$groundwater_adh_vol))), "adh_ratio NA <=> sin firmas observadas o volumen desconocido")
lc_anth <- fread("02_attributes/land_cover_subclass_annual.csv")[year == 2024][match(ids, gauge_id)]
anth <- rowSums(lc_anth[, .(planted_forest_perc, pasture_perc, mosaic_agri_pasture_perc, oil_palm_perc, rice_perc, other_crops_perc, urban_perc, mining_perc)])
nn <- as.integer(hu$reservoir_n == 0 & hu$tailings_dam_n == 0 & anth < 10)
chk(all(nn == hu$near_natural), "near_natural = criterios (sin embalses, sin relaveras, antrópico < 10 %)")
if (!all(nn == hu$near_natural)) print(data.table(ids, hu$near_natural, nn, anth)[nn != hu$near_natural])

cat("\n== 11. Geoespacial\n")
chk(setequal(cats$gauge_id, ids) && setequal(gau$gauge_id, ids) && nrow(cats) == 136 && nrow(gau) == 136, "gpkg con las 136 estaciones")
chk(grepl("4326", crs(cats, describe = TRUE)$code) && grepl("4326", crs(gau, describe = TRUE)$code), "WGS84 (EPSG:4326)")
chk(all(is.valid(cats)), "geometrías de cuencas válidas")
gm <- gau[match(ids, gau$gauge_id)]
chk(all(abs(geom(gm)[, "y"] - st$gauge_lat) < 1e-3 & abs(geom(gm)[, "x"] - st$gauge_lon) < 1e-3), "coordenadas del gpkg = stations.csv")
chk(all(gm$name == st$gauge_name), "nombres del gpkg = stations.csv")
cm <- cats[match(ids, cats$gauge_id)]
for (fld in c("is_nested","downstream_gauge_id","upstream_gauge_id","nested_group_id","nested_group_size")) {
  chk(all(as.character(cm[[fld]][[1]]) == as.character(st[[fld]])) && all(as.character(gm[[fld]][[1]]) == as.character(st[[fld]])), paste("campo", fld, "de los gpkg = stations.csv"))
}
dd <- sapply(seq_along(ids), function(i) distance(project(gm[i], "EPSG:32718"), project(as.lines(cm[i]), "EPSG:32718")) * ifelse(relate(gm[i], cm[i], "within")[1, 1], -1, 1))
info("distancia estación-borde de su cuenca (m; negativo = dentro): fuera", sum(dd > 0), "| máx. fuera", round(max(dd)), "| > 500 m:", sum(dd > 500))
gbc <- list.dirs("04_geospatial/by_catchment", recursive = FALSE, full.names = FALSE)
chk(setequal(gbc, ids) && all(file.exists(file.path("04_geospatial/by_catchment", ids, paste0(ids, "_outlet.gpkg"))) & file.exists(file.path("04_geospatial/by_catchment", ids, paste0(ids, "_catchment.gpkg")))), "by_catchment geoespacial: 136 carpetas con outlet y catchment")

cat("\n== 12. Diccionario\n")
dic <- fread("01_metadata/data_dictionary.csv", encoding = "UTF-8")
chk(!anyDuplicated(dic[, .(folder, file, variable)]), "sin entradas duplicadas")
chk(!any(grepl("v2\\.1|PISCOeo|2025\\)", dic$source)), "sin fuentes antiguas (PISCOp v2.1, PISCOeo, MapBiomas 2025)")
chk(!any(dic$unit == "" | is.na(dic$unit)), "todas las variables con unidad")

cat("\n==== RESUMEN:", length(issues), "problemas\n"); if (length(issues)) cat(paste("-", issues), sep = "\n")
