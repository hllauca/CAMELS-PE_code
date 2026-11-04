# Preparar series anuales de cobertura y uso del suelo (MapBiomas) para CAMELS-PE
# By Harold Llauca
cat('\f')
rm(list=ls())

# Librerías
library(openxlsx)

# Directorio de trabajo
# Run this script from the root of the CAMELS-PE project folder (see README.md).
# Datos de entrada: porcentaje de cada clase y subclase por cuenca y año (MapBiomas)
cl <- read.xlsx('./1_data/land_cover/camels_pe_uso_suelo_clase_porcentaje_ancho_consolidado.xlsx',
                check.names = FALSE, sep.names = " ")
sc <- read.xlsx('./1_data/land_cover/camels_pe_uso_suelo_subclase_porcentaje_ancho_consolidado.xlsx',
                check.names = FALSE, sep.names = " ")
cl <- cl[order(cl$gauge_id, cl$year), ]
sc <- sc[order(sc$gauge_id, sc$year), ]
stopifnot(identical(cl$gauge_id, sc$gauge_id), identical(cl$year, sc$year))

# Una clase o subclase ausente en la cuenca equivale a 0 %
id_cols <- c('gauge_id', 'name', 'name_cat', 'year')
cl[setdiff(names(cl), id_cols)][is.na(cl[setdiff(names(cl), id_cols)])] <- 0
sc[setdiff(names(sc), id_cols)][is.na(sc[setdiff(names(sc), id_cols)])] <- 0

#==============================================================================================
# Clasificación: subclase -> clase (leyendas MapBiomas Perú y países vecinos)
#==============================================================================================
# Las formaciones naturales no forestales aparecen con tres nombres según la leyenda de cada
# país ("Non-woody natural formation", "Non-forested natural formation" y "Natural non-forest
# formation") y se unen en una sola clase
classification <- data.frame(
  mapbiomas_id = c(3, 4, 5, 6,
                   9, 15, 21, 35, 40, 72,
                   12, 13, 29, 66, 11, 70, 81, 82,
                   23, 24, 25, 30, 61, 68, 75,
                   31, 33, 34,
                   27),
  subclass = c('Forest Formation', 'Savanna Formation', 'Mangrove', 'Flooded forest',
               'Planted forest', 'Pasture', 'Mosaic of agriculture and pasture', 'Oil palm', 'Rice', 'Other crops',
               'Grassland / Herbaceous', 'Other non-forest formation', 'Rock outcrop', 'Scrubland',
               'Swamp or flooded Grassland', 'Fog oasis', 'Andean herbs and shrubs', 'Flooded Andean herbs',
               'Beach', 'Urban infrastructure', 'Other non vegetated area', 'Mining', 'Salt flat',
               'Other natural non vegetated area', 'Solar parks',
               'Aquaculture', 'River, lake or ocean', 'Glacier',
               'Not observed'),
  subclass_code = c('forest_formation', 'dry_forest', 'mangrove', 'flooded_forest',
                    'planted_forest', 'pasture', 'mosaic_agri_pasture', 'oil_palm', 'rice', 'other_crops',
                    'grassland_herbaceous', 'other_non_forest', 'rock_outcrop', 'scrubland',
                    'swamp_flooded_grassland', 'fog_oasis', 'andean_herbs_shrubs', 'flooded_andean_herbs',
                    'beach', 'urban', 'other_non_veget', 'mining', 'salt_flat',
                    'other_natural_non_veget', 'solar_parks',
                    'aquaculture', 'river_lake_ocean', 'glacier',
                    'not_observed'),
  class = c(rep('Forest formation', 4),
            rep('Agricultural area', 6),
            rep('Natural non-forest formation', 8),
            rep('Non-vegetated area', 7),
            rep('Water body', 3),
            'Not observed'),
  class_code = c(rep('forest', 4),
                 rep('agricul', 6),
                 rep('non_forest', 8),
                 rep('non_veget', 7),
                 rep('water', 3),
                 'non_identi'),
  stringsAsFactors = FALSE
)
stopifnot(setequal(classification$subclass, setdiff(names(sc), id_cols)))

# Nombre de salida: el código 4 corresponde a bosque seco en la leyenda de MapBiomas Perú
# (en el archivo de entrada figura como "Savanna Formation", nombre de otras leyendas)
classification$subclass_name <- classification$subclass
classification$subclass_name[classification$mapbiomas_id == 4] <- 'Dry forest'

#==============================================================================================
# Series anuales por subclase y por clase [% del área de la cuenca]
#==============================================================================================
subclass_ts <- data.frame(gauge_id = sc$gauge_id, year = sc$year,
                          round(as.matrix(sc[, classification$subclass]), 4), check.names = FALSE)
names(subclass_ts)[-(1:2)] <- paste0(classification$subclass_code, '_perc')

class_codes <- unique(classification$class_code)
class_ts <- data.frame(gauge_id = sc$gauge_id, year = sc$year,
                       sapply(class_codes, function(k) {
                         round(rowSums(as.matrix(sc[, classification$subclass[classification$class_code == k], drop = FALSE])), 4)
                       }), check.names = FALSE)
names(class_ts)[-(1:2)] <- paste0(class_codes, '_perc')

# Verificaciones: las clases reproducen el archivo original de clases (con las tres formaciones
# naturales no forestales unidas) y cada fila suma 100 %
orig <- cbind(forest     = cl[['Forest']],
              agricul    = cl[['Agricultural area']],
              non_forest = cl[['Non-woody natural formation']] + cl[['Non-forested natural formation']] +
                           cl[['Natural non-forest formation']],
              non_veget  = cl[['Non vegetated area']],
              water      = cl[['Water body']],
              non_identi = cl[['Non-identified land cover']])
stopifnot(max(abs(as.matrix(class_ts[, paste0(colnames(orig), '_perc')]) - orig)) < 1e-3)
stopifnot(all(abs(rowSums(class_ts[, -(1:2)]) - 100) < 0.01),
          all(abs(rowSums(subclass_ts[, -(1:2)]) - 100) < 0.01))

# Clase dominante por año
class_ts$dominant_class <- class_codes[apply(class_ts[, paste0(class_codes, '_perc')], 1, which.max)]

#==============================================================================================
# Guardar
#==============================================================================================
write.csv(class_ts, './1_data/land_cover/land_cover_class_annual.csv', row.names = FALSE)
write.csv(subclass_ts, './1_data/land_cover/land_cover_subclass_annual.csv', row.names = FALSE)
write.csv(setNames(classification[, c('mapbiomas_id', 'class', 'class_code', 'subclass_name', 'subclass_code')],
                   c('mapbiomas_id', 'class', 'class_code', 'subclass', 'subclass_code')),
          './1_data/land_cover/land_cover_classification.csv', row.names = FALSE)

#==============================================================================================
# Atributos estáticos: cobertura del último año disponible (2024)
#==============================================================================================
ref_year <- max(class_ts$year)
land_cover <- class_ts[class_ts$year == ref_year, c('gauge_id', paste0(class_codes, '_perc'), 'dominant_class')]

# Glaciares separados de los demás cuerpos de agua (water_perc = glacier_perc + water_body_perc)
sub_ref <- subclass_ts[subclass_ts$year == ref_year, ]
sub_ref <- sub_ref[match(land_cover$gauge_id, sub_ref$gauge_id), ]
land_cover$glacier_perc    <- sub_ref$glacier_perc
land_cover$water_body_perc <- round(sub_ref$river_lake_ocean_perc + sub_ref$aquaculture_perc, 4)
stopifnot(max(abs(land_cover$glacier_perc + land_cover$water_body_perc - land_cover$water_perc)) < 1e-3)
land_cover <- land_cover[, c('gauge_id', paste0(class_codes, '_perc'), 'glacier_perc', 'water_body_perc', 'dominant_class')]

land_cover$gauge_id <- sub('^PE_', '', land_cover$gauge_id)
land_cover$dominant_class <- paste0(land_cover$dominant_class, '_perc')
cat("Atributos estáticos de cobertura: año", ref_year, "\n")
write.csv(land_cover, './1_data/land_cover/land_cover.csv', row.names = FALSE)
