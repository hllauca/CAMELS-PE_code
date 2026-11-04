# Evapotranspiración potencial adicional para CAMELS-PE (1981–2025)
# By Harold Llauca
#
# Además de Hargreaves-Samani (referencia, calculada en extract_variables.R y guardada en
# evap_HS_1981_2025.csv), se calculan dos formulaciones para representar la incertidumbre
# asociada al método de PET (enfoque multi-producto, como la precipitación en CAMELS-CL):
#   - Priestley-Taylor (1972), alfa = 1.26, con radiación neta según FAO-56 (Allen et al., 1998):
#     radiación solar y presión de vapor de ERA5-Land; Tmax y Tmin de PISCOt.
#   - Oudin et al. (2005): PE = Ra / lambda * (T + 5) / 100 si T + 5 > 0, si no 0.
# Se ejecuta al final de extract_variables.R, o por separado (lee los CSV ya generados).

library(terra)
# Run this script from the root of the CAMELS-PE project folder (see README.md).
dir_hm <- './1_data/hydrometeorological/'

read_wide <- function(f) {
  x <- read.csv(file.path(dir_hm, f), check.names = FALSE)
  list(dates = as.Date(paste(x$year, x$month, x$day, sep = '-')), m = as.matrix(x[, -(1:3)]))
}
tx <- read_wide('tmax_PISCO_1981_2025.csv')
tn <- read_wide('tmin_PISCO_1981_2025.csv')
rs <- read_wide('srad_ERA5LAND_1981_2025.csv')   # MJ m-2 d-1
ea <- read_wide('vprp_ERA5LAND_1981_2025.csv')   # hPa
ids <- colnames(tx$m)
stopifnot(identical(tx$dates, rs$dates), identical(tx$dates, ea$dates), identical(tx$dates, tn$dates),
          identical(ids, colnames(rs$m)), identical(ids, colnames(ea$m)), identical(ids, colnames(tn$m)))

# Latitud de un punto interior de la cuenca (igual que en Hargreaves) y elevación media
cat_v   <- vect('./1_data/sig/camels_pe_catchments.gpkg')
cat_id  <- sub('^PE_', '', cat_v$gauge_id)
lat     <- (crds(centroids(cat_v, inside = TRUE))[, 'y'] * pi / 180)[match(ids, cat_id)]
topo    <- read.csv('./1_data/topography/catchment_topography.csv')
z       <- topo$elev_mean[match(ids, sub('^PE_', '', topo$gauge_id))]
stopifnot(!anyNA(lat), !anyNA(z))

# Radiación extraterrestre (FAO-56 Ec. 21–25) [MJ m-2 d-1]
J   <- as.integer(format(tx$dates, '%j'))
dr  <- 1 + 0.033 * cos(2 * pi * J / 365)
dec <- 0.409 * sin(2 * pi * J / 365 - 1.39)
ws  <- acos(pmin(pmax(outer(-tan(dec), tan(lat)), -1), 1))
Ra  <- (24 * 60 / pi) * 0.0820 * dr *
  (ws * outer(sin(dec), sin(lat)) + outer(cos(dec), cos(lat)) * sin(ws))

Tx <- tx$m; Tn <- tn$m; Rs <- rs$m; ea_kpa <- ea$m / 10
Tm <- (Tx + Tn) / 2
zz <- matrix(z, nrow(Tx), ncol(Tx), byrow = TRUE)

# Radiación neta (FAO-56 Ec. 37–40): albedo 0.23, onda larga con Rs/Rso acotado a [0.25, 1]
Rso <- (0.75 + 2e-5 * zz) * Ra
Rnl <- 4.903e-9 * ((Tx + 273.16)^4 + (Tn + 273.16)^4) / 2 *
  (0.34 - 0.14 * sqrt(pmax(ea_kpa, 0))) * (1.35 * pmin(pmax(Rs / Rso, 0.25), 1) - 0.35)
Rn  <- (1 - 0.23) * Rs - Rnl

# Priestley-Taylor (G = 0 a escala diaria)
gam   <- 0.000665 * 101.3 * ((293 - 0.0065 * zz) / 293)^5.26          # kPa °C-1
Delta <- 4098 * (0.6108 * exp(17.27 * Tm / (Tm + 237.3))) / (Tm + 237.3)^2
pet_pt <- pmax(1.26 * Delta / (Delta + gam) * Rn / 2.45, 0)            # mm d-1

# Oudin et al. (2005)
pet_ou <- ifelse(Tm + 5 > 0, Ra / 2.45 * (Tm + 5) / 100, 0)            # mm d-1

write_pet <- function(m, f) {
  out <- data.frame(year  = as.integer(format(tx$dates, '%Y')),
                    month = as.integer(format(tx$dates, '%m')),
                    day   = as.integer(format(tx$dates, '%d')),
                    round(m, 3), check.names = FALSE)
  colnames(out)[-(1:3)] <- ids
  write.csv(out, file.path(dir_hm, f), row.names = FALSE)
}
write_pet(pet_pt, 'evap_PT_1981_2025.csv')
write_pet(pet_ou, 'evap_OU_1981_2025.csv')
cat('PET Priestley-Taylor y Oudin guardadas. NA:', sum(is.na(pet_pt)), sum(is.na(pet_ou)), '\n')
