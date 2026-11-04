# Calcular variables de localizacion de estaciones CAMELS-PE
rm(list=ls())
cat('\f')

library(terra)
library(dplyr)
library(ggplot2)
library(exactextractr)
library(patchwork)
library(sf)
library(tidyterra)
library(scales)
library(terra)
library(ggnewscale)
library(ggspatial)
library(devEMF)

# Directorio de trabajo
# Run this script from the root of the CAMELS-PE project folder (see README.md).
# Leer datos geoespaciales
dem    <- rast('./1_data/sig/dem_peru_strm90m.tif')
gauge  <- vect('./1_data/sig/camels_pe_gauges.gpkg')
cat    <- vect('./1_data/sig/camels_pe_catchments.gpkg')
region <- vect('./1_data/sig/vertientes.shp')

# Calcular atributos de estaciones
gauge_id   <- gauge$gauge_id
gauge_name <- gauge$name
gauge_lat  <- gauge$latitude
gauge_lon  <- gauge$longitude
gauge_elev <- terra::extract(dem, gauge, ID=FALSE) %>% unlist() %>% as.vector()

# Elevación de la estación con FABDEM (mismo DEM que elev_min/mean/max de las cuencas), exportada con
# 1_data/topography/gee/fabdem_gauge_elevation.js; si aún no existe se mantiene SRTM 90 m
f_fab <- './1_data/topography/raw/fabdem_gauge_elevation.csv'
if (file.exists(f_fab)) {
  fab <- read.csv(f_fab)
  gauge_elev <- round(fab$gauge_elev[match(gauge_id, fab$gauge_id)])
  stopifnot(!anyNA(gauge_elev))
}

gauge_region <- terra::extract(region, gauge)[, "VERTIENTE"]
gauge_region <- recode(gauge_region,
                       "Atlantico" = "Atlantic",
                       "Pacifico"  = "Pacific",
                       "Titicaca"  = "Titicaca")
# Estaciones fuera del mapa de vertientes del Perú (Bolivia, Ecuador, Brasil/Colombia) o en su borde
region_fix <- c(
  PE_210003   = 'Titicaca',  # Huancasaya (Suches)
  PE_1113346  = 'Atlantic',  # Napo
  PE_2401081  = 'Atlantic',  # Tabatinga
  PE_47E01126 = 'Pacific',   # Cabo Inga (Tumbes, Ecuador)
  PE_47E9177A = 'Pacific',   # El Ciruelo (Chira, Ecuador)
  PE_47E0C74E = 'Pacific'    # Puente Internacional Macara (Chira, Ecuador)
)
gauge_region[match(names(region_fix), gauge_id)] <- region_fix
stopifnot(!anyNA(gauge_region))

# Leer series de caudal
raw   <- read.csv("./1_data/hydrometeorological/flow_OBS_1981_2025.csv", check.names = FALSE)
dates <- as.Date(paste(raw$year, raw$month, raw$day, sep = "-"))
match <- match(gauge_id, paste0('PE_',colnames(raw[, -c(1:3)])))
flow  <- raw[, -c(1:3)][match]

gauge_record_start <- sapply(flow, function(x) {
  if (all(is.na(x))) return(as.Date(NA))
  as.character(min(dates[!is.na(x)]))
})

gauge_record_end <- sapply(flow, function(x) {
  if (all(is.na(x))) return(as.Date(NA))
  as.character(max(dates[!is.na(x)]))
})



# =========================
# 4. Calcular disponibilidad
# =========================
gauge_years_available <- sapply(flow, function(x) {
  sum(!is.na(x)) / 365
})

gauge_perc_obs <- sapply(flow, function(x) {
  100*sum(!is.na(x)) / nrow(flow)
})

# Exportar base de datos
gauge_location <- data.frame(gauge_id,
                             gauge_name,
                             gauge_region,
                             gauge_lat,
                             gauge_lon,
                             gauge_elev,
                             gauge_record_start,
                             gauge_record_end,
                             gauge_perc_obs,
                             name_cat = gauge$name_cat,
                             is_nested = gauge$is_nested,
                             nested_group_id = gauge$nested_group_id,
                             nested_group_size = gauge$nested_group_size,
                             downstream_gauge_id = gauge$downstream_gauge_id,
                             upstream_gauge_id = gauge$upstream_gauge_id
                             )
write.csv(gauge_location, './1_data/location/gauge_location.csv', row.names = FALSE)

# =========================
# 5. Crear tabla de estaciones
# =========================
gauge_df <- data.frame(
  gauge_id,
  gauge_name,
  gauge_lat,
  gauge_lon,
  gauge_elev,
  gauge_region,
  gauge_record_start,
  gauge_record_end,
  years_available = gauge_years_available
) |>
  mutate(
    years_class = cut(
      years_available,
      breaks = c(0, 10, 20, 30, 45),
      include.lowest = TRUE,
      right = TRUE,
      labels = c("0 - 10", "10 - 20", "20 - 30", "30 - 45")
    ),
    years_class = factor(
      years_class,
      levels = c("0 - 10", "10 - 20", "20 - 30", "30 - 45")
    )
  )

# =========================
# Crear figuras
# =========================
gauge_sf  <- st_as_sf(gauge_df, coords = c("gauge_lon", "gauge_lat"), crs = 4326)
region_sf <- st_as_sf(region)

gauge_sf <- gauge_sf |>
  arrange(desc(years_available))

p1 <- ggplot() +
  geom_sf(
    data = region_sf,
    aes(fill = VERTIENTE),   # o el nombre correcto del campo
    color = 'gray20',
    linewidth = 0.2
  ) +
  scale_fill_manual(
    values = c(
      "Pacifico"  = "#F7E58A",  # amarillo suave pero más visible
      "Atlantico" = "#8FD19E",  # verde medio suave
      "Titicaca"  = "#D9A85B"   # marrón cálido suave
    ),
    guide = "none"
  ) +
  ggnewscale::new_scale_fill() +
  geom_sf(
    data = gauge_sf,
    aes(size = years_class, fill = years_class),
    shape = 21,
    color = "black",
    stroke = 0.5,
    alpha = 0.8
  ) +
  scale_fill_manual(
    values = c(
      "0 - 10"  = "#f0f0f0",
      "10 - 20" = "#9ecae1",
      "20 - 30" = "#3182bd",
      "30 - 45" = "#08519c"
    ),
    name = "Discharge\ndata\navailability\n(years)"
  ) +
  scale_size_manual(
    values = c(
      "0 - 10"  = 2,
      "10 - 20" = 3,
      "20 - 30" = 4,
      "30 - 45" = 5
    ),
    name = "Discharge\ndata\navailability\n(years)"
  ) +
  coord_sf(
    xlim = c(-81.5, -68.5),
    ylim = c(-18.8, 0.5),
    expand = FALSE
  ) +
  scale_x_continuous( n.breaks = 3) +
  scale_y_continuous(n.breaks = 5) +
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    text_cex = 0.8,
    line_width = 0.7,
    pad_x = unit(2, "cm") 
  ) +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(
    panel.grid.major = element_line(color = "grey70", linewidth = 0.1),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 9),
    legend.position = c(0.15, 0.25),
    legend.box = "vertical",
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.margin = margin(8, 8, 8, 8)
  ) +
  # Título (igual)
  annotate(
    "text",
    x = -81.15, y = -0.18,
    label = "Hydrographic regions",
    hjust = 0,
    vjust = 1,
    size = 3
  ) +
  
  # Pacific
  annotate(
    "rect",
    xmin = -81.15, xmax = -80.45,
    ymin = -1.20, ymax = -0.80,
    fill = "#F7E58A", color = NA
  ) +
  annotate(
    "text",
    x = -80.35, y = -1.00,
    label = "Pacific",
    hjust = 0,
    vjust = 0.5,
    size = 3.2
  ) +
  
  # Atlantic (más separado hacia abajo)
  annotate(
    "rect",
    xmin = -81.15, xmax = -80.45,
    ymin = -1.85, ymax = -1.45,
    fill = "#8FD19E", color = NA
  ) +
  annotate(
    "text",
    x = -80.35, y = -1.65,
    label = "Atlantic",
    hjust = 0,
    vjust = 0.5,
    size = 3.2
  ) +
  
  # Titicaca (más separado aún)
  annotate(
    "rect",
    xmin = -81.15, xmax = -80.45,
    ymin = -2.50, ymax = -2.10,
    fill = "#D9A85B", color = NA
  ) +
  annotate(
    "text",
    x = -80.35, y = -2.30,
    label = "Titicaca",
    hjust = 0,
    vjust = 0.5,
    size = 3.2
  )


# Panel B
years <- as.numeric(format(dates, "%Y"))
plot_a_df <- data.frame(
  year = sort(unique(years))
) %>%
  mutate(
    n_gauges = sapply(year, function(y) {
      idx <- years == y
      sum(colSums(!is.na(flow[idx, , drop = FALSE])) > 0)
    })
  )

p2 <- ggplot(plot_a_df, aes(year, n_gauges)) +
  geom_line(linewidth = 1.1, color = "steelblue4") +
  labs(x = "Year", y = "Number of active gauges") +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank())


# Panel C
min_record_years <- c(5, 10, 15, 20, 25, 30)
min_fd <- c(70, 75, 80, 85, 90, 95)

station_metrics <- do.call(rbind, lapply(seq_len(ncol(flow)), function(i) {
  x <- flow[, i]
  vd <- dates[!is.na(x)]
  if (!length(vd)) return(data.frame(record_years = NA, availability = NA))

  start <- min(vd)
  end   <- max(vd)
  span  <- dates >= start & dates <= end

  data.frame(
    record_years = (as.numeric(end - start) + 1) / 365.25,
    availability = sum(!is.na(x[span])) / sum(span) * 100
  )
}))

plot_b_df <- expand.grid(
  min_record_years = min_record_years,
  min_fd = min_fd
) %>%
  mutate(
    n_stations = mapply(function(ry, fd) {
      sum(station_metrics$record_years >= ry &
            station_metrics$availability >= fd, na.rm = TRUE)
    }, min_record_years, min_fd)
  )

p3 <- ggplot(plot_b_df, aes(x = min_record_years, 
                            y = n_stations, 
                            color = factor(min_fd))) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  # scale_color_viridis_d(name = "Min. availability (%)", option = "C", direction = -1) +
  scale_color_manual(
    name = "Minimum\navailability\n(%)",
    values = c(
      "70" = "#F7E58A",
      "75" = "#E6A141",
      "80" = "#D17C3F",
      "85" = "#B85C2E",
      "90" = "#8C3B1F",
      "95" = "#5C2A14"
    )
  ) +
  labs(x = "Minimum record length (years)", 
       y = "Number of gauges") +
  theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank())

# Añadir tags a cada panel
p1 <- p1 + labs(tag = "(a)")
p2 <- p2 + labs(tag = "(b)")
p3 <- p3 + labs(tag = "(c)")

plt <- (p1 | (p2 / p3)) +
  plot_layout(
    widths = c(1.15, 1)
  ) &
  theme(
    plot.margin = margin(1, 1, 1, 1)
  )

# Guardar
ggsave(
  "./2_results/figures/figure_record_availability.png",
  plt,
  width = 9,
  height = 6,
  dpi = 600,
  bg = "white",
  type = 'cairo'
)

emf(
  "./2_results/figures/figure_record_availability.emf",
  width = 9,
  height = 6,
  bg = "white"
)
print(plt)
dev.off()

# Calcular atributos topograficos
match <- match(gauge_id, cat$gauge_id)
area  <- terra::expanse(cat, unit = 'km')[match]
perimeter    <- (terra::perim(cat) / 1000)[match]
fabdem_stats <- read.csv('./1_data/topography/raw/fabdem_catchment_stats.csv')
# gauge_id con prefijo 'PE_' (se agrega solo si el archivo de GEE no lo trae)
fabdem_id    <- ifelse(grepl('^PE_', fabdem_stats$gauge_id), fabdem_stats$gauge_id, paste0('PE_', fabdem_stats$gauge_id))
idx          <- match(gauge_id, fabdem_id)
stopifnot(!anyNA(idx))
elev_min     <- fabdem_stats$elev_min[idx]
elev_max     <- fabdem_stats$elev_max[idx]
elev_mean    <- fabdem_stats$elev_mean[idx]
elev_median  <- fabdem_stats$elev_median[idx]
slope_mean   <- fabdem_stats$slope_mean_mkm[idx]   # pendiente media [m km-1]

catchment_topography <- data.frame(gauge_id,
                                   gauge_elev,
                                   area,
                                   perimeter,
                                   elev_min,
                                   elev_max,
                                   elev_mean,
                                   elev_median,
                                   slope_mean)
write.csv(catchment_topography, './1_data/topography/catchment_topography.csv', row.names = FALSE)
