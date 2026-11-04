// Presión de vapor diaria ERA5-Land promediada por cuenca, 1981–2025
// By Harold Llauca
//
// - Producto: ECMWF/ERA5_LAND/DAILY_AGGR, banda dewpoint_temperature_2m (K).
// - Presión de vapor (hPa) con la ecuación de Magnus a partir de la temperatura de rocío:
//   e = 6.112 * exp(17.67 * Td / (Td + 243.5)), con Td en °C.
// - Mismo esquema que chirps3_daily_rnl.js: cada año se apila como una imagen de 365/366 bandas y se
//   reduce una sola vez por cuenca (mucho más rápido que una reducción por imagen diaria).
// - Cuencas: asset simplificado a 100 m (1_data/sig/camels_pe_catchments_gee.zip), que acelera la
//   reducción sin cambiar los promedios (cambio de área máx. 0.17 %, píxel de ERA5-Land ~11 km).
// - Salida: un CSV por año en Drive/GEE_exports con columnas gauge_id y vp_YYYYMMDD. Se copian a
//   1_data/hydrometeorological/raw/vprp_era5land/ y extract_variables.R escribe
//   vprp_ERA5LAND_1981_2025.csv.

var cuencas = ee.FeatureCollection('users/hllauca/camels_pe_catchments_gee').select(['gauge_id']);
var era5 = ee.ImageCollection('ECMWF/ERA5_LAND/DAILY_AGGR').select('dewpoint_temperature_2m');

// Funciones definidas fuera del bucle (el editor de GEE avisa si se crean dentro)
function toVaporPressure(img) {
  var td = img.subtract(273.15);                                        // K -> °C
  var e = td.expression('6.112 * exp((17.67 * td) / (td + 243.5))', {'td': td});
  return e.copyProperties(img, ['system:time_start']);                  // hPa
}

function bandName(t) {
  return ee.String('vp_').cat(ee.Date(t).format('YYYYMMdd'));
}

function dropGeometry(f) {
  return f.setGeometry(null);
}

function exportYear(year) {
  var ic = era5.filterDate(year + '-01-01', (year + 1) + '-01-01').sort('system:time_start')
    .map(toVaporPressure);
  var names = ic.aggregate_array('system:time_start').map(bandName);
  var stack = ee.ImageCollection(ic).toBands().rename(names);

  var tabla = stack.reduceRegions({
    collection: cuencas,
    reducer: ee.Reducer.mean(),
    scale: 9000,
    tileScale: 4          // evita errores de memoria en las cuencas amazónicas grandes
  }).map(dropGeometry);

  Export.table.toDrive({
    collection: tabla,
    description: 'vprp_era5land_' + year,
    folder: 'GEE_exports',
    fileNamePrefix: 'vprp_era5land_' + year,
    fileFormat: 'CSV'
  });
}

for (var year = 1981; year <= 2025; year++) {
  exportYear(year);
}

// Control rápido: número de días (16 436 en 1981–2025)
print('Días 1981–2025:', era5.filterDate('1981-01-01', '2026-01-01').size());
