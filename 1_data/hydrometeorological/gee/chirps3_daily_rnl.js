// Precipitación diaria CHIRPS v3 (reparto diario con ERA5, "rnl") promediada por cuenca, 1981–2025
// By Harold Llauca
//
// - Producto: UCSB-CHC/CHIRPS/V3/DAILY_RNL (0.05°, mm d-1). Es el único CHIRPS v3 diario disponible
//   desde 1981; el producto "sat" (IMERG) empieza en 1998.
// - Para que la exportación sea rápida, cada año se apila como una imagen de 365/366 bandas y se reduce
//   una sola vez por cuenca (en lugar de una reducción por imagen diaria).
// - Media ponderada por la fracción de píxel dentro de la cuenca (comportamiento por defecto de
//   reduceRegions), así que las cuencas pequeñas también tienen valor.
// - Cuencas: asset simplificado a 100 m (1_data/sig/camels_pe_catchments_gee.zip), el mismo que en
//   vprp_era5land.js y srad_era5land.js (cambio de área máx. 0.17 %, píxel de CHIRPS ~5.5 km).
// - Salida: un CSV por año en Drive/GEE_exports con columnas gauge_id y p_YYYYMMDD. Se copian a
//   1_data/hydrometeorological/raw/chirps3/ y extract_variables.R escribe prec_CHIRPS3_1981_2025.csv.

var cuencas = ee.FeatureCollection('users/hllauca/camels_pe_catchments_gee').select(['gauge_id']);
var chirps = ee.ImageCollection('UCSB-CHC/CHIRPS/V3/DAILY_RNL').select('precipitation');

// Funciones definidas fuera del bucle (el editor de GEE avisa si se crean dentro)
function bandName(t) {
  return ee.String('p_').cat(ee.Date(t).format('YYYYMMdd'));
}

function dropGeometry(f) {
  return f.setGeometry(null);
}

function exportYear(year) {
  var ic = chirps.filterDate(year + '-01-01', (year + 1) + '-01-01').sort('system:time_start');
  var names = ic.aggregate_array('system:time_start').map(bandName);
  var stack = ic.toBands().rename(names);

  var tabla = stack.reduceRegions({
    collection: cuencas,
    reducer: ee.Reducer.mean(),
    scale: 5566,          // resolución nativa (0.05°)
    tileScale: 4          // evita errores de memoria en las cuencas amazónicas grandes
  }).map(dropGeometry);

  Export.table.toDrive({
    collection: tabla,
    description: 'chirps3_rnl_' + year,
    folder: 'GEE_exports',
    fileNamePrefix: 'chirps3_rnl_' + year,
    fileFormat: 'CSV'
  });
}

for (var year = 1981; year <= 2025; year++) {
  exportYear(year);
}

// Control rápido: número de días por año (debe ser 365 o 366, 16 436 días en 1981–2025)
print('Días 1981–2025:', chirps.filterDate('1981-01-01', '2026-01-01').size());
