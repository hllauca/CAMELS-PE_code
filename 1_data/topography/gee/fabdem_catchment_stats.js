// ==============================
// FABDEM stats por cuenca
// gauge_id + elevación + pendiente media (grados y m/km)
// ==============================

// 1. DEM
// mosaic() pierde la proyección nativa (queda en WGS84 a 1°), y ee.Terrain.slope calcula la
// pendiente con los vecinos de esa grilla de ~111 km: los valores salían ~10 veces más bajos.
// Se recupera la proyección de FABDEM (1 arcsec, ~30 m) antes de calcular la pendiente.
var fabdem = ee.ImageCollection("projects/sat-io/open-datasets/FABDEM");
var dem = fabdem.mosaic()
  .setDefaultProjection(fabdem.first().projection())
  .rename('elevation');

// 2. Cuencas (polígonos corregidos, simplificados con tolerancia de 100 m, igual que en las
// variables meteorológicas)
var catchments = ee.FeatureCollection("users/hllauca/camels_pe_catchments_gee");

// 3. Pendiente por píxel: grados y m/km (tan se aplica píxel a píxel, antes de promediar)
var slope_deg = ee.Terrain.slope(dem).rename('slope_deg');
var slope_mkm = slope_deg
  .multiply(Math.PI)
  .divide(180)
  .tan()
  .multiply(1000)
  .rename('slope_mkm');

// 4. Estadísticos de elevación
var elev_stats = dem.reduceRegions({
  collection: catchments,
  reducer: ee.Reducer.min()
    .combine({reducer2: ee.Reducer.max(), sharedInputs: true})
    .combine({reducer2: ee.Reducer.mean(), sharedInputs: true})
    .combine({reducer2: ee.Reducer.median(), sharedInputs: true}),
  scale: 30,
  crs: 'EPSG:4326',
  tileScale: 4
});

// 5. Estadístico de pendiente (media de ambas bandas)
var slope_stats = slope_deg.addBands(slope_mkm).reduceRegions({
  collection: catchments,
  reducer: ee.Reducer.mean(),
  scale: 30,
  crs: 'EPSG:4326',
  tileScale: 4
});

// 6. Join por gauge_id
var join = ee.Join.inner();
var filter = ee.Filter.equals({
  leftField: 'gauge_id',
  rightField: 'gauge_id'
});

var joined = join.apply(elev_stats, slope_stats, filter);

// 7. Construir tabla final con nombres limpios y orden exacto
var stats_final = ee.FeatureCollection(joined.map(function(f) {
  var elev = ee.Feature(f.get('primary'));
  var slope = ee.Feature(f.get('secondary'));

  return ee.Feature(null, {
    gauge_id: elev.get('gauge_id'),
    elev_min: elev.get('min'),
    elev_max: elev.get('max'),
    elev_mean: elev.get('mean'),
    elev_median: elev.get('median'),
    slope_mean_deg: slope.get('slope_deg'),
    slope_mean_mkm: slope.get('slope_mkm')
  });
})).select([
  'gauge_id',
  'elev_min',
  'elev_max',
  'elev_mean',
  'elev_median',
  'slope_mean_deg',
  'slope_mean_mkm'
]);

// 8. Revisar
print('Final stats', stats_final.limit(10));

// 9. Exportar a CSV
Export.table.toDrive({
  collection: stats_final,
  description: 'FABDEM_catchment_stats_with_slope',
  folder: 'GEE_Exports',
  fileNamePrefix: 'fabdem_catchment_stats',
  fileFormat: 'CSV'
});
