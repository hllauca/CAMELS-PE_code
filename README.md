# CAMELS-PE: processing code

Code used to build **CAMELS-PE v2.0** (Catchment Attributes and Meteorology for Large-sample
Studies – Peru), a large-sample hydrological dataset with daily time series (1981–2025) and
catchment attributes for 136 catchments in Peru.

- Dataset (all versions): https://doi.org/10.5281/zenodo.20058778
- Dataset v2.0: https://doi.org/10.5281/zenodo.23890227
- Data description paper: Llauca et al. (2026), *Earth System Science Data Discussions*,
  https://doi.org/10.5194/essd-2026-386
- R package to read the dataset: https://github.com/hllauca/RCamelsPE

The scripts are provided for transparency and reproducibility of the processing steps. They are
research code: paths, file names and some comments (in Spanish) reflect the original project.

## Folder structure

The repository mirrors the structure of the original project, so that the scripts can call each
other with relative paths. All scripts must be run **from the root of this folder** (R working
directory or Python `CAMELS_PE_ROOT` environment variable).

```
1_data/
├── location/            gauge metadata and nesting
├── hydrometeorological/ catchment-averaged forcings, streamflow quality control, PET
│   ├── gee/             Google Earth Engine scripts (CHIRPS v3, ERA5-Land)
│   └── qc/              inspection plots of the quality-controlled streamflow
├── topography/gee/      FABDEM statistics (Google Earth Engine)
├── signatures/          climatic indices and hydrological signatures
├── land_cover/          MapBiomas land-cover attributes and annual series
├── soil/                DSOLMap suborders, great groups and horizon textures
└── human_intervention/  ADH records, reservoirs and tailings dams (ANA)
2_results/
├── prepare_dataset.R    assembly of the CAMELS-PE folders and files
├── make_data_dictionary.R
├── audit_dataset.R      consistency checks of the final dataset
└── create_sample.R      sample dataset bundled with the RCamelsPE package
```

## Workflow

| Step | Script | Description | Main outputs |
|---|---|---|---|
| 1 | `1_data/topography/gee/fabdem_gauge_elevation.js`, `fabdem_catchment_stats.js` | FABDEM v1.2 elevation at the gauges and catchment elevation and slope statistics (Google Earth Engine) | `1_data/topography/raw/*.csv` |
| 2 | `1_data/location/compute_location.R` | Gauge metadata, hydrographic region, record period, nesting topology, area and perimeter (geodesic) | `gauge_location.csv`, `catchment_topography.csv` |
| 3 | `1_data/hydrometeorological/gee/*.js` | Daily catchment averages of CHIRPS v3 and ERA5-Land (precipitation, solar radiation, vapour pressure) | yearly CSV exports |
| 4 | `1_data/hydrometeorological/extract_variables.R` | Catchment averages of PISCOp v3.0 and PISCOt v1.2, gathering of the alternative products, PET (calls `compute_pet.R`), streamflow quality control (calls `flow_qc.R`) and extraction of the PISCO-ARNOVIC v1.1 series at the outlets | `1_data/hydrometeorological/*_1981_2025.csv` |
| 5 | `1_data/signatures/compute_indices_signatures.R` | Climatic indices and hydrological signatures (observed and simulated streamflow), following Addor et al. (2017) | `catchment_signatures*.csv`, climatic indices |
| 6 | `1_data/signatures/compare_signatures_sim_obs.R` | Comparison of simulated and observed signatures on the same days | `signatures_sim_vs_obs*.csv` |
| 7 | `1_data/land_cover/process_land_cover.R` | Land-cover classes and subclasses (MapBiomas Collection 4), annual series 1985–2024 | `land_cover*.csv` |
| 8 | `1_data/human_intervention/read_demands.R` | ADH records, reservoirs, degree of regulation, tailings dams and near-natural indicator | `human_intervention.csv` |
| 8b | `1_data/soil/process_soil_types.R` | Soil suborders, great groups and horizon textures (DSOLMap): dominant classes, areal percentages (long format) and legend | `soil_types_dominant.csv`, `soil_classes.csv`, `soil_classification.csv` |
| 9 | `2_results/prepare_dataset.R`, `make_data_dictionary.R` | Assembly of the CAMELS-PE folders, files and data dictionary | `2_results/CAMELS-PE/` |
| 10 | `2_results/audit_dataset.R` | Consistency checks of the final dataset | console report |
| 11 | `1_data/hydrometeorological/qc/plot_flow_qc.py` | Inspection plots of the quality-controlled streamflow (Supplement) | PNG |

### Notes

- **Catchment boundaries** (`1_data/sig/camels_pe_catchments.gpkg`) are an input of this workflow.
  They were delineated from the 90 m SRTM DEM at the gauge locations and edited in a GIS so that
  adjacent catchments share identical borders and nested catchments are fully contained within
  their downstream catchments.
- **Geology and soil** attributes are computed from the areal percentages of the GLiM v1.1 /
  GLHYMPS 2.0 classes and of the DSOLMap soil orders, suborders, great groups and horizon
  texture classes within each catchment (`1_data/geology/`, `1_data/soil/`), obtained with GIS
  zonal statistics; `process_soil_types.R` derives the soil attributes from these percentages.
- **Simulated streamflow**: step 4 extracts the PISCO-ARNOVIC v1.1 series at the catchment
  gauges (`1_data/hydrometeorological/flow_PISCO_ARNOVIC_1981_2025.csv`), which are distributed
  in CAMELS-PE as `flow_sim`.
- The Google Earth Engine scripts refer to the catchment polygons uploaded as a private asset;
  upload `camels_pe_catchments.gpkg` as your own asset and update the asset path.
- The input data are not included. They are available from their providers: PISCOp v3.0, PISCOt
  v1.2 and PISCO-ARNOVIC v1.1 (SENAMHI), observed streamflow (SENAMHI), CHIRPS v3 (Climate Hazards
  Center), ERA5-Land (ECMWF/Copernicus), SRTM (NASA/USGS), FABDEM v1.2 (University of Bristol),
  MapBiomas Peru, Ecuador and Colombia (Collection 4), GLiM v1.1, GLHYMPS 2.0, DSOLMap and the ANA
  inventories of water availability accreditations and dams.

## Requirements

- **R** (≥ 4.1) with: data.table, datetimeutils, devEMF, dplyr, exactextractr, ggnewscale,
  ggplot2, ggspatial, hyfo, lubridate, ncdf4, openxlsx, patchwork, purrr, readr, scales, sf,
  stringr, terra, tidyr, tidyterra, tidyverse, zoo.
- **Python** (≥ 3.10) with: matplotlib, numpy, pandas (streamflow inspection plots only).
- **Google Earth Engine** account for the `gee/` scripts.

## Citation

If you use this code, please cite the data description paper and the dataset (see
`CITATION.cff`).

## License

GNU General Public License v3.0 (see `LICENSE`). The CAMELS-PE dataset is distributed under
CC BY 4.0.

## Contact

Harold Llauca — Servicio Nacional de Meteorología e Hidrología del Perú (SENAMHI),
hllauca@senamhi.gob.pe
