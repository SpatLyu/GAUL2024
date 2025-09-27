setwd('./project/GAUL2024/')

library(rgee)
ee_Initialize(gcs = TRUE, drive = TRUE)

L0 = ee$FeatureCollection("projects/sat-io/open-datasets/FAO/GAUL/GAUL_2024_L0")
L1 = ee$FeatureCollection("projects/sat-io/open-datasets/FAO/GAUL/GAUL_2024_L1")
L2 = ee$FeatureCollection("projects/sat-io/open-datasets/FAO/GAUL/GAUL_2024_L2")

L0$size()$getInfo()
L1$size()$getInfo()
L2$size()$getInfo()

.get_gaul_layer = \(layer,lid,export = TRUE,export_level = 1){
  feat = ee_as_sf(x = ee$Feature(layer$toList(1, lid-1)$get(0)), 
                  maxFeatures = 1e13) 
  if (export) sf::write_sf(feat,
                           paste0("./data/gaul_shp/L",export_level,
                                  "/L",export_level,"_",lid,".gpkg"),
                           overwrite = TRUE)
  return(feat)
}

purrr::map(seq_len(L1$size()$getInfo()),
           \(.i) .get_gaul_layer(L1,.i))

gaul_level1 = fs::dir_ls(
  path = "./data/L1/",
  regexp = "\\.(geojson|shp|gpkg)$") |> 
  purrr::map_dfr(\(.f) {
    single_region = sf::read_sf(.f)
    geometry_type = sdsfun::sf_geometry_type(single_region)
    if (geometry_type == "geometrycollection") {
      single_region = sf::st_collection_extract(single_region,"POLYGON")
    } 
    if (geometry_type == "polygon") {
      single_region = sf::st_cast(single_region,"MULTIPOLYGON")
    } 
    geom_col = attr(single_region, "sf_column")
    if (geom_col != "geom") {
      names(single_region)[names(single_region) == geom_col] = "geom"
      sf::st_geometry(single_region) = "geom"
    }
    return(single_region)
  }) |> 
  sf::st_cast("MULTIPOLYGON")

gaul_level1 |> 
  dplyr::summarise(geom = sf::st_union(geom),
                   .by = -geom) |> 
  sf::st_cast("MULTIPOLYGON") -> gaul1

sf::st_is_valid(gaul1) |> sum()

sf::write_sf(gaul1,'./data/gaul2024.gdb',layer = "level1",overwrite = TRUE)
