

# Header ------------------------------------------------------------------

pacman::p_load(
  tidyverse,
  terra,
  sf,
  magrittr,
  ggplot2
)

west <- terra::rast("./assignment4/data_local/Hansen_GFC-2022-v1.10_treecover2000_50N_000E.tif")
east <- terra::rast("./assignment4/data_local/Hansen_GFC-2022-v1.10_treecover2000_50N_010E.tif")

wesg <- terra::rast("./assignment4/data_local/Hansen_GFC-2022-v1.10_gain_50N_000E.tif")
easg <- terra::rast("./assignment4/data_local/Hansen_GFC-2022-v1.10_gain_50N_010E.tif")

wesl <- terra::rast("./assignment4/data_local/Hansen_GFC-2022-v1.10_lossyear_50N_000E.tif")
easl <- terra::rast("./assignment4/data_local/Hansen_GFC-2022-v1.10_lossyear_50N_010E.tif")



# Prepare Raster ----------------------------------------------------------

allr <- sprc(west, east) %>%
  merge()

allr %<>%
  terra::aggregate(fact = 60, fun = "mean")

terra::writeRaster(allr, "./assignment4/data/aggregated_raster_start.tif", overwrite = TRUE)

allg <- sprc(wesg, easg) %>%
  merge()

allg %<>%
  terra::aggregate(fact = 60, fun = "max")

terra::writeRaster(allg, "./assignment4/data/aggregated_raster_gain.tif", overwrite = TRUE)

alll <- sprc(wesl, easl) %>%
  merge()

alll %<>%
  terra::aggregate(fact = 60, fun = "max")

terra::writeRaster(alll, "./assignment4/data/aggregated_raster_lossyear.tif", overwrite = TRUE)