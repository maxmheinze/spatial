

# Header ------------------------------------------------------------------

pacman::p_load(
  tidyverse,
  terra,
  sf,
  magrittr,
  ggplot2,
  tidyterra
)

rstr <- terra::rast("./assignment4/data/aggregated_raster_start.tif")
rstg <- terra::rast("./assignment4/data/aggregated_raster_gain.tif")
rstl <- terra::rast("./assignment4/data/aggregated_raster_lossyear.tif")

shp0 <- st_read("./assignment4/data/gadm41_AUT_0.shp")



# Map ---------------------------------------------------------------------

ggplot() +  
  geom_spatraster(data=rstr, aes(fill=`Hansen_GFC-2022-v1.10_treecover2000_50N_000E`)) +
  scale_fill_distiller("Forest Cover (%)", palette = "YlGn", direction = 1) +
  xlim(c(9, 18)) +
  ylim(c(46, 49.5)) +
  coord_sf() +
  labs(title = "Tree Cover in 2000") +
  theme_bw() +
  theme(
    text = element_text(family = "Lato")
  )

ggplot() +  
  geom_spatraster(data=rstr, aes(fill=`Hansen_GFC-2022-v1.10_treecover2000_50N_000E`)) +
  geom_sf(data=shp0, col = "black", lwd = 1.5, fill = "#00000000") +
  scale_fill_distiller("Forest Cover (%)", palette = "YlGn", direction = 1) +
  xlim(c(9, 18)) +
  ylim(c(46, 49.5)) +
  coord_sf() +
  labs(title = "Tree Cover in 2000") +
  theme_bw() +
  theme(
    text = element_text(family = "Lato")
  )
  

ggplot() +  
  geom_spatraster(data=rstg, aes(fill=`Hansen_GFC-2022-v1.10_gain_50N_000E`)) +
  geom_sf(data=shp0, col = "black", lwd = 1.5, fill = "#00000000") +
  scale_fill_distiller("Forest Gain", palette = "Reds", direction = 1) +
  xlim(c(9, 18)) +
  ylim(c(46, 49.5)) +
  coord_sf() +
  labs(title = "Forest Gain 2000–2012",
       subtitle = "Dark = Gain Somewhere, Light = No Gain") +
  theme_bw() +
  guides(fill="none") +
  theme(
    text = element_text(family = "Lato")
  )

ggplot() +  
  geom_spatraster(data=rstl, aes(fill=Layer_1)) +
  geom_sf(data=shp0, col = "black", lwd = 1.5, fill = "#00000000") +
  scale_fill_distiller("Year", palette = "Spectral", direction = 1,  labels = c("none", "2005", "2010", "2015", "2020")) +
  xlim(c(9, 18)) +
  ylim(c(46, 49.5)) +
  coord_sf() +
  labs(title = "Most Recent Deforestation Event per Cell") +
  theme_bw() +
  theme(
    text = element_text(family = "Lato")
  )


# Histogram ---------------------------------------------------------------

terra::extract(rstl, shp0) %>%
  pluck("Layer_1") %>%
  table() %>%
  as_tibble() %>%
  mutate(year = 2000 + as.integer(`.`)) %>%
  ggplot() +
  geom_bar(aes(x = year, y = n, fill = year), col = "black", stat = "identity") +
  scale_fill_distiller("Year", palette = "Spectral", direction = 1) +
  guides(fill = "none") +
  labs(title = "Most Recent Deforestation Event per Cell, Within Austria, Histogram") +
  theme_bw() +
  theme(
    text = element_text(family = "Lato")
  )



