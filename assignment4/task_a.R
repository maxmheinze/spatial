
# Header ------------------------------------------------------------------

pacman::p_load(
  tidyverse,
  magrittr,
  sf,
  tmap,
  ceramic,
  ggplot2,
  ggspatial,
  basemaps
)

muni <- st_read("./assignment4/data/brazil_muni.gpkg")
datl <- read_rds("./assignment4/data/data_legal-amazon.rds")
slau <- read_rds("./assignment4/data/slaughterhouses.rds")

sf_use_s2(FALSE)


# Data Manipulation -------------------------------------------------------

mapd <- muni %>%
  right_join(datl, by = "muni_id")

slau <- st_intersection(slau, mapd) %>%
  mutate(slau = "Slaughterhouse")

mpd1 <- mapd %>%
  st_drop_geometry() %>%
  arrange(year) %>%
  group_by(muni_id) %>%
  summarize(forest_change = 100*(last(forest)/first(forest)-1)) %>%
  left_join(muni) %>%
  st_as_sf()


# Map ---------------------------------------------------------------------

tm_shape(mpd1) +
  tm_fill("forest_change", palette = "RdBu", style = "order", 
          legend.reverse = TRUE, title = "       Forest Change (%) from 2003 to 2022", 
          labels = c("", "most forest lost", "", "", "", "least forest lost", ""),
          legend.is.portrait = FALSE) +
  tm_borders(lwd = 0.1, col = "#444444") +
  tm_shape(slau) +
  tm_dots(col = "black", shape = 21) +
  tm_add_legend("symbol", labels = "Slaughterhouses", title = "") +
  tm_layout(main.title = "Relative Forest Change and Slaughterhouses", 
            legend.outside.position = "bottom",
            legend.outside.size = 0.2,
            legend.outside = TRUE,
            frame = FALSE,
            fontfamily = "Inter")




