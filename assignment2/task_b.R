
# Header ------------------------------------------------------------------

pacman::p_load(
  tidyverse,
  tmap,
  haven,
  foreign,
  magrittr
)


# Prepare Dataframe -------------------------------------------------------

litr <- read_dta("./assignment2/data/literacy_Arg-Bra-Par.dta", encoding = "ISO-8859-1")

shp0 <- read_sf("./assignment2/data/task_b_shapefile_0.shp")
shp1 <- read_sf("./assignment2/data/task_b_shapefile_1.shp")
shp2 <- read_sf("./assignment2/data/task_b_shapefile_2.shp")

spli <- shp2 %>%
  left_join(litr, by = c("NAME_2" = "muni"))


# Literacy Map ------------------------------------------------------------

tm_shape(spli) + 
  tm_fill("literacy", palette = "Reds", style = "equal", n = 10) + 
  tm_borders(col = "#BBBBBB") + 
  tm_shape(shp1) + 
  tm_borders(col = "#444444") + 
  tm_text("NAME_1", size = 0.75, col = "black", bg.color = "#FFFFFF") +
  tm_shape(shp0) + 
  tm_borders(col = "#000000") +
  tm_compass(position = c("right", "top"), size = 2) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 0.5) +
  tm_layout(title = "Literacy Rate by Municipality", 
            title.bg.color = "white", 
            legend.position = c("left", "bottom"),
            legend.bg.color = "white", 
            frame = FALSE,
            fontfamily = "Lato")


# Population Density Map --------------------------------------------------

tm_shape(spli) + 
  tm_fill("popd", palette = "Blues", style = "quantile", n = 10) + 
  tm_borders(col = "#BBBBBB") + 
  tm_shape(shp1) + 
  tm_borders(col = "#444444") + 
  tm_text("NAME_1", size = 0.75, col = "black", bg.color = "#FFFFFF") +
  tm_shape(shp0) + 
  tm_borders(col = "#000000") +
  tm_compass(position = c("right", "top"), size = 2) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 0.5) +
  tm_layout(title = "Population Density by Municipality", 
            title.bg.color = "white", 
            legend.position = c("left", "bottom"),
            legend.bg.color = "white", 
            frame = FALSE,
            fontfamily = "Lato")


# Jesuit Distance Map -----------------------------------------------------

tm_shape(spli) + 
  tm_fill("distmiss", palette = "Purples", style = "equal", n = 10) + 
  tm_borders(col = "#BBBBBB") + 
  tm_shape(shp1) + 
  tm_borders(col = "#444444") + 
  tm_text("NAME_1", size = 0.75, col = "black", bg.color = "#FFFFFF") +
  tm_shape(shp0) + 
  tm_borders(col = "#000000") +
  tm_compass(position = c("right", "top"), size = 2) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 0.5) +
  tm_layout(title = "Distance from Jesuit Missions by Municipality", 
            title.bg.color = "white", 
            legend.position = c("left", "bottom"),
            legend.bg.color = "white", 
            frame = FALSE,
            fontfamily = "Lato")

# Franciscan Distance Map -------------------------------------------------

tm_shape(spli) + 
  tm_fill("distfran", palette = "Greens", style = "equal", n = 10) + 
  tm_borders(col = "#BBBBBB") + 
  tm_shape(shp1) + 
  tm_borders(col = "#444444") + 
  tm_text("NAME_1", size = 0.75, col = "black", bg.color = "#FFFFFF") +
  tm_shape(shp0) + 
  tm_borders(col = "#000000") +
  tm_compass(position = c("right", "top"), size = 2) +
  tm_scale_bar(position = c("right", "bottom"), text.size = 0.5) +
  tm_layout(title = "Distance from Franciscan Missions by Municipality", 
            title.bg.color = "white", 
            legend.position = c("left", "bottom"),
            legend.bg.color = "white", 
            frame = FALSE,
            fontfamily = "Lato")
