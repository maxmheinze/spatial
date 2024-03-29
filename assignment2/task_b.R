
# Header ------------------------------------------------------------------

pacman::p_load(
  tidyverse,
  tmap,
  haven,
  foreign,
  magrittr,
  stargazer,
  conleyreg,
  janitor,
  estimatr,
  fixest
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



# Replicate Results -------------------------------------------------------

col1 <- lm(illiteracy ~ distmiss + lati + longi + corr + ita + mis + mis1, data = litr)
col2 <- lm(illiteracy ~ distmiss + lati + longi + area + tempe + alti + preci + rugg + river + coast + corr + ita + mis + mis1, data = litr)

litr_bra <- subset(litr, country == "BRA")
col3 <- lm(illiteracy ~ distmiss + lati + longi + as.factor(mesorregi), data = litr_bra)
col4 <- lm(illiteracy ~ distmiss + lati + longi + area + tempe + alti + preci + rugg + river + coast + as.factor(mesorregi), data = litr_bra)

litr_arg <- subset(litr, country == "Argentina")
col5 <- lm(illiteracy ~ distmiss + lati + longi + corr, data = litr_arg)
col6 <- lm(illiteracy ~ distmiss + lati + longi + area + tempe + alti + preci + rugg + river + coast + corr, data = litr_arg)

litr_pry <- subset(litr, country == "Paraguay")
col7 <- lm(illiteracy ~ distmiss + ita, data = litr_pry)
col8 <- lm(illiteracy ~ distmiss + area + tempe + alti + preci + rugg + river + coast + ita, data = litr_pry)


stargazer(col1, col2, col3, col4, col5, col6, col7, col8,
          type = "latex",
          se = starprep(col1, col2, col3, col4, col5, col6, col7, col8, se_type = "stata"),
          omit.stat = "f")


lit1 <- litr %>%
  drop_na(lati, longi) %>%
  mutate(lat = lati,
         lon = longi)

col1c <- conleyreg(illiteracy ~ distmiss + lati + longi + corr + ita + mis + mis1, data = lit1,
           dist_cutoff = 11.112, lat = "lat", lon = "lon")
col2c <- conleyreg(illiteracy ~ distmiss + lati + longi + area + tempe + alti + preci + rugg + river + coast + corr + ita + mis + mis1, data = lit1,
           dist_cutoff = 11.112, lat = "lat", lon = "lon")

lit1_bra <- subset(lit1, country == "BRA")
col3c <- conleyreg(illiteracy ~ distmiss + lati + longi + as.factor(mesorregi), data = lit1_bra,
           dist_cutoff = 11.112, lat = "lat", lon = "lon")
col4c <- conleyreg(illiteracy ~ distmiss + lati + longi + area + tempe + alti + preci + rugg + river + coast + as.factor(mesorregi), data = lit1_bra,
           dist_cutoff = 11.112, lat = "lat", lon = "lon")

lit1_arg <- subset(lit1, country == "Argentina")
col5c <- conleyreg(illiteracy ~ distmiss + lati + longi + corr, data = lit1_arg,
           dist_cutoff = 11.112, lat = "lat", lon = "lon")
col6c <- conleyreg(illiteracy ~ distmiss + lati + longi + area + tempe + alti + preci + rugg + river + coast + corr, data = lit1_arg,
           dist_cutoff = 11.112, lat = "lat", lon = "lon")

lit1_pry <- subset(lit1, country == "Paraguay")
col7c <- conleyreg(illiteracy ~ distmiss + ita, data =  lit1_pry,
                   dist_cutoff = 11.112, lat = "lat", lon = "lon")
col8c <- conleyreg(illiteracy ~ distmiss + area + tempe + alti + preci + rugg + river + coast + ita, data =  lit1_pry,
                   dist_cutoff = 11.112, lat = "lat", lon = "lon")

stargazer(col1c, col2c, col3c, col4c, col5c, col6c, col7c, col8c, type = "text")

# Assuming the fixest package is already loaded
# library(fixest)

# Model specifications using feols() from the fixest package

# Models for the full dataset
col1cf <- feols(illiteracy ~ distmiss + lati + longi + corr + ita + mis + mis1, data = litr, 
                 vcov_conley(lat = "lati", lon = "longi", cutoff = 11.112, distance = "spherical"))
col2cf <- feols(illiteracy ~ distmiss + lati + longi + area + tempe + alti + preci + rugg + river + coast + corr + ita + mis + mis1, data = litr, 
                 vcov_conley(lat = "lati", lon = "longi", cutoff = 11.112, distance = "spherical"))

col3cf <- feols(illiteracy ~ distmiss + lati + longi + as.factor(mesorregi), data = litr_bra, 
                 vcov_conley(lat = "lati", lon = "longi", cutoff = 11.112, distance = "spherical"))
col4cf <- feols(illiteracy ~ distmiss + lati + longi + area + tempe + alti + preci + rugg + river + coast + as.factor(mesorregi), data = litr_bra, 
                 vcov_conley(lat = "lati", lon = "longi", cutoff = 11.112, distance = "spherical"))

col5cf <- feols(illiteracy ~ distmiss + lati + longi + corr, data = litr_arg, 
                 vcov_conley(lat = "lati", lon = "longi", cutoff = 11.112, distance = "spherical"))
col6cf <- feols(illiteracy ~ distmiss + lati + longi + area + tempe + alti + preci + rugg + river + coast + corr, data = litr_arg, 
                 vcov_conley(lat = "lati", lon = "longi", cutoff = 11.112, distance = "spherical"))

col7cf <- feols(illiteracy ~ distmiss + ita, data = litr_pry, 
                 vcov_conley(lat = "lati", lon = "longi", cutoff = 11.112, distance = "spherical"))
col8cf <- feols(illiteracy ~ distmiss + area + tempe + alti + preci + rugg + river + coast + ita, data = litr_pry, 
                 vcov_conley(lat = "lati", lon = "longi", cutoff = 11.112, distance = "spherical"))

etable(col1c, col2c, col3c, col4c, col5c, col6c, col7c, col8c)

