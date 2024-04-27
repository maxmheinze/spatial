##  EXERCISE B

# PACKAGES ---------------------------------------------------------------------

library(pacman)
p_load(
  haven,
  dplyr,
  sf,
  tmap,
  data.table,
  viridis,
  tidyverse,
  stargazer,
  SDPDmod,
  spatialreg
)

# LOADING DATA -----------------------------------------------------------------

intersect_coord <- read_dta("./data/harari/intersect_coord.dta")
geoconflict_main <- read_dta("./data/harari/geoconflict_main.dta", encoding = "UTF-8")
raster_Africa <- st_read("./data/harari/raster_Africa.shp")

## Preparing for merging
raster <- raster_Africa %>%
  rename( "_ID" = CELLID,
          lon = longitude_,
          lat = latitude_m)

df <- merge(geoconflict_main, raster, by = c("_ID", "lat", "lon")) %>% 
  filter(!(country_largest_share %in% c("Western Sahara", "Gambia"))) %>% #these countries are not listed in the online appendix
  select(-c("ctry_18", "ctry_46", "W_ctry_18", "W_ctry_46")) %>%  #corresponding dummy columns - not sure is needed
  rename(year_ = year,
         country_ = country_largest_share) %>% 
  mutate(year_ = as.factor(year_),
         country_ = gsub(" ", "_", country_),
         country_ = as.factor(country_),
         cell = as.factor(cell))

#Number of observations is still not correct! They have 35042
colSums(subset(geoconflict_main, select = 73:120) == 1)

# REPLICATION TABLE 2 ----------------------------------------------------------
# Model 1
model_1a <- lm(ANY_EVENT_ACLED ~ SPEI4pg + L1_SPEI4pg + L2_SPEI4pg +
                 GSmain_ext_SPEI4pg + L1_GSmain_ext_SPEI4pg + L2_GSmain_ext_SPEI4pg + 
                 elevation_cell + rough_cell + area_cell + use_primary + 
                 dis_river_cell + shared + border + any_mineral + ELF +
                 year_ + country_, data = df)
summary(model_1a)

#Model 2
# From equation 2: include all W*controls and W*country FE!
df_alternative <- df %>% 
  select(c(2,7:15,20,46:51,58:117, 119:163))
model_1b <- lm(ANY_EVENT_ACLED ~. , data = df_alternative)
summary(model_1b)

#Model 3
W <- mOrdNbr(raster, m =1)

# Dynamic spatial Durbin model with spatial and time fixed effects (with Lee-Yu transformation)
# Example:
# mod5<-SDPDm(formula = logc ~ logp+logy, data = data1, W = W,
#             index = c("state","year"),
#             model = "sdm", 
#             effect = "twoways",
#             LYtrans = T,
#             dynamic = T,
#             tlaginfo = list(ind = NULL, tl = T, stl = T))
# summary(mod5)
