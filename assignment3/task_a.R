
pacman::p_load(
  tidyverse,
  plm,
  readxl,
  spdep,
  splm,
  geosphere,
  igraph
)

cigs <- read_excel("./assignment3/data/cigarettes/cigarette+2var.xls")

cm1 <- plm(logc ~ logp + logy, 
           data = cigs,
           effect = "twoway",
           model = "within",
           index = c("state", "year"))

summary(cm1)

cigw <- read_excel("./assignment3/data/cigarettes/Spat-Sym-US.xls", col_names = FALSE) %>%
  as.matrix()

cign <- c("AL", "AZ", "AR", "CA", "CT", "DE", "DC", "FL", "GA", "ID", "IL", "IN", 
            "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", 
            "NE", "NV", "NH", "NJ", "NM", "NY", "ND", "OH", "OK", "PA", "RI", "SC", 
            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

dimnames(cigw) <- list(cign, cign)
rownames(cigw) <- cign

cigm <- mat2listw(cigw, style = "W")

# Testing for Moran's I does not really make much sense because we have a time dimension (or does it?)

slmtest(cm1, cigm, test = "lme")

slmtest(cm1, cigm, test = "lml")

slmtest(cm1, cigm, test = "rlme")

slmtest(cm1, cigm, test = "rlml")

# Choose SEM Model because also the robust version is significant

cm2 <- spml(
  logc ~ logp + logy, #SEM
  data = cigs,
  listw = cigm,
  effect = "twoways",
  model = "within",
  index = c("state", "year"),
  lag = FALSE,
  spatial.error = "b"
)

summary(cm2)

cm3 <- plm(
  logc ~ logp + logy + slag(logp, listw = cigm) + slag(logy, listw = cigm), #SLX
  data = cigs,
  effect = "twoways",
  model = "within",
  index = c("state", "year")
)

summary(cm3)


cigd <- read_excel("./assignment3/data/cigarettes/cigar_states.xls") %>%
  select(longitude, latitude) %>%
  as.matrix() %>%
  `rownames<-`(cign)

cigi <- distm(cigd, fun = distVincentyEllipsoid)

cigj <- (cigi/1000000)^(-3) %>%
  `diag<-`(0) %>%
  mat2listw(style = "B")

cm4 <- plm(
  logc ~ logp + logy + slag(logp, listw = cigj) + slag(logy, listw = cigj), #SLX w/ distance decay
  data = cigs,
  effect = "twoways",
  model = "within",
  index = c("state", "year")
)

summary(cm4)

coefficients(cm4)

cigk <- listw2mat(cigj) %>%
  `rownames<-`(cign) %>%
  `colnames<-`(cign)

cigg <- graph_from_adjacency_matrix((cigk), mode = "undirected")

cigk

plot(cigg)

heatmap(cigk, Rowv = NA, Colv = "Rowv")

rowSums(cigk)

eigen_centrality(
  cigg,
  directed = FALSE,
  scale = TRUE,
  weights = NULL,
  options = arpack_defaults()
) %>%
  `$`(vector) %>%
  sort(decreasing = TRUE)

