
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

cigk %>%
  round(2)

plot(cigg)

heatmap(cigk, Rowv = NA, Colv = "Rowv")

rowSums(cigk) %>%
  sort(decreasing = TRUE) %>%
  knitr::kable(col.names = "Row Sum")

eigen_centrality(
  cigg,
  directed = FALSE,
  scale = TRUE,
  weights = NULL,
  options = arpack_defaults()
) %>%
  `$`(vector) %>%
  sort(decreasing = TRUE)


# Bonus question

library(bsreg)


cigs <- read_excel("./assignment3/data/cigarettes/cigarette+2var.xls") %>%
  mutate(year = factor(year + 1963), state = factor(state))
cigw <- read_excel("./assignment3/data/cigarettes/Spat-Sym-US.xls",
                   col_names = paste0(1:46)) %>% as.matrix()
cigd <- read_excel("./assignment3/data/cigarettes/cigar_states.xls", col_names = TRUE)

years_numb <- length(unique(cigs$year)) 

W_cont <- kronecker(diag(years_numb), cigw / rowSums(cigw)) 

dist <- as.matrix(dist(cigd))
diag(dist) <- Inf 

Psi <- function(delta) {
  W_dist <- dist ^ (-delta) # Build
  W_dist <- W_dist / max(eigen(W_dist, symmetric = TRUE)$values) # Scale
  kronecker(diag(years_numb), W_dist)
}

y <- cbind(logc = cigs$logc)
X <- model.matrix(logc ~ logp + logy + year + state, data = cigs)
X_lag <- X[, c("logp", "logy")]
colnames(X_lag) <- c("w_logp", "w_logy")

X_cont <- W_cont %*% X_lag

n_save <- 500L
n_burn <- 100L

out_slxdx <- bslx(y ~ X, W = Psi, X_SLX = X_lag,
                  n_save = n_save, n_burn = n_burn, options = set_options(
                    SLX = set_SLX(delta = 3, delta_scale = 0.05, delta_a = 2, delta_b = 2)))

summary(out_slxdx)

draws <- as_tibble(out_slxdx$draws) %>% 
    transmute(delta = delta_SLX)

delta_estimate <- mean(draws$delta)

delta_table <- tibble(
  Parameter = "Distance Decay (Delta)",
  Estimate = delta_estimate
)

print(delta_table)


delta_draws <- draws$delta
delta_mean <- mean(delta_draws)
delta_ci <- quantile(delta_draws, probs = c(0.025, 0.975))
delta_summary <- tibble(
  Parameter = "Distance Decay (Delta)",
  Estimate = delta_mean,
  `2.5%` = delta_ci[1],
  `97.5%` = delta_ci[2]
)

print(delta_summary)
