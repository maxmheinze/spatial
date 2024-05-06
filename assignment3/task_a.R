
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

#install.packages("bsreg")
library(bsreg)

years_numb <- length(unique(cigs$year)) 
# calculates the number of unique years in the dataset.


W_cont <- kronecker(diag(years_numb), cigw / rowSums(cigw)) 
# Creates a spatial contiguity matrix by applying the Kronecker product to the 
# identity matrix of years (diag(years_numb)) and normalizing the weights 


# Inverse-distance decay function
dist <- as.matrix(dist(cigd))
diag(dist) <- Inf 
# Computes the pairwise distances between state centroids using the Euclidean 
# distance. The diagonal is set to infinity (diagonal elements will be 0)
# to prevent self-connections.

Psi <- function(delta) {
  W_dist <- dist ^ (-delta) # Build
  W_dist <- W_dist / max(eigen(W_dist, symmetric = TRUE)$values) # Scale
  kronecker(diag(years_numb), W_dist)
}
# A function to build the inverse-distance decay matrix (W_dist). It takes delta as a parameter and:
#  Calculates inverse distances (dist ^ -delta).
#  Scales the matrix by the maximum eigenvalue to standardize it.
#  Applies the Kronecker product with the years' identity matrix to expand for the panel data.


# Prepare variables
y <- cbind(logc = cigs$logc)
# creates a response variable matrix with logc (log of cigarette consumption)

X <- model.matrix(logc ~ logp + logy + year + state, data = cigs)
# Constructs the model matrix using the model.matrix function. 
# It includes logged cigarette prices (logp), logged income (logy), and fixed effects for year and state. (as stated in the task assignment)
X_lag <- X[, c("logp", "logy")]
colnames(X_lag) <- c("w_logp", "w_logy")
# Extracts the relevant columns for spatial lags (prices and income), renaming them as w_logp and w_logy.

X_cont <- W_cont %*% X_lag
# Calculates the spatially lagged explanatory variables by multiplying W_cont with X_lag


n_save <- 50L
n_burn <- 10L
# Set the number of posterior draws to save and discard, respectively.
# set it to 50000L and 10000L in the end

out_slxdx <- bslx(y ~ X, W = Psi, X_SLX = X_lag,
                  n_save = n_save, n_burn = n_burn, options = set_options(
                    SLX = set_SLX(delta = 3, delta_scale = 0.05, delta_a = 2, delta_b = 2)))
# out_slxdx: Fits the SLX model using the bslx function:
#  y ~ X: Defines the model formula.
#  W = Psi: Provides the function for the distance decay weights matrix.
#  X_SLX = X_lag: Specifies the explanatory variables for spatial lags.
#  n_save & n_burn: Sets the number of posterior draws.
#  options = set_options(...): Provides settings for the SLX model, including the prior parameters for the decay parameter (delta).
# delta = 3 specifies the starting value for the decay parameter in the MCMC sampler. The sampler then iteratively updates this value based on the posterior distribution obtained through sampling.
# The prior distribution for delta is defined by delta_a and delta_b. Together, they determine the shape of the inverse-gamma prior distribution. The prior distribution influences the final estimate of delta by reflecting your beliefs about its likely values before seeing the data.

summary(out_slxdx)


d_te <- rbind(
  as_tibble(out_slxdx$draws) %>% transmute(model = "SLX(delta)", delta = delta_SLX)
)

mean(d_te$delta)
