#------------------------------------->
#- Spatial Economics
#-      Unit 3: Spatial Econometrics
#-
#- Original Authors (i.e. many thanks to):
#-  Mathias Moser
#-  Franziska Disslbacher
#-  Anna Stelzer
#-
#-  Adapted by:
#-  Lukas Vashold
#-    (lukas.vashold@wu.ac.at)
#------------------------------------->


# Preparations ------------------------------------------------------------

library("spdep") 
library("spData")
library("classInt")
library("spdep")    
library("RColorBrewer")
library("colorspace")
library("spatialreg")
library("dplyr")
library("ggplot2")



# Illustrating Bias of OLS under SAR DGP ----------------------------------


# Let's simulate some data to show the bias of OLS when the DGP is SAR. 
# Thus, we assume the spatial autoregressive parameter lambda is known. 
# We then estimate lambda for the simulated data samples using OLS.
# For our simulation experiment, we will assume that the true DGP is:
# 
#  y = lambda W y + e  
# 
# with lambda = 0.7. We simulate samples of size n = 484; and assume that the 
# error term of the SAR model is standard Normal distributed. 
# W is an n × n weight matrix. `S` is the number of simulations.

set.seed(123)
S <- 100                                        # number of simulations
n <- 484                                        # units of observations
lambda <- 0.7                                   # true lambda
w <- cell2nb(sqrt(n), sqrt(n))                  # create an artificial W matrix
iw <- invIrM(w, lambda)                         # Compute inverse of (I - lambda*W) 
lambda_hat <- vector(mode = "numeric",
                     length = S)

for (s in 1:S) {
  e <- rnorm(n, mean = 0 , sd = 1)              # Create error term under assumption of normal dist. 
  y <- iw %*% e                                 # True DGP
  Wy <- lag.listw(nb2listw(w), y)               # Create spatial lag 
  out <- lm(y ~ Wy)                             # Estimate OLS 
  lambda_hat[s] <- coef(out)["Wy"]              # Save results
}

summary(lambda_hat)
plot(density(lambda_hat),
     xlab = expression(hat(lambda)), main = "", xlim = c(0.65, 1.2))
abline(v = lambda, col = "red")

# Note that the distribution of lambda_hat does not contain the true value of 
# lambda, which we set to 0.7. Thus, the OLS estimate of the SAR model is biased. 


# The Columbus dataset ----------------------------------------------------

# We load in the columbus shapefile from the `spdep`package which holds data for
# 49 neighbourhoods in Columbus, Ohio, in 1980. The variables we use are: 
#  `crime` (residential burglaries and vehicle thefts per thousand households in the neighborhood); 
#  `hoval` housing value (in 1.000 USD); 
#  `inc` household income (in 1.000 USD). 
# Additionally, it includes a neighbourhoods list `col.gal.nb`. 
columbus.sf <- st_read(system.file("etc/shapes/columbus.shp",
                                            package = "spdep")[1])
col.gal.nb <- read.gal(system.file("etc/weights/columbus.gal",
                                   package = "spdep")[1])
lw <- nb2listw(col.gal.nb, style = "W")
plot(columbus.sf)

# Let's visualize `crime`
columbus.sf <- columbus.sf %>%
  mutate(CRIME.quant= cut(CRIME, 
                          breaks = c(quantile(columbus.sf$CRIME, probs = seq(0,1, by=0.2))), 
                          include.lowest = TRUE))

ggplot(data = columbus.sf) +
  geom_sf(aes(fill = CRIME.quant), colour = "black") +
  scale_fill_brewer(palette = "YlOrRd") +
  theme_classic() +
  labs(title = "CRIME RATE")

# Is there evidence for spatial dependence being present? Calculate and visualize Moran's I
moran.plot(columbus.sf$CRIME, 
           lw, 
           labels = as.character(columbus.sf$POLYID), 
           pch = 19)
moran.test(columbus.sf$CRIME, lw)

# Say we want now to estimate a simple OLS model without any spatial component.
# We estimate whether `crime` can be explained by `hoval` and `inc`.
data("columbus")

ols_full <- lm(CRIME ~ HOVAL + INC, data = columbus.sf)
summary(ols_full)

# Let's check the errors of the OLS model for spatial dependence
lm.morantest(ols_full, lw)

# As you can see the test is significant and thus we reject the H0 of uncorrelated errors.
# Let's plot the errors and the fitted values:
columbus.sf$u_ols <- ols_full$residuals
columbus.sf$y_hat <- ols_full$fitted.values

columbus.sf <- columbus.sf %>%
  mutate(u_ols.quant = cut(u_ols, 
                           breaks = c(quantile(columbus.sf$u_ols, probs = seq(0,1, by=0.2))), 
                           include.lowest = TRUE), 
         y_hat.quant = cut(y_hat, 
                           breaks = c(quantile(columbus.sf$y_hat, probs = seq(0,1, by=0.2))), 
                           include.lowest = TRUE))


ggplot(data = columbus.sf) +
  geom_sf(aes(fill = CRIME.quant), colour = "black") +
  scale_fill_brewer(palette = "YlOrRd") +
  theme_classic() +
  labs(title = "CRIME RATE")
ggplot(data = columbus.sf) +
  geom_sf(aes(fill = y_hat.quant), colour = "black") +
  scale_fill_brewer(palette = "YlOrRd") +
  theme_classic() +
  labs(title = "FITTED VALUES")
ggplot(data = columbus.sf) +
  geom_sf(aes(fill = u_ols.quant), colour = "black") +
  scale_fill_brewer(palette = "YlOrRd") +
  theme_classic() +
  labs(title = "RESIDUALS")
# We can see that there seems to be systematic overprediction (underprediction) 
# in outer (inner) neighborhoods


# Spatial Model Testing ---------------------------------------------------

# Theory should be your guide in choosing the appropriate model but there are 
# also statistical tests that can be used to assess the probable sources of 
# spatial dependence.


# The Lagrange Multiplier Test Statistics gives us five different statistics to 
# interpret:
#   
#   LMerr tells you if you should include a spatial lag in your error term.
#   LMlag tells you if you should include a spatial lag in your dependent variable.
#   RLMerr tells you if you should include a spatial lag in your error term given 
#          that there is already a spatial lag in your dependent variable.
#   RLMlag tells you if you should include a spatial lag in your dependent variable 
#          given that there is already a spatial lag in your error term.
#   SARMA combines RLMerr and RLMlag.  
# 
# You would always look at LMerr and LMlag first, then you go and check RLMerr and RLMlag. 
lm.LMtests(ols_full,  lw, test = "LMerr")
lm.LMtests(ols_full,  
           lw,
           test = c("all"))


# Estimating different spatial models -------------------------------------

# Let's start off with some some spatial models. First, let's go for a SLX
# specification. The SLX model can be estimated with OLS. Let's do it using lm.
# We can create spatial lags using the lag.listw() function.
columbus.sf$lag.HOVAL <- lag.listw(lw, columbus.sf$HOVAL)
columbus.sf$lag.INC   <- lag.listw(lw, columbus.sf$INC)

slx <- lm(CRIME ~ HOVAL + INC + lag.HOVAL + lag.INC,
          data = columbus.sf) #OLS
summary(slx)

# But there are also routines in the spatialreg package that do that for us
slx <- lmSLX(CRIME ~ HOVAL + INC, data=columbus.sf, listw=lw)
summary(slx)

# Let's check the impacts
summary(impacts(slx))

# Now let's do some SAR and SEM modelling. This requires estimation by other means
# than OLS. We will use ML here.

# SAR:
sar <- lagsarlm(CRIME ~ HOVAL + INC, 
                data = columbus.sf,
                listw = lw)
summary(sar)
# We can also calulcate impacts in the SAR model. Note: only point estimates. 
# More on the impacts in the SAR model later on.
impacts(sar, listw = lw)

# SEM:
sem <- errorsarlm(CRIME ~ INC + HOVAL, 
                  data = columbus.sf,
                  listw = lw)
summary(sem)

## Interpretation exercise for the SAR model:
# What would happen to `crime` in all region if the income in the 30th region rose by one unit?
lambda <- sar$rho
beta_hat <- coef(sar)[-1]

S_inv <- invIrW(lw, rho = lambda) # (I - rho*W)^{-1}
X <- cbind(1, columbus.sf$INC, columbus.sf$HOVAL)    # Matrix of observed variables 
y_hat_pre <- S_inv %*% crossprod(t(X), beta_hat)

col_new <- columbus.sf # copy the data frame for comparing later

# manually inserting the increase in the 30th neighborhood
columbus.sf$INC <- ifelse(columbus.sf$POLYID == 30, 
                          columbus.sf$INC + 1,
                          columbus.sf$INC)

X_d <- cbind(1, columbus.sf$INC, columbus.sf$HOVAL)
y_hat_post <- S_inv %*% crossprod(t(X_d), beta_hat) # calculate the new predicted value for y

delta_y <- y_hat_post - y_hat_pre # calculate the difference
columbus.sf$delta_y <- delta_y

summary(delta_y)
sum(delta_y)

# Let's calculate breaks for the impact it has and split it into two groups, 
# high and low impact regions, and plot it
breaks <- c(-Inf, median(delta_y), Inf) 
labels <- c("High Impact Regions", "Low Impact Regions") 
np <- findInterval(columbus.sf$delta_y, breaks)
colors <- c("red", "blue")
plot(columbus.sf["delta_y"], col = colors[np])
legend("topleft", legend = labels, fill = colors, bty = "n") 
points(33.5, 22.5, pch = 19, col = "black", cex = 2)

# or just do it in intervals
reg.30 <- data.frame(long = 8.7, lat = 12)
columbus.sf <- columbus.sf %>%
  mutate(delta_y.quant = cut(delta_y, 
                           breaks = c(quantile(columbus.sf$delta_y, probs = seq(0,1, by=0.2))), 
                           include.lowest = TRUE))
ggplot(data = columbus.sf) +
  geom_sf(aes(fill = delta_y.quant), colour = "black") +
  geom_point(data = reg.30, aes(long, lat), colour = "black", size = 2) +
  scale_fill_brewer(palette = "YlOrRd") +
  theme_classic() +
  labs(title = "CRIME RATE CHANGE") 


# Delving into impacts ----------------------------------------------------

# We now look at impacts in the SAR model in a bit more detail. Above we only
# looked at the point estimates of the various impacts but could not really do
# inference on them. So now let's go in a bit more detail

# Same as above, get point estimates of different effects
sar.impacts <- impacts(sar, listw = lw)
sar.impacts

# We generate 1000 simulations to do inference on the impacts by setting R=1000
# and can summarize the output to show simulated SEs, z-values and p-values
sar.impacts.iter <- impacts(sar, listw = lw, R = 1000)
summary(sar.impacts.iter, zstats = TRUE, short = TRUE)

# For large matrices, where the inversion of the Neumann series becomes expensive,
# we can use some tricks using sparse representations of W and generate approximations
# using powers of the Neumann series. For the dummy example here, this is not really 
# necessary, for larger N this can be neat.
w_sparse <- as(as_dgRMatrix_listw(lw), "CsparseMatrix")
trMat <- trW(w_sparse, type = "mult") # can also use type = "MC" for Monte Carlo approximation
sar.impactsMat <- impacts(sar, tr = trMat, R = 1000)
summary(sar.impactsMat, zstats = TRUE, short = TRUE)

# We can also construct confidence intervals given our simulations
# Package `coda` comes in handy here to summarise
# install.packages("coda")
library(coda)
HPDinterval(sar.impactsMat, choice = "direct")
HPDinterval(sar.impactsMat, choice = "indirect")
HPDinterval(sar.impactsMat, choice = "total")


# Let's do the calculation of impacts by hand to see where they come from
lambda <- as.numeric(coefficients(sar)["rho"])
beta.hat <- coefficients(sar)[-1]
N <- nrow(columbus)
I <- diag(N)
ones <- as.matrix(rep(1, N))
t_ones <- t(ones)
S_inv <- solve(I - lambda * as.matrix(w_sparse)) # inverse of spatial filter


# Using the inversion, we can build the matrix of partial derivatives dy/dx
# Using the example of the estimated parameter for income estimate, we scale its 
# marginal effect beta by the spatial structure given by (I-lambda W)⁻¹ (S_inv)
S_W <-  S_inv %*% (I * beta.hat["INC"])

# The direct effect is the sum of the diagonal scaled by `N`
S_W_income_direct <- sum(diag(S_W)) / N
S_W_income_direct

# Total effects are the sum of all derivatives (`sum(S_W)`) or in matrix notation:  
S_W_income_tot <- (t_ones %*% (S_W) %*% ones) / N
S_W_income_tot

# The indirect effect is their difference (sum of all off-diagonal elements).  
S_W_income_indirect <- S_W_income_tot - S_W_income_direct
S_W_income_indirect



# HAC-consistent models ---------------------------------------------------

# We will use the baltimore dataset from the spData package for the next exercises
data(baltimore, package="spData")

# If you check `class(baltimore)` you will see that this is _just_ a `data.frame` 
# with coordinate columns `X` and `Y`. Since the `class()` is not a spatial object, 
# R does not know that these columns are spatial information. Converting `baltimore` 
# to an sf data.frame using `st_as_sf()` with the `coords=` option tells R where 
# the spatial information lives.
baltimore <- st_as_sf(baltimore, coords = c("X","Y"))
baltimore$AGE <- ifelse(baltimore$AGE < 1, 1, baltimore$AGE)

lw.nb <- knn2nb(knearneigh(baltimore, k=7))
lw <- nb2listw(lw.nb, style = "W")

# In standard econometrics you have already learned about heteroskedasticity and 
# autocorrelation consistent (HAC) estimation, typically in the form of the 
# Newey-West correction. In spatial applications we can do a similar approach 
# using the variance-covariance (VC) for a vector of sample moments within a spatial context.
# The sphet package provides routines for HAC-consistent estimation of spatial models

# install.packages("sphet")
library(sphet)

# The major function to do the HAC estimation is `stslshac()` 
# (**S**patial **T**wo-**S**tage **L**east **S**quares **H**eteroskedasticity
# and **A**utocorrelation **C**onsistent). 
# In comparison to the estimation procedures we used so far, it is a two stage 
# least squares IV approach that uses spatial lags of the covariates as instruments

# The one thing we need in addition to standard spatial estimation procedures is 
# a measure of distance between the observation: This tells us how, for example, 
# the autocorrelation _levels off_ over space. You can imagine the distance to 
# be a 3-d bell-shaped curve, that sits on top of each observation and tells us
# how strong the autocorrelation relationship is to others

# The argument `distance=` that specifies the distance measure, is also an object 
# of class `distance`. This is a very generic class created for example by 
# functions like `dist()`. In the `sphet` context we need a special format, that 
# is created by `read.gwt2dist()`, which reads in something called a GeoDa GWT file. 
# So the process is two-steps:
#   1. Create the _.gwt_ file from our data
#   2. Read it back in using `read.gwt2dist()`
d <- distance(coord = st_coordinates(baltimore), region.id = baltimore$STATION, 
              output = TRUE, type = "distance", 
              shape.name = "shapefile", region.id.name = "id", firstline = TRUE,
              file.name = "baltimore.gwt")
coldist <- read.gwt2dist(file = "baltimore.gwt",  region.id = baltimore$STATION, skip = 1)

# We also have to decide on the spatial transmission process of autocorrelation.
# This is done by defining a certain Kernel-function that specifies the shape 
# of the bell with which the autocorrelation dissipates. There are several choices
# available for this to be set with the `type` argument of the `stslshac`. We choose 
# the Epanechnikov Kernel, which has a very broad bell with exponentially decreasing 
# weights. For an illustration of other Kernels see: 
#   https://upload.wikimedia.org/wikipedia/commons/4/47/Kernels.svg

# Then plug in the `coldist` as distance object, and set `HAC=TRUE`:
  
# We explain the price of an apartment with its age, its square footage and a dummy 
# for whether it has a patio or not
fm1 <- log(PRICE) ~ PATIO + log(AGE) + log(SQFT)
sar.s2sls.hac <- stslshac(fm1, data = baltimore, listw = lw, distance = coldist, 
                          HAC=TRUE, type="Epanechnikov")
summary(sar.s2sls.hac)


# Non-linear spatial models -----------------------------------------------

# Here, we look at some of the available routines for the estimation of non-linear
# models, in particular for binary outcome models. Note that the estimation of 
# such models with a spatial structure in a classical (i.e. frequentist) framework
# is not straighforward. Bayesian methods can be a remedy but R packages are less 
# developed and scripts are often written ad-hoc for individual applications. 
# Additionally, the computation of the log-determinant, which happens at every 
# iteration of MCMC sampler, can be a substantial burden and model estimation 
# can be tedious. 

# For showcasing some of the routines, we again use the baltimore dataset and 
# set up a simple model of whether houses have air conditioning (`AC`) depending 
# on their `log(PRICE)` and `AGE` and the spatial lag of the dependent (i.e. a SAR model)
fm2 <- AC ~ log(PRICE) + log(AGE)

# We use the package `ProbitSpatial` for ML (or similar) estimation of the model
library(ProbitSpatial)
lw_sparse <- as(as_dgRMatrix_listw(lw), "CsparseMatrix")
probit.ml <- ProbitSpatialFit(fm2, data = baltimore, W = lw_sparse, DGP = "SAR", 
                              method = "full-lik", varcov = "varcov")
summary(probit.ml)

# We use the package `spldv` for GMM estimation of the model
library(spldv)
probit.gmm <- sbinaryGMM(fm2, data = baltimore, listw = lw)
summary(probit.gmm)

# We use the package `spatialprobit` for Bayesian estimation of the model
library(spatialprobit)
mcmc_fit <- sarprobit(fm2, data = baltimore, W = lw_sparse)
summary(mcmc_fit)
# plot(mcmc_fit) # convergence and posterior plots



# MESS --------------------------------------------------------------------

# One way to get around the computation of the log-determinant in the ML estimation
# of e.g. a SAR model, is to use MESS, which is a different way to specify the 
# spatial process.
# We will execute a MESS model and compare it to the standard way through `lagsarlm()` 
# to check for differences. Note, that we do not expect the parameters to be equal 
# since the estimators are specified differently.


# One way to estimate is via truncated approximation of traces of powers for tr(αW). 
# Note how the output specifies `Implied rho:` because before interpreting the 
# spatial parameter we need to undo the MESS-operation on α:
mess1 <- lagmess(fm1, data=baltimore, listw=lw)
summary(mess1)

# The issue with this approach is, that when the spatial effects are very persistent 
# (higher orders of the trace are still large in size), then truncating the powers
# of the trace will cause bias. There is however a clever trick, which allows one 
# to calculate the matrix exponential directly (check `help(expAtv)` for more details). 
# To use the trick, we set `use_expm=TRUE`. The approximation via traces is much faster 
# than the `expm`-trick, but has the potential bias for higher order traces.
mess2 <- lagmess(fm1, data=baltimore, listw=lw, use_expm=TRUE)
summary(mess2)

# Let's compare with the default spatial lag model: We find that most of the parameters 
# are indeed very similar. The (implied) lambda does however differ a bit - not a 
# big surprise, since we modeled slightly different spatial process with MESS and SAR
sar_comp <- lagsarlm(fm1, data=baltimore, listw=lw)
summary(sar_comp)

1 - exp(mess1$alpha)
sar_comp$rho


# Spatial Filtering -------------------------------------------------------

# The basic idea behind spatial filtering is that you remove spatial dependence 
# from the data before estimating the model. Removing the spatial structure 
# means, we do not learn a lot of it in our model. However, there might be some
# reasons for removing spatial dependence a priori, one is that you want to 
# estimate a model that has no notion of space or where a spatial formulation is 
# hard to estimate (e.g. multinomial logit).

# In R, we can use the package `spmoran` for spatial filtering.
# install.packages("spmoran")
# Installation of version 2.3 of `spmoran` failed repeatedly, resort to v2.2
# spmoran_old <- "https://cran.r-project.org/src/contrib/Archive/spmoran/spmoran_0.2.2.tar.gz"
# install.packages(spmoran_old)
# or download archive file from here: https://cran.r-project.org/src/contrib/Archive/spmoran/
library(spmoran)

# We again use the `baltimore` example, since we are already familiar with it.
# First, we create vectors for the `X` (RHS) and `y` (LHS) variables and save the 
# coordinates. We use `st_set_geometry(NULL)` to remove the spatial information 
# from the variables (we still have the coordinates). 
X <- baltimore[, c("PATIO", "AGE", "SQFT")] %>% st_set_geometry(NULL) %>% as.matrix()
y <- baltimore %>% st_set_geometry(NULL) %>% .$PRICE
coords <- st_coordinates(baltimore)

# The most common filtering operation is by using distances. To do this we use 
# the `esf()` function which does the filtering for us. To do that it needs a 
# notion of what is spatial variation in the data, and this provided by Moran 
# eigenvectors through `meigen()` (see slides for a visual interpretation). 
# The default specification of the connectivity matrix here is exponential decay
# but one can also pass user-specified ones using the `cmat=` argument. Note that
# it requires symmetric matrices, otherwise it transform them to be symmetric.
lw.mat <- nb2mat(lw.nb)
meig 	<- meigen(cmat = lw.mat)
meig <- meigen(coords = coords)

# The variance inflation factor cut-off (`vif=`) is used to decide how many 
# eigenvectors we use for the filtering. The resulting estimation object `esfD` 
# then holds our parameters `b`, and the parameters accompanying the eigenvectors 
# stored in `r`. 
esfD	<- esf(y = y, x = X, meig = meig, vif = 5)
esfD$b
esfD$r

# `meigen()` is still computationally expensive as would be for ML estimation of 
# the model. An alternative is to use approximate eigenvalues using `meigen_f()`.
# Currently it works only with distance decay specification that is the default.
# Here we also set `fn="all"` to use all eigenvectors we have instead of selecting 
# the most important ones (Note: This makes sense in large samples):
meig_f <- meigen_f(coords = coords)
esfD	<- esf(y = y, x = X, meig = meig_f, fn = "all")  
esfD$b
esfD$r


# Spatial panels ----------------------------------------------------------

# As example for working with spatial panels, we will use data from the `Ecdat`
# package that holds data on US production of different goods and other economic
# data from 1970 to 1986. Note that the data now has a cross-sectional (`state`) 
# and a time (`year`) dimension.
library(Ecdat)
data("Produc", package="Ecdat")
head(Produc)

# When working with spatial panels, the package `splm` comes in handy, providing 
# routines for their estimation and interpretation. We will also use the spatial
# weights matrix `usaww` for the 48 continental US states based on queen contiguity.
library(splm)
data("usaww")
usalw <- mat2listw(usaww, style="W")

# `splm` provides multiple useful functions, like:    
  
#  `bsjktest`        - Baltagi et al. test for spatial error correlation      
#  `bsktest`         - Baltagi et al. test for spat.err. or random effects  
#  `sphtest`         - Hausmann test for spatial models
#  `rwtest`          - Pesaran test for spatial dependence
#  `slmtest`         - Anselin test for nested spatial dependence (error in lag or vice versa)

# Also other useful little helpers are available:  
  
#  `slag`            - Calculates spatial lag      
#  `vcov.splm`       - Returns variance-covariance matrix
#  `effects.splm`    - Extracts fixed effects

# Functions for estimations would be:  
  
#  `spml`            - ML estimation of spatial panels
#  `spgm`            - GMM estimation of spatial panels
#  `spreml`          - ML estimation w/ serially correlated errors  


# Notation:
# λ : Spatial autoregressive parameter
# σ² ("phi") : Random effects
# ρ : Spatial error component
# ψ (psi) : Serial correlation


# As a basic introduction, let's set up a base model. We will estimate a simple 
# production function, linking gross social product (`gsp`) to  private capital 
# stock (`pcap`), public capital (`pc`), employment (`emp`), and unemployment 
# rate (`unemp`).
fm3 <- log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp

# The weights matrix is still `48x48`, while our data has 816 observations (`n x T`)! 
# We assume the neighborhood structure to be constant over time, thus `usalw` has 
# no notion of time, it defines the cross-sectional relation of observations only. 
# The `splm` package fits this one-time weights matrix to our multi-period data by 
# using Kronecker multiplication (see slides from lecture).
dim(usaww)

## Testing in spatial panels

# Similar to non-panel models, we can run through some testing steps to determine
# the source of the spatial dependence (if there is one). Note however, again, 
# that the selection of the model should rather be guided by theory (what effects
# are you interested in, where do they emanate, ...?)


# `slmtest()` contains Anselin's tests for spatial dependence, the panel equivalent 
# of the `LMtest()` from earlier on. Note that without any prior modification, 
# `slmtest()` assumes that the data can be pooled, i.e. it fixes only the dimension
# of the W model to fit to the panel data. If you however suspect that there are 
# significant individual effects in your panel then you need to get rid of them 
# before, i.e., using a manual `within()` transformation.

# Spatial lag structure?
slmlag <- slmtest(fm3, data=Produc, listw=usalw, test="lml")
slmlag
# Spatial error structure?
slmerr <- slmtest(fm3, data=Produc, listw=usalw, test="lme")
slmerr

# Robust versions:
# Spatial lag allowing for spatial error?
slmle <- slmtest(fm3, data=Produc, listw=usalw, test="rlml")
slmle
# Spatial error allowing for spatial lag?
slmel <- slmtest(fm3, data=Produc, listw=usalw, test="rlme")
slmel


# In panel models, we usually include some interactive effects, in the form of 
# fixed or random effects. Akin to the Hausmann test in standard panels, we can
# use the spatial version of Mutl/Paffermayr, provided by the `sphtest()` function.
# Note that the alternative hypothesis is that _one model is inconsistent_ (the 
# RE model), so if this is the case, we should use the FE estimator.

# For error specification
spherr <- sphtest(x=fm3, data=Produc, listw=usalw, 
                  spatial.model="error", method="ML")
spherr

# For lag specification
sphlag <- sphtest(x=fm3, data=Produc, listw=usalw, 
                  spatial.model="lag", method="ML")
sphlag

# For both lag and error specification
spherrlag <- sphtest(x=fm3, data=Produc, listw=usalw, 
                     spatial.model="sarar", method="ML")
spherrlag

# Digging into the error model, we can use some tests to determine, whether 
# there is spatial correlation or some form of error heterogeneity due to the 
# random region effects (or both)

# "LMH" H0:  ρ=σ²=0? (RE and spatial correlation non-zero?)
lmjoint <- bsktest(x=fm3, data=Produc, listw=usalw, test="LMH")
lmjoint

# "LM1" H0:  σ²=0? assuming ρ=0 (Are there ind effects, assuming no spatial corr.?)
lm1 <- bsktest(x=fm3, data=Produc, listw=usalw, test="LM1")
lm1

# "LM2" H0:  ρ=0? assuming σ²=0 (Is there spatial corr., assuming no ind effects?)
lm2 <- bsktest(x=fm3, data=Produc, listw=usalw, test="LM2")
lm2

# "CLMlambda" H0: ρ=0? (Is there spatial correlation given the possibility of RE?)
clml <- bsktest(x=fm3, data=Produc, listw=usalw, test="CLMlambda")
clml

# "CLMmu" H0:  σ²=0? (Are there RE, given the possibility of spatial correlation?)
clmm <- bsktest(x=fm3, data=Produc, listw=usalw, test="CLMmu")
clmm

# In addition, we can check for serial correlation using `bsjktest()`, a refined
# version o `bsktest()`. It tests in addition for correlation along the time 
# dimension, which is often disregarded in spatial panel setups.

# "J" H0: ρ=σ²=ψ=0? (RE, spatial or serial correlation non-zero?) 
bsjktest(x=fm3, data=Produc, listw=usalw, test="J")

# "C.1" H0: ρ=0? assuming ψ≠0 (serial correlation) and σ²>0 (RE) 
bsjktest(x=fm3, data=Produc, listw=usalw, test="C.1")

# "C.2" H0: ψ=0? assuming ρ≠0 (spatial correlation) and σ²>0
bsjktest(x=fm3, data=Produc, listw=usalw, test="C.2")

# "C.3" H0: σ²=0? assuming ρ≠0 and ψ≠0
bsjktest(x=fm3, data=Produc, listw=usalw, test="C.3")


## Estimating spatial panel models

# Full model including random effects (`model=`), lagged dependent (`lag=TRUE`) 
# and spatial error (`spatial.error=`):  
sararre <- spml(fm3, Produc, listw = usalw, model = "random", lag = TRUE, 
                spatial.error = "b") # Baltagi errors
summary(sararre)

# But, following the Hausmann test, we opt for a fixed effects specification
sararfemod <- spml(fm3, Produc, listw=usalw, lag=TRUE, spatial.error="b", 
                   model="within", effect="individual") # unit foxed effects
summary(sararfemod)

# Alternatively we can specify time FE
sarartfemod <- spml(fm3, Produc, listw=usalw, lag=TRUE, spatial.error = "b", 
                 model="within", effect="time") 
summary(sarartfemod)

# or both
sararbfemod <- spml(fm3, Produc, listw=usalw, lag=TRUE, spatial.error = "b", 
                    model="within", effect="twoways") 
summary(sararbfemod)


## Checking impacts 

# For newer versions of splm, there is `impacts()` for panel models. 
# Make sure that your weights (`usalw`) are row-standardized (`style="W"`). 
imp <- splm::impacts(sararbfemod, listw=usalw, time=1986)
summary(imp, zstats=TRUE, short=TRUE)

# `effects()` extracts the individual FE, which are not visible in the `summary()`  
eff <- effects(sararbfemod)
eff

## Resorting to GMM when ML struggles
GM_sararfemod <- spgm(fm3, Produc, listw=usaww, model="within", lag=TRUE, 
                      spatial.error=FALSE)
summary(GM_sararfemod)
imp.gm <- splm::impacts(GM_sararfemod, listw=usalw, time=1986)
imp.gm
