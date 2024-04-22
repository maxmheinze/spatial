
pacman::p_load(
  tidyverse,
  plm,
  readxl,
  spdep
)

cigs <- read_excel("./assignment3/data/cigarettes/cigarette+2var.xls")

cm1 <- plm(logc ~ logp + logy, data = cigs, effect = "twoways", model = "within")

summary(cm1)

cigw <- read_excel("./assignment3/data/cigarettes/Spat-Sym-US.xls", col_names = FALSE) %>%
  as.matrix()

cign <- c("AL", "AZ", "AR", "CA", "CT", "DE", "DC", "FL", "GA", "ID", "IL", "IN", 
            "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", 
            "NE", "NV", "NH", "NJ", "NM", "NY", "ND", "OH", "OK", "PA", "RI", "SC", 
            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

dimnames(cigw) <- list(cign, cign)
rownames(cigw) <- cign



