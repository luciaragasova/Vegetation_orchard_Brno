# =========================================================

# Soil microbial communities

# Journal of Vegetation Science

# =========================================================

# Load packages

library(readxl)
library(vegan)

# =========================================================

# Import data

# =========================================================

RDA <- read_excel("RDA.xlsx")

# Convert variables to factors

RDA$management <- factor(RDA$management)
RDA$position <- factor(RDA$position)
RDA$year <- factor(RDA$year)

# =========================================================

# Soil biological variables

# =========================================================

# Response matrix

microbial <- RDA[, c(
  "Microbial biomass (mg PLFA/kg)",
  "Total bacteria (mg PLFA/kg)",
  "Gram positive bacteria (mg PLFA/kg)",
  "Gram positive bacteria-Actinomycetes (mg PLFA/kg)",
  "Gram negative bacteria (mg PLFA/kg)",
  "Total fungi (mg PLFA/kg)",
  "Arbuscular mycorrhiza (mg PLFA/kg)",
  "Other fungi (mg PLFA/kg)",
  "Protozoa (mg PLFA/kg)"
)]

# Standardization

microbial_std <- scale(microbial)

# Check total variance

sum(apply(microbial_std, 2, var))

# =========================================================

# Environmental variables

# =========================================================

env <- RDA[, c(
  "management",
  "position",
  "year"
)]

# =========================================================

# Redundancy Analysis (RDA)

# Year included as conditioning variable

# =========================================================

rda_model <- rda(
  microbial_std ~ management + position +
    Condition(year),
  data = env
)

# =========================================================

# RDA summary

# =========================================================

summary(rda_model)

# Eigenvalues

eigenvals(rda_model)

# =========================================================

# Permutation tests (999 permutations)

# =========================================================

# Overall model

anova(
  rda_model,
  permutations = 999
)

# Explanatory variables

anova(
  rda_model,
  by = "terms",
  permutations = 999
)

# Canonical axes

anova(
  rda_model,
  by = "axis",
  permutations = 999
)

# =========================================================

# Ordination plot

# =========================================================

plot(rda_model)

# Note:
# The published Figure 8 was generated using a customized
# graphical layout based on RDA scores.
# The plot above represents the default vegan ordination output.

# =========================================================

# Session information

# =========================================================

sessionInfo()

capture.output(
  sessionInfo(),
  file = "sessionInfo_RDA.txt"
)
