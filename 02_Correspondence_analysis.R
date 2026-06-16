# =========================================================
# Categorical vegetation variables
# Journal of Vegetation Science
#
# Analyses:
# - Pearson chi-square tests
# - Monte Carlo simulated chi-square tests
# - Correspondence Analysis (CA)
# - Eigenvalues and explained inertia
# - cos² values
# =========================================================

# Load packages

library(readxl)
library(FactoMineR)
library(factoextra)

# =========================================================
# 2.1 CSR Plant Strategies
# =========================================================

# Import data

csr_data <- read_excel(
  "Correspondence analysis.xlsx",
  sheet = "CSR Plant Strategies"
)

# Prepare contingency matrix

csr <- as.data.frame(csr_data)

rownames(csr) <- csr$strategy

csr <- csr[, -1]

# =========================================================
# Pearson Chi-square test
# Monte Carlo simulation (10000 replicates)
# =========================================================

chisq.test(
  as.matrix(csr),
  simulate.p.value = TRUE,
  B = 10000
)

# =========================================================
# Correspondence Analysis (CA)
# =========================================================

ca_csr <- CA(
  csr,
  graph = FALSE
)

# Eigenvalues and explained inertia

ca_csr$eig

# =========================================================
# Quality of representation (cos²)
# =========================================================

ca_csr$row$cos2

ca_csr$col$cos2

# =========================================================
# CA biplot
# =========================================================

fviz_ca_biplot(
  ca_csr,
  repel = TRUE
)

# =========================================================
# 2.2 Weed Categories
# =========================================================

# Import data

weed_data <- read_excel(
  "Correspondence analysis.xlsx",
  sheet = "Weed Categories"
)

# Prepare contingency matrix

weed <- as.data.frame(weed_data)

rownames(weed) <- weed$`weed category`

weed <- weed[, -1]

# =========================================================
# Pearson Chi-square test
# Monte Carlo simulation (10000 replicates)
# =========================================================

chisq.test(
  as.matrix(weed),
  simulate.p.value = TRUE,
  B = 10000
)

# =========================================================
# Correspondence Analysis (CA)
# =========================================================

ca_weed <- CA(
  weed,
  graph = FALSE
)

# Eigenvalues and explained inertia

ca_weed$eig

# =========================================================
# Quality of representation (cos²)
# =========================================================

ca_weed$row$cos2

ca_weed$col$cos2

# =========================================================
# CA biplot
# =========================================================

fviz_ca_biplot(
  ca_weed,
  repel = TRUE
)

# =========================================================
# 2.3 Ecosystem Service-Related Functional Groups
# =========================================================

# Import data

ecosystem_data <- read_excel(
  "Correspondence analysis.xlsx",
  sheet = "Ecosystem Service-Related Funct"
)

# Prepare contingency matrix

ecosystem <- as.data.frame(ecosystem_data)

rownames(ecosystem) <- ecosystem$`ecosystem function`

ecosystem <- ecosystem[, -1]

# Pearson Chi-square test
# Monte Carlo simulation (10000 replicates)

chisq.test(
  as.matrix(ecosystem),
  simulate.p.value = TRUE,
  B = 10000
)

# Correspondence Analysis (CA)

ca_ecosystem <- CA(
  ecosystem,
  graph = FALSE
)

# Eigenvalues and explained inertia

ca_ecosystem$eig

# Quality of representation (cos²)

ca_ecosystem$row$cos2

ca_ecosystem$col$cos2

# CA biplot

fviz_ca_biplot(
  ca_ecosystem,
  repel = TRUE
)

# =========================================================
# Session information
# =========================================================

sessionInfo()

capture.output(
  sessionInfo(),
  file = "sessionInfo_CategoricalVegetation.txt"
)

