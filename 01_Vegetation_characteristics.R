# =========================================================
# Vegetation characteristics
# Journal of Vegetation Science
# =========================================================

# Load packages

library(readxl)
library(agricolae)
library(FSA)

# =========================================================
# Import data
# =========================================================

data <- read_excel("souhrnna data fyto.xlsx")

# Convert variables to factors

data$Year <- factor(data$Year)
data$Locality <- factor(data$Locality)

# =========================================================
# Number of species
# =========================================================

# Two-way ANOVA

model_species <- aov(`Number of species` ~ Year * Locality,
                     data = data)

summary(model_species)

# Assumption checking

shapiro.test(residuals(model_species))

par(mfrow = c(2,2))
plot(model_species)

# Fisher LSD test - 2021

mod2021_species <- aov(`Number of species` ~ Locality,
                       data = subset(data, Year == "2021"))

LSD.test(mod2021_species, "Locality")$groups

# Fisher LSD test - 2022

mod2022_species <- aov(`Number of species` ~ Locality,
                       data = subset(data, Year == "2022"))

LSD.test(mod2022_species, "Locality")$groups

# Summary statistics

aggregate(`Number of species` ~ Year + Locality,
          data = data,
          mean)

aggregate(`Number of species` ~ Year + Locality,
          data = data,
          sd)

# =========================================================
# Total vegetation cover (%)
# =========================================================

# Two-way ANOVA

model_cover <- aov(`Total vegetation cover (%)` ~ Year * Locality,
                   data = data)

summary(model_cover)

# Assumption checking

shapiro.test(residuals(model_cover))

par(mfrow = c(2,2))
plot(model_cover)

# Fisher LSD test - 2021

mod2021_cover <- aov(`Total vegetation cover (%)` ~ Locality,
                     data = subset(data, Year == "2021"))

LSD.test(mod2021_cover, "Locality")$groups

# Fisher LSD test - 2022

mod2022_cover <- aov(`Total vegetation cover (%)` ~ Locality,
                     data = subset(data, Year == "2022"))

LSD.test(mod2022_cover, "Locality")$groups

# Summary statistics

aggregate(`Total vegetation cover (%)` ~ Year + Locality,
          data = data,
          mean)

aggregate(`Total vegetation cover (%)` ~ Year + Locality,
          data = data,
          sd)

# =========================================================
# Fabaceae cover (%)
# =========================================================

# Log transformation

data$Fabaceae_log <- log(data$`Fabaceae cover (%)` + 1)

# Two-way ANOVA

model_fab <- aov(Fabaceae_log ~ Year * Locality,
                 data = data)

summary(model_fab)

# Assumption checking

shapiro.test(residuals(model_fab))

par(mfrow = c(2,2))
plot(model_fab)

# Fisher LSD test - 2021

mod2021_fab <- aov(Fabaceae_log ~ Locality,
                   data = subset(data, Year == "2021"))

LSD.test(mod2021_fab, "Locality")$groups

# Fisher LSD test - 2022

mod2022_fab <- aov(Fabaceae_log ~ Locality,
                   data = subset(data, Year == "2022"))

LSD.test(mod2022_fab, "Locality")$groups

# Summary statistics

aggregate(`Fabaceae cover (%)` ~ Year + Locality,
          data = data,
          mean)

aggregate(`Fabaceae cover (%)` ~ Year + Locality,
          data = data,
          sd)

# =========================================================
# Brassicaceae cover (%)
# =========================================================

# Non-parametric analysis
# Normality assumptions were not met

# Kruskal-Wallis test - 2021

kruskal.test(`Brassicaceae cover (%)` ~ Locality,
             data = subset(data, Year == "2021"))

# Kruskal-Wallis test - 2022

kruskal.test(`Brassicaceae cover (%)` ~ Locality,
             data = subset(data, Year == "2022"))

# Dunn post-hoc test with Bonferroni correction

dunnTest(`Brassicaceae cover (%)` ~ Locality,
         data = subset(data, Year == "2021"),
         method = "bonferroni")

dunnTest(`Brassicaceae cover (%)` ~ Locality,
         data = subset(data, Year == "2022"),
         method = "bonferroni")

# Summary statistics (Mean)

aggregate(`Brassicaceae cover (%)` ~ Year + Locality,
          data = data,
          mean)

# Summary statistics (SE)

aggregate(`Brassicaceae cover (%)` ~ Year + Locality,
          data = data,
          function(x) sd(x)/sqrt(length(x)))

# =========================================================
# Poaceae cover (%)
# =========================================================

# Log transformation

data$Poaceae_log <- log(data$`Poaceae cover (%)` + 1)

# Two-way ANOVA

model_poaceae <- aov(Poaceae_log ~ Year * Locality,
                     data = data)

summary(model_poaceae)

# Assumption checking

shapiro.test(residuals(model_poaceae))

par(mfrow = c(2,2))
plot(model_poaceae)

# Fisher LSD test - 2021

mod2021_poaceae <- aov(Poaceae_log ~ Locality,
                       data = subset(data, Year == "2021"))

LSD.test(mod2021_poaceae, "Locality")$groups

# Fisher LSD test - 2022

mod2022_poaceae <- aov(Poaceae_log ~ Locality,
                       data = subset(data, Year == "2022"))

LSD.test(mod2022_poaceae, "Locality")$groups

# Summary statistics

aggregate(`Poaceae cover (%)` ~ Year + Locality,
          data = data,
          mean)

aggregate(`Poaceae cover (%)` ~ Year + Locality,
          data = data,
          sd)

# =========================================================
# Session information
# =========================================================

sessionInfo()

capture.output(
  sessionInfo(),
  file = "sessionInfo.txt"
)

