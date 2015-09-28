library(ggplot2)

soil <- read.csv(path.expand("Q:/Research/All_Projects_by_Species/Sclerocactus SPECIES/Sclerocactus_glaucus/BlackHillsDocuments/2015_Black Hills/SoilSamplesResults.csv"))
head(soil)
str(soil)
  
  # for all numeric, no factors
  library(vegan)
  set.seed(2)
  soil.nmds <- metaMDS(soil[,-1], k=2)
  
  # nonmetrix MDS
  # use isoMDS for non-numeric data
  library(MASS)
  dist(soil[,-1])
  
  library(cluster)
  
  # defaults to gower distance
  daisy.soildist <- daisy(soil[,-1])
  soil.mds <- isoMDS(daisy.soildist)

  ggplot(data.frame(soil.mds$points), aes(X1,X2)) +
    geom_point()

# FactoMineR: Multivariate Exploratory Data Analysis and Data Mining
install.packages("FactoMineR")
library(FactoMineR)

# Hierarchical clustering on Principle components
# agglomerative hierarchical clustering on results from a factor analysis. 
#     Means each observation is its own cluster and then pairs are merged
# Is this really a factor analysis?
soil[,1:2]

# MCA to tranform categorical variables into continuous ones
soil.mca <- MCA(soil, ncp = 10, quanti.sup = c(2:3,6:17), quali.sup = c(4:5,18), level.ventil = 2)

HCPC(soil[,-1])
HCPC(soil.mds)
