# libraries
library(nFactors)
library(psych)
library(FactoMineR)

# load datafiles 
set14_min <- readRDS("/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_nationals/set14_min.rds")

# LEVEL 2

# Subsetting only on the variable I intend to use in this section:
set14_min <- set14_min[,-c(1:2,8:12,14:21)]

## Determine Number of Factors to Extract
ev <- eigen(cor(set14_min[,7:ncol(set14_min)]))
ap <- parallel(subject=nrow(set14_min[,7:ncol(set14_min)]),var=ncol(set14_min[,7:ncol(set14_min)]),
               rep=100,cent=.02)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
jpeg("nScree_14_min")
plotnScree(nS, main = "National") # optimal = 5
dev.off()

# # will set them at six for both Jhb and CT for now
# npc <- 6
# 
# # creating objects with supplementary variables (qualitative and quantitative) and active one defined:
# set.seed(56)
# pca_14_min <- PCA(set14_min,
#                   quanti.sup = c(1,3,4,6),
#                   quali.sup = c(2,5),
#                   ncp = npc,
#                   graph = FALSE)
# saveRDS(pca_14_min, "pca_14_min.rds")

# pa method of factor analysis with oblimin rotation allowed....to try and get better estimation
set.seed(123)
fact_14 <- fa(set14_min[7:ncol(set14_min)], nfactors = 7, fm = "ml") # default rotation oblimin, so does allow correlation between factors
fact_14_loadings <- fact_14$loadings
fact_14_scores <- fact_14$scores

# save model
saveRDS(fact_14, "fact_14.rds")

# save loadings:
saveRDS(fact_14_loadings, "fact_14_loadings.rds")

# save scores:
saveRDS(fact_14_scores, "fact_14_scores.rds")

write.csv(round(loadings(fact_14, sort = TRUE), 2), file = "loadings_min_14.csv")
