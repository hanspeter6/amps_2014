# libraries
library(stringr)
library(tidyverse)
library(corrplot)
library(rpart)
library(rpart.plot)
library(scatterplot3d)
library(rgl)
library(kohonen)
library(caret)
library(randomForest)
library(MASS)
library(CCA)
library(nFactors)
library(FactoMineR)
library(factoextra)
library(gridExtra)
# 

# read datafiles
magazines_engagement_14 <- readRDS("magazines_engagement_14.rds")
magazines_engagement_14_simple <- readRDS("magazines_engagement_14_simple.rds")
newspapers_engagement_14 <- readRDS("newspapers_engagement_14.rds")
newspapers_engagement_14_simple <- readRDS("newspapers_engagement_14_simple.rds")
radio_engagement_14 <- readRDS("radio_engagement_14.rds")
tv_engagement_14 <- readRDS("tv_engagement_14.rds")
internet_engagement_14 <- readRDS("internet_engagement_14.rds")
internet_engagement_14_simple <- readRDS("internet_engagement_14_simple.rds")

media_type_14 <- readRDS("media_type_14.rds")
media_vehicles_14 <- readRDS("media_vehicles_14.rds")
media_type_14_simple <- readRDS("media_type_14_simple.rds")
media_vehicles_14_simple <- readRDS("media_vehicles_14_simple.rds")

demographics_14 <- readRDS("demographics_14.rds")

#reducing levels of categorical variables and setting factor types for demographics:

# age:
demographics_14$age <- ifelse(demographics_14$age %in% c(1,2), 1, demographics_14$age)
demographics_14$age <- ifelse(demographics_14$age %in% c(3,4), 2, demographics_14$age)
demographics_14$age <- ifelse(demographics_14$age %in% c(5,6), 3, demographics_14$age)
demographics_14$age <- ifelse(demographics_14$age %in% c(7,8), 4, demographics_14$age)
demographics_14$age <- factor(demographics_14$age, ordered = TRUE)

# sex:
demographics_14$sex <- factor(demographics_14$sex, ordered = FALSE)

#edu:
demographics_14$edu <- ifelse(demographics_14$edu %in% c(1,2,3,4), 1, demographics_14$edu)
demographics_14$edu <- ifelse(demographics_14$edu %in% c(5), 2, demographics_14$edu)
demographics_14$edu <- ifelse(demographics_14$edu %in% c(6,7,8), 3, demographics_14$edu)
demographics_14$edu <- factor(demographics_14$edu, ordered = TRUE)

#hh_inc
demographics_14$hh_inc <- ifelse(demographics_14$hh_inc %in% c(1,2,3,4), 1, demographics_14$hh_inc)
demographics_14$hh_inc <- ifelse(demographics_14$hh_inc %in% c(5,6), 2, demographics_14$hh_inc)
demographics_14$hh_inc <- ifelse(demographics_14$hh_inc %in% c(7), 3, demographics_14$hh_inc)
demographics_14$hh_inc <- ifelse(demographics_14$hh_inc %in% c(8), 4, demographics_14$hh_inc)
demographics_14$hh_inc <- factor(demographics_14$hh_inc, ordered = TRUE)

demographics_14$race <- factor(demographics_14$race, ordered = FALSE)
demographics_14$province <- factor(demographics_14$province, ordered = FALSE)
demographics_14$metro <- factor(demographics_14$metro, ordered = FALSE)
demographics_14$lang <- factor(demographics_14$lang, ordered = FALSE)
demographics_14$lifestages <- factor(demographics_14$lifestages, ordered = FALSE)
demographics_14$mar_status <- factor(demographics_14$mar_status, ordered = FALSE)
# demographics_14$pers_inc <- factor(demographics_14$pers_inc, ordered = TRUE)

# lsm
demographics_14$lsm <- ifelse(demographics_14$lsm %in% c(1,2), 1, demographics_14$lsm)
demographics_14$lsm <- ifelse(demographics_14$lsm %in% c(3,4), 2, demographics_14$lsm)
demographics_14$lsm <- ifelse(demographics_14$lsm %in% c(5,6), 3, demographics_14$lsm)
demographics_14$lsm <- ifelse(demographics_14$lsm %in% c(7,8), 4, demographics_14$lsm)
demographics_14$lsm <- ifelse(demographics_14$lsm %in% c(9,10), 5, demographics_14$lsm)
demographics_14$lsm <- factor(demographics_14$lsm, ordered = TRUE)

demographics_14$lifestyle <- factor(demographics_14$lifestyle, ordered = FALSE)
demographics_14$attitudes <- factor(demographics_14$attitudes, ordered = FALSE)



# #create single dataset minus non metropolitans
set14 <- demographics_14 %>%
        left_join(media_type_14) %>%
        left_join(media_vehicles_14) %>%
        filter(metro != 0)
set14_simple <- demographics_14 %>%
        left_join(media_type_14_simple) %>%
        left_join(media_vehicles_14_simple) %>%
        filter(metro != 0)

# consider some correlations

png('corTypePlot2014.jpeg')
corrplot(cor(set14[,c("newspapers","magazines","radio", "tv", "internet")]),
         method = "pie",
         order = "hclust",
         hclust.method = "complete",
         tl.col = 'black',
         mar = c(1,1,1,1),
         addCoefasPercent = TRUE,
         tl.pos = TRUE)
dev.off()

# # consider some clustering
# # construct distance matrix for newspapers, magazines, radio, tv and internet engagement:
# 
# dist14 <- dist(set14[,c("newspapers","magazines","radio", "tv", "internet")])
# clust14 <- hclust(dist14, method = "complete")
# plot(clust14) # messy, unhelpful

## consider kmeans
wss <- vector()
for(k in c(3,4,5,6,7,8,9,10,11,14)) {
        temp <- kmeans(set14[,c("newspapers","magazines","radio", "tv", "internet")],
                       centers = k,
                       nstart = 3,
                       iter.max = 20)
        wss <- append(wss,temp$tot.withinss)
}

png('kmeansTypePlot2014.png')
plot(c(3,4,5,6,7,8,9,10,11,14), wss, type = "b", xlab = "k-values", ylab = "total within sum of squares" )
dev.off()

set.seed(56)
kmeans14 <- kmeans(set14[,c("newspapers","magazines","radio", "tv", "internet")],
                   centers = 5,
                   nstart = 20,
                   iter.max = 20)
set.seed(56)
kmeans14_simple <- kmeans(set14_simple[,c("newspapers","magazines","radio", "tv", "internet")],
                   centers = 5,
                   nstart = 20)

table(kmeans14$cluster) #

# add cluster labels to the dataset
set14 <- set14 %>%
        mutate(cluster = factor(kmeans14$cluster))
set14_simple <- set14_simple %>%
        mutate(cluster = factor(kmeans14_simple$cluster))


# trying out idea of first pc scores as measure of media type mix...kinda engagement...think about this
pc_type <- princomp(set14[,c('newspapers', 'magazines', 'tv', 'radio', 'internet')])
screeplot(pc_type, type = "lines")

set14 <- set14 %>%
        mutate(typePC = scale(pc_type$scores[,1]))

pc_type_simple <- princomp(set14_simple[,c('newspapers', 'magazines', 'tv', 'radio', 'internet')])
screeplot(pc_type_simple, type = "lines")

set14_simple <- set14_simple %>%
        mutate(typePC = scale(pc_type_simple$scores[,1]))





saveRDS(set14, "set14.rds")
saveRDS(set14_simple, "set14_simple.rds")

set14 <- readRDS("set14.rds")
set14_simple <- readRDS("set14_simple.rds")

# consider multidimensional scaling and self organising maps on the clusters :

# 1st create a subset to ensure easier running
set.seed(56)
sub14 <- set14[sample(nrow(set14), size = 1000),]

# distance matrix and MDS
sub14_dist <- dist(sub14[,c("newspapers","magazines","radio", "tv", "internet")])
mds14 <- cmdscale(sub14_dist)
plot(mds14, col = as.numeric(sub14$cluster) + 1, pch = 19, ylab = "", xlab = "")

# 3D scaling
mds3 <- cmdscale(dist(sub14[,c("newspapers", "magazines", "radio", "tv", "internet")]), k = 3)
mds3 <- as.data.frame(mds3)

# 2D & 3D Scatterplots of 5 centers
jpeg('kmeans2DPlot2014.jpeg')
plot(mds14, col = as.numeric(sub14$cluster) + 1, ylab = "", xlab = "", pch = 19)
dev.off()

jpeg('kmeans3DPlot2014.jpeg')
scatterplot3d(mds3, color = as.numeric(sub14$cluster) + 1, xlab = '', ylab = '', zlab = '')
dev.off()

# Spinning 3D for 5 classes
jpeg('kmeansSpinningPlot2014.png')
plot3d(jitter(mds3$V1), jitter(mds3$V2), jitter(mds3$V3), col= as.numeric(sub14$cluster) + 1, size=5, xlab = '', ylab = '', zlab = '', pch = 19)
dev.off()

# try some Self Organising Maps.... try to explain the differences....

# set up somgrid
grid <- somgrid(xdim = 10, ydim = 10, topo = "hexagonal")

# run som
# set up as data matrix
mat_sub <- as.matrix(sub14[,c('newspapers', 'magazines', 'radio', 'tv','internet')])
som_sub <- som(mat_sub, grid = grid, rlen = 10000) 

par(mfrow = c(1,1))
plot(som_sub, type = "codes")
plot(som_sub, type = "changes")
plot(som_sub, type = "counts")
plot(som_sub, type = "dist.neighbours")
plot(som_sub, type = "quality")

par(mfrow = c(3,2))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,1], main = names(sub14['newspapers']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,2], main = names(sub14['magazines']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,3], main = names(sub14['radio']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,4], main = names(sub14['tv']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,5], main = names(sub14['internet']))

par(mfrow = c(1,1))
plot(som_sub, type = "mapping", bgcol = sub14$cluster ) # not very good organising??

# Try pca to get sense of relative use of media type... not very helpful since in most cases require many components to reflect variation in the data.

mags_pca <- princomp(scale(magazines_engagement_14))
screeplot(mags_pca, type = "lines")
newsp_pca <- princomp(scale(newspapers_engagement_14))
screeplot(newsp_pca, type = "lines")
tv_pca <- princomp(scale(tv_engagement_14))
screeplot(tv_pca, type = "lines")
rad_pca <- princomp(scale(radio_engagement_14[,-60])) # cant divide by zero
screeplot(rad_pca, type = "lines")
int_pca <- princomp(scale(internet_engagement_14))
screeplot(int_pca, type = "lines")

all_pca <- princomp(set14[,c('newspapers','magazines', 'tv', 'radio', 'internet')])
screeplot(all_pca, type = "lines")
summary(all_pca) # first component could be useful (@~40% of variation) to give relative multimedia scores

# try kmeans on the first pca and compare with cluster values...
test <- kmeans(all_pca$scores[,1], centers = 6)
test$cluster
set14$cluster
cor(test$cluster, as.numeric(set14$cluster))

# consider for some predictions:
# create training and test sets:

set.seed(56)
ind_train <- createDataPartition(set14$cluster, p = 0.7, list = FALSE)
training <- set14[ind_train,]
testing <- set14[-ind_train,]

# # using random forest:
forest14_type <- randomForest(cluster ~ newspapers
                              + tv
                              + radio
                              + magazines
                              + internet,
                              data = training )

pred_forest14_type <- predict(forest14_type, newdata = testing)

confusionMatrix(pred_forest14_type, testing$cluster) 

# with lda. Although given accuracy of forest,  no real need.
set.seed(56)
lda14 <- lda(cluster ~ newspapers
             + tv
             + radio
             + magazines
             + internet,
             data = training)
summary(lda14)

pred_lda14 <- predict(lda14, newdata = testing)
confusionMatrix(pred_lda14$class, testing$cluster) # 

# using only demographic information
forest14_demogr <- randomForest(cluster ~ age
                                + sex
                                + edu
                                + hh_inc
                                + race
                                + lang
                                + lifestages
                                + mar_status
                                + lsm
                                + lifestyle
                                + attitudes,
                                data = training)

pred_forest14_demogr <- predict(forest14_demogr, newdata = testing)

confusionMatrix(pred_forest14_demogr, testing$cluster)

# with lda
set.seed(56)
lda14_demogr <- lda(cluster ~ age
                    + sex
                    + edu
                    + hh_inc
                    + race
                    + lang
                    + lifestages
                    + mar_status
                    + lsm
                    + lifestyle
                    + attitudes,
                    data = training)

pred_lda14_demogr <- predict(lda14_demogr, newdata = testing)
confusionMatrix(pred_lda14_demogr$class, testing$cluster)

##  some qualitative consideration of the four types:

# consider a single tree partitioning to try to add meaning to the six clusters
control <- rpart.control(maxdepth = 4, cp = 0.001)
tree14 <- rpart(cluster ~ newspapers + tv + radio + magazines + internet, 
                data = set14,
                control = control) # weights = pwgt
par(mfrow = c(1,1))
plot(tree14, uniform = TRUE, margin = 0.2)
text(tree14, pretty = 0, cex = 0.8)

# for more detail
rpart.plot(tree14, type = 4, extra = 1, cex = 0.5)

percentile <- ecdf(set14$internet)
percentile(1.4)

# some plots
jpeg('typeBoxPlots_14.jpeg', quality = 100, type = "cairo")
par(mfrow = c(2,3))
plot(set14$radio ~ set14$cluster, col = c(2,3,4,5,6), main = "radio", xlab = "cluster", ylab = '')
plot(set14$tv ~ set14$cluster, col = c(2,3,4,5,6), main = "tv", xlab = "cluster", ylab = '')
plot(set14$newspapers ~ set14$cluster, col = c(2,3,4,5,6), main = "newspapers", xlab = "cluster", ylab = '')
plot(set14$magazines ~ set14$cluster, col = c(2,3,4,5,6), main = "magazines", xlab = "cluster", ylab = '')
plot(set14$internet ~ set14$cluster, col = c(2,3,4,5,6), main = "internet", xlab = "cluster", ylab = '')
dev.off()

# try to make sense of demographics
jpeg('typeDemogPlots1_14.jpeg', quality = 100, type = "cairo")
par(mfrow = c(2,2))
plot(set14$cluster ~ factor(set14$race,labels = c("black", "coloured", "indian", "white")), col = c(2,3,4,5,6), main = "race", xlab = "", ylab = "")
plot(set14$cluster ~ factor(set14$edu, labels = c("<matric", "matric",">matric" )), col = c(2,3,4,5,6), main = "education", xlab = "", ylab = "")
plot(set14$cluster ~ factor(set14$age, labels = c("15-24","25-44", "45-54","55+")), col = c(2,3,4,5,6), main = "age", xlab = "", ylab = "")
plot(set14$cluster ~ factor(set14$lsm, labels = c("1-2", "3-4", "5-6", "7-8", "9-10")), col = c(2,3,4,5,6), main = "LSM", xlab = "", ylab = "")
dev.off()

jpeg('typeDemogPlots2_14.jpeg', quality = 100, type = "cairo")
par(mfrow = c(2,2))
plot(set14$cluster ~ factor(set14$sex, labels = c("male", "female")), col = c(2,3,4,5,6), main = "sex", xlab = "", ylab = "")
plot(set14$cluster ~ factor(set14$hh_inc, labels = c("<2500","2500-6999","7000-11999",">=14000")), col = c(2,3,4,5,6), main = "hh_inc", xlab = "", ylab = "")
plot(set14$cluster ~ set14$lifestages, col = c(2,3,4,5,6), main = "lifestages", xlab = "", ylab = "")
plot(set14$cluster ~ set14$lifestyle, col = c(2,3,4,5,6), main = "lifestyle", xlab = "", ylab = "")
dev.off()
