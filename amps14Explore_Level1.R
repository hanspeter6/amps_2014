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
library(ggplot2)

#  read in datasets
set14 <- readRDS("set14.rds")
set14_simple <- readRDS("set14_simple.rds")


# consider some correlations
png('corTypePlot2014.png')
corrplot(cor(set14_other[,c("newspapers","magazines","radio", "tv", "internet")]),
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
# dist14 <- dist(set14_other[,c("newspapers","magazines","radio", "tv", "internet")])
# clust14 <- hclust(dist14, method = "complete")
# plot(clust14) # messy, unhelpful

## consider kmeans
wss <- vector()
set.seed(123)
for(k in c(1,2,3,4,5,6)) {
        temp <- kmeans(set14[,c("newspapers","magazines","radio", "tv", "internet")],
                       centers = k,
                       nstart = 5,
                       iter.max = 30)
        wss <- append(wss,temp$tot.withinss)
}

png('kmeansTypePlot2014.png')
plot(c(1,2,3,4,5,6), wss, type = "b", xlab = "k-values", ylab = "total within sum of squares" )
dev.off()

set.seed(123)
kmeans14 <- kmeans(set14[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                   centers = 4,
                   nstart = 5,
                   iter.max = 100)
set.seed(123)
kmeans14_simple <- kmeans(set14_simple[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                          centers = 4,
                          nstart = 5,
                          iter.max = 100)

# Comparing 2014 with 2012... will change colours if necessary to reflect meaning based on 2012:

# green to green:  2 to 2
# lilac to blue: 4 to 3
# blue to lilac: 3 to 4
# red to red: 1 to 1
kmeans14_simple$cluster <- ifelse(kmeans14_simple$cluster == 1, 6, kmeans14_simple$cluster)
kmeans14_simple$cluster <- ifelse(kmeans14_simple$cluster == 2, 7, kmeans14_simple$cluster)
kmeans14_simple$cluster <- ifelse(kmeans14_simple$cluster == 3, 9, kmeans14_simple$cluster)
kmeans14_simple$cluster <- ifelse(kmeans14_simple$cluster == 4, 8, kmeans14_simple$cluster)
kmeans14_simple$cluster <- kmeans14_simple$cluster - 5

# add cluster labels to the dataset
set14c <- set14 %>%
        mutate(cluster = factor(kmeans14$cluster)) %>%
        dplyr::select(qn, pwgt, cluster, everything())
set14c_simple <- set14_simple %>%
        mutate(cluster = factor(kmeans14_simple$cluster)) %>%
        dplyr::select(qn, pwgt, cluster, everything())

saveRDS(set14c, "set14c.rds")
saveRDS(set14c_simple, "set14c_simple.rds")

set14c <- readRDS("set14c.rds")
set14c_simple <- readRDS("set14c_simple.rds")


# some plots
# boxplots of clusters and media types
p1 <- ggplot(set14c_simple, aes(cluster, all, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "all")
p2 <- ggplot(set14c_simple, aes(cluster, newspapers, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "newspapers")
p3 <- ggplot(set14c_simple, aes(cluster, magazines, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "magazines")
p4 <- ggplot(set14c_simple, aes(cluster, radio, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "radio")
p5 <- ggplot(set14c_simple, aes(cluster, tv, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "tv")
p6 <- ggplot(set14c_simple, aes(cluster, internet, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "internet")

jpeg('typeBoxPlots_14_simpe.jpeg', quality = 100, type = "cairo")
grid.arrange(p1, p2, p3, p4, p5,p6,  ncol=3, nrow = 2)
dev.off()

# try to make sense of demographics
d1 <- ggplot(set14c_simple, aes(race, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "race", y = "", x = "") +
        scale_x_discrete(labels=c("black", "coloured", "indian", "white"))
d2 <- ggplot(set14c_simple, aes(edu, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "education", y = "", x = "") +
        scale_x_discrete(labels=c("<matric", "matric",">matric"))
d3 <- ggplot(set14c_simple, aes(age, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "age", y = "", x = "") +
        scale_x_discrete(labels=c("15-24","25-44", "45-54","55+"))
d4 <- ggplot(set14c_simple, aes(lsm, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "lsm", y = "", x = "") +
        scale_x_discrete(labels=c("1-2", "3-4", "5-6", "7-8", "9-10"))

jpeg('typeDemogPlots1_14_simple.jpeg', quality = 100, type = "cairo")
grid.arrange(d1, d2, d3, d4, ncol=2, nrow = 2)
dev.off()

d5 <- ggplot(set14c_simple, aes(sex, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "gender", y = "", x = "") +
        scale_x_discrete(labels=c("male", "female"))
d6 <- ggplot(set14c_simple, aes(hh_inc, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "household income", y = "", x = "") +
        scale_x_discrete(labels=c("<5000","5000-10999","11000-19999",">=20000"))
d7 <- ggplot(set14c_simple, aes(lifestages, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "lifestages", y = "", x = "")# +
# scale_x_discrete(labels=c("<5000","5000-10999","11000-19999",">=20000"))
d8 <- ggplot(set14c_simple, aes(lifestyle, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "lifestyle", y = "", x = "")# +
# scale_x_discrete(labels=c("<5000","5000-10999","11000-19999",">=20000"))
jpeg('typeDemogPlots2_14_simple.jpeg', quality = 100, type = "cairo")
grid.arrange(d5, d6, d7, d8, ncol=2, nrow = 2)
dev.off()









# consider multidimensional scaling and self organising maps on the clusters :

# 1st create a subset to ensure easier running
set.seed(56)
sub14 <- set14_other[sample(nrow(set14_other), size = 1000),]

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



# consider for some predictions:
# create training and test sets:

set.seed(56)
ind_train <- createDataPartition(set14_other$cluster, p = 0.7, list = FALSE)
training <- set14_other[ind_train,]
testing <- set14_other[-ind_train,]

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
                data = set14_other,
                control = control) # weights = pwgt
par(mfrow = c(1,1))
plot(tree14, uniform = TRUE, margin = 0.2)
text(tree14, pretty = 0, cex = 0.8)

# for more detail
rpart.plot(tree14, type = 4, extra = 1, cex = 0.5)

percentile <- ecdf(set14_other$internet)
percentile(1.4)
